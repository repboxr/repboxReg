
example = function() {
  project = "testsupp"
  project_dir = file.path("~/repbox/projects_reg",project)

  rr = readRDS(file.path(project_dir,"repbox/stata/repbox_results.Rds"))
  run.df = rr$run.df
  #set.seed(1234)
  dap = make.dap(run.df)
  step.df = dap$step.df
  rows = step.df$step_type == "mod" & !step.df$need_cache
  r.code = sapply(step.df$stata_code[rows], stata.to.r, merge.lines=TRUE, add.comment=TRUE)
  r.code
  code = step.df$stata_code[[2]]
}

stata.to.r = function(code, merge.lines=FALSE, add.comment=FALSE) {
  restore.point("stata.to.r")
  txt = code
  #txt = sep.lines(code)
  if (any(has.substr(txt, "e(sample)"))) {
    set.repbox.flag("make.is.in.sample",TRUE)
    txt = gsub("e(sample)",".is.in.sample",txt, fixed=TRUE)
  } else {
    set.repbox.flag("make.is.in.sample",FALSE)
  }

  s = repboxStata::repbox.normalize.do(txt)
  res = repboxStata::repbox.do.table(s)
  tab = res$tab
  ph.df = res$ph.df
  tab$code = txt

  r.code = sapply(seq_len(NROW(tab)), function(i) {
    stata.tab.to.r(tab=tab[i,], add.comment=add.comment)
  })
  if (merge.lines) r.code = merge.lines(r.code)
  r.code
}

parse_eval = function(cmd, envir=parent.frame()) {
  if (is.null(cmd)) return(invisible(NULL))
  eval(parse(text=cmd),envir = envir)
}

stata.tab.to.r = function(code=tab$code, tab, just.chain=FALSE, add.comment=FALSE,...) {
  restore.point("stata2r")
  cmd = tab$cmd
  fname = paste0("stata2r.",cmd)
  if (!exists(fname, mode='function')) {
    restore.point("stata2r.not.exist")
    return(paste0("\n# ", code,"\n# NOT TRANSLATED TO R"))
  }
  do.call(fname, list(code=code, tab=tab, just.chain=just.chain, add.comment=add.comment,...))
}

stata2r.gen = function(code=tab$code, tab=NULL, just.chain=FALSE, add.comment=FALSE, from.replace=FALSE) {
  restore.point("stata2r.gen")
  str = code %>% str.right.of(":") %>% str.right.of(":") %>% trimws() %>%
    str.right.of(" ")
  ifcode = str.right.of(str, " if ", not.found = "") %>% trimws()

  str = str.left.of(str, " if ") %>% trimws()

  # replace _n
  str = gsub("\\b_n\\b","i(dat)", str)
  # replace _N
  str = gsub("\\b_N\\b","n()", str)

  # replace [ ]
  str = gsub("[","[rows(",str, fixed=TRUE)
  str = gsub("]",")]",str, fixed=TRUE)


  lhs = str.left.of(str,"=")
  rhs = str.right.of(str, "=")
  rhs = replace.stata.rhs.na(rhs)

  if (!is_empty(ifcode)) {
    restore.point("gen.ifcode")

    # A stata condition like x<-400
    # actually means x < -400
    # but due to (uneccessary!!!) assignment operator <- in R
    # it thinks there should be an assignment
    ifcode = gsub("<-","< -", ifcode, fixed=TRUE)

    # In Stata ~= is a synonym for !=
    ifcode = gsub("~=","!=",ifcode,fixed=TRUE)

    # Replace var == . with is.na(var)
    ifcode = replace.stata.is.na(ifcode)

    if (from.replace) {
      inner = paste0("mutate(",lhs," = ifelse(", ifcode,",",rhs,",",lhs,"))")
    } else {
      inner = paste0("mutate(",lhs," = ifelse(", ifcode,",",rhs,",NA))")
    }
    #inner = paste0("dplyrExtras::mutate_rows(",ifcode, ",", str,")")
  } else {
    inner = paste0("mutate(",lhs," = ", rhs, ")")
  }

  links = s2r.add.by(inner, tab)

  return.stata2r(links,code, just.chain,add.comment)
}

stata2r.replace = function(...) stata2r.gen(..., from.replace=TRUE)
stata2r.generate = function(...) stata2r.gen(...)
stata2r.gener = function(...) stata2r.gen(...)
stata2r.ge = function(...) stata2r.gen(...)
stata2r.g = function(...) stata2r.gen(...)
stata2r.egen = function(...) stata2r.gen(...)

stata2r.keep =  function(code=tab$code, tab=NULL, just.chain=FALSE, add.comment=FALSE, drop=FALSE) {
  restore.point("stata2r.keep")
  links = NULL
  if (!is_empty(tab$arg_str)) {
    vars = separate.stata.arg.str(tab$arg_str)
    if (drop) {
      #links = paste0("select(", paste0("-",vars, collapse=", "),")")
      links = paste0("remove.cols(c(", paste0('"',vars,'"', collapse=", "),"))")
    } else {
      #links = paste0("select(", paste0(vars, collapse=", "),")")
      links = paste0("keep.cols(c(", paste0('"',vars,'"', collapse=", "),"))")
    }
  }
  if (!is_empty(tab$if_arg)) {
    str = replace.stata.is.na(tab$if_arg)
    if (drop) {
      links = c(links,paste0("filter(!(", str, "))"))
    } else {
      links = c(links,paste0("filter(", str, ")"))
    }
  }
  return.stata2r(links,code, just.chain,add.comment)
}



stata2r.preserve = function(code, add.comment=TRUE,...) {
  r = 'saveRDS(dat,"repox_preserve.rds")'
  if (!add.comment) return(r)
  paste0("# ", code,"\n",r)
}


stata2r.restore = function(code, add.comment=TRUE,...) {
  r = 'dat = readRDS("repox_preserve.rds")'
  if (!add.comment) return(r)
  paste0("# ", code,"\n",r)
}

stata2r.drop =  function(...) {
  stata2r.keep(..., drop=TRUE)
}

stata2r.rename =  function(code,tab,...) {
  restore.point("stata2r.rename")
  arg = str.right.of(code, "rename ") %>% trimws()
  links = paste0('stata_rename("',arg,'")')
  return.stata2r(links,code=code, ...)
}


stata2r.xtset =  function(code,tab,add.comment=TRUE,...) {
  restore.point("stata2r.xtset")
  args = str.right.of(code, "xtset ") %>% trimws() %>% str.left.of(",") %>% ws_to_single_space() %>% strsplit(" ") %>% first()
  if (length(args)==2) {
    r = paste0('options(stata.panelvar = "', args[1],'")\n',
               'options(stata.timevar = "', args[2],'")')
    # Stata sorts
    r = paste0(r, "\ndat = arrange(dat,",args[1],",",args[2],")")

  } else if (length(args)==1) {
    r = paste0('options(stata.panelvar = "', args[1],'")')
    # Stata sorts not of only panelvar is provided
  }
  if (!add.comment) return(r)
  paste0("# ", code,"\n",r)
}


stata2r.tsset =  function(code,tab,add.comment=TRUE,...) {
  restore.point("stata2r.tsset")
  args = str.right.of(code, "tsset ") %>% trimws() %>% str.left.of(",") %>% ws_to_single_space() %>% strsplit(" ") %>% first()
  if (length(args)==2) {
    r = paste0('options(stata.panelvar = "', args[1],'")\n',
               'options(stata.timevar = "', args[2],'")')
    # Stata sorts
    r = paste0(r, "\ndat = arrange(dat,",args[1],",",args[2],")")

  } else {
    r = paste0('options(stata.timevar = "', args[1],'")')
    # Stata sorts
    r = paste0(r, "\ndat = arrange(dat,",args[1],")")
  }


  if (!add.comment) return(r)
  paste0("# ", code,"\n",r)
}


separate.stata.arg.str = function(arg_str) {
  vars = trimws(arg_str) %>% trimws_around(" ") %>% strsplit(" ", fixed=TRUE)
  vars[[1]]
}

replace.stata.rhs.na = function(rhs) {
  # Replace . by NA
  # if . is not part of a variable name
  #rhs = c(".","(.)","a.",".b","a.1")

  rx = "(?<![a-zA-Z0-9])\\.(?![a-zA-Z0-9])"
  rhs = gsub(rx,"NA", rhs,perl = TRUE)
  #rhs = gsub(".","NA", rhs)
  rhs
}

replace.stata.is.na = function(str) {
  str = str %>% trimws_around("==") %>% trimws_around("!=")
  str = gsub("([a-zA-Z_0-9]+)==\\.","is.na(\\1)", str)
  str = gsub("([a-zA-Z_0-9]+)!=\\.","!is.na(\\1)", str)
  str
}

return.stata2r = function(links,code, just.chain, add.comment,...) {
  if (just.chain) return(links)
  r = paste0("dat = dat %>%\n  ", paste0(links, collapse=" %>%\n  "))
  if (!add.comment) return(r)
  paste0("# ", code,"\n",r)
}

set.repbox.flag = function(name, val=TRUE) {
  globalenv
  li = list(val)
  names(li)=paste0("repbox.flag.",name)
  do.call(options, li)
}

has.repbox.flag = function(name) {
  isTRUE(getOption(paste0("repbox.flag.",name)))
}

remove.nonreg.preserve.restore = function(bdf) {
  restore.point("remove.nonreg.preserve.restore")
  start = which(bdf$type=="preserve")
  if (length(start)==0) return(bdf)
  end = which(bdf$type=="restore")

  nb = min(length(start),length(end))
  if (nb==0) return(bdf)
  start = start[1:nb]
  end = end[1:nb]

  if (any(end<=start)) return(bdf)

  bdf$.keep = TRUE
  i = 1
  for (i in seq_along(start)) {
    rows = start[i]:end[i]
    has.reg = any(bdf$type[rows]=="reg")
    if (!has.reg) bdf$.keep[rows] = FALSE
  }
  bdf[bdf$.keep,]

}

s2r.add.by = function(links, tab) {
  restore.point("s2r.add.by")
  colon = NULL
  for (col in c(tab$colon1,tab$colon2,tab$colon3)) {
    if (is.na(col)) return(links)
    if (startsWith(col,"by")) {
      colon = trimws(col) %>% trimws_around(col," ")
      break
    }
  }
  if (is.null(colon)) return(links)

  cmd = str.left.of(colon, " ")
  str = str.right.of(colon, " ")
  opt = str.right.of(str, ",",not.found = "") %>% trimws()

  sort = cmd %in% c("bys","bysort") | opt=="sort"

  str = str.left.of(colon,",",not.found = str)
  justsortstr = str.between(str,"(",")", not.found="") %>% trimws()
  groupstr = str.left.of(str,"(", not.found=str) %>% trimws()

  groupcode = groupstr %>% gsub(" ",", ",., fixed=TRUE)
  links = c(
    paste0("group_by(",groupcode,")"),
    links,
    "ungroup()"
  )

  if (sort) {
    sortcode = paste0(groupstr," ",justsortstr) %>% trimws() %>%
      gsub(" ",", ",., fixed=TRUE)
    links = c(
      paste0("arrange(",sortcode,")"),
      links
    )
  }
  links
}
