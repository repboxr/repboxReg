# Split Stata regression commands into parts that will be stored in cmdpart

# This code adapts much of the code from stata_reg_info.R

cp_init = function(str, org_rows=NULL, str_part = "main") {
  cp = list(
    str=str,
    org_rows = org_rows,
    n = 0,
    start = rep(1, length(str)),
    df = cp_add_empty_df(df = NULL, size=max(length(str)*20))
  )
  if (!is.null(str_part)) {
    cp = cp_add_part_in_df(cp, seq_along(str),str_part,str,tag="", parent="")
  }

  cp
}

cp_add_empty_df = function(df = NULL,min_size=NROW(df), size=max(min_size,NROW(df))) {
  new_df = tibble(
    str_row = rep(NA, size),
    parent = rep("", size),
    part = rep("", size),
    content = rep("", size),
    tag = rep("",size),
    counter = rep(0, size)
    #ph = rep("", size)
  )
  if (is.null(df)) return(new_df)
  bind_rows(df, new_df)
}

cp_add_part_in_df = function(cp, str_rows, part, content, tag=NA, parent="main", counter=rep(0, length(part))) {
  restore.point("cp_add_part_in_df")
  n_add = max(length(str_rows), length(part), length(content))
  if (n_add == 0) return(cp)
  if (cp$n + n_add >= NROW(cp$df)) {
    cp$df = cp_add_empty_df(cp$df, n_add*2)
  }
  inds = (cp$n+1):(cp$n+n_add)

  cp$df$parent[inds] = parent
  cp$df$str_row[inds] = str_rows
  cp$df$part[inds] = part
  cp$df$content[inds] = content
  cp$df$tag[inds] = tag
  cp$df$counter[inds] = counter
  cp$n = cp$n + n_add
  cp
}

# Add extracted parts to cp
cp_add = function(cp, str_rows, start, end, part, tag=NA, use_counter=FALSE,ignore.right.ws=FALSE, parent="main") {
  restore.point("cp_add")
  n_add = length(str_rows)
  if (n_add==0) return(cp)
  if (cp$n + n_add >= NROW(cp$df)) {
    cp$df = cp_add_empty_df(cp$df, n_add*2)
  }

  inds = (cp$n+1):(cp$n+n_add)
  if (use_counter) {
    prev_count_df = cp$df[cp$df$part == part & cp$df$str_row %in% unique(str_rows),] %>%
      group_by(str_row) %>%
      summarize(
        count = n()
      )
    count_df = tibble(str_row = str_rows) %>%
      left_join(prev_count_df, by = "str_row")
    count_df$count[is.na(count_df$count)] = 0
    count_df$count = count_df$count+1


    counter = count_df$count[match(str_rows, count_df$str_row)]
    cp$df$counter[inds] = counter

    ph = paste0("{{", part, counter,"}}")
  } else {
    counter = rep(0, length(str_rows))
    ph = paste0("{{", part,"}}")
  }

  sstr = substring(cp$str[str_rows], start, end)
  if (ignore.right.ws) {
    len_ws = nchar(sstr) - nchar(trimws(sstr,"right"))
    end = end - len_ws
    sstr = substring(cp$str[str_rows], start, end)
  }


  content = sstr
  cp$df$parent[inds] = parent
  cp$df$str_row[inds] = str_rows
  cp$df$part[inds] = part
  cp$df$content[inds] = content
  cp$df$tag[inds] = tag
  cp$df$counter[inds] = counter
  #cp$df$ph[inds] = ph

  # Use stringi:stri_sub_replace since substring <- requires replacement
  # to be of same than original string
  cp$str[str_rows] = stringi::stri_sub_replace(cp$str[str_rows], from=start, to=end, replacement=ph)

  cp$added_inds = inds
  cp$start[str_rows] = cp$start[str_rows]+nchar(ph)
  cp$n = cp$n + n_add

  cp
}

cp_add_starts_with = function(cp, patterns, part, tag, use_counter = FALSE, ignore.right.ws = TRUE) {
  restore.point("set_cmdpart_starts_with")
  cp$did_change = FALSE
  unused = rep(TRUE, length(str))
  start = cp$start
  sstr = substring(cp$str, cp$start)

  s = patterns[1]
  for (s in patterns) {
    str_rows = which(startsWith(sstr, s) & unused)
    if (length(str_rows)>0) {
      if (ignore.right.ws) {
        s = trimws(s, "right")
      }
      len = nchar(s)

      end = cp$start[str_rows]+len-1
      cp = cp_add(cp, str_rows = str_rows, start=start[str_rows], end=end, part=part, tag=tag, use_counter=use_counter)
      unused[str_rows] = FALSE
      cp$did_change = TRUE
    }
  }
  cp
}



find_ws_around = function(txt) {
  txt2 = trimws(txt,"left")
  ws_left = nchar(txt) - nchar(txt2)
  txt3 = trimws(txt2,"right")
  ws_right = nchar(txt2) - nchar(txt3)
  list(ws_left = ws_left, ws_right=ws_right, txt = txt3)
}

# TO DO: Allow to ignore white spaces on the left, but keep them in str
cp_add_left_of = function(cp, left_of, part, tag, use_counter = FALSE, include_split=FALSE, ignore.right.ws = TRUE, fixed=TRUE) {
  restore.point("cp_add_left_of")
  start = cp$start
  sstr = substring(cp$str, cp$start)

  left = left_of(sstr, left_of, fixed=fixed, not.found = rep(NA, length(sstr)))
  str_rows = which(!is.na(left))

  if (length(str_rows)==0) {
    cp$did_change = FALSE
    return(cp)
  }
  start = start[str_rows]
  left = left[str_rows]

  if (ignore.right.ws) {
    left = trimws(left, "right")
  }

  if (include_split) {
    left = paste0(left, left_of)
  }

  len = nchar(left)
  end = start+len-1
  cp = cp_add(cp, str_rows = str_rows, start=start, end=end, part=part, tag=tag, use_counter=use_counter)
  cp$did_change = TRUE

  cp
}

cp_jump_ws = function(cp) {
  sstr = substring(cp$str, cp$start)

  left_ws = nchar(sstr) - nchar(trimws(sstr,"left"))
  cp$start = cp$start + left_ws
  cp
}

example = function() {
  cmdlines = c(
    "ivregress 2sls y1 x1 (y2 y3 = z1 z2), vce(robust)",
    "capt: xi: regress y i.i1##c.d1 [aw=x] if a == 5 in 3, vce(hc2)",
    #"capture quietly xi: regress y i.i1##c.d1, vce(hc2)",
    "capt: regress y i.i1##c.d1 if a==5 [aw=z], vce (robust)  opt2(arg2 = fun( funarg )) noarg"
  )
  options(warn=2)
  df = cmdparts_of_stata_reg(cmdlines)
}



cmdparts_of_stata_reg = function(cmdlines) {
  restore.point("stata_reg_cmdpart")


  str = trimws(cmdlines)
  # Replace tabs with spaces
  # Otherwise we wont correctly store the cmd
  # variable
  str = gsub("\t"," ", str, fixed=TRUE)

  cp = cp_init(str)

  while(TRUE) {
    cp = cp_jump_ws(cp)
    cp = cp_add_starts_with(cp,c("quietly:","quiet:","qui:","quietly ","quiet ","qui ", "capture ","capture:","cap ", "cap:","capt ", "capt:"),"pre","cap_quiet",use_counter=TRUE)
    if (!cp$did_change) break
  }
  cp$str
  substring(cp$str, cp$start)

  # Find colon prefixes before command
  while(TRUE) {
    cp = cp_jump_ws(cp)
    cp = cp_add_left_of(cp,":","pre","",use_counter=TRUE, include_split=TRUE)
    if (!cp$did_change) break
  }

  cp$str
  substring(cp$str, cp$start)
  # Set brackets () into ph
  # res = set_cmdpart_block(str, cp, "(", ")","()","",counter=TRUE)
  # str = res$str; cp = res$cp

  cp = cp_jump_ws(cp)
  cp = cp_add_left_of(cp,"( )|(,)|$|(\\[)", "cmd", "",fixed = FALSE)



  cp = cp_jump_ws(cp)
  cp = cp_add_left_of(cp, "( in )|( if )|(\\[)|,|$", "varlist","", use_counter=FALSE, fixed=FALSE)



  # varlist ############################################


  cmds = cp$df$content[cp$df$part=="cmd"]
  vl_rows = which(cp$df$part=="varlist")
  varlists = cp$df$content[vl_rows]
  i = 2
  for (i in seq_along(cmds)) {

    rv = parse.stata.reg.vars(cmds[i], varlists[i])

    # The varlist contains another cmd specifier (case for ivregress)
    if (!is.empty(rv$method)) {
      cp$str[i] = gsub("{{varlist}}","{{subcmd}} {{varlist}}", cp$str[[i]], fixed=TRUE)
      cp$start[i] = cp$start[i] + 11
      cp = cp_add_part_in_df(cp, str_rows = i, part = "subcmd", content = rv$method)
    }

    exo = rv$exo_parts[[1]]
    endo = rv$endo_parts[[1]]
    instr = rv$instr_parts[[1]]

    vars = c(rv$depvar,exo, endo,instr)
    tags = c(
      rep("depvar", length(rv$depvar)),
      rep("exo", length(exo)),
      rep("endo", length(endo)),
      rep("instr", length(instr))
    )
    cp = cp_add_part_in_df(cp, str_rows = i, part = "v", content=vars, tag=tags, counter=seq_along(vars), parent="varlist")
    if (is.empty(endo)) {
      cont = paste0("{{v", seq_along(vars),"}}", collapse=" ")
    } else {
      len1 = length(rv$depvar) + length(exo)
      cont = paste0(
        paste0("{{v", seq_len(len1),"}}", collapse=" "),
        " (",
          paste0("{{v", seq_along(endo)+len1,"}}", collapse=" "),
        " = ",
          paste0("{{v", seq_along(instr)+length(endo)+len1,"}}", collapse=" "),
        ")"
      )
    }
    cp$df$content[vl_rows[i]] = cont
  }

  df = cp$df


  # weight_str, in_str and if_atr #################################

  # Now it is getting a bit more complicated
  # We want to set if_str, in_str and weight_str
  # but they may appear in different orders or not at all

  # a) weight_str
  sub_start = cp$start
  sub_end = stri_locate_first_fixed(cp$str,",")[,1]-1
  sub_end = ifelse(is.na(sub_end),nchar(cp$str), sub_end )

  sstr = substring(cp$str, sub_start, sub_end)

  weight_start = stri_locate_first_fixed(sstr, "[")[,1] + sub_start -1
  weight_end = stri_locate_first_fixed(sstr, "]")[,2] + sub_start -1


  str_rows = which(!is.na(weight_start))

  if (length(str_rows)>0) {
    weight_str = substring(cp$str[str_rows], weight_start[str_rows], weight_end[str_rows])

    cp$str[str_rows] = stringi::stri_sub_replace(cp$str[str_rows], from=weight_start[str_rows], to=weight_end[str_rows], replacement="{{weight_str}}")



    equal_pos = stringi::stri_locate_first_fixed(weight_str,"=")[,1]
    has_weight_type = !is.na(equal_pos)
    weight_type = ifelse(has_weight_type,substring(weight_str,2, equal_pos-1) %>% trimws(), NA)
    var_start = ifelse(has_weight_type,equal_pos+1,2 )
    weight_var = substring(weight_str,var_start, nchar(weight_str)-1) %>% trimws()
    weight_str = ifelse(has_weight_type,
      paste0("[{{weight_type}}={{weight_var}}]"),
      paste0("[{{weight_var}}]")
    )

    n_add = NROW(weight_str)*3
    if (cp$n + n_add >= NROW(cp$df)) {
      cp$df = cp_add_empty_df(cp$df, n_add*2)
    }

    # 1a) add weight_str
    new.n = cp$n+length(weight_str)
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = "main"
    cp$df$str_row[inds] = str_rows
    cp$df$part[inds] = "weight_str"
    cp$df$content[inds] = weight_str
    cp$n = new.n

    # 1b) Add weight_var
    new.n = cp$n+length(weight_str)
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = "weight_str"
    cp$df$str_row[inds] = str_rows
    cp$df$part[inds] = "weight_var"
    cp$df$content[inds] = weight_var
    cp$n = new.n

    # 1c) Add weight_type
    new.n = cp$n+length(weight_str)
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = "weight_type"
    cp$df$str_row[inds] = str_rows
    cp$df$part[inds] = "weight_type"
    cp$df$content[inds] = weight_type

    cp$n = new.n

  }

  # 2. if_str
  sub_end = stri_locate_first_fixed(cp$str,",")[,1]-1
  sub_end = ifelse(is.na(sub_end),nchar(cp$str), sub_end )
  sstr = substring(cp$str, sub_start, sub_end)

  if_start = stri_locate_first_fixed(sstr, " if ")[,1] + sub_start
  sstr = substring(sstr, if_start-sub_start+1)
  if_end = stri_locate_first_regex(sstr, "( in )|(\\{\\{)")[,1] + if_start - 2
  if_end = ifelse(is.na(if_end),sub_end, if_end)

  str_rows = which(!is.na(if_start))
  if_start = if_start[str_rows]; if_end = if_end[str_rows]
  cp = cp_add(cp,str_rows,if_start,if_end,"if_str","",ignore.right.ws=TRUE)

  cp$str

  # 3. in_str

  sub_end = stri_locate_first_fixed(cp$str,",")[,1]-1
  sub_end = ifelse(is.na(sub_end),nchar(cp$str), sub_end )
  sstr = substring(cp$str, sub_start, sub_end)

  in_start = stri_locate_first_fixed(sstr, " in ")[,1] + sub_start
  sstr = substring(sstr, in_start-sub_start+1)
  in_end = stri_locate_first_fixed(sstr, "{{")[,1] + in_start - 2
  in_end = ifelse(is.na(in_end),sub_end, in_end)

  str_rows = which(!is.na(in_start))
  in_start = in_start[str_rows]; in_end = in_end[str_rows]
  cp = cp_add(cp,str_rows,in_start,in_end,"in_str","",ignore.right.ws=TRUE)

  cp$str

  # options ###########################################

  start = stri_locate_first_regex(cp$str,",")[,1]+1
  end = nchar(cp$str)
  str_rows = which(!is.na(start))

  if (length(str_rows) > 0) {
    start = start[str_rows]; end = end[str_rows]
    all_opt_str = substring(cp$str[str_rows],start)
    res = cmdpart_parse_stata_opt_str(all_opt_str)

    # Replace str
    opt_ph = sapply(res$opt_str, function(x) paste0("{{opt_str", seq_along(x),"}}", collapse=" "))
    cp$str[str_rows] = stringi::stri_sub_replace(cp$str[str_rows], from=start, to=end, replacement=opt_ph)

    lens = lapply(res$opt_str,length)

    opt_df = tibble(
        str_row = unlist(mapply(rep,x=str_rows, times=lens,SIMPLIFY = FALSE)),
        parent = "main",
        opt_str = unlist(res$opt_str),
        opt = unlist(res$opt),
        opt_arg = unlist(res$opt_arg)
      ) %>%
      group_by(str_row) %>%
      mutate(opt_num = 1:n()) %>%
      ungroup()

    n_add = NROW(opt_df)*2 + sum(!is.na(opt_df$opt_arg))
    if (cp$n + n_add >= NROW(cp$df)) {
      cp$df = cp_add_empty_df(cp$df, n_add*2)
    }

    # 4a) add opt_str
    new.n = (cp$n+NROW(opt_df))
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = "main"
    cp$df$str_row[inds] = opt_df$str_row
    cp$df$part[inds] = "opt_str"
    cp$df$counter[inds] = opt_df$opt_num
    cp$df$content[inds] = opt_df$opt_str

    cp$n = new.n

    # 4b) add opt
    new.n = (cp$n+NROW(opt_df))
    inds = (cp$n+1):new.n
    cp$df$parent[inds] = paste0("opt_str")
    cp$df$str_row[inds] = opt_df$str_row
    cp$df$part[inds] = "opt"
    cp$df$counter[inds] = opt_df$opt_num
    cp$df$content[inds] = opt_df$opt

    cp$n = new.n

    # 4c) add opt_arg where it exists
    rows = which(!is.na(opt_df$opt_arg))

    if (length(rows)>0) {
      new.n = cp$n+length(rows)
      inds = (cp$n+1):new.n
      cp$df$parent[inds] = "opt_str"
      cp$df$str_row[inds] = opt_df$str_row[rows]
      cp$df$part[inds] = "opt_arg"
      cp$df$counter[inds] = opt_df$opt_num[rows]
      cp$df$content[inds] = opt_df$opt_arg[rows]

      cp$n = new.n
    }

  }

  # adapt tags and df ##########################################

  df = cp$df[seq_len(cp$n),]
  rows = which(df$tag == "cap_quiet")
  if (length(rows)>0) {
    is_capture = has.substr(df$content[rows], "cap")
    df$tag[rows[is_capture]] = "capture"
    df$tag[rows[!is_capture]] = "quietly"
  }

  cont = trimws(df$content)
  rows = which(df$part == "pre")
  if (length(rows) > 0) {
    df$content[rows] = gsub("[ \t]+\\:",":", df$content[rows])
    irows = rows[df$content[rows]] == "xi:"
    df$tag[irows] = "xi"
  }

  rows = which(df$part == "cmd" | df$part == "subcmd")
  df$tag[rows] = df$content[rows]

  df$content[seq_along(cp$str)] = cp$str


  restore.point("jslkfslfhksdfh")

  opt_rows = which(df$part == "opt" & df$content %in% c("vce","robust","cluster"))
  df$tag[opt_rows] = "se"

  opt_str_rows = cmdpart_find_parent_rows(df, opt_rows)
  df$tag[opt_str_rows] = "se"

  opt_arg_rows = cmdpart_find_child_rows(df, opt_str_rows, "opt_arg") %>% na.omit()
  df$tag[opt_arg_rows] = "se"


  opt_rows = which(df$part == "opt" & df$content %in% c("absorb"))
  df$tag[opt_rows] = "absorb"

  opt_str_rows = cmdpart_find_parent_rows(df, opt_rows)
  df$tag[opt_str_rows] = "absorb"

  opt_arg_rows = cmdpart_find_child_rows(df, opt_str_rows, "opt_arg") %>% na.omit()
  df$tag[opt_arg_rows] = "absorb"


  df
}


cmdpart_parse_stata_opt_str = function(str) {
  restore.point("cmdpart_of_stata_opt_str")

  # str = c(
  #   "vce (robust)  opt2(arg2 = fun( funarg )) noarg",
  #   "absorb(x) robust"
  # )



  all_opt_str = all_opt = all_opt_arg = vector("list",NROW(str))

  i = 3
  for (i in seq_along(str)) {
    s = str[i]
    # 1. Find 1st level braces and replace with ph
    brace_pos = locate_1st_level_braces(s,open="(",close=")")

    if (NROW(brace_pos)==0) {
      s = shorten.spaces(s) %>% trimws()
      all_opt[[i]] = strsplit(s, " ", fixed=TRUE)
      all_opt_str[[i]] = paste0("{{opt,", seq_along(all_opt[[i]]),"}}")
      all_opt_arg[[i]] = rep(NA, length(all_opt[[i]]))
      next
    }

    # 1. Replace 1st level braces with placeholders
    brace_content = substring(s, brace_pos[,1], brace_pos[,2])
    ph = paste0("#~(", seq_len(NROW(brace_pos)),"~#")
    s = str.replace.at.pos(s, brace_pos, ph)

    # 2. Remove all ws before "#~()~#" and shorten ws
    s = gsub("[ \t]+#~\\(","#~(", s)
    s = shorten.spaces(s) %>% trimws()

    # 3. Split by " "
    opt_str = strsplit(s," ", fixed=TRUE)[[1]]

    # 4. locate argument placeholders
    arg_start_pos = stringi::stri_locate_first_fixed(opt_str, "#~(")[,1]
    has_arg = !is.na(arg_start_pos)

    opt_arg = rep(NA, length(opt_str))
    opt_arg[has_arg] = str.remove.ends(brace_content,1,1)

    opt = ifelse(has_arg,substring(opt_str,1, arg_start_pos-1), opt_str)

    all_opt_str[[i]] = opt_str
    all_opt[[i]] = opt
    all_opt_arg[[i]] = opt_arg

    all_opt_str[[i]] = ifelse(has_arg,
      paste0("{{opt", seq_along(opt),"}}({{opt_arg",seq_along(opt),"}})"),
      paste0("{{opt", seq_along(opt),"}}")
    )

  }
  list(
    opt_str = all_opt_str,
    opt = all_opt,
    opt_arg = all_opt_arg
  )
}

locate_1st_level_braces = function(txt, open="(", close = ")") {
  if (length(txt))
  res = str.blocks.pos(txt, open, close)
  res$outer[res$levels==1,,drop=FALSE]
}

cmdpart_get_placeholders = function(cp_df, prefix="{{", postfix="}}") {
  ifelse(is.true(cp_df$counter > 0),
    paste0(prefix, cp_df$part, cp_df$counter, postfix),
    paste0(prefix, cp_df$part, postfix)
  )
}

cmdpart_create_cmdline = function(cp_df) {
  main_row = which(cp_df$part == "main")
  str = cp_df$content[main_row]
  ph = cmdpart_get_placeholders(cp_df)

  stri_replace_all_fixed(str, ph, cp_df$content, vectorize_all = FALSE)
}

cmdpart_find_parent_rows = function(df, rows=seq_along(NROW(df)), remove.na=FALSE) {
  if ("str_row" %in% colnames(df)) {
    part.key = paste0(df$str_row,"|", df$part ,"|",df$counter)
    parent.key = paste0(df$str_row,"|", df$parent,"|",df$counter)
  } else if ("artid" %in% colnames(df)) {
    part.key = paste0(df$artid,"|", df$step, "|", df$part,"|",df$counter)
    parent.key = paste0(df$artid,"|", df$step, "|", df$parent,"|",df$counter)
  } else {
    part.key = paste0(df$step, "|", df$part,"|",df$counter)
    parent.key = paste0(df$step, "|", df$parent,"|",df$counter)
  }
  match(parent.key[rows], part.key)
}

cmdpart_find_child_rows = function(df, rows=seq_along(NROW(df)), child_part) {
  if ("str_row" %in% colnames(df)) {
    part.key = paste0(df$str_row,"|", df$part,"|",df$counter)
    parent.key = paste0(df$str_row,"|", df$parent,"|",df$counter)
  } else if ("artid" %in% colnames(df)) {
    part.key = paste0(df$artid,"|", df$step, "|", df$part,"|",df$counter)
    parent.key = paste0(df$artid,"|", df$step, "|", df$parent,"|",df$counter)
  } else {
    part.key = paste0(df$step, "|", df$part,"|",df$counter)
    parent.key = paste0(df$step, "|", df$parent,"|",df$counter)
  }
  parent.key[df$part != child_part] = NA

  match(part.key[rows], parent.key)

}


example = function() {
  cmdpart = readRDS("C:/libraries/repbox/projects_reg/testsupp/regdb/base_cmdpart.Rds")$cmdpart
  cmdpart_to_opts_df(cmdpart)
}

cmdpart_to_opts_df = function(cmdpart) {
  restore.point("cmdpart_to_opts_df")
  opt_df = filter(cmdpart, part=="opt") %>% select(artid,step, counter, opt=content, tag=tag)
  opt_arg = filter(cmdpart, part=="opt_arg") %>% select(artid, step, counter, opt_arg=content)
  left_join(opt_df, opt_arg, by=c("artid","step","counter"))
}


