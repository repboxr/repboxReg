# Should be put into a repbox tools package


example = function() {
  str = "Hallo ((L2) Hier) und (noch) einer 'Mein Name lautet (Hi)'."
  start = "("
  end = ")"
  res = blocks.to.placeholder(str, start=c("'","("), end=c("'",")"))
  res

  res = blocks.to.placeholder(str, start, end)

  str = res$str; ph.df = res$ph.df
  replace.placeholders(str, ph.df)
}



replace.placeholders = function(str, ph.df, recursive = TRUE, rows=NULL) {
  restore.point("ksjlfdlfjdfdfl")
  multiline = length(str)>1

  str = merge.lines(str)
  if (!is.null(rows))
    rows = which(stringi::stri_detect_fixed(str, ph.df$ph))
  for (row in rows) {
    str = gsub(ph.df$ph[row],ph.df$content[row],str,fixed=TRUE)
  }
  if (recursive) {
    rows = which(stringi::stri_detect_fixed(str, ph.df$ph))
    if (length(rows)>0) {
      restore.point("ksjlfdlfjl")
      str = replace.placeholders(str, ph.df, recursive = TRUE, rows=rows)
    }
  }
  if (multiline)
    str = sep.lines(str)
  str
}


replace.ph.keep.lines = function(str, ph.df, recursive = TRUE, rows=NULL, line.temp="<§°>") {
  restore.point("replace.ph.keep.lines")
  temp.ph.df = ph.df %>%
    mutate(content = gsub("\n",line.temp, content, fixed=TRUE))
  str = gsub("\n",line.temp, str, fixed=TRUE)
  str = replace.placeholders(str, temp.ph.df, recursive, rows)
  str = gsub(line.temp,"\n", str, fixed=TRUE)
  str
}


ignore.from.pattern = function(str, pattern, ...) {
  ignore.pos = str.find(str, pattern, ...)
  ignore = get.ignore(ignore.pos = ignore.pos, str = str)
  if (!any(ignore==TRUE)) ignore=NULL
  ignore
}

empty.ph.df = function() {
  data.frame(ph=character(0), content=character(0),stringsAsFactors = FALSE)
}

start.end.line.blocks = function(txt, start, end, multi.end = TRUE, startsWith = FALSE) {
  restore.point("start.end.line.blocks")
  if (!startsWith) {
    start.lines = which(!is.na(match(txt, start)))
    end.lines = which(!is.na(match(txt, end)))
  } else {
    start.lines = which(startsWith(txt,start))
    end.lines = which(startsWith(txt,end))
  }

  ldf = data.frame(type = c(rep("s", NROW(start.lines)), rep("e",NROW(end.lines))), line = c(start.lines, end.lines)) %>% arrange(line)
  if (NROW(ldf)==0 | NROW(start.lines)==0 | NROW(end.lines)==0) return(data.frame(start=integer(0), end=integer(0)))


  if (multi.end) {
    keep = ldf$type=="s" | (ldf$type=="e" & is.true(lag(ldf$type)=="s"))
    ldf = ldf[keep,,drop=FALSE]
  }
  if (any(is.true(ldf$type == lead(ldf$type))) | NROW(ldf)%%2 == 1) {
    stop("could not find matching blocks")
  }
  n = NROW(ldf)
  data.frame(start = ldf$line[seq(1,n,by=2)], end=ldf$line[seq(2,n,by=2)])

}

line.blocks.to.placeholder = function(str, pos, ph.prefix = "#~", ph.suffix="~#", ph.df = empty.ph.df(), ind.start=1) {
  restore.point("line.blocks.to.placeholder")
  n = NROW(pos)
  if (n==0) return(list(str=str, ph.df=ph.df))
  content = sapply(1:n, function(i) {
    merge.lines(str[pos[i,1]:pos[i,2]])
  })
  ph.ind = ind.start:(ind.start+n-1)

  ph = paste0(ph.prefix, ph.ind, ph.suffix)
  ph.df = rbind(ph.df, data.frame(ph=ph, content=content))
  str[pos[,1]] = ph
  del.lines = unlist(lapply(1:n, function(i) {
    if (pos[i,2]>pos[i,1])
      return((pos[i,1]+1):pos[i,2])
    return(NULL)
  }))
  if (length(del.lines)>0) {
    str = str[-del.lines]
  }
  return(list(str=str, ph.df = ph.df))
}

pos.to.placeholder = function(str, pos, ph.prefix = "#~", ph.suffix="~#", ph.df = empty.ph.df(),ind.start=1) {
  restore.point("pos.to.placeholder")
  if (is.list(pos)) {
    if (!is.null(pos$outer))
      pos = pos$outer
  }
  n = NROW(pos)
  if (n==0) return(list(str=str, ph.df=ph.df))
  content = substring(str, pos[,1], pos[,2])
  ph.ind = ind.start:(ind.start+n-1)
  ph = paste0(ph.prefix, ph.ind, ph.suffix)
  ph.df = rbind(ph.df, data.frame(ph=ph, content=content))
  str = str.replace.at.pos(str,pos, new=ph)
  return(list(str=str, ph.df = ph.df))

}

lines.pos.to.placeholder = function(str, lines.pos, ph.prefix = "#~", ph.suffix="~#", ph.df = empty.ph.df(), ind.start=1) {
  restore.point("lines.pos.to.placeholder")
  lines = which(!is.na(lines.pos[,1]) & !is.na(lines.pos[,2]))
  n = length(lines)
  if (n==0) return(list(str=str, ph.df=ph.df))
  lines.pos = lines.pos[lines,,drop=FALSE]

  content = substring(str[lines], lines.pos[,1], lines.pos[,2])
  ph.ind = indstart:(indstart+n-1)
  ph = paste0(ph.prefix, ph.ind, ph.suffix)
  ph.df = rbind(ph.df, data.frame(ph=ph, content=content))
  str[lines] = paste0(
    substring(str[lines], 1, lines.pos[,1]-1),
    ph,
    substring(str[lines], lines.pos[,2]+1, nchar(str[lines]))
  )

  return(list(str=str, ph.df = ph.df))

}

lines.to.placeholder = function(str, lines, ph.prefix = "#~", ph.suffix="~#", ph.df = empty.ph.df(), ind.start=1) {
  restore.point("lines.to.placeholder")
  n = length(lines)
  if (n==0) return(list(str=str, ph.df=ph.df))
  content = str[lines]
  ph.ind = ind.start:(ind.start+n-1)

  ph = paste0(ph.prefix, ph.ind, ph.suffix)
  ph.df = rbind(ph.df, data.frame(ph=ph, content=content))
  str[lines] = ph
  return(list(str=str, ph.df = ph.df))

}

blocks.to.placeholder = function(str,start, end, ph.prefix = "#~", ph.suffix = "~#", ph.df = empty.ph.df(),ignore.pattern=NULL, ignore.start.pattern=ignore.pattern, ignore.end.pattern = ignore.pattern, before.start = NULL, ind.start=1) {
  if (length(start)>1) {
    for (i in seq_along(start)) {
      res = blocks.to.placeholder(str, start[i], end[i], ph.prefix, ph.suffix, ph.df, ignore.start = ignore.start, ignore.end=ignore.end, before.start = before.start, ind.start=ind.start)
      str = res$str; ph.df = res$ph.df
      ind.start = max(as.numeric(str.between(ph.df$ph,ph.prefix,ph.suffix)))+1
      if (isTRUE(length(ind.start)==0 | is.na(ind.start))) ind.start = 1
    }
    return(res)
  }

  restore.point("blocks.to.placeholder")
  #stop()
  stopifnot(length(str)==1)
  old.ph.df = ph.df

  ignore.start = ignore.end = NULL
  if (!is.null(ignore.start.pattern)) {
    ignore.start = ignore.from.pattern(str, ignore.start.pattern)
  }
  if (!is.null(ignore.end.pattern)) {
    ignore.end = ignore.from.pattern(str, ignore.end.pattern)
  }


  pos = suppressMessages(str.blocks.pos.starting.with(str,start = start, end=end, ignore.start = ignore.start, ignore.end=ignore.end, before.start=before.start))

  n = NROW(pos$outer)
  if (n == 0) {
    return(list(str=str, ph.df = ph.df))
  }


  if (max(pos$levels)>1) {
    redo = TRUE
    use = which(pos$levels == max(pos$levels))
    pos$outer = pos$outer[use,,drop=FALSE]
    pos$inner = pos$inner[use,,drop=FALSE]
    pos$levels = pos$levels[use]
    n = NROW(pos$outer)
  } else {
    redo = FALSE
  }


  content = substring(str, first = pos$outer[,1],last=pos$outer[,2])
  ph.ind = ind.start:(ind.start+n-1)
  ph = paste0(ph.prefix, ph.ind, ph.suffix)

  res.str = str.replace.at.pos(str, ph, pos = pos$outer)
  ph.df = rbind(old.ph.df, data.frame(ph=ph, content=content))

  if (redo) {
    res = blocks.to.placeholder(res.str,start = start, end=end,ph.prefix = ph.prefix,ph.suffix = ph.suffix, ph.df = ph.df, ignore.start = ignore.start, ignore.end=ignore.end, before.start = NULL, ind.start = ind.start+n)
    return(res)
  } else {
    return(list(str=res.str, ph.df = ph.df))
  }

}

#str =  '"this \\"name " *5'; stata.strings.to.ph(str)

stata.strings.to.ph = function(str, ...) {
  str = gsub('\\\\\\"','#~~1~~#',str, fixed = TRUE)
  str = gsub('\\\\"','#~~2~~#"',str, fixed = TRUE)
  str = gsub('\\"','#~~3~~#',str, fixed = TRUE)

  res = blocks.to.placeholder(str, start='"', end='"',...)
  #res = blocks.to.placeholder(str, start='"', end='"',...)
  res
}

str.blocks.pos.starting.with = function(
    str,start, end, before.start=NULL,
    ignore = NULL, ignore.start = ignore,
    ignore.end = ignore, fixed = TRUE,
    fixed.start = fixed, fixed.end = fixed, verbose=FALSE) {

  restore.point("str.blocks.pos.starting.with")
  pos = str.blocks.pos(str, start, end,ignore=ignore, ignore.start=ignore.start, ignore.end = ignore.end, fixed=fixed, fixed.start=fixed, fixed.end=fixed, verbose=verbose)
  if (length(before.start)==0 | NROW(pos$outer)==0) return(pos)

  found.which = rep(0,NROW(pos$outer))
  i = 1
  for (i in seq_along(before.start)) {
    s = before.start[i]
    len = nchar(before.start[i])
    before = substring(str,pos$outer[,1]-len,pos$outer[,1]-1)
    found.which[before == s] = i
  }

  rows = found.which > 0
  pos$found.which = found.which[rows]
  pos$outer = pos$org.outer = pos$outer[rows,,drop=FALSE]
  pos$outer[,1] = pos$outer[,1]-nchar(before.start[pos$found.which])
  pos$inner = pos$inner[rows,,drop=FALSE]
  pos$levels = pos$levels[rows]
  return(pos)
}

