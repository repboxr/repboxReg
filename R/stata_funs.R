# Stata functions used by egen and possibly similar functions
# that can be translated to R


make.stata.funs.env = function(parent.env = globalenv()) {
  li = stata.funs()
  env = as.environment(li)
  parent.env(env) = parent.env
  env
}

add.stata.funs.to.globenv = function() {
  stata.funs = stata.funs()
  restorepoint::copy.into.env(stata.funs, globenv())
}


remove.stata.funs.from.globenv = function() {
  stata.funs = stata.funs()
  fun.names = names(stata.funs)
  remove(fun.names,pos = globenv())
}

stata.funs = function() {
  list(
    mean = function(x,...) base::mean(x, ..., na.rm=TRUE),
    min = function(x,...) {
      args = list(...)
      # gen version of min
      if (length(args)>0) {
        return(do.call(base::pmin, c(list(na.rm=TRUE,x),args)))
      }
      # egen version
      base::min(x, ..., na.rm=TRUE)
    },
    max = function(x,...) {
      args = list(...)
      #restore.point("jflsjflk")
      # gen version of max
      if (length(args)>0) {
        return(do.call(base::pmax, c(list(na.rm=TRUE,x),args)))
      }
      # egen version
      base::max(x, ..., na.rm=TRUE)
    },
    median = function(x,...) base::median(x, ..., na.rm=TRUE),
    sum = function(x,...) base::sum(x, ..., na.rm=TRUE)
  )
}

METAREG_STATA_ENV = make.stata.funs.env()
METAREG_STATA_FUN_NAMES = names(METAREG_STATA_ENV)
