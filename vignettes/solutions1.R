## ----echo=FALSE----------------------------
library(tufte)
knitr::opts_chunk$set(results = "hide", echo = TRUE)

## ---- eval=FALSE, echo = TRUE--------------
#  file.exists("~/.Rprofile")

## ---- message=FALSE, echo = TRUE-----------
if(interactive()) {
   message("Successfully loaded .Rprofile at ", date(), "\n")
}

## ---- eval=FALSE, tidy=FALSE, echo = TRUE----
#  options(prompt="R> ", digits=4,
#          show.signif.stars=FALSE)

## ---- eval=FALSE, tidy=FALSE, echo = TRUE----
#  r = getOption("repos")
#  r["CRAN"] = "http://cran.rstudio.com/"
#  options(repos = r)
#  rm(r)

## ---- tidy=FALSE, echo = TRUE--------------
arg_explore = function(arg1, rg2, rg3)
    paste("a1, a2, a3 = ", arg1, rg2, rg3)

## ---- eval=FALSE, echo = TRUE--------------
#  arg_explore(1, 2, 3)
#  arg_explore(2, 3, arg1 = 1)
#  arg_explore(2, 3, a = 1)
#  arg_explore(1, 3, rg = 1)

## ---- tidy=FALSE---------------------------
## SOLUTION
## See http://goo.gl/NKsved for the offical document
## To summeriase, matching happens in a three stage pass:
#1. Exact matching on tags
#2. Partial matching on tags.
#3. Positional matching

## ---- fig.keep="none", echo = TRUE---------
plot(type="l", 1:10, 11:20)

## ---- results='hide', echo = TRUE----------
rnorm(mean=4, 4, n=5)

## ---- tidy=FALSE, results='hide', fig.keep='none'----
## SOLUTION
#plot(type="l", 1:10, 11:20) is equivilent to
plot(x=1:10, y=11:20, type="l")
#rnorm(mean=4, 4, n=5) is equivilent to
rnorm(n=5, mean=4, sd=4)

## ---- echo = TRUE--------------------------
## Use regression as an example
stat_ana = function(x, y) {
  lm(y ~ x)
}

## ----  tidy=FALSE, results='hide', fig.keep='none'----
## SOLUTION
stat_ana = function(x, y, trans=NULL) {
  lm(y ~ x)
}

## ---- eval=FALSE, echo = TRUE--------------
#  stat_ana(x, y, trans=log)

## ----  tidy=FALSE, results='hide', fig.keep='none'----
## SOLUTION
stat_ana = function(x, y, trans=NULL) {
  if(is.function(trans)) {
    x = trans(x)
    y = trans(y)
  }
  lm(y ~ x)
}

## ----  tidy=FALSE, results='hide', fig.keep='none'----
## SOLUTION
stat_ana = function(x, y, trans=NULL) {
  if(is.function(trans)) {
    x = trans(x)
    y = trans(y)
  } else if (trans == "normalise") {
    x = scale(x)
    y = scale(y)
  }
  lm(y ~ x)
}

## ---- results='hide', echo = TRUE----------
f = function(x) return(x + 1)
f(10)

## ----  tidy=FALSE--------------------------
##Nothing strange here. We just get
f(10)

## ---- results='hide', echo = TRUE----------
f = function(x) {
  f = function(x) {
    x + 1
  }
  x = x + 1
  return(f(x))
}
f(10)

## ---- results='hide', echo = TRUE----------
f = function(x) {
  f = function(x) {
    f = function(x) {
      x + 1
    }
    x = x + 1
    return(f(x))
  }
  x = x + 1
  return(f(x))
}
f(10)

## ------------------------------------------
## Solution: The easiest way to understand is to use print statements
f = function(x) {
  f = function(x) {
    f = function(x) {
      message("f1: = ", x)
      x + 1
    }
    message("f2: = ", x)
    x = x + 1
    return(f(x))
  }
  message("f3: = ", x)
  x = x + 1
  return(f(x))
}
f(10)

## ---- results='hide'-----------------------
f = function(x) {
  f = function(x) {
    x = 100
    f = function(x) {
      x + 1
    }
    x = x + 1
    return(f(x))
  }
  x = x + 1
  return(f(x))
}
f(10)

## ----  results='hide'----------------------
##Solution: The easiest way to understand is to use print statements as above

## ------------------------------------------
poisson = function(lambda) {
     r = function(n=1) rpois(n, lambda)
     d = function(x, log=FALSE) dpois(x, lambda, log=log)
     return(list(r=r, d=d))
}

## ------------------------------------------
geometric = function(prob) {
     r = function(n=1) rgeom(n, prob)
     d = function(x, log=FALSE) dgeom(x, prob, log=log)
     return(list(r=r, d=d))
}

## ---- randu, results="hide", echo = FALSE----
##Solutions
randu = function(seed) {
  state = seed
  calls = 0 #Store the number of calls
  r = function() {
    state <<- (65539*state) %% 2^31
    ## Update the variable outside of this enviroment
    calls <<- calls + 1
    state/2^31
  }
  set_state = function(initial) state <<- initial
  get_state = function() state
  get_seed = function() seed
  get_num_calls = function() calls
  list(r=r, set_state=set_state, get_state=get_state,
       get_seed = get_seed, get_num_calls=get_num_calls)
}
r = randu(10)
r$r()
r$get_state()
r$get_seed()

## ---- echo = TRUE--------------------------
r = randu(10)
r$r()
r$get_state()
r$get_seed()

## ---- echo = TRUE--------------------------
r = randu(10)
r$get_num_calls()
r$r()
r$r()
r$get_num_calls()

## ---- randu--------------------------------
##Solutions
randu = function(seed) {
  state = seed
  calls = 0 #Store the number of calls
  r = function() {
    state <<- (65539*state) %% 2^31
    ## Update the variable outside of this enviroment
    calls <<- calls + 1
    state/2^31
  }
  set_state = function(initial) state <<- initial
  get_state = function() state
  get_seed = function() seed
  get_num_calls = function() calls
  list(r=r, set_state=set_state, get_state=get_state,
       get_seed = get_seed, get_num_calls=get_num_calls)
}
r = randu(10)
r$r()
r$get_state()
r$get_seed()

## ---- eval=FALSE---------------------------
#  library("jrAdvanced")
#  vignette("solutions1", package="jrAdvanced")

