## ----echo=FALSE----------------------------
library(tufte)
knitr::opts_chunk$set(results = "hide", echo = FALSE)

## ----P2, echo=FALSE------------------------
## Solutions ##
randu = setRefClass("randu", 
                    fields = list(calls = "numeric", 
                                  seed="numeric", 
                                  state="numeric"))
randu$methods(get_state = function() state)
randu$methods(set_state = function(initial) state <<- initial)
randu$methods(get_seed = function() seed)
randu$methods(get_num_calls = function() calls)
randu$methods(r = function() {
  calls <<- calls + 1
  state <<- (65539*state) %% 2^31
  return(state/2^31)
})

## ------------------------------------------
r = randu(calls=0, seed=10, state=10)
r$r()
r$get_state()
r$get_seed()

## ------------------------------------------
##Solutions - see below

## ------------------------------------------
r = randu(calls=0, seed=10, state=10)
r$get_num_calls()
r$r()
r$r()
r$get_num_calls()

## ----P2------------------------------------
## Solutions ##
randu = setRefClass("randu", 
                    fields = list(calls = "numeric", 
                                  seed="numeric", 
                                  state="numeric"))
randu$methods(get_state = function() state)
randu$methods(set_state = function(initial) state <<- initial)
randu$methods(get_seed = function() seed)
randu$methods(get_num_calls = function() calls)
randu$methods(r = function() {
  calls <<- calls + 1
  state <<- (65539*state) %% 2^31
  return(state/2^31)
})

## ----eval=FALSE, echo = TRUE---------------
#  library("jrAdvanced")
#  vignette("solutions4", package="jrAdvanced")

