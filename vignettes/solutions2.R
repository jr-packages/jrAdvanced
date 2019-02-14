## ----echo=FALSE----------------------------
library(tufte)
knitr::opts_chunk$set(results = "show", echo = TRUE)

## ------------------------------------------
methods("mean")

## ----results="hide"------------------------
body("mean")

## ------------------------------------------
args("mean")

## ----results='hide'------------------------
mean.cohort = function(x, ...) {
  m1 = mean(x$details[ ,1], ...)
  m2 = mean(x$details[ ,2], ...)
  return(c(m1, m2))
}

## ------------------------------------------
sd = function(x, ...) UseMethod("sd")
sd.default = function(x, ...) stats::sd(x, ...)
sd.cohort = function(x, ...) {
  s1 = sd(x$details[ ,1], ...)
  s2 = sd(x$details[ ,2], ...)
  return(c(s1, s2))
} 

## ------------------------------------------
## summary is already a generic
body(summary)

## Match the args
args(summary)

## Function
summary.cohort = function(object, ...) summary(object$details, ...)

## ------------------------------------------
## hist is already a generic
body(hist)

## Match the args
args(hist)

## Function
hist.cohort = function(x, ...) {
  op = par(mfrow=c(1, 2))
  hist(x$details[,1], main="Weight")
  hist(x$details[,2], main="Height")
  par(op)
}

## ---- eval=FALSE---------------------------
#  cc[1:3,]

## ------------------------------------------
## Lots of methods available. 
methods('[')

## Examine [.data.frame
args('[.data.frame')

'[.cohort' = function(x, ...){
  x$details = x$details[...]
  x
}

## ---- eval=FALSE---------------------------
#  cc[1,1] = 10

## ---- eval = FALSE-------------------------
#  ## Lots of methods available.
#  methods('[<-')
#  
#  ## Examine [.data.frame
#  args('[<-.data.frame')
#  
#  '[<-.cohort' = function(x, i, j, value){
#    x$details[i, j] = value
#    x
#  }
#  cc[1:3, ] = 55

## ---- eval=FALSE---------------------------
#  library("jrAdvanced")
#  vignette("solutions2", package="jrAdvanced")

