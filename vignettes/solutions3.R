## ----echo=FALSE----------------------------
library(tufte)
knitr::opts_chunk$set(results = "show", echo = TRUE)

## ----echo=FALSE----------------------------
setClass("Cohort",
         representation(
           details = "data.frame",
           centre = "character"
         )
)
w = c(32, 39, 33, 66, 24)
h = c(1.4, 1.47, 1.47, 1.58, 1.25)
coh_s4 = new("Cohort",
             details = data.frame(weight = w, height = h),
             centre = "NCL"
)


## ------------------------------------------
isGeneric("mean")
setGeneric("mean")

## ---- , message=FALSE----------------------
setMethod("mean", signature = c("Cohort"), 
          definition = function(x, ...) {
            m1 = mean(x@details[, 1], ...)
            m2 = mean(x@details[, 2], ...)
            return(c(m1, m2))
          }
)

## ---- , message=FALSE----------------------
isGeneric("sd")
setGeneric("sd")
setMethod("sd", signature = c("Cohort"), 
          definition = function(x, na.rm=FALSE) {
            m1 = sd(x@details[, 1], na.rm = na.rm)
            m2 = sd(x@details[, 2], na.rm = na.rm)
            return(c(m1, m2))
          }
)

## ---- , message=FALSE----------------------
isGeneric("summary")
setGeneric("summary")
setMethod("summary", signature = c("Cohort"), 
                    definition = function(object, ...) {
            summary(object@details)
          }
)

## ---- , message=FALSE----------------------
isGeneric("hist")
setGeneric("hist")
setMethod("hist", signature = c("Cohort"), 
          definition = function(x, ...) {
            op = par(mfrow = c(1, 2))
            hist(x@details[, 1], main = "Weight", ...)
            hist(x@details[, 2], main = "Height", ...)
            par(op)
          }
)

## ------------------------------------------
isGeneric("[")
getGeneric("[")
## Can you determine what drop does?
setMethod("[", signature = c("Cohort"), 
          definition = function(x, i, j, ..., drop = TRUE) {
            x@details = x@details[i, j, ..., drop = drop]
            x
          }
)

## ------------------------------------------
isGeneric("[<-")
setGeneric("[<-")

setMethod("[<-", signature = c("Cohort"), 
          definition = function(x, i, j, value) {
            x@details[i, j] = value
            x
          }
)
coh_s4[1, ] = 5

## ----eval=FALSE, echo = TRUE---------------
#  library("jrAdvanced")
#  vignette("solutions3", package="jrAdvanced")
