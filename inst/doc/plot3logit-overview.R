## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 5,
  fig.asp = 1
)

## ---- message = FALSE----------------------------------------------------
library(plot3logit)
data(cross_1year)
library(nnet)
mod0 <- multinom(employment_sit ~ ., data = cross_1year)

## ------------------------------------------------------------------------
library(plot3logit)

## ------------------------------------------------------------------------
field0 <- field3logit(mod0, 'genderFemale')

## ------------------------------------------------------------------------
gg3logit(field0) + stat_3logit()

## ------------------------------------------------------------------------
plot(field0)

## ------------------------------------------------------------------------
data(cross_1year)
str(cross_1year)
head(cross_1year)

## ------------------------------------------------------------------------
mod0 <- nnet::multinom(employment_sit ~ finalgrade + irregularity + hsscore, cross_1year)
mod0

## ------------------------------------------------------------------------
coef(mod0)

## ------------------------------------------------------------------------
field0 <- field3logit(mod0, c(0, 0, 1, 0, 0, 0))
field0

## ------------------------------------------------------------------------
field0 <- field3logit(mod0, c(0, 0, 1, 0, 0, -10))
field0

## ------------------------------------------------------------------------
field0 <- field3logit(mod0, 'finalgradeHigh')
field0

## ------------------------------------------------------------------------
field0 <- field3logit(mod0, 'finalgradeHigh - 10 * hsscore')
field0

## ---- results='hide'-----------------------------------------------------
data(cross_1year)
mod0 <- nnet::multinom(employment_sit ~ ., data = cross_1year)

refpoint <- list(c(0.7, 0.15, 0.15))

field_Sdur <- field3logit(mod0, 'durationShort', label = 'Short duration', p0 = refpoint, narrows = 1)
field_Ldur <- field3logit(mod0, 'durationLong', label = 'Long duration', p0 = refpoint, narrows = 1)
field_Hfgr <- field3logit(mod0, 'finalgradeHigh', label = 'High final grade', p0 = refpoint, narrows = 1)
field_Lfgr <- field3logit(mod0, 'finalgradeLow', label = 'Low final grade', p0 = refpoint, narrows = 1)

## ------------------------------------------------------------------------
mfields <- field_Sdur + field_Ldur + field_Lfgr + field_Hfgr
mfields

## ---- message = FALSE----------------------------------------------------
gg3logit(mfields) +
  stat_3logit(aes(colour = label)) +
  theme_zoom_L(0.45)

