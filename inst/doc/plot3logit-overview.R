## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4,
  fig.align = "center"
)

## ----message=FALSE------------------------------------------------------------
library(plot3logit)
data(cross_1year)
str(cross_1year)

## ----message=FALSE, results='hide'--------------------------------------------
library(MASS)

mydata <- cross_1year
mydata$finalgrade <- factor(
  x = mydata$finalgrade,
  levels = c('Low', 'Average', 'High'),
  ordered = TRUE
)

mod0 <- polr(finalgrade ~ gender + irregularity, data = mydata)
field3logit(mod0, 'genderFemale')

## ----message=FALSE, results='hide'--------------------------------------------
library(mlogit)
mydata <- mlogit.data(cross_1year, choice = 'employment_sit', shape = 'wide')
mod0 <- mlogit(employment_sit ~ 0 | gender + finalgrade, data = mydata)
field3logit(mod0, 'genderFemale')

## ----message=FALSE, results='hide'--------------------------------------------
library(nnet)
mod0 <- multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
field3logit(mod0, 'genderFemale')

## ----message=FALSE, results='hide'--------------------------------------------
library(ordinal)

mydata$finalgrade <- factor(mydata$finalgrade, c('Low', 'Average', 'High'))

mod0 <- clm(finalgrade ~ gender + irregularity, data = mydata)
field3logit(mod0, 'genderFemale')

mod0 <- clm2(finalgrade ~ gender + irregularity, data = mydata)
field3logit(mod0, 'genderFemale')

## ----message=FALSE, results='hide'--------------------------------------------
library(VGAM)

mod0 <- vgam(
  formula = employment_sit ~ gender + finalgrade,
  family = multinomial(),
  data = cross_1year
)
field3logit(mod0, 'genderFemale')

mod0 <- vglm(
  formula = employment_sit ~ gender + finalgrade,
  family = multinomial(),
  data = cross_1year
)
field3logit(mod0, 'genderFemale')

## ----message=FALSE, results='hide'--------------------------------------------
mod0 <- list(
  B = matrix(
    data = c(-2.05, 0.46, -2.46, 0.37, -1.2, 0.8, 0.7, 0.4),
    ncol = 2,
    dimnames = list(c('Intercept', 'X1', 'X2', 'X3'))
  ),
  levels = c('Class A', 'Class B', 'Class C')
)
field3logit(mod0, 'X1')

## ----message = FALSE----------------------------------------------------------
library(plot3logit)
data(cross_1year)
library(nnet)
mod0 <- multinom(employment_sit ~ ., data = cross_1year)

## -----------------------------------------------------------------------------
field0 <- field3logit(mod0, 'genderFemale')

## -----------------------------------------------------------------------------
gg3logit(field0) + stat_field3logit()

## ----fig.width=6, fig.asp=1---------------------------------------------------
plot(field0)

## -----------------------------------------------------------------------------
library(plot3logit)
library(nnet)
mod0 <- multinom(employment_sit ~ finalgrade + irregularity + hsscore, cross_1year)
mod0

## -----------------------------------------------------------------------------
coef(mod0)

## -----------------------------------------------------------------------------
field3logit(mod0, c(0, 0, 1, 0, 0, 0))

## -----------------------------------------------------------------------------
field3logit(mod0, c(0, 0, 1, 0, 0, -10))

## -----------------------------------------------------------------------------
# Named numeric vectors:
field3logit(mod0, c(finalgradeHigh = 1))

# R code:
field3logit(mod0, 'finalgradeHigh')

## -----------------------------------------------------------------------------
# Named numeric vectors:
field3logit(mod0, c(finalgradeHigh = 1, hsscore = -10))

# R code:
field3logit(mod0, 'finalgradeHigh - 10 * hsscore')

## ----results='hide'-----------------------------------------------------------
data(cross_1year)
mod0 <- nnet::multinom(employment_sit ~ ., data = cross_1year)

refpoint <- list(c(0.7, 0.15, 0.15))

field_Sdur <- field3logit(mod0, 'durationShort', label = 'Short duration', p0 = refpoint, narrows = 1)
field_Ldur <- field3logit(mod0, 'durationLong', label = 'Long duration', p0 = refpoint, narrows = 1)
field_Hfgr <- field3logit(mod0, 'finalgradeHigh', label = 'High final grade', p0 = refpoint, narrows = 1)
field_Lfgr <- field3logit(mod0, 'finalgradeLow', label = 'Low final grade', p0 = refpoint, narrows = 1)

## -----------------------------------------------------------------------------
mfields <- field_Sdur + field_Ldur + field_Lfgr + field_Hfgr
mfields

## ----message = FALSE----------------------------------------------------------
gg3logit(mfields) +
  stat_field3logit(aes(colour = label)) +
  theme_zoom_L(0.45)

## -----------------------------------------------------------------------------
depo <- list(
  list(delta = 'durationShort', label = 'Short duration'),
  list(delta = 'durationLong', label = 'Long duration'),
  list(delta = 'finalgradeHigh', label = 'High final grade'),
  list(delta = 'finalgradeLow', label = 'Low final grade')
)

mfields <- field3logit(mod0, delta = depo, p0 = refpoint, narrows = 1)
mfields

## -----------------------------------------------------------------------------
field3logit(mod0, delta = '<<finalgrade>>', p0 = refpoint, narrows = 1)

## -----------------------------------------------------------------------------
field0 <- add_confregions(field0, conf = 0.95)
field0

## -----------------------------------------------------------------------------
mfields <- add_confregions(mfields, conf = 0.95)

## ----eval=FALSE---------------------------------------------------------------
# gg3logit(field0) + stat_field3logit() + stat_conf3logit()

## ----message = FALSE----------------------------------------------------------
gg3logit(mfields) +
  stat_field3logit(aes(colour = label)) +
  stat_conf3logit(aes(fill = label)) +
  theme_zoom_L(0.45)

