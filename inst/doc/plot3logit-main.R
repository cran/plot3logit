## ----include=FALSE------------------------------------------------------------
library(knitr)
opts_chunk$set(
engine='R', tidy=FALSE
)

## ----preliminaries, echo=FALSE, results='hide'----------------------
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("MASS")
library(plot3logit)

## ----include=FALSE--------------------------------------------------
library(knitr)
opts_chunk$set(
concordance=FALSE
)

## -------------------------------------------------------------------
library("nnet")
data("USvote2016", package = "plot3logit")
modVote <- multinom(vote ~ educ + gender + race + birthyr,
  data = droplevels(USvote2016), trace = FALSE)

## -------------------------------------------------------------------
fittedModel <- list(B = matrix(c(2, 0.3, -0.2, 0.2, 1, 0.1, -0.4, -0.3),
  ncol = 2, dimnames = list(c("(Intercept)", "X1", "X2", "X3"))),
  levels = c("Class A", "Class B", "Class C"))

## -------------------------------------------------------------------
library("plot3logit")
extract3logit(fittedModel)

## -------------------------------------------------------------------
Delta <- rep(0, 17)
Delta[7] <- 1
Delta

## ----message=FALSE--------------------------------------------------
field3logit(model = modVote, delta = Delta)

## -------------------------------------------------------------------
field3logit(model = modVote, delta = c(genderFemale = 1, raceBlack = 1))

## -------------------------------------------------------------------
field3logit(model = modVote, delta = "genderFemale")

## ----eval = FALSE---------------------------------------------------
# field3logit(model = fittedModel, delta = c(X1 = 0.5, X2 = -2, X3 = 1))

## -------------------------------------------------------------------
field3logit(model = fittedModel, delta = "0.5 * X1 + X3 - 2 * X2")

## -------------------------------------------------------------------
field3logit(modVote, delta = "genderFemale + `birthyr[1940,1950)`")

## ----eval = FALSE---------------------------------------------------
# ptsAB <- list(A = c(0.3, 0.4, 0.3), B = c(0.5, 0.1, 0.4))
# par(mfrow = c(2, 2), cex = 0.5, mar = rep(0, 4))
# # Top-left
# plot(field3logit(modVote, "genderFemale", edge = 0.1))
# # Top-right
# plot(field3logit(modVote, "genderFemale", nstreams = 4))
# # Bottom-left
# plot(field3logit(modVote, "genderFemale", p0 = ptsAB))
# TernaryPoints(ptsAB)
# TernaryText(ptsAB, labels = names(ptsAB), pos = 1)
# # Bottom-right
# plot(field3logit(modVote, "genderFemale", p0 = ptsAB, narrows = 1))
# TernaryPoints(ptsAB)
# TernaryText(ptsAB, labels = names(ptsAB), pos = 1)

## ----visualization, echo=FALSE, fig.height=4, fig.width=4-----------
ptsAB <- list(A = c(0.3, 0.4, 0.3), B = c(0.5, 0.1, 0.4))
par(mfrow = c(2, 2), cex = 0.5, mar = rep(0, 4))
# Top-left
plot(field3logit(modVote, "genderFemale", edge = 0.1))
# Top-right
plot(field3logit(modVote, "genderFemale", nstreams = 4))
# Bottom-left
plot(field3logit(modVote, "genderFemale", p0 = ptsAB))
TernaryPoints(ptsAB)
TernaryText(ptsAB, labels = names(ptsAB), pos = 1)
# Bottom-right
plot(field3logit(modVote, "genderFemale", p0 = ptsAB, narrows = 1))
TernaryPoints(ptsAB)
TernaryText(ptsAB, labels = names(ptsAB), pos = 1)

## ----eval=FALSE-----------------------------------------------------
# fieldFemale <- field3logit(modVote, "genderFemale")
# gg3logit(fieldFemale) + stat_field3logit()

## -------------------------------------------------------------------
set.seed(3109)
fortfieldFemale <- fortify(field3logit(modVote, "genderFemale"))
set.seed(NULL)
fortfieldFemale

## ----eval=FALSE-----------------------------------------------------
# gg3logit(fortfieldFemale, aes(x = Clinton, y = Trump, z = Other,
#   xend = Clinton_end, yend = Trump_end, zend = Other_end, group = group,
#   type = type)) + stat_field3logit()

## -------------------------------------------------------------------
refprofile <- list(c(1/3, 1/3, 1/3))

fieldBlack <- field3logit(model = modVote, delta = "raceBlack",
  label = "Black", p0 = refprofile, narrows = 1)

fieldHispanic <- field3logit(model = modVote, delta = "raceHispanic",
  label = "Hispanic", p0 = refprofile, narrows = 1)

mfieldrace <- fieldBlack + fieldHispanic
mfieldrace

## -------------------------------------------------------------------
fieldAsian <- field3logit(model = modVote, delta = "raceAsian",
  label = "Asian", p0 = refprofile, narrows = 1)

mfieldrace <- mfieldrace + fieldAsian
mfieldrace

## -------------------------------------------------------------------
race_effects <- list(
  list(delta = "raceBlack", label = "Black"),
  list(delta = "raceHispanic", label = "Hispanic"),
  list(delta = "raceAsian", label = "Asian"),
  list(delta = "raceMixed", label = "Mixed"),
  list(delta = "raceOther", label = "Other")
)

## -------------------------------------------------------------------
mfieldrace <- field3logit(model = modVote, delta = race_effects,
  p0 = refprofile, narrows = 1)

mfieldrace

## -------------------------------------------------------------------
field3logit(model = modVote, delta = "<<race>>", p0 = refprofile,
  narrows = 1)

## -------------------------------------------------------------------
field3logit(model = modVote, delta = c("raceBlack", "raceAsian"),
  label = c("BLACK", "ASIAN"))

## -------------------------------------------------------------------
mfdecade <- field3logit(modVote, "<<birthyr>>", label = "Born in ")
mfdecade

## -------------------------------------------------------------------
labels(mfdecade)
labels(mfdecade) <- c("Fourties", "Fifties", "Sixties", "Seventies",
  "Eighties and Nineties")
mfdecade

## ----eval = FALSE---------------------------------------------------
# gg3logit(mfieldrace, aes(colour = label)) + stat_field3logit() +
#   labs(colour = "Race (ref.: White)")

## ----eval=FALSE-----------------------------------------------------
# mfieldrace <- add_confregions(mfieldrace)

## ----eval = FALSE---------------------------------------------------
# gg3logit(mfieldrace) + stat_field3logit(aes(colour = label)) +
#   stat_conf3logit(aes(fill = label)) +
#   labs(colour = "Race (ref.: White)", fill = "Race (ref.: White)")

## ----eval=FALSE-----------------------------------------------------
# library("tidyverse")
# 
# tibble(race = levels(USvote2016$race), educ = "High school grad.",
#     gender = "Male", birthyr = "[1970,1980)"
#   ) %>%
#   mutate(delta = "genderFemale", label = race) %>%
#   group_by(delta, label) %>%
#   nest() %>%
#   mutate(p0 = map(data, ~list(predict(modVote, .x, type = "probs")))) %>%
#   select(-data) %>%
#   transpose -> gender_by_race
# 
# mfieldGbyR <- field3logit(modVote, gender_by_race, narrows = 1,
#   conf = 0.95)
# 
# gg3logit(mfieldGbyR) + stat_field3logit(aes(colour = label)) +
#   stat_conf3logit(aes(fill = label)) + tern_limits(T = 0.8, R = 0.8) +
#   labs(colour = "Profile", fill = "Profile")

## ----eval=FALSE-----------------------------------------------------
# stat_field3logit() + stat_conf3logit()

## ----eval=FALSE-----------------------------------------------------
# gg3logit() + stat_3logit()

## ----eval=FALSE-----------------------------------------------------
# gg3logit() + stat_field3logit() + stat_conf3logit()

## ----eval = FALSE---------------------------------------------------
# gg3logit(mfieldrace) + stat_field3logit(aes(colour = label)) +
#   stat_conf3logit(aes(fill = label))

## ----eval = FALSE---------------------------------------------------
# gg3logit(mfieldrace) + stat_3logit(aes(colour = label), aes(fill = label))

## ----eval = FALSE---------------------------------------------------
# autoplot(mfieldrace, mapping_field = aes(colour = label),
#   mapping_conf = aes(fill = label))

