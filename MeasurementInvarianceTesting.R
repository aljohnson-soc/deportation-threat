# Replication Package for "Deportation Threat Predicts Latino U.S. Citizens and Noncitizens' Psychological Distress, 2011-2018"
# Authors: Amy L. Johnson, Christopher Levesque, Neil A. Lewis, Jr., Asad L. Asad

# Testing measurement invariance of K6 & related measures

library(rio)
library(lavaan)
library(tidyverse)

d_raw <- import("nhis_2011-18_clean.dta")

d <- d_raw %>% filter(filter==1) %>%
  mutate(race_grp = ifelse(race2 %in% c(4,5), NA, race2),
         racecit_grp = case_when(race2==2 & citizen2==2 ~ 1,
                                 race2==1 & citizen2==2 ~ 2,
                                 usb==1 ~ 3,
                                 nat==1 ~ 4,
                                 nczn==1 ~ 5),
         region_grp = regionbr2,
         lang_grp = case_when(intervlang==1 ~ 1,
                              intervlang %in% c(2,3) ~2),
         yrsinus_grp = case_when(yrsinus %in% c(1, 2) ~ 1,
                                 yrsinus %in% c(3, 4) ~ 2,
                                 yrsinus ==5 ~ 3))

# groups
groups <- c("race_grp", "racecit_grp", "region_grp", "lang_grp", "yrsinus_grp")

# outcomes
cfa.k6 = 'K6 =~ aeffort + ahopeless + anervous + arestless + asad + aworthless'
cfa.anx = 'K6_anx =~ anervous + arestless'
cfa.depr = 'K6_depr =~ aeffort + ahopeless + asad + aworthless'

# models
m_configural <- function(outcome, group_var){
  fit <- cfa(outcome, data = d, estimator = "WLSMV",
             ordered=c("aeffort","ahopeless","anervous","arestless","asad",
                       "aworthless"),
             group = group_var)
  fitm <- fitmeasures(fit, fit.measures = c("chisq", "df", "cfi",
                                            "rmsea",  "srmr"))
  print(group_var)
  print(fitm)
  return(invisible(NULL))
}

m_metric <- function(outcome, group_var){
  fit <- cfa(outcome, data = d, estimator = "WLSMV",
             ordered=c("aeffort","ahopeless","anervous","arestless","asad",
                       "aworthless"),
             group = group_var,
             group.equal = "loadings")
  fitm <- fitmeasures(fit, fit.measures = c("chisq", "df", "cfi",
                                            "rmsea",  "srmr"))
  print(group_var)
  print(fitm)
  return(invisible(NULL))
}

m_scalar <- function(outcome, group_var){
  fit <- cfa(outcome, data = d, estimator = "WLSMV",
             ordered=c("aeffort","ahopeless","anervous","arestless","asad",
                       "aworthless"),
             group = group_var,
             group.equal = c("loadings", "intercepts"))
  fitm <- fitmeasures(fit, fit.measures = c("chisq", "df", "cfi",
                                            "rmsea",  "srmr"))
  print(group_var)
  print(fitm)
  return(invisible(NULL))
}


lapply(groups, m_configural, outcome = cfa.k6)
lapply(groups, m_metric, outcome = cfa.k6)
lapply(groups, m_scalar, outcome = cfa.k6)

lapply(groups, m_scalar, outcome = cfa.anx)

lapply(groups, m_configural, outcome = cfa.depr)
lapply(groups, m_metric, outcome = cfa.depr)
lapply(groups, m_scalar, outcome = cfa.depr)