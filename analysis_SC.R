# set up ------------------------------------------------------------------
library(tidyverse)
library(rstatix)
library(sjPlot)
library(interactions)
library(pwrss)
library(lmPerm)
library(parameters)
library(ggstatsplot)
setwd("C:/Users/eliza/Box/Pragmatics/Pragmatics_GT/")
#dir.create("general")
#dir.create("table")

# data --------------------------------------------------------------------
data <- read_csv("generals/data_SC.csv") %>% 
  mutate_at(c(1,3:5), as.factor)

outliers.data <-data %>%
  mutate_at(c(1,3:5), as.factor) %>% 
  gather(6:446, key = "task", value = "result") %>% 
  group_by(task) %>%
  identify_outliers(result) %>% 
  filter(is.extreme == TRUE) %>% 
  dplyr::select(1:4,6,9) %>% 
  write_csv("table/SI 5.csv")

data1 <- data %>% 
  gather(6:446, key = "task", value = "result2") %>% 
  full_join(outliers.data) %>% 
  mutate(is.extreme = replace_na(is.extreme,FALSE)) %>% 
  filter(is.extreme == FALSE) %>% 
  dplyr::select(-8) %>% 
  spread(task, result2) %>% 
  dplyr::select(1:5,8:14,154,155,295:297,437:448,156:294,298:436,15:153)
# dplyr::select(1:12,154,155,295:297,434:443,13:153,156:294,298:433)

# Power analysis ----------------------------------------------------------
pwr::pwr.t.test(d = .5, power = 0.80, sig.level = 0.05, type = "one")

power.f.regression(r.squared = 0.35,
                   k.total = 4, 
                   power = 0.80,
                   alpha = 0.05)

power.f.regression(r.squared = 0.15,
                   k.total = 4, 
                   power = 0.80,
                   alpha = 0.05)

# descriptives -------------------------------------------------------------
descriptives <- data1 %>% 
  dplyr::select(6:26) %>% 
  get_summary_stats(type = "common") %>% 
  mutate_if(is.numeric, round,2) %>% 
  write_csv("table/descriptives_new.csv")

descriptives.Sex <- data %>% 
  dplyr::select(2,3,6:26) %>% 
  dplyr::group_by(Sex) %>% 
  get_summary_stats(type = "common") %>% 
  mutate_if(is.numeric, round,2) %>% 
  dplyr::select(1:5,8,9) %>% 
  write_csv("table/descriptives_Sex.csv")

comparision.Sex <- data %>% 
  gather(c(2,3,6:26), key = "test", value = "score") %>% 
  dplyr::group_by(test) %>% 
  t_test(score ~ Sex) %>% 
  adjust_pvalue(method="fdr") %>%
  mutate_if(is.numeric, round,2) %>% 
  write_csv("table/descriptives_Sex.csv")

descriptives.Group <- data %>% 
  dplyr::select(c(2,5,6:26)) %>% 
  dplyr::group_by(Group) %>% 
  get_summary_stats(type = "common") %>% 
  mutate_if(is.numeric, round,2) %>% 
  dplyr::select(1:5,8,9) %>% 
  write_csv("table/descriptives_Group.csv")

comparision.Group <- data %>% 
  gather(c(2,6:26), key = "test", value = "score") %>% 
  dplyr::group_by(test) %>% 
  t_test(score ~ Group) %>% 
  adjust_pvalue(method="fdr") %>%
  mutate_if(is.numeric, round,2) %>% 
  write_csv("table/descriptives_Group.csv")

descriptives.ROI <- data %>% 
  gather(contains(c("PN","ToM","LN", "All")), key = "measure", value = "result") %>% 
  separate(measure, c("measure", "network", "ROI"), sep = "_") %>% 
  dplyr::group_by(network, measure, ROI) %>% 
  get_summary_stats(result, type = "common") %>% 
  mutate_if(is.numeric, round,2) %>% 
  write_csv("table/descriptives_ROI.csv")

shapiro <- data %>% 
  gather(c(6,12:26), key = "variables", value = "result") %>% 
  dplyr::group_by(variables) %>% 
  shapiro_test(result) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  adjust_pvalue(method="fdr") %>%
  add_significance("p.adj")

# Regression models -------------------------------------------------------
# All ---------------------------------------------------------------------
set.seed(100)
lm.GE.All.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_All` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.All.SC.sw) #not sig
lm.GE.All.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_All` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.All.EF.sw)#not sig
lm.GE.All.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_All` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.All.LN.sw) #not sig
lm.GE.All.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_All` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.All.PS.sw) #not sig

lm.LE.All.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_All` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.All.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_All` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.All.EF.sw) #not sig
lm.LE.All.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_All` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.All.LN.sw) #not sig
lm.LE.All.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_All` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.All.PS.sw) #not sig

lm.clcO.All.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_All` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.All.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_All` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.All.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_All` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.All.LN.sw) # not sig
lm.clcO.All.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_All` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.All.PS.sw) # not sig

# Pragmatic Network -------------------------------------------------------
lm.GE.PN.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_PN_All ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.PN.EF.sw <- MASS::stepAIC(lm(GlobalEfficiency_PN_All ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.PN.EF.sw) # not sig
lm.GE.PN.LN.sw <- MASS::stepAIC(lm(GlobalEfficiency_PN_All ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.PN.LN.sw) # not sig
lm.GE.PN.PS.sw <- MASS::stepAIC(lm(GlobalEfficiency_PN_All ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.PN.SC.sw <- MASS::stepAIC(lm(LocalEfficiency_PN_All ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.PN.SC.sw) # not sig
lm.LE.PN.EF.sw <- MASS::stepAIC(lm(LocalEfficiency_PN_All ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.PN.EF.sw) # not sig
lm.LE.PN.LN.sw <- MASS::stepAIC(lm(LocalEfficiency_PN_All ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.PN.LN.sw) #not sig
lm.LE.PN.PS.sw <- MASS::stepAIC(lm(LocalEfficiency_PN_All ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.PN.PS.sw) #not sig

lm.clcO.PN.SC.sw <- MASS::stepAIC(lm(ClusteringCoefficient_PN_All ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.PN.SC.sw) # not sig
lm.clcO.PN.EF.sw <- MASS::stepAIC(lm(ClusteringCoefficient_PN_All ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.PN.EF.sw) # not sig
lm.clcO.PN.LN.sw <- MASS::stepAIC(lm(ClusteringCoefficient_PN_All ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.PN.LN.sw) # not sig
lm.clcO.PN.PS.sw <- MASS::stepAIC(lm(ClusteringCoefficient_PN_All ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.PN.PS.sw) # not sig

# LN ----------------------------------------------------------------------
lm.GE.LN.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_LN_All ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.LN.SC.sw) # not sig
lm.GE.LN.EF.sw <- MASS::stepAIC(lm(GlobalEfficiency_LN_All ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.LN.EF.sw) # not sig
lm.GE.LN.LN.sw <- MASS::stepAIC(lm(GlobalEfficiency_LN_All ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.LN.LN.sw) # not sig
lm.GE.LN.PS.sw <- MASS::stepAIC(lm(GlobalEfficiency_LN_All ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.LN.SC.sw <- MASS::stepAIC(lm(LocalEfficiency_LN_All ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.LN.EF.sw <- MASS::stepAIC(lm(LocalEfficiency_LN_All ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.LN.EF.sw) # not sig
lm.LE.LN.LN.sw <- MASS::stepAIC(lm(LocalEfficiency_LN_All ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.LN.LN.sw) # not sig
lm.LE.LN.PS.sw <- MASS::stepAIC(lm(LocalEfficiency_LN_All ~ AQ * (BD + MaTr + VP), data = data1))

lm.clcO.LN.SC.sw <- MASS::stepAIC(lm(ClusteringCoefficient_LN_All ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.LN.EF.sw <- MASS::stepAIC(lm(ClusteringCoefficient_LN_All ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.LN.LN.sw <- MASS::stepAIC(lm(ClusteringCoefficient_LN_All ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.LN.LN.sw) # not sig
lm.clcO.LN.PS.sw <- MASS::stepAIC(lm(ClusteringCoefficient_LN_All ~ AQ * (BD + MaTr + VP), data = data1))

# ToM ---------------------------------------------------------------------
lm.GE.ToM.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_ToM_All ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.ToM.SC.sw) # not sig
lm.GE.ToM.EF.sw <- MASS::stepAIC(lm(GlobalEfficiency_ToM_All ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.ToM.LN.sw <- MASS::stepAIC(lm(GlobalEfficiency_ToM_All ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.ToM.LN.sw) # not sig
lm.GE.ToM.PS.sw <- MASS::stepAIC(lm(GlobalEfficiency_ToM_All ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.ToM.SC.sw <- MASS::stepAIC(lm(LocalEfficiency_ToM_All ~ AQ * (SST + RMET + IRI + SSS ), data = data1))
summary(lm.LE.ToM.SC.sw) # not sig
lm.LE.ToM.EF.sw <- MASS::stepAIC(lm(LocalEfficiency_ToM_All ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.ToM.EF.sw) # not sig
lm.LE.ToM.LN.sw <- MASS::stepAIC(lm(LocalEfficiency_ToM_All ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.ToM.LN.sw) # not sig
lm.LE.ToM.PS.sw <- MASS::stepAIC(lm(LocalEfficiency_ToM_All ~ AQ * (BD + MaTr + VP), data = data1))

lm.clcO.ToM.SC.sw <- MASS::stepAIC(lm(ClusteringCoefficient_ToM_All ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.ToM.EF.sw <- MASS::stepAIC(lm(ClusteringCoefficient_ToM_All ~ AQ *(gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.ToM.EF.sw) # not sig
lm.clcO.ToM.LN.sw <- MASS::stepAIC(lm(ClusteringCoefficient_ToM_All ~ AQ *(VF + IF + SE + VB), data = data1))
summary(lm.clcO.ToM.LN.sw) # not sig
lm.clcO.ToM.PS.sw <- MASS::stepAIC(lm(ClusteringCoefficient_ToM_All ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.ToM.PS.sw) # not sig

# ROI by ROI --------------------------------------------------------------
# AC ----------------------------------------------------------------------
lm.GE.AC.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_AC~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.AC.SC.sw) # not sig
lm.GE.AC.EF.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_AC ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.AC.EF.sw) # not sig
lm.GE.AC.LN.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_AC ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.AC.LN.sw) #not sig
lm.GE.AC.PS.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_AC ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.AC.PS.sw) #not sig

lm.LE.AC.SC.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_AC ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.AC.EF.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_AC ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.AC.EF.sw) # not sig
lm.LE.AC.LN.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_AC ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.AC.LN.sw) #not sig
lm.LE.AC.PS.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_AC ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.AC.PS.sw) #not sig

lm.clcO.AC.SC.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_AC ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.AC.EF.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_AC ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.AC.LN.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_AC ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.AC.LN.sw) # not sig
lm.clcO.AC.PS.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_AC ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.AC.PS.sw) # not sig

# LH.AG -------------------------------------------------------------------
lm.GE.lAG.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.AG~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lAG.SC.sw) #not sig
lm.GE.lAG.EF.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.AG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.lAG.LN.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.AG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lAG.LN.sw) #not sig
lm.GE.lAG.PS.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.AG ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.lAG.SC.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.AG ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lAG.SC.sw) #not sig
lm.LE.lAG.EF.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.AG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lAG.EF.sw) #not sig
lm.LE.lAG.LN.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.AG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lAG.LN.sw) #not sig
lm.LE.lAG.PS.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.AG ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lAG.PS.sw) #not sig

lm.clcO.lAG.SC.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.AG ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lAG.SC.sw) #not sig
lm.clcO.lAG.EF.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.AG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lAG.EF.sw) #not sig  
lm.clcO.lAG.LN.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.AG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lAG.LN.sw) # not sig
lm.clcO.lAG.PS.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.AG ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lAG.PS.sw) # not sig

# LH.aMTG -------------------------------------------------------------------
lm.GE.laMTG.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.aMTG~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.laMTG.EF.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.aMTG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.laMTG.EF.sw) #not sig
lm.GE.laMTG.LN.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.aMTG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.laMTG.LN.sw) #not sig
lm.GE.laMTG.PS.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.aMTG ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.laMTG.SC.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.aMTG ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.laMTG.SC.sw) #not sig
lm.LE.laMTG.EF.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.aMTG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.laMTG.EF.sw) #not sig
lm.LE.laMTG.LN.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.aMTG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.laMTG.LN.sw) #not sig
lm.LE.laMTG.PS.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.aMTG ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.laMTG.PS.sw) #not sig

lm.clcO.laMTG.SC.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.aMTG ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.laMTG.SC.sw) #not sig
lm.clcO.laMTG.EF.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.aMTG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.laMTG.EF.sw) #not sig
lm.clcO.laMTG.LN.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.aMTG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.laMTG.LN.sw) # not sig
lm.clcO.laMTG.PS.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.aMTG ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.laMTG.PS.sw) # not sig

# LH.aMTG -------------------------------------------------------------------
lm.GE.laSTG.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.aSTG ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.laSTG.EF.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.aSTG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.laSTG.EF.sw) #not sig
lm.GE.laSTG.LN.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.aSTG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.laSTG.LN.sw) #not sig
lm.GE.laSTG.PS.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.aSTG ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.laSTG.PS.sw) #not sig

lm.LE.laSTG.SC.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.aSTG ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.laSTG.SC.sw) # not sig
lm.LE.laSTG.EF.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.aSTG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.laSTG.LN.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.aSTG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.laSTG.LN.sw) #sig .005
lm.LE.laSTG.PS.sw <- MASS::stepAIC(lm(LocalEfficiency_SC_LH.aSTG ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.laMTG.PS.sw) #not sig

lm.clcO.laSTG.SC.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.aSTG ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.laSTG.EF.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.aSTG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.laSTG.LN.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.aSTG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.laSTG.LN.sw) # sig, does not survive
lm.clcO.laSTG.PS.sw <- MASS::stepAIC(lm(ClusteringCoefficient_SC_LH.aSTG ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.laSTG.PS.sw) # not sig

# LH.FOrb -------------------------------------------------------------------
lm.GE.lForb.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FOrb` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lForb.SC.sw) # not sig
lm.GE.lForb.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FOrb` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lForb.EF.sw) # not sig
lm.GE.lForb.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FOrb` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lForb.LN.sw) #not sig
lm.GE.lForb.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FOrb` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lForb.PS.sw) # not sig

lm.LE.lForb.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.FOrb` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.lForb.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.FOrb` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lForb.EF.sw) # not sig
lm.LE.lForb.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.FOrb` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lForb.LN.sw) # not sig
lm.LE.lForb.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.FOrb` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lForb.PS.sw) # not sig

lm.clcO.lForb.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FOrb` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lForb.SC.sw) # not sig
lm.clcO.lForb.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FOrb` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lForb.EF.sw) # not sig
lm.clcO.lForb.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FOrb` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lForb.LN.sw) # not sig
lm.clcO.lForb.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FOrb` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lForb.PS.sw) # not sig

# LLH.FP -------------------------------------------------------------------
lm.GE.lFP.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FP`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.lFP.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.lFP.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FP` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lFP.LN.sw) #not sig
lm.GE.lFP.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.FP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lFP.PS.sw) #not sig

lm.LE.lFP.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.FP` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lFP.SC.sw) # not sig
lm.LE.lFP.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.FP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lFP.EF.sw) #not sig
lm.LE.lFP.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.FP` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.LE.lFP.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.FP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lFP.PS.sw) #not sig

lm.clcO.lFP.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.FP` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.lFP.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.FP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lFP.EF.sw) # not sig
lm.clcO.lFP.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.FP` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.clcO.lFP.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.FP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lFP.PS.sw) # not sig

# LH HG -------------------------------------------------------------------
lm.GE.lHG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.HG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lHG.SC.sw) # not sig
lm.GE.lHG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.HG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lHG.EF.sw) # not sig
lm.GE.lHG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.HG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lHG.LN.sw) #not sig
lm.GE.lHG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.HG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lHG.PS.sw) #not sig


lm.LE.lHG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.HG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.lHG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.HG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lHG.EF.sw) # not sig
lm.LE.lHG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.HG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lHG.LN.sw) #not sig
lm.LE.lHG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.HG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lHG.PS.sw) #not sig

lm.clcO.lHG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.HG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.lHG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.HG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lHG.EF.sw) # not sig
lm.clcO.lHG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.HG` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.clcO.lHG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.HG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lHG.PS.sw) # not sig

# LH.IC -------------------------------------------------------------------
lm.GE.lIC.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IC`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.lIC.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.lIC.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lIC.LN.sw) #not sig
lm.GE.lIC.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IC` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lIC.PS.sw) #not sig

lm.LE.lIC.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IC` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.lIC.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.lIC.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lIC.LN.sw) #not sig
lm.LE.lIC.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IC` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lIC.PS.sw) #not sig

lm.clcO.lIC.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IC` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.lIC.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.lIC.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lIC.LN.sw) # not sig
lm.clcO.lIC.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IC` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lIC.PS.sw) # not sig

# LH.IFGop -------------------------------------------------------------------
lm.GE.lIFGop.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IFGop`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.lIFGop.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IFGop` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.lIFGop.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IFGop` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lIFGop.LN.sw) #not sig
lm.GE.lIFGop.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IFGop` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lIFGop.PS.sw) #not sig

lm.LE.lIFGop.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IFGop` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lIFGop.SC.sw) #not sig
lm.LE.lIFGop.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IFGop` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lIFGop.EF.sw) #sig, does not survive
lm.LE.lIFGop.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IFGop` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lIFGop.LN.sw) #not sig
lm.LE.lIFGop.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IFGop` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lIFGop.PS.sw) #not sig

lm.clcO.lIFGop.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IFGop` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lIFGop.SC.sw) # not sig
lm.clcO.lIFGop.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IFGop` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lIFGop.EF.sw) #sig, does not survive
lm.clcO.lIFGop.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IFGop` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lIFGop.LN.sw) # not sig
lm.clcO.lIFGop.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IFGop` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lIFGop.PS.sw) # not sig

# LH.IFGtr -------------------------------------------------------------------
lm.GE.lIFGtr.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IFGtr`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lIFGtr.SC.sw) # not sig
lm.GE.lIFGtr.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IFGtr` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lIFGtr.EF.sw) #not sig
lm.GE.lIFGtr.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IFGtr` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lIFGtr.LN.sw) #not sig
lm.GE.lIFGtr.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.IFGtr` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lIFGtr.PS.sw) #sig

lm.LE.lIFGtr.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IFGtr` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lIFGtr.SC.sw) #not sig
lm.LE.lIFGtr.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IFGtr` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lIFGtr.EF.sw) #not sig
lm.LE.lIFGtr.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IFGtr` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lIFGtr.LN.sw) #not sig
lm.LE.lIFGtr.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.IFGtr` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lIFGtr.PS.sw) #not sig

lm.clcO.lIFGtr.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IFGtr` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lIFGtr.SC.sw) #not sig
lm.clcO.lIFGtr.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IFGtr` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lIFGtr.EF.sw) #not sig
lm.clcO.lIFGtr.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IFGtr` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lIFGtr.LN.sw) #not sig
lm.clcO.lIFGtr.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.IFGtr` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lIFGtr.PS.sw) # not sig

# LH.IMFG -------------------------------------------------------------------
lm.GE.lMFG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.MFG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lMFG.SC.sw) #not sig
lm.GE.lMFG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.MFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.lMFG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.MFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lMFG.LN.sw) #not sig
lm.GE.lMFG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.MFG` ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.lMFG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.MFG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lMFG.SC.sw) # not sig
lm.LE.lMFG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.MFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lMFG.EF.sw) #not sig
lm.LE.lMFG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.MFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lMFG.LN.sw) #not sig
lm.LE.lMFG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.MFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lMFG.PS.sw) #not sig

lm.clcO.lMFG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.MFG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.lMFG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.MFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lIFGtr.EF.sw) #not sig
lm.clcO.lMFG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.MFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lIFGtr.LN.sw) #not sig
lm.clcO.lMFG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.MFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lIFGtr.PS.sw) # not sig

# LH.PaCiG -------------------------------------------------------------------
lm.GE.lPaCiG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PaCiG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.lPaCiG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PaCiG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lPaCiG.EF.sw) # not sig
lm.GE.lPaCiG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PaCiG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lPaCiG.LN.sw) #not sig
lm.GE.lPaCiG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PaCiG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lPaCiG.PS.sw) #not sig

lm.LE.lPaCiG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PaCiG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lPaCiG.SC.sw) #sig, does not survive
lm.LE.lPaCiG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PaCiG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lPaCiG.EF.sw) #not sig
lm.LE.lPaCiG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PaCiG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lPaCiG.LN.sw) #not sig
lm.LE.lPaCiG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PaCiG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lPaCiG.PS.sw) #sig, does not survive

lm.clcO.lPaCiG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PaCiG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.lPaCiG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PaCiG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lPaCiG.EF.sw) # not sig
lm.clcO.lPaCiG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PaCiG` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.clcO.lPaCiG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PaCiG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lPaCiG.PS.sw) # sig, does not survive

# LH.PCG -------------------------------------------------------------------
lm.GE.lPCG.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_LH.PreCG ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lPCG.SC.sw) # not sig
lm.GE.lPCG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PreCG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lPCG.EF.sw) # not sig
lm.GE.lPCG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PreCG` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.GE.lPCG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PreCG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lPCG.PS.sw) #not sig

lm.LE.lPCG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PreCG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.lPCG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PreCG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lPCG.EF.sw) #not sig
lm.LE.lPCG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PreCG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lPCG.LN.sw) # not sig
lm.LE.lPCG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PreCG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lPaCiG.PS.sw) #sig, does not survive

lm.clcO.lPCG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PreCG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.lPCG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PreCG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.lPCG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PreCG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lPCG.LN.sw) # not sig
lm.clcO.lPCG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PreCG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lPCG.PS.sw) # not sig

# LH.pMTG -------------------------------------------------------------------
lm.GE.lpMTG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pMTG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.lpMTG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lpMTG.EF.sw) # not sig
lm.GE.lpMTG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lpMTG.LN.sw) #sig, does not survive
lm.GE.lpMTG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pMTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lpMTG.PS.sw) #not sig

lm.LE.lpMTG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pMTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lpMTG.SC.sw) #not sig
lm.LE.lpMTG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lpMTG.EF.sw) #not sig
lm.LE.lpMTG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lpMTG.LN.sw) #sig, does not survive
lm.LE.lpMTG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pMTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lpMTG.PS.sw) #not sig

lm.clcO.lpMTG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pMTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lpMTG.SC.sw) # not sig
lm.clcO.lpMTG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lpMTG.EF.sw) # not sig
lm.clcO.lpMTG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lPCG.LN.sw) # not sig
lm.clcO.lpMTG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pMTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lpMTG.PS.sw) # not sig

# LH.pSMG -------------------------------------------------------------------
lm.GE.lpSMG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pSMG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lpSMG.SC.sw) # not sig
lm.GE.lpSMG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pSMG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lpSMG.EF.sw) # not sig
lm.GE.lpSMG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pSMG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lpSMG.LN.sw) # not sig
lm.GE.lpSMG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pSMG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lpSMG.PS.sw) #not sig

lm.LE.lpSMG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pSMG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lpSMG.SC.sw) #not sig
lm.LE.lpSMG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pSMG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lpSMG.EF.sw) # not sig
lm.LE.lpSMG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pSMG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lpSMG.LN.sw) #not sig
lm.LE.lpSMG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pSMG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lpSMG.PS.sw) #not sig

lm.clcO.lpSMG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pSMG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lpSMG.SC.sw) # not sig
lm.clcO.lpSMG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pSMG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lpSMG.EF.sw) #not sig
lm.clcO.lpSMG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pSMG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lpSMG.LN.sw) # not sig
lm.clcO.lpSMG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pSMG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lpSMG.PS.sw) # not sig

# LH.pSTG -------------------------------------------------------------------
lm.GE.lpSTG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pSTG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lpSTG.SC.sw) #not sig
lm.GE.lpSTG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.lpSTG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pSTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lpSTG.LN.sw) #not sig
lm.GE.lpSTG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.pSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lpSTG.PS.sw) #sig, does not survive

lm.LE.lpSTG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pSTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lpSTG.SC.sw) #not sig
lm.LE.lpSTG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.lpSTG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pSTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lpSTG.LN.sw) #not sig
lm.LE.lpSTG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.pSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lpSTG.PS.sw) #not sig

lm.clcO.lpSTG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pSTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lpSTG.SC.sw) # not sig
lm.clcO.lpSTG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.lpSTG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pSTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lpSTG.LN.sw) # not sig
lm.clcO.lpSTG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.pSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lpSTG.PS.sw) #  sig, does not survive

# LH.PT -------------------------------------------------------------------
lm.GE.lPT.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PT`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lPT.SC.sw) #not sig
lm.GE.lPT.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PT` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lPT.EF.sw) #not sig
lm.GE.lPT.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PT` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lPT.LN.sw) #not sig
lm.GE.lPT.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.PT` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lPT.PS.sw) #not sig

lm.LE.lPT.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PT` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lPT.SC.sw) # not sig
lm.LE.lPT.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PT` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.lPT.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PT` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lPT.LN.sw) #not sig
lm.LE.lPT.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.PT` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lPT.PS.sw) #not sig

lm.clcO.lPT.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PT` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lPT.SC.sw) # not sig
lm.clcO.lPT.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PT` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lPT.EF.sw) # not sig
lm.clcO.lPT.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PT` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lPT.LN.sw) # not sig
lm.clcO.lPT.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.PT` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lPT.PS.sw) #not sig

# LH.SFG -------------------------------------------------------------------
lm.GE.lSFG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.SFG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.lSFG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.SFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lSFG.EF.sw) #not sig
lm.GE.lSFG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.SFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lSFG.LN.sw) #sig, does not survive
lm.GE.lSFG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.SFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lSFG.PS.sw) #not sig

lm.LE.lSFG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.SFG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.lSFG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.SFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lSFG.EF.sw) # not sig
lm.LE.lSFG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.SFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lSFG.LN.sw) # not sig
lm.LE.lSFG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.SFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lSFG.PS.sw) #not sig

lm.clcO.lSFG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.SFG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.lSFG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.SFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lSFG.EF.sw) # not sig
lm.clcO.lSFG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.SFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lSFG.LN.sw) # not sig
lm.clcO.lSFG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.SFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lSFG.PS.sw) #not sig

# LH.SMA -------------------------------------------------------------------
lm.GE.lSMA.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.SMA`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.lSMA.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.SMA` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.lSMA.EF.sw) # not sig
lm.GE.lSMA.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.SMA` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.GE.lSMA.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.SMA` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lSMA.PS.sw) # not sig

lm.LE.lSMA.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.SMA` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.lSMA.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.SMA` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.lSMA.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.SMA` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lSMA.LN.sw) #not sig
lm.LE.lSMA.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.SMA` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.lSMA.PS.sw) #not sig

lm.clcO.lSMA.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.SMA` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lSMA.SC.sw) # not sig
lm.clcO.lSMA.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.SMA` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.lSMA.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.SMA` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lSMA.LN.sw) # not sig
lm.clcO.lSMA.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.SMA` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lSMA.PS.sw) #not sig

# LH.TP -------------------------------------------------------------------
lm.GE.lTP.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.TP`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.lTP.SC.sw) # not sig
lm.GE.lTP.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.TP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.lTP.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.TP` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lTP.LN.sw) #not sig
lm.GE.lTP.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_LH.TP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.lTP.PS.sw) #not sig

lm.LE.lTP.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.TP` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.lTP.SC.sw) #not sig
lm.LE.lTP.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.TP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.lTP.EF.sw) # not sig
lm.LE.lTP.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.TP` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.lTP.LN.sw) # not sig
lm.LE.lTP.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_LH.TP` ~ AQ * (BD + MaTr + VP), data = data1))

lm.clcO.lTP.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.TP` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.lTP.SC.sw) # not sig
lm.clcO.lTP.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.TP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.lTP.EF.sw) # not sig
lm.clcO.lTP.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.TP` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.lTP.LN.sw) # sig, does not survive
lm.clcO.lTP.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_LH.TP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.lTP.PS.sw) #not sig

# MedFC -------------------------------------------------------------------
lm.GE.MedFC.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_MedFC`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.MedFC.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_MedFC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.MedFC.EF.sw) # not sig
lm.GE.lMedFC.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_MedFC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lMedFC.LN.sw) # not sig
lm.GE.MedFC.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_MedFC` ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.MedFC.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_MedFC` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.MedFC.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_MedFC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.MedFC.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_MedFC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.MedFC.LN.sw) # not sig
lm.LE.MedFC.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_MedFC` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.MedFC.PS.sw) #not sig

lm.clcO.MedFC.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_MedFC` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.MedFC.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_MedFC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.MedFC.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_MedFC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.MedFC.LN.sw) # not sig
lm.clcO.MedFC.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_MedFC` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.MedFC.PS.sw) #not sig

# Precuneus -------------------------------------------------------------------
lm.GE.Precuneus.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_Precuneus`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.Precuneus.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_Precuneus` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.lPrecuneus.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_Precuneus` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.lPrecuneus.LN.sw) #not sig
lm.GE.Precuneus.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_Precuneus` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.Precuneus.PS.sw) #sig, .0008

lm.LE.Precuneus.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_Precuneus` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.Precuneus.SC.sw) #not sig
lm.LE.Precuneus.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_Precuneus` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.Precuneus.EF.sw) #not sig
lm.LE.Precuneus.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_Precuneus` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.Precuneus.LN.sw) #not sig
lm.LE.Precuneus.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_Precuneus` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.Precuneus.PS.sw) #not sig

lm.clcO.Precuneus.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_Precuneus` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.Precuneus.SC.sw) # not sig
lm.clcO.Precuneus.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_Precuneus` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.Precuneus.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_Precuneus` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.Precuneus.LN.sw) # not sig
lm.clcO.Precuneus.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_Precuneus` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.Precuneus.PS.sw) #not sig

# RH.AG -------------------------------------------------------------------
lm.GE.rAG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.AG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rAG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.AG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rAG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.AG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rAG.LN.sw) #sig, does not survive
lm.GE.rAG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.AG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rAG.PS.sw) #not sig

lm.LE.rAG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.AG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rAG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.AG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rAG.EF.sw) # not sig
lm.LE.rAG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.AG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rAG.LN.sw) # not sig
lm.LE.rAG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.AG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rAG.PS.sw) #not sig

lm.clcO.rAG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.AG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rAG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.AG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.rAG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.AG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rAG.LN.sw) # not sig
lm.clcO.rAG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.AG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rAG.PS.sw) #not sig

# RH.aMTG -------------------------------------------------------------------
lm.GE.raMTG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.aMTG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.raMTG.SC.sw) #not sig
lm.GE.raMTG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.aMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.raMTG.EF.sw) #not sig
lm.GE.raMTG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.aMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.raMTG.LN.sw) # not sig
lm.GE.raMTG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.aMTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.raMTG.PS.sw) #not sig

lm.LE.raMTG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.aMTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.raMTG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.aMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.raMTG.EF.sw) # sig
lm.LE.raMTG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.aMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.raMTG.LN.sw) #not sig
lm.LE.raMTG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.aMTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.raMTG.PS.sw) #not sig

lm.clcO.raMTG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.aMTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.raMTG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.aMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.raMTG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.aMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.raMTG.LN.sw) # not sig
lm.clcO.raMTG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.aMTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.raMTG.PS.sw) # not sig

# RH.aSTG -------------------------------------------------------------------
lm.GE.raSTG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.aSTG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.raSTG.SC.sw) # not sig
lm.GE.raSTG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.aSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.raSTG.EF.sw) # not sig
lm.GE.raSTG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.aSTG` ~ AQ * (VF + IF + SE + VB), data = data1, maxIter=16000))
summary(lm.GE.raSTG.LN.sw) #not sig
lm.GE.raSTG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.aSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.raSTG.PS.sw) #not sig

lm.LE.raSTG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.aSTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.raSTG.SC.sw) #not sig
lm.LE.raSTG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.aSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.raSTG.EF.sw) #not sig
lm.LE.raSTG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.aSTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.raSTG.LN.sw) #not sig
lm.LE.raSTG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.aSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.raSTG.PS.sw) #not sig

lm.clcO.raSTG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.aSTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.raSTG.SC.sw) #not sig
lm.clcO.raSTG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.aSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.raSTG.EF.sw) #not sig
lm.clcO.raSTG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.aSTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.raSTG.LN.sw) #sig
lm.clcO.raSTG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.aSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.raSTG.PS.sw) #not sig

# RH.FOrb -------------------------------------------------------------------
lm.GE.rFOrb.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_RH.FOrb~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rFOrb.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.FOrb` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rFOrb.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.FOrb` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rFOrb.LN.sw) # not sig
lm.GE.rFOrb.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.FOrb` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rFOrb.PS.sw) #not sig

lm.LE.rFOrb.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.FOrb` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.rFOrb.SC.sw) #not sig
lm.LE.rFOrb.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.FOrb` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rFOrb.EF.sw) # not sig
lm.LE.rFOrb.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.FOrb` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rFOrb.LN.sw) # not sig
lm.LE.rFOrb.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.FOrb` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rFOrb.PS.sw) #not sig

lm.clcO.rFOrb.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.FOrb` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.rFOrb.SC.sw) #not sig
lm.clcO.rFOrb.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.FOrb` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rFOrb.EF.sw) # not sig
lm.clcO.rFOrb.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.FOrb` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rFOrb.LN.sw) # not sig
lm.clcO.rFOrb.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.FOrb` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rFOrb.PS.sw) # not sig

# RH.FP -------------------------------------------------------------------
lm.GE.rFP.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.FP`~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rFP.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.FP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.rFP.EF.sw) # not sig
lm.GE.rFP.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.FP` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rFP.LN.sw) # not sig
lm.GE.rFP.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.FP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rFP.PS.sw) #not sig

lm.LE.rFP.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.FP` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rFP.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.FP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rFP.EF.sw) # sig
lm.LE.rFP.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.FP` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rFP.LN.sw) #not sig
lm.LE.rFP.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.FP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rFP.PS.sw) #sig, does not survive

lm.clcO.rFP.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.FP` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rFP.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.FP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rFP.EF.sw) # not sig
lm.clcO.rFP.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.FP` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rFP.LN.sw) # not sig
lm.clcO.rFP.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.FP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rFP.PS.sw) # not sig

# RH.HG -------------------------------------------------------------------
lm.GE.rHG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.HG`~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.rHG.SC.sw) # not sig
lm.GE.rHG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.HG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.rHG.EF.sw) # not sig
lm.GE.rHG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.HG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rHG.LN.sw) #not sig
lm.GE.rHG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.HG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rHG.PS.sw) #not sig

lm.LE.rHG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.HG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.rHG.SC.sw) # not sig
lm.LE.rHG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.HG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rHG.EF.sw) # not sig
lm.LE.rHG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.HG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rHG.LN.sw) #not sig
lm.LE.rHG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.HG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rHG.PS.sw) #not sig

lm.clcO.rHG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.HG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.rHG.SC.sw) # not sig
lm.clcO.rHG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.HG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rHG.EF.sw) # not sig
lm.clcO.rHG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.HG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rHG.LN.sw) # not sig
lm.clcO.rHG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.HG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rHG.PS.sw) #not sig

# RH.IC -------------------------------------------------------------------
lm.GE.rIC.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IC` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.rIC.SC.sw) # not sig
lm.GE.rIC.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.rIC.EF.sw) # not sig
lm.GE.rIC.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rIC.LN.sw) #not sig
lm.GE.rIC.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IC` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rIC.PS.sw) #not sig

lm.LE.rIC.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IC` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rIC.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rIC.EF.sw) # not sig
lm.LE.rIC.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rIC.LN.sw) # not sig
lm.LE.rIC.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IC` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rIC.PS.sw) #not sig

lm.clcO.rIC.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IC` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.rIC.SC.sw) # not sig
lm.clcO.rIC.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IC` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rIC.EF.sw) # not sig
lm.clcO.rIC.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IC` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rIC.LN.sw) # not sig
lm.clcO.rIC.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IC` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rIC.PS.sw) #not sig

# RH.IFGop -------------------------------------------------------------------
lm.GE.rIFGop.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IFGop` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.rIFGop.SC.sw) #not sig
lm.GE.rIFGop.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IFGop` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rIFGop.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IFGop` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.GE.rIFGop.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IFGop` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rIFGop.PS.sw) # not sig

lm.LE.rIFGop.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IFGop` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.rIFGop.SC.sw) #sig, does not survive
lm.LE.rIFGop.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IFGop` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.rIFGop.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IFGop` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rIFGop.LN.sw) #sig, does not survive
lm.LE.rIFGop.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IFGop` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rIFGop.PS.sw) #sig, does not survive

lm.clcO.rIFGop.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IFGop` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.rIFGop.SC.sw) # not sig
lm.clcO.rIFGop.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IFGop` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rIFGop.EF.sw) # not sig
lm.clcO.rIFGop.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IFGop` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rIFGop.LN.sw) # not sig
lm.clcO.rIFGop.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IFGop` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rIFGop.PS.sw) #sig, does not survive

# RH.IFGtr -------------------------------------------------------------------
lm.GE.rIFGtr.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IFGtr` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.rIFGtr.SC.sw) # not sig
lm.GE.rIFGtr.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IFGtr` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.rIFGtr.EF.sw) # not sig
lm.GE.rIFGtr.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IFGtr` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rIFGtr.LN.sw) #sig, does not survive
lm.GE.rIFGtr.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.IFGtr` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rIFGtr.PS.sw) #sig, does not survive

lm.LE.rIFGtr.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IFGtr` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rIFGtr.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IFGtr` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.rIFGtr.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IFGtr` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.LE.rIFGtr.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.IFGtr` ~ AQ * (BD + MaTr + VP), data = data1))

lm.clcO.rIFGtr.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IFGtr` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rIFGtr.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IFGtr` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.rIFGtr.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IFGtr` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.clcO.rIFGtr.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.IFGtr` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rIFGtr.PS.sw) #not sig

# RH.MFG -------------------------------------------------------------------
lm.GE.rMFG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.MFG` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rMFG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.MFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rMFG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.MFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rMFG.LN.sw) #not sig
lm.GE.rMFG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.MFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rMFG.PS.sw) #not sig

lm.LE.rMFG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.MFG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rMFG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.MFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rMFG.EF.sw) # not sig
lm.LE.rMFG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.MFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rMFG.LN.sw) # not sig
lm.LE.rMFG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.MFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rMFG.PS.sw) #not sig

lm.clcO.rMFG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.MFG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rMFG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.MFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rMFG.EF.sw) # not sig
lm.clcO.rMFG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.MFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rMFG.LN.sw) # not sig
lm.clcO.rMFG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.MFG` ~ AQ * (BD + MaTr + VP), data = data1))

# RH.PaCiG -------------------------------------------------------------------
lm.GE.rPaCiG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PaCiG` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.rPaCiG.SC.sw) # not sig
lm.GE.rPaCiG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PaCiG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.rPaCiG.EF.sw) # not sig
lm.GE.rPaCiG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PaCiG` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.GE.rPaCiG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PaCiG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rPaCiG.PS.sw) #not sig

lm.LE.rPaCiG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PaCiG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.rPaCiG.SC.sw) # not sig
lm.LE.rPaCiG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PaCiG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rPaCiG.EF.sw) #not sig
lm.LE.rPaCiG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PaCiG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rPaCiG.LN.sw) #not sig
lm.LE.rPaCiG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PaCiG` ~ AQ * (BD + MaTr + VP), data = data1))

lm.clcO.rPaCiG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PaCiG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rPaCiG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PaCiG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rPaCiG.EF.sw) # sig, does not survive
lm.clcO.rPaCiG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PaCiG` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.clcO.rPaCiG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PaCiG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rPaCiG.PS.sw) #not sig

# RH.PCG -------------------------------------------------------------------
lm.GE.rPCG.SC.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_RH.PreCG ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rPCG.EF.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_RH.PreCG ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.rPCG.EF.sw) #sig, does not survive
lm.GE.rPCG.LN.sw <- MASS::stepAIC(lm(GlobalEfficiency_SC_RH.PreCG ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rPCG.LN.sw) #sig, does not survive
lm.GE.rPCG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PreCG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rPCG.PS.sw) #not sig

lm.LE.rPCG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PreCG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.rPCG.SC.sw) #not sig
lm.LE.rPCG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PreCG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.rPCG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PreCG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rPCG.LN.sw) #not sig
lm.LE.rPCG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PreCG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rPCG.PS.sw) #not sig

lm.clcO.rPCG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PreCG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.rPCG.SC.sw) # not sig
lm.clcO.rPCG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PreCG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.rPCG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PreCG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rPCG.LN.sw) # not sig
lm.clcO.rPCG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PreCG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rPCG.PS.sw) #not sig

# RH.pMTG -------------------------------------------------------------------
lm.GE.rpMTG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pMTG` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.rpMTG.SC.sw) #not sig
lm.GE.rpMTG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.GE.rpMTG.EF.sw) #not sig
lm.GE.rpMTG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.GE.rpMTG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pMTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rpMTG.PS.sw) #not sig

lm.LE.rpMTG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pMTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rpMTG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.rpMTG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.LE.rpMTG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pMTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rpMTG.PS.sw) #not sig

lm.clcO.rpMTG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pMTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rpMTG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pMTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rpMTG.EF.sw) # not sig
lm.clcO.rpMTG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pMTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rpMTG.LN.sw) # not sig
lm.clcO.rpMTG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pMTG` ~ AQ * (BD + MaTr + VP), data = data1))

# RH.pSMG -------------------------------------------------------------------
lm.GE.rpSMG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pSMG` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rpSMG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pSMG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rpSMG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pSMG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rpSMG.LN.sw) #sig
lm.GE.rpSMG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pSMG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rpSMG.PS.sw) #not sig

lm.LE.rpSMG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pSMG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.rpSMG.SC.sw) #not sig
lm.LE.rpSMG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pSMG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.rpSMG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pSMG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rpSMG.LN.sw) #not sig
lm.LE.rpSMG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pSMG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rpSMG.PS.sw) #sig, does not survive

lm.clcO.rpSMG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pSMG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.rpSMG.SC.sw) #sig, does not survive
lm.clcO.rpSMG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pSMG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.rpSMG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pSMG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rpSMG.LN.sw) # not sig
lm.clcO.rpSMG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pSMG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rpSMG.PS.sw) #sig, does not survive

# RH.pSTG -------------------------------------------------------------------
lm.GE.rpSTG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pSTG` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rpSTG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rpSTG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pSTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rpSTG.LN.sw) #sig, does not survive
lm.GE.rpSTG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.pSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rpSTG.PS.sw) #not sig

lm.LE.rpSTG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pSTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rpSTG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.rpSTG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pSTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rpSTG.LN.sw) #not sig
lm.LE.rpSTG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.pSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rpSTG.PS.sw) #not sig

lm.clcO.rpSTG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pSTG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rpSTG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pSTG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.rpSTG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pSTG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rpSTG.LN.sw) # not sig
lm.clcO.rpSTG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.pSTG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rpSTG.PS.sw) #sig, does not survive

# RH.PT -------------------------------------------------------------------
lm.GE.rPT.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PT` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rPT.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PT` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rPT.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PT` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rPT.LN.sw) #not sig
lm.GE.rPT.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.PT` ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.rPT.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PT` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.rPT.SC.sw) #not sig
lm.LE.rPT.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PT` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.rPT.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PT` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rPT.LN.sw) #not sig
lm.LE.rPT.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.PT` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rPT.PS.sw) #not sig

lm.clcO.rPT.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PT` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rPT.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PT` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.clcO.rPT.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PT` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rPT.LN.sw) # not sig
lm.clcO.rPT.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.PT` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rPT.PS.sw) #not sig

# RH.SFG -------------------------------------------------------------------
lm.GE.rSFG.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.SFG` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.rSFG.SC.sw) #not sig
lm.GE.rSFG.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.SFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rSFG.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.SFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rSFG.LN.sw) #not sig
lm.GE.rSFG.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.SFG` ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.rSFG.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.SFG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rSFG.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.SFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.LE.rSFG.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.SFG` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.LE.rSFG.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.SFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rSFG.PS.sw) #not sig

lm.clcO.rSFG.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.SFG` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rSFG.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.SFG` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rSFG.EF.sw) # not sig
lm.clcO.rSFG.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.SFG` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rSFG.LN.sw) # not sig
lm.clcO.rSFG.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.SFG` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rSFG.PS.sw) #not sig

# RH.SMA -------------------------------------------------------------------
lm.GE.rSMA.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.SMA` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
lm.GE.rSMA.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.SMA` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rSMA.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.SMA` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.GE.rSMA.LN.sw) #not sig
lm.GE.rSMA.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.SMA` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.GE.rSMA.PS.sw) #not sig

lm.LE.rSMA.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.SMA` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.LE.rSMA.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.SMA` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rSMA.EF.sw) #sig, does not survive
lm.LE.rSMA.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.SMA` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.LE.rSMA.LN.sw) #not sig
lm.LE.rSMA.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.SMA` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rSMA.PS.sw) #sig, does not survive

lm.clcO.rSMA.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.SMA` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
lm.clcO.rSMA.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.SMA` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rSMA.EF.sw) # sig, does not survive
lm.clcO.rSMA.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.SMA` ~ AQ * (VF + IF + SE + VB), data = data1))
summary(lm.clcO.rSMA.LN.sw) # not sig
lm.clcO.rSMA.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.SMA` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rSMA.PS.sw) #not sig

# RH.TP -------------------------------------------------------------------
lm.GE.rTP.SC.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.TP` ~ AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.GE.rTP.SC.sw) #sig
lm.GE.rTP.EF.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.TP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
lm.GE.rTP.LN.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.TP` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.GE.rTP.PS.sw <- MASS::stepAIC(lm(`GlobalEfficiency_SC_RH.TP` ~ AQ * (BD + MaTr + VP), data = data1))

lm.LE.rTP.SC.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.TP` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.LE.rTP.SC.sw) #not sig
lm.LE.rTP.EF.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.TP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.LE.rTP.EF.sw) #not sig
lm.LE.rTP.LN.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.TP` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.LE.rTP.PS.sw <- MASS::stepAIC(lm(`LocalEfficiency_SC_RH.TP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.LE.rTP.PS.sw) #not sig

lm.clcO.rTP.SC.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.TP` ~  AQ * (SST + RMET + IRI + SSS), data = data1))
summary(lm.clcO.rTP.SC.sw) # not sig
lm.clcO.rTP.EF.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.TP` ~ AQ * (gonogo + Tower.of.London + nback + Digit.span), data = data1))
summary(lm.clcO.rTP.EF.sw) # sig, does not survive
lm.clcO.rTP.LN.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.TP` ~ AQ * (VF + IF + SE + VB), data = data1))
lm.clcO.rTP.PS.sw <- MASS::stepAIC(lm(`ClusteringCoefficient_SC_RH.TP` ~ AQ * (BD + MaTr + VP), data = data1))
summary(lm.clcO.rTP.PS.sw) # sig

# Bootstrap ---------------------------------------------------------------
set.seed(123)
lm.GE.rpSMG.LN.sw.bs <- bootstrap_parameters(lm.GE.rpSMG.LN.sw, iterations = 10000, ci = 0.95)
summary(lm.GE.rpSMG.LN.sw) # 0.001, IF ** and AQ:VB*
summary(lm.GE.rpSMG.LN.sw.bs) # IF**, AQ:VB 0.10

lm.GE.Precuneus.PS.sw.bs <- bootstrap_parameters(lm.GE.Precuneus.PS.sw, iterations = 10000, ci = 0.95)
summary(lm.GE.Precuneus.PS.sw) # 0.0008, AQ *, MaTr ***, AQ:VP *
summary(lm.GE.Precuneus.PS.sw.bs) # AQ **, MaTr ***, AQ:VP **

lm.LE.laSTG.LN.sw.bs <-bootstrap_parameters(lm.LE.laSTG.LN.sw, iterations = 10000, ci = 0.95)
summary(lm.LE.laSTG.LN.sw) # .0009, VF**, VB*, AQ:VF**
summary(lm.LE.laSTG.LN.sw.bs) #VF*, VB*, AQ:VF*

lm.LE.lpMTG.LN.sw.bs <- bootstrap_parameters(lm.LE.lpMTG.LN.sw, iterations = 10000, ci = 0.95)
summary(lm.LE.lpMTG.LN.sw) # 0.0009, AQ**, VF**, AQ:VF*
summary(lm.LE.lpMTG.LN.sw.bs) # AQ**, VF**, AQ:VF 0.06

lm.LE.rFP.EF.sw.bs <- bootstrap_parameters(lm.LE.rFP.EF.sw, iterations = 10000, ci = 0.95)
summary(lm.LE.rFP.EF.sw) # 0.0006, AQ*, ToL**, AQ*ToL***
summary(lm.LE.rFP.EF.sw.bs) # AQ*, ToL 0.13, AQ*ToL 0.07

lm.LE.raMTG.EF.sw.bs <- bootstrap_parameters(lm.LE.raMTG.EF.sw, iterations = 10000, ci = 0.95)
summary(lm.LE.raMTG.EF.sw) # 0.009, AQ*, gonogo*, nback*, AQ*gonogo*
summary(lm.LE.raMTG.EF.sw.bs) # AQ*, gonogo 0.09, nback 0.07, AQ*gonogo*

# Testing model assumptions -----------------------------------------------
# LE.raMTG.EF 
performance::check_normality(lm.GE.rpSMG.LN.sw) #normal
performance::check_heteroscedasticity(lm.GE.rpSMG.LN.sw) #homoscedastic
performance::check_outliers(lm.GE.rpSMG.LN.sw) # not ourliers
performance::check_autocorrelation(lm.GE.rpSMG.LN.sw) # not autocorrelated

# GE.Precuneus.PS 
performance::check_normality(lm.GE.Precuneus.PS.sw) #normal
performance::check_heteroscedasticity(lm.GE.Precuneus.PS.sw) #homoscedastic
performance::check_outliers(lm.GE.Precuneus.PS.sw) # not ourliers
performance::check_autocorrelation(lm.GE.Precuneus.PS.sw) # not autocorrelated

# LE.laSTG.LN 
performance::check_normality(lm.LE.laSTG.LN.sw) #warning not normal
performance::check_heteroscedasticity(lm.LE.laSTG.LN.sw) #homoscedastic
performance::check_outliers(lm.LE.laSTG.LN.sw) # not ourliers
performance::check_autocorrelation(lm.LE.laSTG.LN.sw) # not autocorrelated

# LE.lpMTG 
performance::check_normality(lm.LE.lpMTG.LN.sw) #normal
performance::check_heteroscedasticity(lm.LE.lpMTG.LN.sw) # warning heteroscedasticity
performance::check_outliers(lm.LE.lpMTG.LN.sw) # not ourliers
performance::check_autocorrelation(lm.LE.lpMTG.LN.sw) # not autocorrelated

# LE.rFP.EF 
performance::check_normality(lm.LE.rFP.EF.sw) #normal
performance::check_heteroscedasticity(lm.LE.rFP.EF.sw) #homoscedastic
performance::check_outliers(lm.LE.rFP.EF.sw) # not ourliers
performance::check_autocorrelation(lm.LE.rFP.EF.sw) # not autocorrelated

# LE.raMTG.EF 
performance::check_normality(lm.LE.raMTG.EF.sw) #normal
performance::check_heteroscedasticity(lm.LE.raMTG.EF.sw) #homoscedastic
performance::check_outliers(lm.LE.raMTG.EF.sw) # not ourliers
performance::check_autocorrelation(lm.LE.raMTG.EF.sw) # not autocorrelated

# Relative contribution ---------------------------------------------------
lm.GE.rpSMG.LN.sw.ppd <- relaimpo::calc.relimp(lm.GE.rpSMG.LN.sw, type = c("lmg"))
lm.GE.Precuneus.PS.sw.ppd <- relaimpo::calc.relimp(lm.GE.Precuneus.PS.sw, type = c("lmg"))
lm.LE.laSTG.LN.sw.ppd <- relaimpo::calc.relimp(lm.LE.laSTG.LN.sw, type = c("lmg"))
lm.LE.lpMTG.LN.sw.ppd <- relaimpo::calc.relimp(lm.LE.lpMTG.LN.sw, type = c("lmg"))
lm.LE.rFP.EF.sw.ppd <- relaimpo::calc.relimp(lm.LE.rFP.EF.sw, type = c("lmg"))
lm.LE.raMTG.EF.sw.ppd <- relaimpo::calc.relimp(lm.LE.raMTG.EF.sw, type = c("lmg"))

library(pandoc)
# Saving regression models ------------------------------------------------
sink("results/model.GE.rpSMG.LN.txt")
summary(lm.GE.rpSMG.LN.sw) # 1
sink()  

sink("results/model.GE.Precuneus.PS.txt")
summary(lm.GE.Precuneus.PS.sw) # 1
sink()  

sink("results/model.LE.laSTG.LN.txt")
summary(lm.LE.laSTG.LN.sw) # 1
sink()  

sink("results/model.LE.lpMTG.LN.txt")
summary(lm.LE.lpMTG.LN.sw) # 1
sink()  

sink("results/model.LE.rFP.EF.txt")
summary(lm.LE.rFP.EF.sw) # 1
sink()  

sink("results/model.LE.raMTG.EF.txt")
summary(lm.LE.raMTG.EF.sw) # 1
sink()  

# Figures -----------------------------------------------------------------
data1 %>% 
  ggplot(aes(AQ, GlobalEfficiency_SC_RH.pSMG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("Right pSMG") +
  xlab("AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))

# Global Efficiency - RH pSMG
summary(lm.GE.rpSMG.LN.sw)
interact_plot(lm.GE.rpSMG.LN.sw, pred = "VB", modx = "AQ", interval = FALSE) +
  jtools::theme_apa() +
  ylab("RH pSMG") +
  xlab("Vocabulary*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10)) + 
  theme(legend.position = "none")
ggsave("figure/GE_RH.pSMG_Vocabulary.AQ_LN.png", width = 2.4, height = 1.5)

ggplot(data1, aes(VB*AQ, GlobalEfficiency_SC_RH.pSMG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH pSMG") +
  xlab("Vocabulary*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10)) + 
  theme(legend.position = "none")
ggsave("figure/GE_RH.pSMG_Vocabulary.AQ_LN_Simple.png", width = 2.4, height = 1.5)

ggplot(data1, aes(IF, GlobalEfficiency_SC_RH.pSMG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH pSMG") +
  xlab("Information") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/GE_RH.pSMG_Information_LN.png", width = 2.4, height = 1.5)

# Global Efficiency - RH IFGor
summary(lm.GE.rIFGor.EF.sw)
ggplot(data1, aes(gonogo, GlobalEfficiency_SC_RH.FOrb)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH IFGor") +
  xlab("Go/No-Go") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/GE_RH.IFGor_gonogo_EF.png", width = 2.4, height = 1.5)

ggplot(data1, aes(Tower.of.London, GlobalEfficiency_SC_RH.FOrb)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH IFGor") +
  xlab("Tower of London") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/GE_RH.IFGor_ToL_EF.png", width = 2.4, height = 1.5)

# Global Efficiency - RH IFGor
summary(lm.GE.Precuneus.PS.sw)
data1 %>% 
  ggplot(aes(AQ, GlobalEfficiency_SC_Precuneus)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("Precuneus") +
  xlab("AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/GE_Precuneus_AQ_PS.png", width = 2.4, height = 1.5)


data1 %>% 
  ggplot(aes(MaTr, GlobalEfficiency_SC_Precuneus)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("Precuneus") +
  xlab("Matrix") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/GE_Precuneus_Matrix_PS.png", width = 2.4, height = 1.5)

interact_plot(lm.GE.Precuneus.PS.sw, pred = "RV", modx = "AQ", interval = FALSE) +
  jtools::theme_apa() +
  ylab("Precuneus") +
  xlab("Visual Puzzle*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10)) + 
  theme(legend.position = "none")
ggsave("figure/GE_precuneus_RV.AQ_PS.png", width = 2.4, height = 1.5)

data1 %>% 
  filter(GlobalEfficiency_SC_Precuneus >= 0.1 ) %>% 
  ggplot(aes(RV*AQ, GlobalEfficiency_SC_Precuneus)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("Precuneus") +
  xlab("Visual Puzzle*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/GE_Precuneus_RV.AQ_PS_Simple.png", width = 2.4, height = 1.5)


# Global Efficiency - RH IFGor
summary(lm.GE.lHG.SC.sw)
ggplot(data1, aes(SSS, GlobalEfficiency_SC_LH.HG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("LH Heschl") +
  xlab("SSS") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/GE_LH.HG_SSS_SC.png", width = 2.4, height = 1.5)

# Local Efficiency - LH aSTG
summary(lm.LE.laSTG.LN.sw) # 1
interact_plot(lm.LE.laSTG.LN.sw, pred = "VF", modx = "AQ", interval = FALSE) +
  jtools::theme_apa() +
  ylab("LH aSTG") +
  xlab("Verbal Fluency*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10)) + 
  theme(legend.position = "none")
ggsave("figure/LE_LH.aSTG_VerbalFluency.AQ_LN.png", width = 2.4, height = 1.5)

data1 %>% 
  ggplot(aes(VF*AQ, LocalEfficiency_SC_LH.aSTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("LH aSTG") +
  xlab("Verbal Fluency*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_LH.aSTG_VerbalFluency.AQ_LN_Simple.png", width = 2.4, height = 1.5)

data1 %>% 
  filter(LocalEfficiency_SC_LH.aSTG >=0.01) %>% 
  ggplot(aes(AQ, LocalEfficiency_SC_LH.aSTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("LH aSTG") +
  xlab("AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_LH.aSTG_AQ.png", width = 2.4, height = 1.5)

data1 %>% 
  filter(LocalEfficiency_SC_LH.aSTG >=0.01) %>% 
  ggplot(aes(VB, LocalEfficiency_SC_LH.aSTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("LH aSTG") +
  xlab("Vocabulary") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_LH.aSTG_Vocabulary.png", width = 2.4, height = 1.5)

data1 %>% 
  filter(LocalEfficiency_SC_LH.aSTG >=0.01) %>% 
  ggplot(aes(VF, LocalEfficiency_SC_LH.aSTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("LH aSTG") +
  xlab("Verbal Fluency") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_LH.aSTG_VF.png", width = 2.4, height = 1.5)

# Local Efficiency - LH pMTG
summary(lm.LE.lpMTG.LN.sw) # 1
interact_plot(lm.LE.lpMTG.LN.sw, pred = "VF", modx = "AQ", interval = FALSE) +
  jtools::theme_apa() +
  ylab("LH pMTG") +
  xlab("Verbal Fluency*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10)) + 
  theme(legend.position = "none")
ggsave("figure/LE_LH.pMTG_VF.AQ_LN.png", width = 2.4, height = 1.5)

data1 %>% 
#  filter(LocalEfficiency_SC_LH.pMTG >=0.01) %>% 
  ggplot(aes(VF, LocalEfficiency_SC_LH.pMTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("LH pMTG") +
  xlab("Verbal Fluency") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_LH.pMTG_VF.AQ_LN_Simple.png", width = 2.4, height = 1.5)

ggplot(data1, aes(AQ, LocalEfficiency_SC_LH.pMTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("LH pMTG") +
  xlab("AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_LH.pMTG_AQ.png", width = 2.4, height = 1.5)

ggplot(data1, aes(VF, LocalEfficiency_SC_LH.pMTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("LH pMTG") +
  xlab("Verbal Fluency") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_LH.pMTG_VF.png", width = 2.4, height = 1.5)

# Local Efficiency - RH FP
summary(lm.LE.rFP.EF.sw) # 1
interact_plot(lm.LE.rFP.EF.sw, pred = "Tower.of.London", modx = "AQ", interval = FALSE) +
  jtools::theme_apa() +
  ylab("RH FP") +
  xlab("Tower of London*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10)) + 
  theme(legend.position = "none")
ggsave("figure/LE_RH.FP_ToL.AQ_EF.png", width = 2.4, height = 1.5)

data1 %>% 
#  filter(LocalEfficiency_SC_RH.FP >= 0.001) %>% 
  ggplot(aes(Tower.of.London*AQ, LocalEfficiency_SC_RH.FP)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH FP") +
  xlab("Tower.of.London*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_RH.FP_ToL.AQ_EF_Simple.png", width = 2.4, height = 1.5)

data1 %>% 
  filter(LocalEfficiency_SC_RH.FP >= 0.001) %>% 
  ggplot(aes(AQ, LocalEfficiency_SC_RH.FP)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH FP") +
  xlab("AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_RH.FP_AQ.png", width = 2.4, height = 1.5)

data1 %>% 
  filter(LocalEfficiency_SC_RH.FP >= 0.01) %>% 
  ggplot(aes(Tower.of.London, LocalEfficiency_SC_RH.FP)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH FP") +
  xlab("Tower of London") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_RH.FP_ToL.png", width = 2.4, height = 1.5)

# Local Efficiency - RH aMTG 
ggplot(data1, aes(nback, LocalEfficiency_SC_RH.aMTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH aMTG") +
  xlab("Nback") +
  theme(axis.title = element_text(family = "Arial", size = 10)) 
ggsave("figure/LE_RH.aMTG_nback_EF.png", width = 2.4, height = 1.5)

summary(lm.LE.raMTG.EF.sw) # 1
interact_plot(lm.LE.raMTG.EF.sw, pred = "gonogo", modx = "AQ", interval = FALSE) +
  jtools::theme_apa() +
  ylab("RH aMTG") +
  xlab("Go/No-Go*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10)) + 
  theme(legend.position = "none")
ggsave("figure/LE_RH.aMTG_gonogo.AQ_EF.png", width = 2.4, height = 1.5)

ggplot(data1, aes(nback, LocalEfficiency_SC_RH.aMTG)) +
  geom_jitter() +
  geom_smooth(method = "lm", color = "black", se = F) +
  jtools::theme_apa() +
  ylab("RH aMTG") +
  xlab("Go/NoGo*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10)) 
ggsave("figure/LE_RH.aMTG_gonogo.AQ_EF_Simple.png", width = 2.4, height = 1.5)

#Local Efficiency LH aSTG
interact_plot(lm.LE.lpMTG.LN.sw, pred = "VF", modx = "AQ", interval = FALSE) +
  jtools::theme_apa() +
  ylab("Local Efficiency - LH pMTG") +
  xlab("Verbal Fluency*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_LH.pMTG_VerbalFluency.png", width = 5, height = 2.5)

# Local Efficiency - RH ' Frontal Pole
summary(lm.LE.rFP.EF.sw)
interact_plot(lm.LE.rFP.EF.sw, pred = "Tower.of.London", modx = "AQ", interval = FALSE) +
  jtools::theme_apa() +
  ylab("RH Frontal Pole") +
  xlab("Tower of London*AQ") +
  theme(axis.title = element_text(family = "Arial", size = 10))
ggsave("figure/LE_RH.FP_ToL_EF.png", width = 5, height = 2.5)

# Supplementary Information -----------------------------------------------
data1 %>% 
  gather(6:11, key = "AQ", value = "score") %>% 
  ggstatsplot::grouped_ggbetweenstats(Sex, score, 
                                      grouping.var = AQ)
ggsave("publication/Cognitive Neurodynamics/First Review/SI 2.png", height = 5, width = 7.5)

data1 %>% 
  gather(6:11, key = "AQ", value = "score") %>% 
  ggstatsplot::grouped_ggbetweenstats(Group, score, 
                                      grouping.var = AQ, 
                                      plotgrid.args =  list(ncol = 2, nrow = 3)) 
ggsave("publication/Cognitive Neurodynamics/First Review/SI 3.png", height = 7.5, width = 7.5)

data1 %>% 
  dplyr::select(2,5,3,6:11) %>% 
  ggstatsplot::ggcorrmat(type = "np") 
ggsave("publication/Cognitive Neurodynamics/First Review/SI 4.png", height = 5, width = 6)

f.ge <- data1 %>% gather(contains(c("PN_All","ToM_All","LN_All")), key = "measure", value = "result") %>% 
  separate(measure, c("measure", "network"), sep = "_") %>% 
  filter(measure == "GlobalEfficiency") %>% 
  ggstatsplot::ggbetweenstats(network, result,
                              title = "A. Global Efficiency",
                              type = "np", 
                              p.adjust.method = "fdr")

f.lg <- data1 %>% gather(contains(c("PN_All","ToM_All","LN_All")), key = "measure", value = "result") %>% 
  separate(measure, c("measure", "network"), sep = "_") %>% 
  filter(measure == "LocalEfficiency") %>% 
  ggstatsplot::ggbetweenstats(network, result,
                              title = "B. Local Efficiency",
                              type = "np", 
                              p.adjust.method = "fdr")

f.clustering <- data1 %>% gather(contains(c("PN_All","ToM_All","LN_All")), key = "measure", value = "result") %>% 
  separate(measure, c("measure", "network"), sep = "_") %>% 
  filter(measure == "ClusteringCoefficient") %>% 
  ggstatsplot::ggbetweenstats(network, result,
                              title = "C. Clustering Coefficient",
                              type = "np", 
                              bf.message = "FALSE", 
                              results.subtitle = FALSE,
                              p.adjust.method = "fdr") 


ggpubr::ggarrange(f.ge, f.lg, f.clustering, ncol = 1, nrow = 3)
ggsave("figure/SI_6.tif", width = 4.7, height =8)
