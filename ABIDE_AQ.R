library(tidyverse)
library(lme4)
library(lmerTest)
library(parameters)
library(rstatix)
setwd("C:/Users/eliza/Box/Pragmatics/Pragmatics_GT/")

data_ABIDE_AQ_Full <- read_csv("generals/ABIDE_DataWith_AQ.csv") %>% 
  drop_na(AQ) %>% 
  mutate_at(c(1:3,5), as.factor) %>% 
  filter(fmriprep == "1") 

outliers.data <-data_ABIDE_AQ_Full %>%
  mutate_at(c(1:3,5), as.factor) %>% 
  gather(c(11,15:137), key = "task", value = "result") %>% 
  group_by(task) %>%
  identify_outliers(result) %>% 
  filter(is.extreme == TRUE) %>% 
  dplyr::select(1:6,15,17)

data_ABIDE_AQ <- data_ABIDE_AQ_Full %>% 
  gather(c(11,15:137), key = "task", value = "result2") %>% 
  full_join(outliers.data) %>% 
  mutate(is.extreme = replace_na(is.extreme,FALSE)) %>% 
  filter(is.extreme == FALSE) %>% 
  dplyr::select(-10:-12,-6,-16,-17) %>% 
  spread(task, result2) 
  
descriptives <- data_ABIDE_AQ %>% 
  dplyr::select(4,6:10) %>% 
  get_summary_stats(type = "common") %>% 
  mutate_if(is.numeric, round,2) 

comparision.groups <- data_ABIDE_AQ %>% 
  gather(c(4,6:10), key = "test", value = "score") %>% 
  dplyr::group_by(test) %>% 
  t_test(score ~ dx_group) %>% 
  adjust_pvalue(method="fdr") %>%
  mutate_if(is.numeric, round,2)

# All ---------------------------------------------------------------------
lmer.GE.All.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_All` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.All.SC.sw.ABIDE) # not sig

lmer.LE.All.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_All` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.All.SC.sw.ABIDE)

lmer.clcO.All.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_All` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.All.SC.sw.ABIDE)

# ROI by ROI --------------------------------------------------------------
# AC ----------------------------------------------------------------------
lmer.GE.AC.SC.sw.ABIDE <- lmer(GlobalEfficiency_SC_AC~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.AC.SC.sw.ABIDE)

lmer.LE.AC.SC.sw.ABIDE <- lmer(LocalEfficiency_SC_AC ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.AC.SC.sw.ABIDE)

lmer.clcO.AC.SC.sw.ABIDE <- lmer(ClusteringCoefficient_SC_AC ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.AC.SC.sw.ABIDE)

# LH.AG -------------------------------------------------------------------
lmer.GE.lAG.SC.sw.ABIDE <- lmer(GlobalEfficiency_SC_LH.AG~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lAG.SC.sw.ABIDE) #not sig

lmer.LE.lAG.SC.sw.ABIDE <- lmer(LocalEfficiency_SC_LH.AG ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lAG.SC.sw.ABIDE) #not sig

lmer.clcO.lAG.SC.sw.ABIDE <- lmer(ClusteringCoefficient_SC_LH.AG ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lAG.SC.sw.ABIDE) #not sig

# LH.aMTG -------------------------------------------------------------------
lmer.GE.laMTG.SC.sw.ABIDE <- lmer(GlobalEfficiency_SC_LH.aMTG~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.laMTG.SC.sw.ABIDE)

lmer.LE.laMTG.SC.sw.ABIDE <- lmer(LocalEfficiency_SC_LH.aMTG ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.laMTG.SC.sw.ABIDE) #not sig

lmer.clcO.laMTG.SC.sw.ABIDE <- lmer(ClusteringCoefficient_SC_LH.aMTG ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.laMTG.SC.sw.ABIDE) #not sig

# LH.aMTG -------------------------------------------------------------------
lmer.GE.laSTG.SC.sw.ABIDE <- lmer(GlobalEfficiency_SC_LH.aSTG ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.laSTG.SC.sw.ABIDE) #sig, does not survive

lmer.LE.laSTG.SC.sw.ABIDE <- lmer(LocalEfficiency_SC_LH.aSTG ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.laSTG.SC.sw.ABIDE) #sig

lmer.clcO.laSTG.SC.sw.ABIDE <- lmer(ClusteringCoefficient_SC_LH.aSTG ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.laSTG.SC.sw.ABIDE) # sig AQ 0.04

# LH.FOrb -------------------------------------------------------------------
lmer.GE.lForb.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.FOrb` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lForb.SC.sw.ABIDE) #not sig

lmer.LE.lForb.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.FOrb` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lForb.SC.sw.ABIDE) # not sig

lmer.clcO.lForb.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.FOrb` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lForb.SC.sw.ABIDE) # sig, does not survive

# LLH.FP -------------------------------------------------------------------
lmer.GE.lFP.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.FP`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lFP.SC.sw.ABIDE) #not sig

lmer.LE.lFP.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.FP` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lFP.SC.sw.ABIDE) #not sig

lmer.clcO.lFP.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.FP` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lFP.SC.sw.ABIDE) # not sig

# LH HG -------------------------------------------------------------------
lmer.GE.lHG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.HG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lHG.SC.sw.ABIDE) #sig, does not survive

lmer.LE.lHG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.HG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lHG.SC.sw.ABIDE) # sig AQ 0.01

lmer.clcO.lHG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.HG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lHG.SC.sw.ABIDE) # not sig

# LH.IC -------------------------------------------------------------------
lmer.GE.lIC.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.IC`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lIC.SC.sw.ABIDE) #sig, does not survive

lmer.LE.lIC.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.IC` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lIC.SC.sw.ABIDE) #not sig

lmer.clcO.lIC.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.IC` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lIC.SC.sw.ABIDE) # not sig

# LH.IFGop -------------------------------------------------------------------
lmer.GE.lIFGop.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.IFGop`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lIFGop.SC.sw.ABIDE) #not sig

lmer.LE.lIFGop.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.IFGop` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lIFGop.SC.sw.ABIDE) #not sig

lmer.clcO.lIFGop.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.IFGop` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lIFGop.SC.sw.ABIDE) # not sig

# LH.IFGtr -------------------------------------------------------------------
lmer.GE.lIFGtr.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.IFGtr`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lIFGtr.SC.sw.ABIDE) #sig

lmer.LE.lIFGtr.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.IFGtr` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lIFGtr.SC.sw.ABIDE) #not sig

lmer.clcO.lIFGtr.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.IFGtr` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lIFGtr.SC.sw.ABIDE) #not sig

# LH.IMFG -------------------------------------------------------------------
lmer.GE.lmerFG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.MFG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lmerFG.SC.sw.ABIDE) #not sig

lmer.LE.lmerFG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.MFG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lmerFG.SC.sw.ABIDE) #sig, does not survive

lmer.clcO.lmerFG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.MFG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lmerFG.SC.sw.ABIDE) # sig AQ 0.04

# LH.PaCiG -------------------------------------------------------------------
lmer.GE.lPaCiG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.PaCiG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lPaCiG.SC.sw.ABIDE) #not sig

lmer.LE.lPaCiG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.PaCiG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lPaCiG.SC.sw.ABIDE) #sig, does not survive

lmer.clcO.lPaCiG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.PaCiG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lPaCiG.SC.sw.ABIDE) #not sig

# LH.PCG -------------------------------------------------------------------
lmer.GE.lPCG.SC.sw.ABIDE <- lmer(GlobalEfficiency_SC_LH.PreCG ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lPCG.SC.sw.ABIDE)

lmer.LE.lPCG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.PreCG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lPCG.SC.sw.ABIDE)

lmer.clcO.lPCG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.PreCG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lPCG.SC.sw.ABIDE)

# LH.pMTG -------------------------------------------------------------------
lmer.GE.lpMTG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.pMTG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lpMTG.SC.sw.ABIDE) #sig, does not survive

lmer.LE.lpMTG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.pMTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lpMTG.SC.sw.ABIDE) #sig, does not survive

lmer.clcO.lpMTG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.pMTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lpMTG.SC.sw.ABIDE) # not sig

# LH.pSMG -------------------------------------------------------------------
lmer.GE.lpSMG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.pSMG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lpSMG.SC.sw.ABIDE) # sig AQ 0.01

lmer.LE.lpSMG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.pSMG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lpSMG.SC.sw.ABIDE) #not sig

lmer.clcO.lpSMG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.pSMG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lpSMG.SC.sw.ABIDE) # not sig

# LH.pSTG -------------------------------------------------------------------
lmer.GE.lpSTG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.pSTG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lpSTG.SC.sw.ABIDE) #not sig

lmer.LE.lpSTG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.pSTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lpSTG.SC.sw.ABIDE) #not sig

lmer.clcO.lpSTG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.pSTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lpSTG.SC.sw.ABIDE) # not sig


# LH.PT -------------------------------------------------------------------
lmer.GE.lPT.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.PT`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lPT.SC.sw.ABIDE) #not sig

lmer.LE.lPT.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.PT` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lPT.SC.sw.ABIDE) #sig AQ 0.02

lmer.clcO.lPT.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.PT` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lPT.SC.sw.ABIDE) #not sig

# LH.SFG -------------------------------------------------------------------
lmer.GE.lSFG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.SFG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lSFG.SC.sw.ABIDE) #not sig

lmer.LE.lSFG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.SFG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lSFG.SC.sw.ABIDE) #not sig

lmer.clcO.lSFG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.SFG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lSFG.SC.sw.ABIDE) #not sig

# LH.SMA -------------------------------------------------------------------
lmer.GE.lSMA.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.SMA`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lSMA.SC.sw.ABIDE)

lmer.LE.lSMA.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.SMA` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lSMA.SC.sw.ABIDE)

lmer.clcO.lSMA.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.SMA` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lSMA.SC.sw.ABIDE)

# LH.TP -------------------------------------------------------------------
lmer.GE.lTP.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_LH.TP`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.lTP.SC.sw.ABIDE) #not sig

lmer.LE.lTP.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_LH.TP` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.lTP.SC.sw.ABIDE) #not sig

lmer.clcO.lTP.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_LH.TP` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.lTP.SC.sw.ABIDE) # not sig

# MedFC -------------------------------------------------------------------
lmer.GE.MedFC.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_MedFC`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.MedFC.SC.sw.ABIDE)

lmer.LE.MedFC.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_MedFC` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.MedFC.SC.sw.ABIDE)

lmer.clcO.MedFC.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_MedFC` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.MedFC.SC.sw.ABIDE)

# Precuneus -------------------------------------------------------------------
lmer.GE.Precuneus.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_Precuneus`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.Precuneus.SC.sw.ABIDE) #sig, does not survive

lmer.LE.Precuneus.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_Precuneus` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.Precuneus.SC.sw.ABIDE) #not sig

lmer.clcO.Precuneus.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_Precuneus` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.Precuneus.SC.sw.ABIDE) # not sig

# RH.AG -------------------------------------------------------------------
lmer.GE.rAG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.AG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rAG.SC.sw.ABIDE)

lmer.LE.rAG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.AG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rAG.SC.sw.ABIDE)

lmer.clcO.rAG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.AG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rAG.SC.sw.ABIDE)

# RH.aMTG -------------------------------------------------------------------
lmer.GE.raMTG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.aMTG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.raMTG.SC.sw.ABIDE) #not sig

lmer.LE.raMTG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.aMTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.raMTG.SC.sw.ABIDE) #not sig

lmer.clcO.raMTG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.aMTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.raMTG.SC.sw.ABIDE) # sig, does not survive

# RH.aSTG -------------------------------------------------------------------
lmer.GE.raSTG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.aSTG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.raSTG.SC.sw.ABIDE)

lmer.LE.raSTG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.aSTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.raSTG.SC.sw.ABIDE) #not sig

lmer.clcO.raSTG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.aSTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.raSTG.SC.sw.ABIDE) #not sig

# RH.FOrb -------------------------------------------------------------------
#lmer.GE.rFOrb.SC.sw.ABIDE <- lmer(GlobalEfficiency_SC_RH.FOrb~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
#summary(lmer.GE.rFOrb.SC.sw.ABIDE) #not sig

#lmer.LE.rFOrb.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.FOrb` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
#summary(lmer.LE.rFOrb.SC.sw.ABIDE) #not sig

#lmer.clcO.rFOrb.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.FOrb` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
#summary(lmer.clcO.rFOrb.SC.sw.ABIDE) #not sig

# RH.FP -------------------------------------------------------------------
lmer.GE.rFP.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.FP`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rFP.SC.sw.ABIDE) #sig

lmer.LE.rFP.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.FP` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rFP.SC.sw.ABIDE) #sig

lmer.clcO.rFP.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.FP` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rFP.SC.sw.ABIDE) #sig, does not survive

# RH.HG -------------------------------------------------------------------
lmer.GE.rHG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.HG`~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rHG.SC.sw.ABIDE) #sig, does not survive

lmer.LE.rHG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.HG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rHG.SC.sw.ABIDE) #not sig

lmer.clcO.rHG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.HG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rHG.SC.sw.ABIDE) # not sig

# RH.IC -------------------------------------------------------------------
lmer.GE.rIC.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.IC` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rIC.SC.sw.ABIDE)

lmer.LE.rIC.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.IC` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rIC.SC.sw.ABIDE) # sig AQ 0.01

lmer.clcO.rIC.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.IC` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rIC.SC.sw.ABIDE) # sig AQ 0.003

# RH.IFGop -------------------------------------------------------------------
lmer.GE.rIFGop.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.IFGop` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rIFGop.SC.sw.ABIDE) #sig AQ 0.03

lmer.LE.rIFGop.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.IFGop` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rIFGop.SC.sw.ABIDE) #sig, does not survive

lmer.clcO.rIFGop.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.IFGop` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rIFGop.SC.sw.ABIDE) # not sig

# RH.IFGtr -------------------------------------------------------------------
lmer.GE.rIFGtr.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.IFGtr` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rIFGtr.SC.sw.ABIDE) #sig, does not survive

lmer.LE.rIFGtr.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.IFGtr` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rIFGtr.SC.sw.ABIDE)

lmer.clcO.rIFGtr.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.IFGtr` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rIFGtr.SC.sw.ABIDE) #not sig

# RH.MFG -------------------------------------------------------------------
lmer.GE.rMFG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.MFG` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rMFG.SC.sw.ABIDE)

lmer.LE.rMFG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.MFG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rMFG.SC.sw.ABIDE)

lmer.clcO.rMFG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.MFG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rMFG.SC.sw.ABIDE) #AQ

# RH.PaCiG -------------------------------------------------------------------
lmer.GE.rPaCiG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.PaCiG` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rPaCiG.SC.sw.ABIDE)

lmer.LE.rPaCiG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.PaCiG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rPaCiG.SC.sw.ABIDE)

lmer.clcO.rPaCiG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.PaCiG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rPaCiG.SC.sw.ABIDE)

# RH.PCG -------------------------------------------------------------------
lmer.GE.rPCG.SC.sw.ABIDE <- lmer(GlobalEfficiency_SC_RH.PreCG ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rPCG.SC.sw.ABIDE)

lmer.LE.rPCG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.PreCG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rPCG.SC.sw.ABIDE)

lmer.clcO.rPCG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.PreCG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rPCG.SC.sw.ABIDE)

# RH.pMTG -------------------------------------------------------------------
lmer.GE.rpMTG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.pMTG` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rpMTG.SC.sw.ABIDE) #not sig

lmer.LE.rpMTG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.pMTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rpMTG.SC.sw.ABIDE) #not sig

lmer.clcO.rpMTG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.pMTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rpMTG.SC.sw.ABIDE) # not sig

# RH.pSMG -------------------------------------------------------------------
lmer.GE.rpSMG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.pSMG` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rpSMG.SC.sw.ABIDE)

lmer.LE.rpSMG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.pSMG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rpSMG.SC.sw.ABIDE)

lmer.clcO.rpSMG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.pSMG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rpSMG.SC.sw.ABIDE)

# RH.pSTG -------------------------------------------------------------------
lmer.GE.rpSTG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.pSTG` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rpSTG.SC.sw.ABIDE) #not sig

lmer.LE.rpSTG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.pSTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rpSTG.SC.sw.ABIDE) #not sig

lmer.clcO.rpSTG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.pSTG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rpSTG.SC.sw.ABIDE) #sig, does not survive

# RH.PT -------------------------------------------------------------------
lmer.GE.rPT.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.PT` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rPT.SC.sw.ABIDE) #not sig

lmer.LE.rPT.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.PT` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rPT.SC.sw.ABIDE) #not sig

lmer.clcO.rPT.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.PT` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rPT.SC.sw.ABIDE) #not sig

# RH.SFG -------------------------------------------------------------------
lmer.GE.rSFG.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.SFG` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rSFG.SC.sw.ABIDE) #not sig

lmer.LE.rSFG.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.SFG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rSFG.SC.sw.ABIDE) #not sig

lmer.clcO.rSFG.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.SFG` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rSFG.SC.sw.ABIDE) #not sig

# RH.SMA -------------------------------------------------------------------
lmer.GE.rSMA.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.SMA` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rSMA.SC.sw.ABIDE)

lmer.LE.rSMA.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.SMA` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rSMA.SC.sw.ABIDE)

lmer.clcO.rSMA.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.SMA` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rSMA.SC.sw.ABIDE)

# RH.TP -------------------------------------------------------------------
lmer.GE.rTP.SC.sw.ABIDE <- lmer(`GlobalEfficiency_SC_RH.TP` ~ AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.GE.rTP.SC.sw.ABIDE) #sig

lmer.LE.rTP.SC.sw.ABIDE <- lmer(`LocalEfficiency_SC_RH.TP` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.LE.rTP.SC.sw.ABIDE) #not sig

lmer.clcO.rTP.SC.sw.ABIDE <- lmer(`ClusteringCoefficient_SC_RH.TP` ~  AQ+ sex + ( 1 | site_id) + ( 1 | dx_group), data= data_ABIDE_AQ)
summary(lmer.clcO.rTP.SC.sw.ABIDE) # not sig

# Mixed model - AQ controlled by sex and site
summary(lmer.GE.lpSMG.SC.sw.ABIDE) # sig AQ 0.01 
summary(lmer.GE.rIFGop.SC.sw.ABIDE) #sig AQ 0.03

summary(lmer.LE.lHG.SC.sw.ABIDE) # sig AQ 0.01
summary(lmer.LE.lPT.SC.sw.ABIDE) #sig AQ 0.02
summary(lmer.LE.rAG.SC.sw.ABIDE) # sig after removing outliers
summary(lmer.LE.rMFG.SC.sw.ABIDE) # sig after removing outliers

summary(lmer.clcO.laSTG.SC.sw.ABIDE) # sig AQ 0.04
summary(lmer.clcO.lHG.SC.sw.ABIDE) # sig AQ 0.01
summary(lmer.clcO.lmerFG.SC.sw.ABIDE) # sig AQ 0.04
summary(lmer.clcO.rAG.SC.sw.ABIDE) # sig after removing outliers
summary(lmer.clcO.rMFG.SC.sw.ABIDE) # sig after removing outliers

# Bootstrap ---------------------------------------------------------------
set.seed(123)
lmer.GE.lpSMG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.GE.lpSMG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 
lmer.GE.rIFGop.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.GE.rIFGop.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 

lmer.LE.lHG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.LE.lHG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 
lmer.LE.lPT.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.LE.lPT.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 
lmer.LE.rAG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.LE.rAG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 
lmer.LE.rMFG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.LE.rMFG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 

lmer.clcO.laSTG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.clcO.laSTG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 
lmer.clcO.lHG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.clcO.lHG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 
lmer.clcO.lmerFG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.clcO.lmerFG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 
lmer.clcO.lmerAG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.clcO.rAG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 
lmer.clcO.lmerMFG.SC.sw.ABIDE.bs <- bootstrap_parameters(lmer.clcO.rMFG.SC.sw.ABIDE, iterations = 10000, ci = 0.95) 

summary(lmer.GE.lpSMG.SC.sw.ABIDE.bs) # sig AQ 0.01 - without changes
summary(lmer.GE.rIFGop.SC.sw.ABIDE.bs) # sig AQ 0.03 - without changes

summary(lmer.LE.lHG.SC.sw.ABIDE.bs) # sig AQ 0.01 - change to 0.009
summary(lmer.LE.lPT.SC.sw.ABIDE.bs) # sig AQ 0.02 - without changes 
summary(lmer.LE.rAG.SC.sw.ABIDE.bs) # sig AQ 0.02 - without changes 
summary(lmer.LE.rMFG.SC.sw.ABIDE.bs) # sig AQ 0.02 - without changes 

summary(lmer.clcO.laSTG.SC.sw.ABIDE.bs) # sig AQ 0.04 - without changes
summary(lmer.clcO.lHG.SC.sw.ABIDE.bs) # sig AQ 0.01- change to 0.009
summary(lmer.clcO.lmerFG.SC.sw.ABIDE.bs) # sig AQ 0.04 - without changes
summary(lmer.clcO.lmerAG.SC.sw.ABIDE.bs) # sig AQ 0.04 - without changes
summary(lmer.clcO.lmerMFG.SC.sw.ABIDE.bs) # sig AQ 0.04 - without changes

# Test Assumptions --------------------------------------------------------
#summary(lmer.GE.lpSMG.SC.sw.ABIDE) # sig AQ 0.01
performance::check_normality(lmer.GE.lpSMG.SC.sw.ABIDE) #normal
performance::check_outliers(lmer.GE.lpSMG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.GE.lpSMG.SC.sw.ABIDE) # not autocorrelated

#summary(lmer.GE.rIFGop.SC.sw.ABIDE) #sig AQ 0.03
performance::check_normality(lmer.GE.rIFGop.SC.sw.ABIDE) #normal
performance::check_outliers(lmer.GE.rIFGop.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.GE.rIFGop.SC.sw.ABIDE) # not autocorrelated

#summary(lmer.LE.lHG.SC.sw.ABIDE) # sig AQ 0.01
performance::check_normality(lmer.LE.lHG.SC.sw.ABIDE) # not normal
performance::check_outliers(lmer.LE.lHG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.LE.lHG.SC.sw.ABIDE) # not autocorrelated

#summary(lmer.LE.lPT.SC.sw.ABIDE) #sig AQ 0.02
performance::check_normality(lmer.LE.lPT.SC.sw.ABIDE) # not normal
performance::check_outliers(lmer.LE.lPT.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.LE.lPT.SC.sw.ABIDE) # not autocorrelated

#summary(lmer.LE.rAG.SC.sw.ABIDE) # sig AQ 0.02 - without changes 
performance::check_normality(lmer.LE.rAG.SC.sw.ABIDE) #not normal
performance::check_outliers(lmer.LE.rAG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.LE.rAG.SC.sw.ABIDE) # autocorrelated

#summary(lmer.LE.rMFG.SC.sw.ABIDE) # sig AQ 0.02 - without changes 
performance::check_normality(lmer.LE.rMFG.SC.sw.ABIDE) # not normal
performance::check_outliers(lmer.LE.rMFG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.LE.rMFG.SC.sw.ABIDE) # not autocorrelated

#summary(lmer.clcO.laSTG.SC.sw.ABIDE) # sig AQ 0.04
performance::check_normality(lmer.clcO.laSTG.SC.sw.ABIDE) #normal
performance::check_outliers(lmer.clcO.laSTG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.clcO.laSTG.SC.sw.ABIDE) # not autocorrelated

#summary(lmer.clcO.lHG.SC.sw.ABIDE) # sig AQ 0.01
performance::check_normality(lmer.clcO.lHG.SC.sw.ABIDE) # not normal
performance::check_outliers(lmer.clcO.lHG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.clcO.lHG.SC.sw.ABIDE) # not autocorrelated

#summary(lmer.clcO.lmerFG.SC.sw.ABIDE) # sig AQ 0.04
performance::check_normality(lmer.clcO.lmerFG.SC.sw.ABIDE) #normal
performance::check_outliers(lmer.clcO.lmerFG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.clcO.lmerFG.SC.sw.ABIDE) # not autocorrelated

#summary(lmer.clcO.lmerAG.SC.sw.ABIDE) # sig AQ 0.04 - without changes
performance::check_normality(lmer.clcO.rAG.SC.sw.ABIDE) #normal
performance::check_outliers(lmer.clcO.rAG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.clcO.rAG.SC.sw.ABIDE) # autocorrelated

#summary(lmer.clcO.lmerMFG.SC.sw.ABIDE) # sig AQ 0.04 - without changes
performance::check_normality(lmer.clcO.rMFG.SC.sw.ABIDE) #normal
performance::check_outliers(lmer.clcO.rMFG.SC.sw.ABIDE) # not ourliers
performance::check_autocorrelation(lmer.clcO.rMFG.SC.sw.ABIDE) # not autocorrelated

# Figures  ----------------------------------------------------------------
f4a <- ggplot(data_ABIDE_AQ, aes(AQ,GlobalEfficiency_SC_LH.pSMG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  ggtitle("a") +
  jtools::theme_apa() +
  guides(color = FALSE) +
  ylab("GE - lpSMG")

f4b <- ggplot(data_ABIDE_AQ, aes(AQ,GlobalEfficiency_SC_RH.IFGop)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  jtools::theme_apa() +
  ggtitle("b") +
  guides(color = FALSE) +
  ylab("GE - rIFGop")

f4c <- ggplot(data_ABIDE_AQ, aes(AQ,LocalEfficiency_SC_LH.HG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  jtools::theme_apa() +
  ggtitle("c") +
  guides(color = FALSE) +
  ylab("LE - lHG")

f4d <- ggplot(data_ABIDE_AQ, aes(AQ,LocalEfficiency_SC_LH.PT)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  ggtitle("d") +
  jtools::theme_apa() +
  guides(color = FALSE) +
  ylab("LE - lPT")

f4e <- ggplot(data_ABIDE_AQ, aes(AQ,LocalEfficiency_SC_RH.AG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  ggtitle("e") +
  jtools::theme_apa() +
  guides(color = FALSE) +
  ylab("LE - rAG")

f4f <- ggplot(data_ABIDE_AQ, aes(AQ,LocalEfficiency_SC_RH.MFG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  ggtitle("f") +
  jtools::theme_apa() +
  guides(color = FALSE) +
  ylab("LE - rMFG")

f4g <- ggplot(data_ABIDE_AQ, aes(AQ,ClusteringCoefficient_SC_LH.aSTG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  jtools::theme_apa() +
  ggtitle("g") +
  guides(color = FALSE) +
  ylab("CC - laSTG")

f4h <- ggplot(data_ABIDE_AQ, aes(AQ,ClusteringCoefficient_SC_LH.HG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  jtools::theme_apa() +
  ggtitle("h") +
  guides(color = FALSE) +
  ylab("CC - lHG")

f4i <- ggplot(data_ABIDE_AQ, aes(AQ,ClusteringCoefficient_SC_LH.MFG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  jtools::theme_apa() +
  ggtitle("i") +
  guides(color = FALSE) +
  ylab("CC - lMFG")

f4j <- ggplot(data_ABIDE_AQ, aes(AQ,ClusteringCoefficient_SC_RH.AG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  jtools::theme_apa() +
  ggtitle("j") +
  guides(color = FALSE) +
  ylab("CC - rAG")

f4k <- ggplot(data_ABIDE_AQ, aes(AQ,ClusteringCoefficient_SC_RH.MFG)) +
  geom_jitter(aes(color = dx_group)) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  jtools::theme_apa() +
  ggtitle("k") +
  guides(color = FALSE) +
  ylab("CC - rMFG")

ggpubr::ggarrange(f4a,f4b,f4c,f4d, f4e,f4f,f4g,f4h, f4i,f4j,f4k, ncol = 4, nrow = 3)
ggsave("figure/Figure 4.tiff", height = 5.25, width = 7.5)
