###############################################################################
#
# R code to reproduce analysis presented in the paper:
# An Adaptation for Spanish Speakers of the Ambivalent Classism Inventory (ACI)
# Written by: Roberto M. Lobato 
# e-mail: romulobato@gmail.com
# Please email us if you see any errors or have any questions
# last update: 29/03/2021
# 
###############################################################################

#### 0. Packages ####
install.packages("haven")
install.packages("nFactors")
install.packages("FactoMineR")
install.packages("relimp")
install.packages("paran")
install.packages("psych")
install.packages("lavaan")
install.packages("sjPlot")
install.packages("dplyr")

#### 1. Upload Database ####
library(haven)
ACI_database <- read_sav("processing/ECA Exploratory and Confirmatory Data.sav")

#### 2. Compute variables ####

# Exploratoty factor analysis sample
ACI_EFA <- subset(ACI_database, Samples == 1, select = c("CLA_1", "CLA_2", "CLA_3", "CLA_4", "CLA_5", "CLA_6", 
                                                       "CLA_7", "CLA_8", "CLA_9", "CLA_10", "CLA_11", "CLA_12", 
                                                       "CLA_13", "CLA_14", "CLA_15", "CLA_16", "CLA_17", 
                                                       "CLA_18", "CLA_19", "CLA_20"))

# Confirmatory factor analysis sample
ACI_CFA <- subset(ACI_database, Samples == 2, select = c("CLA_1", "CLA_2", "CLA_3", "CLA_4", "CLA_5", "CLA_6", 
                                                         "CLA_7", "CLA_8", "CLA_9", "CLA_10", "CLA_11", "CLA_12", 
                                                         "CLA_13", "CLA_14", "CLA_15", "CLA_16", "CLA_17", 
                                                         "CLA_18", "CLA_19", "CLA_20"))

# Correlations analysis sample
ACI_Cor <- subset(ACI_database, select = c("CLA_1", "CLA_2", "CLA_3", "CLA_4", "CLA_5", "CLA_6", "CLA_7", "CLA_8", 
                                           "CLA_9", "CLA_10", "CLA_11", "CLA_12", "CLA_13", "CLA_14", "CLA_15", 
                                           "CLA_16", "CLA_17", "CLA_18", "CLA_19", "CLA_20", "PO", "TOLE_1", "TOLE_2R", 
                                           "TOLE_3R", "TOLE_4", "TOLE_5R","SDO_1", "SDO_2", "SDO_3R", "SDO_4R", 
                                           "SDO_5", "SDO_6", "SDO_7R", "SDO_8R", "SJ_1", "SJ_2", "SJ_3", "SJ_4", 
                                           "SJ_5", "SJ_6", "SJ_7", "DH_poor", "DH_rich", "Income", "HM","SES")) 

#### 3. Exploratory factor analysis ####
library(nFactors)
library(FactoMineR)
library(relimp, pos = 4)
library(paran)
library(psych)

# Data fit
KMO(ACI_EFA)
bartlett.test(ACI_EFA)

# Paralell analysis
paran(ACI_EFA)

# Descriptive statistics
describe(ACI_EFA)

# Reliabitity

## Reliability total scale
alfa_ACI_EFA <- alpha(ACI_EFA)
alfa_ACI_EFA

## Reliability Protective Paternalism
ACI_PP_EFA <- subset(ACI_database, Samples == 1, select = c("CLA_13", "CLA_14", "CLA_15", "CLA_16"))
alfa_ACI_PP_EFA <- alpha(ACI_PP_EFA)
alfa_ACI_PP_EFA$total

## Reliability Complementary Class Differentiation
ACI_CCD_EFA <- subset(ACI_database, Samples == 1, select = c("CLA_17", "CLA_18", "CLA_19", "CLA_20"))
alfa_ACI_CCD_EFA <- alpha(ACI_CCD_EFA)
alfa_ACI_CCD_EFA$total

## Reliability Hostile Classism
ACI_HC_EFA <- subset(ACI_database, Samples == 1, select = c("CLA_1", "CLA_2", "CLA_3", "CLA_4", "CLA_5", "CLA_6", 
                                                        "CLA_7", "CLA_8", "CLA_9", "CLA_10", "CLA_11", "CLA_12"))
alfa_ACI_HC_EFA <- alpha(ACI_HC_EFA)
alfa_ACI_HC_EFA$total

# Exploratoty facor analysis
EFA_ACI_EFA <- fa(ACI_EFA, nfactors=3, rotate="Promax", missing=T, fm="ml")
EFA_ACI_EFA

#### 4. Confirmatory Factor Analysis  ####
library(lavaan)
library(psych)

# Assumption of multivariate normality
mardia(ACI_CFA, na.rm = TRUE, plot=TRUE)

# Models
## Unidimensional
Model1 <- 
  'ACI=~CLA_1+CLA_2+CLA_3+CLA_4+CLA_5+CLA_6+CLA_7+CLA_8+CLA_9+CLA_10+CLA_11+CLA_12+CLA_13+CLA_14+CLA_15+CLA_16+CLA_17+CLA_18+CLA_19+CLA_20'

## Two dimensions
Model2 <- 
  'ACI_BC=~CLA_13+CLA_14+CLA_15+CLA_16+CLA_17+CLA_18+CLA_19+CLA_20
  ACI_HC=~CLA_1+CLA_2+CLA_3+CLA_4+CLA_5+CLA_6+CLA_7+CLA_8+CLA_9+CLA_10+CLA_11+CLA_12'

## Three dimensions
Model3 <- 
  'ACI_PP=~CLA_13+CLA_14+CLA_15+CLA_16
  ACI_CCD=~CLA_17+CLA_18+CLA_19+CLA_20
  ACI_HC=~CLA_1+CLA_2+CLA_3+CLA_4+CLA_5+CLA_6+CLA_7+CLA_8+CLA_9+CLA_10+CLA_11+CLA_12'

# Models' fit
## Model 1
Fit1 <- cfa(Model1, data = ACI_CFA, estimator = 'MLR')
summary(Fit1, fit.measures=T, standardized=T)

## Model 2
Fit2 <- cfa(Model2, data = ACI_CFA, estimator = 'MLR')
summary(Fit2, fit.measures=T, standardized=T)

## Model 3
Fit3 <- cfa(Model3, data = ACI_CFA, estimator = 'MLR')
summary(Fit3, fit.measures=T, standardized=T)

# Model comparison
anova(Fit1, Fit2)
anova(Fit2, Fit3)

# Reliability
## Reliability Ambivalent Classism Inventary
alfa_ACI_CFA <- alpha(ACI_CFA)
alfa_ACI_CFA$total

## Reliability Protective Paternalism
ACI_PP <- subset(ACI_database, Samples == 2, select = c("CLA_13", "CLA_14", "CLA_15", "CLA_16"))
alfa_ACI_PP <- alpha(ACI_PP)
alfa_ACI_PP$total

## Reliability Complementary Class Differentiation
ACI_CCD <- subset(ACI_database, Samples == 2, select = c("CLA_17", "CLA_18", "CLA_19", "CLA_20"))
alfa_ACI_CCD <- alpha(ACI_CCD)
alfa_ACI_CCD$total

## Reliability Hostile Classism
ACI_HC <- subset(ACI_database, Samples == 2, select = c("CLA_1", "CLA_2", "CLA_3", "CLA_4", "CLA_5", "CLA_6", 
                                                        "CLA_7", "CLA_8", "CLA_9", "CLA_10", "CLA_11", "CLA_12"))
alfa_ACI_HC <- alpha(ACI_HC)
alfa_ACI_HC$total

#### 5. Correlates  ####
library(sjPlot)
library(dplyr)

# Compute variables
## Ambivalent Classism Inventary
ACI_Cor <- ACI_Cor %>%
  mutate(ACI = (CLA_1+CLA_2+CLA_3+CLA_4+CLA_5+CLA_6+CLA_7+CLA_8+CLA_9+CLA_10+CLA_11+CLA_12+CLA_13+CLA_14+CLA_15+CLA_16+CLA_17+CLA_18+CLA_19+CLA_20)/20) 

## Protective Paternalism
ACI_Cor <- ACI_Cor %>%
  mutate(ACI_PP = (CLA_13+CLA_14+CLA_15+CLA_16)/4) 

## Complementary Class Differentiation
ACI_Cor <- ACI_Cor %>%
  mutate(ACI_CCD = (CLA_17+CLA_18+CLA_19+CLA_20)/4) 

## Hostile Classism
ACI_Cor <- ACI_Cor %>%
  mutate(ACI_HC = (CLA_1+CLA_2+CLA_3+CLA_4+CLA_5+CLA_6+CLA_7+CLA_8+CLA_9+CLA_10+CLA_11+CLA_12)/12) 

## Tolerance to Inequality
ACI_Cor <- ACI_Cor %>%
  mutate(TOLE = (TOLE_1+TOLE_2R+TOLE_3R+TOLE_4+TOLE_5R)/5) 

## Social Dominance 
ACI_Cor <- ACI_Cor %>%
  mutate(SD = (SDO_1+SDO_2+SDO_3R+SDO_4R)/4)

## Anti-egalitarianism
ACI_Cor <- ACI_Cor %>%
mutate(AE = (SDO_5+SDO_6+SDO_7R+SDO_8R)/4)      

## Sistem Justification
ACI_Cor <- ACI_Cor %>%
  mutate(SJ = (SJ_1+SJ_2+SJ_3+SJ_4+SJ_5+SJ_6+SJ_7)/7)

## Objetive Socio-Economic Status
ACI_Cor <- ACI_Cor %>%
  mutate(OSES = (Income/HM))

# Subset variables
ACI_Cor_var <- subset(ACI_Cor, select = c("ACI", "ACI_HC", "ACI_PP", "ACI_CCD", "TOLE", "SD", "AE", 
                                          "SJ", "DH_poor", "SES", "OSES", "PO"))

# Descriptive statistics
describe(ACI_Cor_var)

# Reliability
## Reliability Ambivalent Classism Inventary 
ACI_cor <- subset(ACI_Cor, select = c("CLA_1", "CLA_2", "CLA_3", "CLA_4", "CLA_5", 
                                         "CLA_6", "CLA_7", "CLA_8", "CLA_9", "CLA_10", 
                                         "CLA_11", "CLA_12", "CLA_13", "CLA_14", "CLA_15", 
                                         "CLA_16", "CLA_17", "CLA_18", "CLA_19", "CLA_20"))
alfa_ACI_cor <- alpha(ACI_cor)
alfa_ACI_cor$total

## Reliability Protective Paternalism
ACI_PP_cor <- subset(ACI_Cor, select = c("CLA_13", "CLA_14", "CLA_15", "CLA_16"))
alfa_ACI_PP_cor <- alpha(ACI_PP_cor)
alfa_ACI_PP_cor$total

## Reliability Complementary Class Differentiation
ACI_CCD_cor <- subset(ACI_Cor, select = c("CLA_17", "CLA_18", "CLA_19", "CLA_20"))
alfa_ACI_CCD_cor <- alpha(ACI_CCD_cor)
alfa_ACI_CCD_cor$total

## Reliability Hostile Classism
ACI_HC_cor <- subset(ACI_Cor, select = c("CLA_1", "CLA_2", "CLA_3", "CLA_4", "CLA_5", 
                                         "CLA_6", "CLA_7", "CLA_8", "CLA_9", "CLA_10", 
                                         "CLA_11", "CLA_12"))
alfa_ACI_HC_cor <- alpha(ACI_HC_cor)
alfa_ACI_HC_cor$total

## Reliability Tolerance
TOLE_cor <- subset(ACI_Cor, select = c("TOLE_1", "TOLE_2R", "TOLE_3R", "TOLE_4", "TOLE_5R"))
alfa_TOLE_cor <- alpha(TOLE_cor)
alfa_TOLE_cor$total

## Reliability Social Dominance  
SD_cor <- subset(ACI_Cor, select = c("SDO_1", "SDO_2", "SDO_3R", "SDO_4R"))
alfa_SD_cor <- alpha(SD_cor)
alfa_SD_cor$total

## Reliability Anti-Egalitarianism 
AE_cor <- subset(ACI_Cor, select = c("SDO_1", "SDO_2", "SDO_3R", "SDO_4R", "SDO_5", "SDO_6", "SDO_7R", "SDO_8R"))
alfa_AE_cor <- alpha(AE_cor)
alfa_AE_cor$total

## Reliabiltiy System Justification
SJ_cor <- subset(ACI_Cor, select = c("SJ_1", "SJ_2", "SJ_3", "SJ_4", "SJ_5", "SJ_6", "SJ_7"))
alfa_SJ_cor <- alpha(SJ_cor)
alfa_SJ_cor$total

# Correlations
sjt.corr(ACI_Cor_var, var.labels = c("Ambivalent Classism Inventary", "Hostile Classism", 
                                    "Protective Paternalism",  "Complementary Class Differentiation", 
                                    "Tolerance", "Social Dominance", "Anti-egalitarianism",
                                    "System Justification", "DH_poor",
                                    "Subjective socio-economic status", "Objective socio-economic status", "Political Orientation"),
         na.deletion = c("pairwise"),
         corr.method = c("pearson"), 
         fade.ns = TRUE, digits = 3,triangle = "lower")

