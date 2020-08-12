# Exploratory Data Analysis; Chronic Conditions Public Use File (PUF) - 2010
# Kyle P Rasku RN BSN  ~  8/10/2020
# 
# ssa.gov - 47 million enrollees with Part A, 44 million with Part B (2010)

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)

# CHANGE ALL THE COLUMN NAMES 
##############################

library(janitor)

cc <- Chronic.Conditions.2
cc <- cc %>% row_to_names(row_number = 1)


# GATHER AND RECODE #
#####################

# Gather data on existing conditions
cc <- cc %>% gather(CONDITION, HAS_CONDITION, c(CC_ALZHDMTA, CC_CANCER, CC_CHF, CC_CHRNKIDN, CC_COPD, CC_DEPRESSN, CC_DIABETES, CC_ISCHMCHT, CC_OSTEOPRS, CC_RA_OA, CC_STRKETIA))

# Recode to readable
cc <- cc %>% mutate(CONDITION = recode(CONDITION,
                                           CC_ALZHDMTA = "ALZHEIMERS",
                                           CC_CANCER = "CANCER (4 TYPES)",
                                           CC_CHF = "CHF",
                                           CC_CHRNKIDN = "CKD",
                                           CC_COPD = "COPD",
                                           CC_DEPRESSN = "DEPRESSION",
                                           CC_DIABETES = "DIABETES",
                                           CC_ISCHMCHT = "ISCHEMIC_HD",
                                           CC_OSTEOPRS = "OSTEOPOROSIS",
                                           CC_RA_OA = "RA_OA",
                                           CC_STRKETIA = "STROKE_TIA"))

cc <- cc %>% mutate(SEX_CD = recode(BENE_SEX_IDENT_CD, 
                                    "1" = "MALE",
                                    "2" = "FEMALE", 
                                    "3" = "OTHER"))

cc <- cc %>% mutate(AGE_CATEGORY = recode(BENE_AGE_CAT_CD, 
                                    "1" = "<65",
                                    "2" = "65-69", 
                                    "3" = "70-74",
                                    "4" = "75-79",
                                    "5" = "80-84",
                                    "6" = "85+"))

cc <- cc %>% mutate(NUM_CCS = recode(CC_2_OR_MORE,
                                     "0" = "<2",
                                     "1" = "2+ (LISTED)"))

# ADD COUNT SUMMARIES 
#########################
# Note: Syntax for Conversion of FACTOR to NUMERIC (must pass through CHARACTER)
# x_num <- as.numeric(as.character(x))  

  cc <- cc %>% mutate(TOTAL_BENE_A = as.numeric(as.character(BENE_COUNT_PA_EQ_12)) + as.numeric(as.character(BENE_COUNT_PA_LT_12)))
  cc <- cc %>% mutate(TOTAL_BENE_B = as.numeric(as.character(BENE_COUNT_PB_EQ_12)) + as.numeric(as.character(BENE_COUNT_PB_LT_12)))
  cc <- cc %>% mutate(TOTAL_BENE_C = as.numeric(as.character(BENE_COUNT_PC_EQ_12)) + as.numeric(as.character(BENE_COUNT_PC_LT_12)))
  cc <- cc %>% mutate(TOTAL_BENE_D = as.numeric(as.character(BENE_COUNT_PD_EQ_12)) + as.numeric(as.character(BENE_COUNT_PD_LT_12)))

# ADD PART A SUMMARIES
#########################

cc <- cc %>% mutate(TOTAL_AVE_REIMB_PA = as.numeric(as.character(AVE_PA_PAY_PA_EQ_12)) + as.numeric(as.character(AVE_PA_PAY_PA_LT_12)))
cc <- cc %>% mutate(TOTAL_AVE_REIMB_PA_IP = as.numeric(as.character(AVE_IP_PAY_PA_EQ_12)) + as.numeric(as.character(AVE_IP_PAY_PA_LT_12)))
cc <- cc %>% mutate(TOTAL_AVE_REIMB_PA_SNF = as.numeric(as.character(AVE_SNF_PAY_PA_LT_12)) + as.numeric(as.character(AVE_SNF_PAY_PA_EQ_12)))
cc <- cc %>% mutate(TOTAL_AVE_REIMB_PA_HCH = as.numeric(as.character(AVE_OTH_PAY_PA_EQ_12)) + as.numeric(as.character(AVE_OTH_PAY_PA_LT_12)))

cc <- cc %>% mutate(TOTAL_IP_ADM_PER_BENE = as.numeric(as.character(AVE_IP_ADM_PA_EQ_12)) + as.numeric(as.character(AVE_IP_ADM_PA_LT_12)))
cc <- cc %>% mutate(TOTAL_AVE_NUM_SNF_DAYS_PER_BENE = as.numeric(as.character(AVE_SNF_DAYS_PA_EQ_12)) + as.numeric(as.character(AVE_SNF_DAYS_PA_LT_12)))

# ADD PART B SUMMARIES
##########################

cc <- cc %>% mutate(TOTAL_AVE_REIMB_PB = as.numeric(as.character(AVE_PB_PAY_PB_EQ_12)) + as.numeric(as.character(AVE_PB_PAY_PB_LT_12)))
cc <- cc %>% mutate(TOTAL_AVE_REIMB_PB_CARRIER = as.numeric(as.character(AVE_CA_PAY_PB_EQ_12)) + as.numeric(as.character(AVE_CA_PAY_PB_LT_12)))
cc <- cc %>% mutate(TOTAL_AVE_REIMB_PB_OP = as.numeric(as.character(AVE_OP_PAY_PB_EQ_12)) + as.numeric(as.character(AVE_OP_PAY_PB_LT_12)))
cc <- cc %>% mutate(TOTAL_AVE_REIMB_BP_HE = as.numeric(as.character(AVE_OTH_PAY_PB_EQ_12)) + as.numeric(as.character(AVE_OTH_PAY_PB_LT_12)))

cc <- cc %>% mutate(TOTAL_AVE_PHYS_VISITS_PER_BENE = as.numeric(as.character(AVE_CA_VST_PB_EQ_12)) + as.numeric(as.character(AVE_CA_VST_PB_LT_12)))
cc <- cc %>% mutate(TOTAL_AVE_OP_VISITS_PER_BENE = as.numeric(as.character(AVE_OP_VST_PB_EQ_12)) + as.numeric(as.character(AVE_OP_VST_PB_LT_12)))

# ADD PART D SUMMARIES
###########################

cc <- cc %>% mutate(TOTAL_AVE_DRUG_COST_PER_BENE = as.numeric(as.character(AVE_PDE_CST_PD_EQ_12)) + as.numeric(as.character(AVE_PDE_CST_PD_LT_12)))
cc <- cc %>% mutate(TOTAL_AVE_NUM_SCRIPTS_PER_BENE = as.numeric(as.character(AVE_PDE_PD_EQ_12)) + as.numeric(as.character(AVE_PDE_PD_LT_12)))
    
# ADD GENERAL SUMMARIES

cc <- cc %>% mutate(TOTAL_AVE_COSTS = TOTAL_AVE_REIMB_PA + TOTAL_AVE_REIMB_PB)

# FLATTENED, CATEGORIZED DATA: 242033 rows and 67 cols #
########################################################

# CONSTANT ASSIGNMENT
TOTAL_2010_BENEFICIARIES = 50088835
TOTAL_2010_AB_SPEND = 338000000000

# REMOVE NA VALUES FOR CALCULATIONS ONLY
cc_1 <- cc
cc_1[is.na(cc_1)] <- 0

# TOTAL CC BENEFICIARIES
TOTAL_2010_CC_BENEFICIARIES = 404226020
PROP_2010_CC_BENEFICIARIES = TOTAL_2010_CC_BENEFICIARIES / TOTAL_2010_BENEFICIARIES

# TOTAL CC SPENDING
TOTAL_2010_CC_AB_SPEND = 6059858244
PROP_2010_CC_AB_SPEND = TOTAL_2010_CC_AB_SPEND / TOTAL_2010_AB_SPEND

# SUMMARIZE
cc_sum <- cc_1 %>% group_by(CONDITION) %>% filter(HAS_CONDITION == 1) %>% summarize(ROWCOUNT = n(), SUM_BENE_A = sum(TOTAL_BENE_A), PROP_BENE_A = (SUM_BENE_A / TOTAL_2010_BENEFICIARIES), PROP_BENE_A_CC = (SUM_BENE_A / TOTAL_2010_CC_BENEFICIARIES), SUM_BENE_B = sum(TOTAL_BENE_B), SUM_BENE_C = sum(TOTAL_BENE_C), SUM_BENE_D = sum(TOTAL_BENE_D), SUM_AVE_A_COST = sum(TOTAL_AVE_REIMB_PA), SUM_AVE_B_COST = sum(TOTAL_AVE_REIMB_PB), SUM_AVE_DRUG_COST = sum(TOTAL_AVE_DRUG_COST_PER_BENE), SUM_AVE_ALL_AB = sum(TOTAL_AVE_COSTS), PROP_AB_COST = (SUM_AVE_ALL_AB / TOTAL_2010_AB_SPEND), PROP_CC_AB_COST = (SUM_AVE_ALL_AB / TOTAL_2010_CC_AB_SPEND), MEAN_IP_ADM = mean(TOTAL_IP_ADM_PER_BENE), MEAN_SNF_DAYS = mean(TOTAL_AVE_NUM_SNF_DAYS_PER_BENE), MEAN_SCRIPTS = mean(TOTAL_AVE_NUM_SCRIPTS_PER_BENE))
