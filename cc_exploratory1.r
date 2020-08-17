  # Exploratory Data Analysis; Chronic Conditions Public Use File (PUF) - 2010
  # Kyle P Rasku RN BSN  ~  For and With Data Mining, GROUP A 
  # 8/16/2020
  # 
  
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
  
  cc <- cc %>% mutate(AGE_FLAG = recode(BENE_AGE_CAT_CD, 
                                        "1" = 0,
                                        "2" = 1,
                                        "3" = 1,
                                        "4" = 1,
                                        "5" = 2, 
                                        "6" = 2))
  
  cc <- cc %>% mutate(NUM_CCS = recode(CC_2_OR_MORE,
                                       "0" = "<2",
                                       "1" = "2+ (LISTED)"))
 
  
  # NOTE: SAMPLE = PROFILES OF PART A BENEFICIARIES WITH AT LEAST 1 CHRONIC CONDITION ENROLLED FOR THE ENTIRE YEAR (EQ 12)
  # This is a conservative sample.  It contains neither the youngest / newest nor the oldest / dying members
  # It facilitates calculation of prevalence, and comparisons between prevalence calculations and population
  # It comes the closest to allowing us to reliably sample all part A beneficiaries, however the exact
  # number of beneficiaries cannot be calculated so we must given n in terms of profiles: 
  
  # n (profiles) = 89167 out of 216877 (total uncensored profiles with part A spending) (41%), representing part A
  # beneficiaries included in this file, with at least 1 of the conditions listed (HAS_CONDITION == 1).
  # This itself is a subset (89.6%) of the total # of profiles 242033.
  
  # This does not represent the true % of 2010 Part A Beneficiaries with at least 1 CC, however, because the two most 
  # common CCs, Hypertension and Hyperlipidemia, are excluded from this data file.

  # 4608 profiles are censored for privacy; these profiles are excluded.
 
  cc_copy <- cc
  cc <- cc %>% mutate(AVE_PA_PAY_PA_EQ_12 = as.numeric(as.character(AVE_PA_PAY_PA_EQ_12)))
  cc <- cc %>% filter((HAS_CONDITION == 1 | HAS_CONDITION == 0) & AVE_PA_PAY_PA_EQ_12 > 0)
  
  # EQ 12 TO NUMERIC FIELDS FOR EASE OF CALCULATIONS
  ####################################################
  # Note: Syntax for Conversion of FACTOR to NUMERIC (must pass through CHARACTER)
  # x_num <- as.numeric(as.character(x))  
  
  cc <- cc %>% mutate(BENE_COUNT_PA_EQ_12 = as.numeric(as.character(BENE_COUNT_PA_EQ_12)))
  cc <- cc %>% mutate(BENE_COUNT_PB_EQ_12 = as.numeric(as.character(BENE_COUNT_PB_EQ_12)))
  cc <- cc %>% mutate(BENE_COUNT_PC_EQ_12 = as.numeric(as.character(BENE_COUNT_PC_EQ_12)))
  cc <- cc %>% mutate(BENE_COUNT_PD_EQ_12 = as.numeric(as.character(BENE_COUNT_PD_EQ_12)))

  cc <- cc %>% mutate(AVE_IP_PAY_PA_EQ_12 = as.numeric(as.character(AVE_IP_PAY_PA_EQ_12)))
  cc <- cc %>% mutate(AVE_SNF_PAY_PA_EQ_12 = as.numeric(as.character(AVE_SNF_PAY_PA_EQ_12)))
  cc <- cc %>% mutate(AVE_OTH_PAY_PA_EQ_12 = as.numeric(as.character(AVE_OTH_PAY_PA_EQ_12)))
  cc <- cc %>% mutate(AVE_IP_ADM_PA_EQ_12 = as.numeric(as.character(AVE_IP_ADM_PA_EQ_12)))
  cc <- cc %>% mutate(AVE_SNF_DAYS_PA_EQ_12 = as.numeric(as.character(AVE_SNF_DAYS_PA_EQ_12)))
  
  cc <- cc %>% mutate(AVE_PB_PAY_PB_EQ_12 = as.numeric(as.character(AVE_PB_PAY_PB_EQ_12)))
  cc <- cc %>% mutate(AVE_CA_PAY_PB_EQ_12 = as.numeric(as.character(AVE_CA_PAY_PB_EQ_12)))
  cc <- cc %>% mutate(AVE_OP_PAY_PB_EQ_12 = as.numeric(as.character(AVE_OP_PAY_PB_EQ_12)))
  cc <- cc %>% mutate(AVE_OTH_PAY_PB_EQ_12 = as.numeric(as.character(AVE_OTH_PAY_PB_EQ_12)))
  cc <- cc %>% mutate(AVE_OP_VST_PB_EQ_12 = as.numeric(as.character(AVE_OP_VST_PB_EQ_12)))
  cc <- cc %>% mutate(AVE_CA_VST_PB_EQ_12 = as.numeric(as.character(AVE_CA_VST_PB_EQ_12)))
  
  cc <- cc %>% mutate(AVE_PDE_CST_PD_EQ_12 = as.numeric(as.character(AVE_PDE_CST_PD_EQ_12)))
  cc <- cc %>% mutate(AVE_PDE_PD_EQ_12 = as.numeric(as.character(AVE_PDE_PD_EQ_12)))
  
  # FLATTENED, CATEGORIZED DATA: 216877 rows and 49 cols #
  ########################################################
  
  cc_err <- cc %>% filter(HAS_CONDITION == 0)
  cc_sample <- cc %>% filter(HAS_CONDITION == 1)
  
  # cc_err: 127710 profiles with spending but no CCs from THIS file (likely have other CCs that were excluded)
  # cc_sample: our 89167 profiles that have at least 1 chronic condition from this file

  # CONSTANT ASSIGNMENT
  #######################
  # Part A Beneficiaries, 97.4% of all A&B Beneficiaries for 2010
  TOTAL_2010_PTA_BENEFICIARIES = 48808455
  TOTAL_2010_AB_BENEFICIARIES = 50088835
  
  # Beneficiaries with any Chronic Condition (Sample used by CMS, 2010) Source: Chartbook
  TOTAL_2010_CC_BENEFICIARIES = 31000000
  # Beneficiaries with 2 or More Chronic Conditions (2/3rds - 69% - of all CC BENEFICIARIES) Source: Chartbook
  TOTAL_2010_CC_2ORMORE = 21400000
  
  # FROM THE CMS WEBSITE: TOTAL A & B SPENDING FOR ALL BENFICIARIES 2010
  TOTAL_2010_AB_SPEND = 338000000000
  

  # REMOVE NA VALUES FOR CALCULATIONS ONLY
  cc_1 <- cc_sample
  cc_1[is.na(cc_1)] <- 0
  
  cc_sum <- cc_1 %>% group_by(CONDITION) %>% filter(HAS_CONDITION == 1) %>% summarize(ROWCOUNT = n(), SUM_BENE_A = sum(BENE_COUNT_PA_EQ_12), SUM_BENE_B = sum(BENE_COUNT_PB_EQ_12), SUM_BENE_C = sum(BENE_COUNT_PC_EQ_12), SUM_BENE_D = sum(BENE_COUNT_PD_EQ_12), SUM_AVE_A_COST = sum(AVE_PA_PAY_PA_EQ_12), SUM_AVE_B_COST = sum(AVE_PB_PAY_PB_EQ_12), SUM_AVE_DRUG_COST = sum(AVE_PDE_CST_PD_EQ_12), SUM_AVE_AB_COST = SUM_AVE_A_COST + SUM_AVE_B_COST, MEAN_IP_ADM = mean(AVE_IP_ADM_PA_EQ_12), MEAN_SNF_DAYS = mean(AVE_SNF_DAYS_PA_EQ_12), MEAN_PHYS_VISITS = mean(AVE_CA_VST_PB_EQ_12), MEAN_OP_VISITS = mean(AVE_OP_VST_PB_EQ_12))
  cc_sum_sex <- cc_1 %>% group_by(CONDITION, SEX_CD) %>% filter(HAS_CONDITION == 1) %>% summarize(ROWCOUNT = n(), SUM_BENE_A = sum(BENE_COUNT_PA_EQ_12), SUM_BENE_B = sum(BENE_COUNT_PB_EQ_12), SUM_BENE_C = sum(BENE_COUNT_PC_EQ_12), SUM_BENE_D = sum(BENE_COUNT_PD_EQ_12), SUM_AVE_A_COST = sum(AVE_PA_PAY_PA_EQ_12), SUM_AVE_B_COST = sum(AVE_PB_PAY_PB_EQ_12), SUM_AVE_DRUG_COST = sum(AVE_PDE_CST_PD_EQ_12), SUM_AVE_AB_COST = SUM_AVE_A_COST + SUM_AVE_B_COST , PROP_AB_COST = (SUM_AVE_AB_COST / TOTAL_2010_AB_SPEND), MEAN_IP_ADM = mean(AVE_IP_ADM_PA_EQ_12), MEAN_SNF_DAYS = mean(AVE_SNF_DAYS_PA_EQ_12), MEAN_PHYS_VISITS = mean(AVE_CA_VST_PB_EQ_12), MEAN_OP_VISITS = mean(AVE_OP_VST_PB_EQ_12))
  cc_sum_age <- cc_1 %>% group_by(CONDITION, AGE_FLAG) %>% filter(HAS_CONDITION == 1) %>% summarize(ROWCOUNT = n(), SUM_BENE_A = sum(BENE_COUNT_PA_EQ_12), SUM_BENE_B = sum(BENE_COUNT_PB_EQ_12), SUM_BENE_C = sum(BENE_COUNT_PC_EQ_12), SUM_BENE_D = sum(BENE_COUNT_PD_EQ_12), SUM_AVE_A_COST = sum(AVE_PA_PAY_PA_EQ_12), SUM_AVE_B_COST = sum(AVE_PB_PAY_PB_EQ_12), SUM_AVE_DRUG_COST = sum(AVE_PDE_CST_PD_EQ_12), SUM_AVE_AB_COST = SUM_AVE_A_COST + SUM_AVE_B_COST , PROP_AB_COST = (SUM_AVE_AB_COST / TOTAL_2010_AB_SPEND), MEAN_IP_ADM = mean(AVE_IP_ADM_PA_EQ_12), MEAN_SNF_DAYS = mean(AVE_SNF_DAYS_PA_EQ_12), MEAN_PHYS_VISITS = mean(AVE_CA_VST_PB_EQ_12), MEAN_OP_VISITS = mean(AVE_OP_VST_PB_EQ_12))
  
  # QUESTION 1: CHRONIC CONDITIONS BY "IMPACT" - # OF PT A BENEFICIARIES
  # Three ways of measuring impact within our profiles sample: # of beneficiaries with the condition
  #                                                            average A & B cost per beneficiary with the condition 
  #                                                            mean inpatient admissions for each condition

  # Summary Graphs: QUESTION 1
  
  # Profiles by Condition
  cc_sum2aa <- cc_sum %>% group_by(CONDITION) %>% arrange(desc(ROWCOUNT))
  cc_sum2aa$CONDITION <- reorder(cc_sum2aa$CONDITION, cc_sum2aa$ROWCOUNT, FUN = mean)
  ggplot(cc_sum2aa, aes(x = CONDITION, y = ROWCOUNT, fill = CONDITION)) + 
    geom_col(alpha = 0.7) + 
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Summary: Profiles by Condition", x = "Condition",  y = "Profiles")
  
  # Beneficiaries by Condition
  cc_sum2 <- cc_sum %>% group_by(CONDITION) %>% arrange(desc(SUM_BENE_A))
  cc_sum2$CONDITION <- reorder(cc_sum2$CONDITION, cc_sum2$SUM_BENE_A, FUN = mean)
  ggplot(cc_sum2, aes(x = CONDITION, y = SUM_BENE_A, fill = CONDITION)) + 
    geom_col(alpha = 0.7) + 
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Summary: Beneficiaries by Condition", x = "Condition",  y = "Beneficiaries")
  
  # Beneficiaries by Condition, Sex
  cc_sum_sex$CONDITION <- reorder(cc_sum_sex$CONDITION, cc_sum_sex$SUM_BENE_A, FUN = mean)
  cc_sum_sex$SEX_CD <- reorder(cc_sum_sex$SEX_CD, cc_sum_sex$SUM_BENE_A, FUN = mean)
  ggplot(cc_sum_sex, aes(x = CONDITION, y = SUM_BENE_A, fill = SEX_CD)) + 
    geom_col(position="dodge", alpha = 0.7) + 
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Summary: Beneficiaries by Condition, Sex", x = "Condition",  y = "Beneficiaries")
  
  # Beneficiaries by Condition, Age < or > 65 years
  cc_sum_age$CONDITION <- reorder(cc_sum_age$CONDITION, cc_sum_age$SUM_BENE_A, FUN = mean)
  cc_sum_age$AGE_FLAG <- reorder(cc_sum_age$AGE_FLAG, cc_sum_age$SUM_BENE_A, FUN = mean)
  ggplot(cc_sum_age, aes(x = CONDITION, y = SUM_BENE_A, fill = AGE_FLAG)) + 
    geom_col(position="dodge", alpha = 0.7) + 
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_discrete(labels = c("< 65", "65-79", "80+")) +
    labs(title = "Summary: Beneficiaries by Condition, Age", x = "Condition",  y = "Beneficiaries", fill = "Age")
  
  # Total AB Cost by Condition
  cc_sum3 <- cc_sum %>% arrange(desc(SUM_AVE_AB_COST))
  cc_sum3$CONDITION <- reorder(cc_sum3$CONDITION, cc_sum3$SUM_AVE_AB_COST, FUN = mean)
  ggplot(cc_sum3, aes(x = CONDITION, y = SUM_AVE_AB_COST, fill = CONDITION)) + 
    geom_col(alpha = 0.7) + 
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Summary: Average AB Cost/Bene by Condition", x = "Condition",  y = "Avg AB Cost/Bene")
  
  # Mean Inpatient Admissions by Condition
  cc_sum4 <- cc_sum %>% arrange(desc(MEAN_IP_ADM))
  cc_sum4$CONDITION <- reorder(cc_sum4$CONDITION, cc_sum4$MEAN_IP_ADM, FUN = mean)
  ggplot(cc_sum4, aes(x = CONDITION, y = MEAN_IP_ADM, fill = CONDITION)) + 
    geom_col(alpha = 0.7) + 
    coord_flip() +
    guides(fill = guide_legend(reverse = TRUE)) +
    labs(title = "Summary: Mean Inpatient Admits/Bene by Condition", x = "Condition",  y = "Mean Admits/Bene")
  
  # QUESTION 2: CC PREVALENCE
  # Prevalence relies on time period.  We are starting with our 89167 profiles sample
  # 88,903 of these profiles are marked as 2+ chronic conditions (99.7%)
  # 
  cc_prev_groups <- cc_1 %>% group_by(CONDITION) %>% filter(HAS_CONDITION == 1 & CC_2_OR_MORE == "1")
  cc_prev_sum <- cc_prev_groups %>% group_by(CONDITION) %>% summarize(TWO_OR_MORE = n(), SUM_BENE_A_EQ_12 = sum(BENE_COUNT_PA_EQ_12), PREV = (TWO_OR_MORE * 100) / SUM_BENE_A_EQ_12) %>% arrange(desc(PREV))
  cc_prev_sum2 <- cc_prev_sum %>% summarize(MEAN_PREVALENCE = mean(PREV), MEDIAN_PREVALENCE = median(PREV), RANGE1_PREVALENCE = min(range(PREV)), RANGE2_PREVALENCE = max(range(PREV)))
    

  # QUESTION 3: SNF COSTS & SNF DAYS 
  # Top chronic conditions from our sample profiles in terms of SNF costs / days
  cc_snf <- cc_1 %>% group_by(CONDITION) %>% filter(HAS_CONDITION == 1 & AVE_SNF_DAYS_PA_EQ_12 > 0)
  cc_snf_sum <- cc_snf %>% group_by(CONDITION) %>% summarize(ROWCOUNT = n(), TOTAL_AVE_SNF_SPEND = sum(AVE_SNF_PAY_PA_EQ_12), TOTAL_SNF_DAYS = sum(AVE_SNF_DAYS_PA_EQ_12)) %>% arrange(desc(TOTAL_AVE_SNF_SPEND))
  
  # QUESTION 4: Average # of Outpatient Visits / Physician Visits 
  # This looks at Part B beneficiaries from our profiles sample, because OP is paid by Part B
  cc_sum5 <- cc_sum %>% group_by(CONDITION) %>% select(SUM_BENE_B, SUM_AVE_B_COST, MEAN_PHYS_VISITS, MEAN_OP_VISITS) %>% arrange(desc(MEAN_OP_VISITS))
  
  # QUESTION 5: DRUG REDUCTION TARGETS 
  # Drug costs and Mean # of scripts per beneficiary from our sample, grouped by age to see what the large costs are for younger vs. older beneficiaries
  cc_sum6 <- cc_1 %>% group_by(CONDITION, AGE_CATEGORY) %>% filter(HAS_CONDITION == 1) %>% summarize(ROWCOUNT = n(), ALL_DRUG_COSTS = sum(AVE_PDE_CST_PD_EQ_12), MEAN_SCRIPTS = mean(AVE_PDE_PD_EQ_12)) %>% arrange(desc(ALL_DRUG_COSTS))
  
  
