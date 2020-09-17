  # DIGOXIN STUDY DATA ANALYSIS
  # TARGETS: Treatment Type, Event Type and Time to Event, Controlling for Dig Treatment & Ethnicity
  # Kyle P Rasku RN BSN
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(broom)
  
  dig <- read.csv("/home/kylier/Documents/UNH/800/Final/dig.csv")

# GATHER AND RECODE #
#####################

# Add CENSORED flag for survival analysis
# DEATHDAY contains the days to EITHER death or censorship; and only indicates true death when DEATH==1
dig_r <- dig %>% mutate(CENSORED=!is.na(DEATHDAY) & DEATH==0) 

# Code BMI groups
dig_r <- dig_r %>% filter(!is.na(BMI))
dig_r <- dig_r %>% mutate(BMIGROUP=cut(as.numeric(BMI), breaks=c(0, 18.5, 25, 30, Inf), labels=c("Underweight", 
                                                                                                 "Normal", 
                                                                                                 "Overweight", 
                                                                                                 "Obese+")))
# Filter out coerced NAs
dig_r <- dig_r %>% filter(!is.na(BMIGROUP))

# Recode SEX
dig_r <- dig_r %>% mutate(SEXGROUP=cut(SEX, breaks=c(-Inf, 1, 2), labels=c("Male", "Female")))

# Recode RACE
dig_r <- dig_r %>% mutate(RGROUP=cut(RACE, breaks=c(-Inf, 1, 2), labels=c("White", "Non-White")))

# FILTER to 2 Groups
wt_dig <- dig_r %>% filter(RACE==1)
nonwt_dig <- dig_r %>% filter(RACE==2)

# SAMPLE the white group to make it equal in size
set.seed(123)
index <- sample(1:nrow(wt_dig), 991)
wt_dig <- wt_dig[index, ]

full_sample <- full_join(wt_dig, nonwt_dig)

# CVD (logistic outcome) v. Age, BMI
  ggplot(full_sample, aes(x=AGE, y=CVD, color=BMIGROUP)) +
          geom_col() +
          facet_grid(~RGROUP) + 
          labs(title = "CV Caused Hospitalization x Age, BMI; Race (White v. Non-White)", x = "Age (Years)", y = "Number of CV Hospitalizations") +
          theme_minimal()
  
  
  #Severity of Hypertension Comparison (No major differences, slightly younger onset non-whites)
  sample_digh <- full_sample %>% filter(HYPERTEN==1)
  ggplot(sample_digh, aes(x=SYSBP, y=AGE, color=BMIGROUP)) +
        geom_point(position = "jitter", alpha = 0.5) +
        facet_grid(~RGROUP) + 
        labs(title = "Severity of HTN vs. Age, BMI; Race (White v. Non-White)", x = "Systolic BP", y = "Age") + 
        theme_minimal()


# Summary groups
  
# Summarize Data on Symptoms by Age, Race
sum_sx <- full_sample %>% gather(SYMPTOM, HAS_SYMPTOM, c(RALES, PEDEMA, RESTDYS, EXERTDYS, ACTLIMIT, S3, PULCONG))
sum_sxg <- sum_sx %>% filter(HAS_SYMPTOM==1)
  ggplot(sum_sxg, aes(x=AGE, y=HAS_SYMPTOM, color=SYMPTOM)) +
    geom_col() + 
    facet_grid(~RGROUP) +
    labs(title = "Symptoms by Age, Race (White v. Non-White)", x="Age", y = "Symptoms") + 
    theme_minimal()
  
  
# Summarize Data on Diagnoses by Age, Race
sum_dx <- full_sample %>% gather(DIAGNOSIS, HAS_DIAGNOSIS, c(ANGINA, DIABETES, PREVMI))
sum_dxg <- sum_dx %>% filter(HAS_DIAGNOSIS==1)
ggplot(sum_dxg, aes(x=AGE, y=HAS_DIAGNOSIS, color=DIAGNOSIS)) +
  geom_col() + 
  facet_grid(~RGROUP) +
  labs(title = "Existing Diagnosis by Age, Race (White v. Non-White)", x="Age", y = "Diagnoses") + 
  theme_minimal()

# Summarize Event Data by Age, Race
sum_ev <- full_sample %>% gather(EVENT, HAD_EVENT, c(WHF, DIG, MI, STRK, UANG, SVA, VENA, CREV, OCVD, OTH))
sum_evg <- sum_ev %>% filter(HAD_EVENT==1)
ggplot(sum_evg, aes(x=AGE, y=HAD_EVENT, color=EVENT)) +
  geom_col() + 
  facet_grid(~RGROUP) +
  labs(title = "Events by Age, Race (White v. Non-White)", x="Age", y = "Events") + 
  theme_minimal()

  
# Gather data on Treatments (Leave out Digoxin)
sum_tx <- full_sample %>% gather(TREATMENT, HAS_TREATMENT, c(DIURETK, DIURET, KSUPP, ACEINHIB, NITRATES, HYDRAL, VASOD))
sum_tx <- sum_tx %>% mutate(TGROUP=cut(TRTMT, breaks=c(-Inf, 0, 1), labels=c("Placebo", "Digoxin")))
sum_txg <- sum_tx %>% filter(HAS_TREATMENT==1)
ggplot(sum_txg, aes(x=AGE, y=HAS_TREATMENT, color=TREATMENT)) +
  geom_col() + 
  facet_grid(TGROUP~RGROUP) +
  labs(title = "Treatments by Dig, Age, Race (White v. Non-White)", x="Age", y = "Other Treatments") + 
  theme_minimal()

# Severity of HF
# With Prior MI
# Without Prior MI
# Among Patients who Eventually Needed CABG/PCI
# Among Patients who Eventually Needed Other CT Surgery
# Treatments vs. Labs

