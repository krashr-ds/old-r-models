# Exploratory data analysis; Framingham data
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(purrr)
  library(broom)
  
  # USE frmgham data where each person has 3 entries / periods (9618 rows / 3 = 3206 individuals)
  
  fr_ex <- frmgham %>% 
    group_by(RANDID) %>% 
    filter(n()==3)
  
  # RECODE #
  ##########
  
  
  # Code BMI groups
  fr_ex <- fr_ex %>% filter(!is.na(BMI))
  fr_ex <- fr_ex %>% mutate(BMIGROUP=cut(as.numeric(BMI), breaks=c(0, 18.5, 25, 30, Inf), labels=c("Underweight", 
                                                                                           "Normal", 
                                                                                           "Overweight", 
                                                                                           "Obese+")))
  fr_ex <- fr_ex %>% filter(!is.na(BMIGROUP))
  
  # Code HTN clinical groups
  # Diastolic first, since most people with high diastolic pressure also have high systolic pressure.
  
  fr_ex <- fr_ex %>% mutate(HTNGROUP=cut(DIABP, breaks=c(0, 60, 80, 90, 120, Inf), labels=c("Low", 
                                                                                            "Normal", 
                                                                                            "Hypertension Stage 1", 
                                                                                            "Hypertension Stage 2",
                                                                                            "Hypertensive Crisis")))
  
  fr_ex <- fr_ex %>% filter(DIABP < 90) %>% mutate(HTNGROUP=cut(SYSBP, breaks=c(0, 91, 121, 131, 141, 181, Inf), labels=c("Low", 
                                                                                                                          "Normal", 
                                                                                                                          "Prehypertension", 
                                                                                                                          "Hypertension Stage 1", 
                                                                                                                          "Hypertension Stage 2",
                                                                                                                          "Hypertensive Crisis")))
  
  # Recode SEX
  fr_ex <- fr_ex %>% mutate(SEXGROUP=cut(SEX, breaks=c(-Inf, 1, 2), labels=c("Male", "Female")))
  
  # Add CENSORED flag for survival analysis
  # TIMEDTH contains the days to EITHER death or censorship; and only indicates true death when DEATH==1
  fr_ex <- fr_ex %>% mutate(CENSORED=!is.na(TIMEDTH) & DEATH==0)
  
  
  # N = 7173 rows and 42 columns #
  ################################
  
  fr_sum1 <- fr_ex %>% filter(PERIOD==1)
  fr_sum1 <- fr_sum1 %>% mutate(NUM_DX = PREVAP + PREVCHD + PREVMI + PREVSTRK + DIABETES + PREVHYP)
  fr_sum1 <- fr_sum1 %>% mutate(NUM_EVENTS = ANGINA + HOSPMI + MI_FCHD + STROKE + CVD + HYPERTEN)
  
  fr_sum2 <- fr_ex %>% filter(PERIOD==2)
  fr_sum2 <- fr_sum2 %>% mutate(NUM_DX = PREVAP + PREVCHD + PREVMI + PREVSTRK + DIABETES + PREVHYP)
  fr_sum2 <- fr_sum2 %>% mutate(NUM_EVENTS = ANGINA + HOSPMI + MI_FCHD + STROKE + CVD + HYPERTEN)
  
  fr_sum3 <- fr_ex %>% filter(PERIOD==3)
  fr_sum3 <- fr_sum3 %>% mutate(NUM_DX = PREVAP + PREVCHD + PREVMI + PREVSTRK + DIABETES + PREVHYP)
  fr_sum3 <- fr_sum3 %>% mutate(NUM_EVENTS = ANGINA + HOSPMI + MI_FCHD + STROKE + CVD + HYPERTEN)

# Primary Summary Graph
  ggplot() + 
    geom_col(data = fr_sum1, aes(x = NUM_DX, y = NUM_EVENTS), color="red") + 
    geom_col(data = fr_sum2, aes(x = NUM_DX, y = NUM_EVENTS), color="blue") + 
    geom_col(data = fr_sum3, aes(x = NUM_DX, y = NUM_EVENTS), color="dark green") +
    facet_wrap(~PREVHYP) +
    labs(title = "Summary: Number of Events x Number of Diagnoses, Hypertension Status", x = "Number of Diagnoses",  y = "Number of Events")

  # If BP readings were high, PREVHTN assumed for Period 1 and 2
  fr_sum1a <- fr_ex %>% filter(PERIOD==1)
  fr_sum1a <- fr_sum1a %>% mutate(PREVHYP=PREVHYP + as.numeric(PREVHYP==0 & HTNGROUP!="Low" & HTNGROUP!="Normal"))
  fr_sum1a <- fr_sum1a %>% mutate(NUM_DX = PREVAP + PREVCHD + PREVMI + PREVSTRK + DIABETES + PREVHYP)
  fr_sum1a <- fr_sum1a %>% mutate(NUM_EVENTS = ANGINA + HOSPMI + MI_FCHD + STROKE + CVD + HYPERTEN)
  
  fr_sum2a <- fr_ex %>% filter(PERIOD==2)
  fr_sum2a <- fr_sum2a %>% mutate(PREVHYP=PREVHYP + as.numeric(PREVHYP==0 & HTNGROUP!="Low" & HTNGROUP!="Normal"))
  fr_sum2a <- fr_sum2a %>% mutate(NUM_DX = PREVAP + PREVCHD + PREVMI + PREVSTRK + DIABETES + PREVHYP)
  fr_sum2a <- fr_sum2a %>% mutate(NUM_EVENTS = ANGINA + HOSPMI + MI_FCHD + STROKE + CVD + HYPERTEN)
  
  # Summary Regraph
  ggplot() + 
    geom_col(data = fr_sum1a, aes(x = NUM_DX, y = NUM_EVENTS), color="red") + 
    geom_col(data = fr_sum2a, aes(x = NUM_DX, y = NUM_EVENTS), color="blue") + 
    geom_col(data = fr_sum3, aes(x = NUM_DX, y = NUM_EVENTS), color="dark green") +
    facet_wrap(~PREVHYP) +
  labs(title = "Summary: Number of Events x Number of Diagnoses, Hypertension Status", x = "Number of Diagnoses",  y = "Number of Events")

# Keep recoded diagnosis data  
#fr_sum1 <- fr_sum1a
#fr_sum2 <- fr_sum2a

# Period 1 - Gather diagnoses
fr_gathered1 <- fr_sum1 %>% gather(DIAGNOSIS, HAS_DIAGNOSIS, c(PREVAP, PREVMI, PREVSTRK, DIABETES))

# Recode to readable
fr_gathered1 <- fr_gathered1 %>% mutate(DIAGNOSIS = recode(DIAGNOSIS,
                                             PREVAP = "Angina (Diagnosis)",
                                             PREVMI = "MI (Diagnosis)",
                                             PREVSTRK = "Stroke (Diagnosis)",
                                             DIABETES = "Diabetes"))
# Period 1 - Gather events
fr_gathered1 <- fr_gathered1 %>% gather(EVENT_NAME, EVENT_FLAG_VAL, c(ANGINA, HOSPMI, MI_FCHD, ANYCHD, STROKE, CVD, DEATH))
fr_gathered1 <- fr_gathered1 %>% mutate(EVENT_NAME = recode(EVENT_NAME,
                                          ANGINA = "ANGINA",
                                          HOSPMI = "MI",
                                          MI_FCHD = "MI/FCHD",
                                          ANYCHD = "ANG/MI/FCHD",
                                          STROKE = "STROKE",
                                          CVD = "ANYCHD/STROKE",
                                          DEATH = "DEATH"))

#Period 1 - Number & Type of Events by Number of Diagnoses; Facet Grid: PREVHYP
ggplot(fr_gathered1, aes(x = NUM_DX, y=EVENT_FLAG_VAL, col=EVENT_NAME)) + 
      geom_col() + 
      facet_grid(~PREVHYP) +
    labs(title = "Period 1: Events x Diagnosis, Event Type for Patients with and without Hypertension")

ggplot(fr_gathered1, aes(x = SYSBP, y=HYPERTEN)) + 
    geom_point(position="jitter", alpha=0.5) + 
    geom_smooth(method = "lm") + 
    facet_grid(~CURSMOKE) +
    labs(title = "Period 1: Hypertension as Event x Systolic BP, Smoking Status")
    
ggplot(fr_gathered1, aes(x = DIABP, y=HYPERTEN)) + 
      geom_point(position="jitter", alpha=0.5) + 
      geom_smooth(method = "lm") + 
      facet_grid(~CURSMOKE) +
    labs(title = "Period 1: Hypertension as Event x Diastolic BP, Smoking Status")


ggplot(fr_sum1, aes(x = SYSBP, y=HYPERTEN)) + 
      geom_point(position="jitter", alpha=0.5) + 
      geom_smooth(method = "lm") + 
      facet_grid(~DIABETES) +
    labs(title = "Period 1: Hypertension as Event x Systolic BP, Diabetes")
    
ggplot(fr_sum1, aes(x = DIABP, y=HYPERTEN)) + 
      geom_point(position="jitter", alpha=0.5) + 
      geom_smooth(method = "lm") + 
      facet_grid(~DIABETES) +
    labs(title = "Period 1: Hypertension as Event x Diastolic BP, Diabetes")
    
    
ggplot(fr_gathered1, aes(x = AGE, y=HYPERTEN)) + 
      geom_point(position="jitter", alpha=0.5) + 
      geom_smooth(method = "lm") + 
      facet_grid(~SEXGROUP) +
    labs(title = "Period 1: Hypertension as Event x Age, Sex")
    
ggplot(fr_gathered1, aes(x = AGE, y=SYSBP)) + 
      geom_point(position="jitter", alpha=0.5) + 
      geom_smooth(method = "lm") + 
      facet_grid(~SEXGROUP) +
    labs(title = "Period 1: Systolic BP x Age, Sex")
    
ggplot(fr_gathered1, aes(x = AGE, y=DIABP)) + 
      geom_point(position="jitter", alpha=0.5) + 
      geom_smooth(method = "lm") + 
      facet_grid(~SEXGROUP) +
    labs(title = "Period 1: Diastolic BP x Age, Sex")
    

ggplot(fr_gathered1, aes(x = BMIGROUP, y=HYPERTEN)) + 
      geom_col() + 
      facet_grid(~SEXGROUP) +
labs(title = "Period 1: Hypertension as Event x BMI, Sex")
    
ggplot(fr_gathered1, aes(x = BMIGROUP, y=SYSBP)) + 
      geom_col() + 
      facet_grid(~SEXGROUP) +
labs(title = "Period 1: Systolic BP x BMI, Sex")
    
ggplot(fr_gathered1, aes(x = BMIGROUP, y=DIABP)) + 
      geom_col() + 
      facet_grid(~SEXGROUP) +
labs(title = "Period 1: Diastolic BP x BMI, Sex")

ggplot(fr_gathered1, aes(x = GLUCOSE, y=HYPERTEN)) + 
  geom_col() + 
  facet_grid(~SEXGROUP) +
  labs(title = "Period 1: Hypertension x Glucose mg/dL")

fr_glucose <- fr_sum1 %>% filter(!is.na(GLUCOSE))
ggplot(fr_glucose, aes(x = GLUCOSE, y=HYPERTEN, col=HTNGROUP)) + 
  geom_col() + 
  facet_grid(~SEXGROUP) +
  labs(title = "Period 1: Hypertension x Glucose mg/dL")

ggplot(fr_glucose, aes(x = GLUCOSE, y=SYSBP, col=HTNGROUP)) + 
  geom_col() + 
  facet_grid(~SEXGROUP) +
  labs(title = "Period 1: Systolic Pressure x Glucose mg/dL")

ggplot(fr_glucose, aes(x = GLUCOSE, y=DIABP, col=HTNGROUP)) + 
  geom_col() + 
  facet_grid(~SEXGROUP) +
  labs(title = "Period 1: Diastolic Pressure x Glucose mg/dL")

ggplot(fr_sum1, aes(x = HYPERTEN, y = log(TIMEDTH), col=HTNGROUP)) + 
    geom_col() + 
    facet_grid(~PREVHYP) + 
    labs(title = "Period 1: Hypertensive Severity and Time to Death")

ggplot(fr_sum1a, aes(x = HYPERTEN, y = log(TIMEDTH), col=HTNGROUP)) + 
  geom_col() + 
  facet_grid(~PREVHYP) + 
  labs(title = "Period 1: Hypertensive Severity and Time to Death")

fr_sum1a_nocen <- fr_sum1a %>% filter(CENSORED==FALSE & !is.na(BPMEDS) & BPMEDS!=".")
ggplot(fr_sum1a_nocen, aes(x = SYSBP, y = log(TIMEDTH), col=HTNGROUP)) + 
  geom_point(position="jitter", alpha=0.7) +
  facet_grid(~BPMEDS) + 
  labs(title = "Period 1: Death vs. Hypertension, BPMeds")

fr_sum2a_nocen <- fr_sum2a %>% filter(CENSORED==FALSE & !is.na(BPMEDS) & BPMEDS!=".")
ggplot(fr_sum2a_nocen, aes(x = SYSBP, y = log(TIMEDTH), col=HTNGROUP)) + 
  geom_point(position="jitter", alpha=0.7) +
  facet_grid(~BPMEDS) + 
  labs(title = "Period 2: Death vs. Hypertension, BPMeds")

fr_sum3_nocen <- fr_sum3 %>% filter(CENSORED==FALSE & !is.na(BPMEDS) & BPMEDS!=".")
ggplot(fr_sum3_nocen, aes(x = SYSBP, y = log(TIMEDTH), col=HTNGROUP)) + 
  geom_point(position="jitter", alpha=0.7) +
  facet_grid(~BPMEDS) + 
  labs(title = "Period 3: Death vs. Hypertension, BPMeds")
