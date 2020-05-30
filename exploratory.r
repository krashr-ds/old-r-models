# Exploratory data analysis; Framingham data

library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)

# USE frmgham data where each person has 3 entries / periods (9618 / 3 = 3206)
# GROUP
fr_ex <- frmgham %>% 
  group_by(RANDID) %>% 
  filter(n()==3)

# GATHER AND RECODE #
#####################

# Gather data on existing diagnoses
fr_ex <- fr_ex %>% gather(DIAGNOSIS, HAS_DIAGNOSIS, PREVCHD:PREVHYP)

# Recode to readable
fr_ex <- fr_ex %>% mutate(DIAGNOSIS = recode(DIAGNOSIS,
                                           PREVAP = "Angina (Diagnosis)",
                                           PREVCHD = "Angina or MI (Diagnosis)",
                                           PREVMI = "MI (Diagnosis)",
                                           PREVSTRK = "Stroke (Diagnosis)",
                                           PREVHYP = "Hypertension (Diagnosis)"))


# Gather data on flags
fr_ex <- fr_ex %>% gather(FLAG, FLAG_VAL, c(DIABETES, ANGINA, HOSPMI, MI_FCHD, ANYCHD, STROKE, CVD, HYPERTEN))

# Recode to readable
fr_ex <- fr_ex %>% mutate(FLAG = recode(FLAG,
                                         DIABETES = "Diabetes",
                                         ANGINA = "Angina",
                                         HOSPMI = "MI",
                                         MI_FCHD = "MI/Fatal CHD",
                                         ANYCHD = "Angina/MI/Fatal CHD",
                                         STROKE = "Any Stroke",
                                         CVD = "MI/Fatal CHD/Stroke (Any)",
                                         HYPERTEN = "Hypertension"))


# Gather data on events
fr_ex <- fr_ex %>% gather(EVENT, DAYS_TO_EVENT, TIMEAP:TIMEHYP)

# Recode to readable
fr_ex <- fr_ex %>% mutate(EVENT = recode(EVENT,
                                        TIMEAP = "Angina",
                                        TIMEMI = "MI",
                                        TIMEMIFC = "MI/Fatal CHD",
                                        TIMECHD = "Angina/MI/Fatal CHD",
                                        TIMESTRK = "Any Stroke",
                                        TIMECVD = "MI/Fatal CHD/Stroke (Any)",
                                        TIMEDTH = "Death",
                                        TIMEHYP = "Hypertension"))


# Code BMI and HTN clinical groups
# EXAMPLE: df %>% mutate(category=cut(a, breaks=c(-Inf, 0.5, 0.6, Inf), labels=c("low","middle","high")))

    
# Separate into periods
fr_p1 <- fr_ex %>% filter(PERIOD == 1)
fr_p2 <- fr_ex %>% filter(PERIOD == 2)
fr_p3 <- fr_ex %>% filter(PERIOD == 3)


