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

# Gather data on time to events
fr_ex <- fr_ex %>% gather(EVENT, DAYS_TO_EVENT, TIMEAP:TIMEHYP)

# Recode to readable
fr_ex <- fr_ex %>% mutate(EVENT = recode(EVENT,
                                         TIMEAP = "Days to 1st Angina",
                                         TIMEMIFC = "Days to 1st MI",
                                         TIMECHD = "Days to 1st Angina/MI/Fatal CHD",
                                         TIMESTR = "Days to 1st Stroke (Any)",
                                         TIMECVD = "Days to 1st MI/Fatal CHD/Stroke (Any)",
                                         TIMEHYP = "Days to 1st Hypertension"))


# Remove Zeros
fr_ex <- fr_ex %>% filter(DAYS_TO_EVENT == 0)

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

