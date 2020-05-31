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

# GATHER AND RECODE #
#####################

# Gather data on existing diagnoses
fr_ex <- fr_ex %>% gather(DIAGNOSIS, HAS_DIAGNOSIS, c(PREVAP, PREVCHD, PREVMI, PREVSTRK, PREVHYP, DIABETES))

# Recode to readable
fr_ex <- fr_ex %>% mutate(DIAGNOSIS = recode(DIAGNOSIS,
                                           PREVAP = "Angina (Diagnosis)",
                                           PREVCHD = "Angina or MI (Diagnosis)",
                                           PREVMI = "MI (Diagnosis)",
                                           PREVSTRK = "Stroke (Diagnosis)",
                                           PREVHYP = "Hypertension (Diagnosis)",
                                           DIABETES = "Diabetes"))


# Gather data on flags
fr_ex <- fr_ex %>% gather(FLAG, FLAG_VAL, c(ANGINA, HOSPMI, MI_FCHD, ANYCHD, STROKE, CVD))

# Recode to readable
fr_ex <- fr_ex %>% mutate(FLAG = recode(FLAG,
                                         ANGINA = "ANGINA",
                                         HOSPMI = "MI",
                                         MI_FCHD = "MI/FCHD",
                                         ANYCHD = "ANG/MI/FCHD",
                                         STROKE = "STROKE",
                                         CVD = "ANYCHD/STROKE"))


# Gather data on events (Leave out DEATH)
fr_ex <- fr_ex %>% gather(EVENT, DAYS_TO_EVENT, c(TIMEAP, TIMEMI, TIMEMIFC, TIMECHD, TIMESTRK, TIMECVD, TIMEHYP))

# Recode to readable
fr_ex <- fr_ex %>% mutate(EVENT = recode(EVENT,
                                        TIMEAP = "Angina",
                                        TIMEMI = "MI",
                                        TIMEMIFC = "MI/Fatal CHD",
                                        TIMECHD = "Angina/MI/Fatal CHD",
                                        TIMESTRK = "Any Stroke",
                                        TIMECVD = "MI/Fatal CHD/Stroke (Any)",
                                        TIMEHYP = "Hypertension"))



# Code BMI groups

fr_ex <- fr_ex %>% mutate(BMIGROUP=cut(BMI, breaks=c(0, 18.5, 25, 30, 40, Inf), labels=c("Underweight", 
                                                                                         "Normal", 
                                                                                         "Overweight", 
                                                                                         "Obese", 
                                                                                         "Morbidly Obese")))

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

# Remove rows with no information (NOTE: This removed more than a million rows.)
fr_ex <- fr_ex %>% filter(HAS_DIAGNOSIS!=0 | FLAG_VAL!=0 | DAYS_TO_EVENT!=0)


# FLATTENED, CATEGORIZED DATA: 1,785,340 rows and 28 cols #
###########################################################

# SUMMARIZE

fr_sum <- fr_ex %>% group_by(RANDID) %>% summarize(T_TIME = max(TIME), M_AGE = mean(AGE), NUM_DX = mean(HAS_DIAGNOSIS), NUM_EVENTS = mean(FLAG_VAL), MEAN_DAYS = mean(DAYS_TO_EVENT), DDEATH = mean(TIMEDTH), CPD = mean(CIGPDAY))
fr_sum <- fr_sum %>% filter(T_TIME!=0) 

# Primary Summary Graph: Mean Days to Event x Number of Diagnoses
ggplot(fr_sum, aes(x = NUM_DX, y = MEAN_DAYS)) + 
  geom_point(position = "jitter", alpha = 0.3) + 
  geom_smooth(method = "lm") +
  labs(title = "Summary: Days to Event Explained by Number of Diagnoses", x = "Mean Diagnoses / Prev Conditions",  y = "Mean Days to Event")

# Summary Graph 2: Days to Death x Number of Diagnoses
ggplot(fr_sum, aes(x = NUM_DX, y = DDEATH)) + 
  geom_point(position = "jitter", alpha = 0.3) + 
  geom_smooth(method = "lm") +
  labs(title = "Summary: Days to Death Explained by Number of Diagnoses", x = "Mean Diagnoses / Prev Conditions",  y = "Days to Death")

# Summary Graph 3: Days to Death x Number of Events
ggplot(fr_sum, aes(x = NUM_EVENTS, y = DDEATH)) + 
  geom_point(position = "jitter", alpha = 0.3) + 
  geom_smooth(method = "lm") +
  labs(title = "Summary: Days to Death Explained by Number of Events", x = "Mean Events",  y = "Days to Death")

# Summary Graph 4: Days to Event x Age
ggplot(fr_sum, aes(x = M_AGE, y = MEAN_DAYS)) + 
  geom_point(position = "jitter", alpha = 0.3) + 
  geom_smooth(method = "lm") +
  labs(title = "Summary: Days to Event Explained by Age", x = "Mean Age",  y = "Days to Event")

# Summary Graph 5: Days to Death x Age
ggplot(fr_sum, aes(x = M_AGE, y = DDEATH)) + 
  geom_point(position = "jitter", alpha = 0.3) + 
  geom_smooth(method = "lm") +
  labs(title = "Summary: Days to Death Explained by Age", x = "Mean Age",  y = "Days to Death")

# Summary Graph 6: Days to Event x CPD (Smokers only)
frs <- fr_sum %>% filter(CPD>0)
ggplot(frs, aes(x = CPD, y = MEAN_DAYS)) + 
  geom_point(position = "jitter", alpha = 0.3) + 
  geom_smooth(method = "lm") +
  labs(title = "Summary: Days to Event Explained by Cigarettes per Day > 0", x = "Mean CPD",  y = "Days to Event")

# Separate into periods
fr_p1 <- fr_ex %>% filter(PERIOD == 1) %>% select(RANDID, TIME, AGE, SEXGROUP, HTNGROUP, HYPERTEN, BPMEDS, BMIGROUP, CURSMOKE, CIGPDAY, DIAGNOSIS, HAS_DIAGNOSIS, FLAG, FLAG_VAL, EVENT, DAYS_TO_EVENT, TIMEDTH) 
fr_p2 <- fr_ex %>% filter(PERIOD == 2) %>% select(RANDID, TIME, AGE, SEXGROUP, HTNGROUP, HYPERTEN, BPMEDS, BMIGROUP, CURSMOKE, CIGPDAY, DIAGNOSIS, HAS_DIAGNOSIS, FLAG, FLAG_VAL, EVENT, DAYS_TO_EVENT, TIMEDTH) 
fr_p3 <- fr_ex %>% filter(PERIOD == 3) %>% select(RANDID, TIME, AGE, SEXGROUP, HTNGROUP, HYPERTEN, BPMEDS, BMIGROUP, CURSMOKE, CIGPDAY, DIAGNOSIS, HAS_DIAGNOSIS, FLAG, FLAG_VAL, EVENT, DAYS_TO_EVENT, TIMEDTH, HDLC, LDLC) 

# Period 1: 705948, Period 2: 653063, Period 3: 685277

# Strongest Correlation: Diagnoses & Events
# Let's explore this in more detail among the PERIODs

# PERIOD 1
fr_p1 <- fr_p1 %>% filter(BPMEDS %in% c(0,1))

# No Diagnoses v. Events by Age, Sex, BMI
# Males
fr_p1z <- fr_p1 %>% filter(FLAG_VAL==1 & HAS_DIAGNOSIS==0 & SEXGROUP=="Male")
ggplot(fr_p1z, aes(x = AGE, y = log(DAYS_TO_EVENT), color = EVENT)) + 
  geom_point(position = "jitter", alpha=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "P1 Males with No Prior Diagnosis, Time to Event", x = "AGE", y = "LOG(DAYS_TO_EVENT)")

# Females
fr_p1y <- fr_p1 %>% filter(FLAG_VAL==1 & HAS_DIAGNOSIS==0 & SEXGROUP=="Female")
ggplot(fr_p1y, aes(x = AGE, y = log(DAYS_TO_EVENT), color = EVENT)) + 
  geom_point(position = "jitter", alpha=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "P1 Females with No Prior Diagnosis, Time to Event", x = "AGE", y = "LOG(DAYS_TO_EVENT)")

# DIAGNOSIS, EVENT & TIME TO DEATH
fr_p1a <- fr_p1 %>% select(FLAG, FLAG_VAL, TIMEDTH, DIAGNOSIS, HAS_DIAGNOSIS) %>% filter(HAS_DIAGNOSIS==1)
ggplot(fr_p1a, aes(x = log(TIMEDTH), y = FLAG, color = DIAGNOSIS)) + 
  geom_col() + 
  facet_grid(~FLAG_VAL) + 
  labs(title = "P1 Time to Death with a prior Diagnosis by Event (1) v. No Event(0)", y = "Event Flag", x = "LOG(Days to Death)")

# HYPERTENSION and EVENTS
# NOTE: I graphed people on BP Meds separately, because there are so few of them.
fr_p1b <- fr_p1 %>% filter(BPMEDS==0 & EVENT!="Hypertension")
ggplot(fr_p1b, aes(x = EVENT, y = log(DAYS_TO_EVENT), color=HTNGROUP)) +
  geom_col() + 
  labs(title = "P1 Time to Event by Hypertension (Unmedicated)", x="Event", y = "LOG(Days to Event)")

fr_p1b2 <- fr_p1 %>% filter(BPMEDS==1 & EVENT!="Hypertension")
ggplot(fr_p1b2, aes(x = EVENT, y = log(DAYS_TO_EVENT), color=HTNGROUP)) +
  geom_col() + 
  labs(title = "P1 Time to Event by Hypertension (Medicated)", x="Event", y = "LOG(Days to Event)")

# OTHER DIAGNOSES and EVENTS

# Angina: Days to Event by BMI, Sex
fr_p1c <- fr_p1 %>% filter(FLAG=="ANGINA" & FLAG_VAL==1 | (DIAGNOSIS=="Angina (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p1c, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P1 Persons with Angina: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

# MI:
fr_p1d <- fr_p1 %>% filter(FLAG=="MI" & FLAG_VAL==1 | (DIAGNOSIS=="MI (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p1d, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P1 Persons with MI: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

# Stroke:
fr_p1e <- fr_p1 %>% filter(FLAG=="STROKE" & FLAG_VAL==1 | (DIAGNOSIS=="Stroke (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p1e, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~CURSMOKE) +
  labs(title = "P1 Persons with Stroke: Time to an Event by BMI Group / Smoker", x = "Days to Event", y = "Event")

# Diabetes
fr_p1h <- fr_p1 %>% filter(DIAGNOSIS=="Diabetes" & HAS_DIAGNOSIS==1)
ggplot(fr_p1h, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P1 Persons with Diabetes: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

ggplot(fr_p1h, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~CURSMOKE) +
  labs(title = "P1 Persons with Diabetes: Time to an Event by BMI Group / Smoker", x = "Days to Event", y = "Event")

# Cigs per day x Days to Event (Among smokers with at least 1 event)
fr_p1f <- fr_p1 %>% filter(CIGPDAY>0 & FLAG_VAL>0)
ggplot(fr_p1f, aes(x = CIGPDAY, y = log(DAYS_TO_EVENT), color=EVENT)) + 
  geom_point(position = "jitter", alpha = 0.4) + 
  geom_smooth(method = "lm") + 
  labs(title = "P1 Days to Events x Cigarettes per Day", x = "Cigarettes per Day", y = "LOG(Days to Events)")

# Cigs per day x Age @ Diagnoses (Among smokers with at least 1 diagnosis)
fr_p1g <- fr_p1 %>% filter(CIGPDAY>0 & HAS_DIAGNOSIS>0)
ggplot(fr_p1f, aes(x = CIGPDAY, y = AGE, color=DIAGNOSIS)) + 
  geom_point(position = "jitter", alpha = 0.4) + 
  geom_smooth(method = "lm") + 
  labs(title = "P1 Age @ Diagnosis x Cigarettes per Day", x = "Cigarettes per Day", y = "Age")

#####################
# PERIOD 2
fr_p2 <- fr_p2 %>% filter(BPMEDS %in% c(0,1))

# No Diagnoses v. Events by Age, Sex, BMI
# Males
fr_p2z <- fr_p2 %>% filter(FLAG_VAL==1 & HAS_DIAGNOSIS==0 & SEXGROUP=="Male")
ggplot(fr_p2z, aes(x = AGE, y = log(DAYS_TO_EVENT), color = EVENT)) + 
  geom_point(position = "jitter", alpha=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "P2 Males with No Prior Diagnosis, Time to Event", x = "AGE", y = "LOG(DAYS_TO_EVENT)")

# Females
fr_p2y <- fr_p2 %>% filter(FLAG_VAL==1 & HAS_DIAGNOSIS==0 & SEXGROUP=="Female")
ggplot(fr_p2y, aes(x = AGE, y = log(DAYS_TO_EVENT), color = EVENT)) + 
  geom_point(position = "jitter", alpha=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "P2 Females with No Prior Diagnosis, Time to Event", x = "AGE", y = "LOG(DAYS_TO_EVENT)")

# DIAGNOSIS, EVENT & TIME TO DEATH
fr_p2a <- fr_p2 %>% select(FLAG, FLAG_VAL, TIMEDTH, DIAGNOSIS, HAS_DIAGNOSIS) %>% filter(HAS_DIAGNOSIS==1)
ggplot(fr_p2a, aes(x = log(TIMEDTH), y = FLAG, color = DIAGNOSIS)) + 
  geom_col() + 
  facet_grid(~FLAG_VAL) + 
  labs(title = "P2 Time to Death with a prior Diagnosis by Event (1) v. No Event(0)", y = "Event Flag", x = "LOG(Days to Death)")

# HYPERTENSION and EVENTS
# NOTE: I graphed people on BP Meds separately, because there are so few of them.
fr_p2b <- fr_p2 %>% filter(BPMEDS==0 & EVENT!="Hypertension")
ggplot(fr_p2b, aes(x = EVENT, y = log(DAYS_TO_EVENT), color=HTNGROUP)) +
  geom_col() + 
  labs(title = "P2 Time to Event by Hypertension (Unmedicated)", x="Event", y = "LOG(Days to Event)")

fr_p2b2 <- fr_p2 %>% filter(BPMEDS==1 & EVENT!="Hypertension")
ggplot(fr_p2b2, aes(x = EVENT, y = log(DAYS_TO_EVENT), color=HTNGROUP)) +
  geom_col() + 
  labs(title = "P2 Time to Event by Hypertension (Medicated)", x="Event", y = "LOG(Days to Event)")

# OTHER DIAGNOSES and EVENTS

# Angina: Days to Event by BMI, Sex
fr_p2c <- fr_p2 %>% filter(FLAG=="ANGINA" & FLAG_VAL==1 | (DIAGNOSIS=="Angina (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p2c, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P2 Persons with Angina: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

# MI:
fr_p2d <- fr_p2 %>% filter(FLAG=="MI" & FLAG_VAL==1 | (DIAGNOSIS=="MI (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p2d, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P2 Persons with MI: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

# Stroke:
fr_p2e <- fr_p2 %>% filter(FLAG=="STROKE" & FLAG_VAL==1 | (DIAGNOSIS=="Stroke (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p2e, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~CURSMOKE) +
  labs(title = "P2 Persons with Stroke: Time to an Event by BMI Group / Smoker", x = "Days to Event", y = "Event")

# Diabetes
fr_p2h <- fr_p2 %>% filter(DIAGNOSIS=="Diabetes" & HAS_DIAGNOSIS==1)
ggplot(fr_p2h, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P2 Persons with Diabetes: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

ggplot(fr_p2h, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~CURSMOKE) +
  labs(title = "P2 Persons with Diabetes: Time to an Event by BMI Group / Smoker", x = "Days to Event", y = "Event")

# Cigs per day x Days to Event (Among smokers with at least 1 event)
fr_p2f <- fr_p2 %>% filter(CIGPDAY>0 & FLAG_VAL>0)
ggplot(fr_p2f, aes(x = CIGPDAY, y = log(DAYS_TO_EVENT), color=EVENT)) + 
  geom_point(position = "jitter", alpha = 0.4) + 
  geom_smooth(method = "lm") + 
  labs(title = "P2 Days to Events x Cigarettes per Day", x = "Cigarettes per Day", y = "LOG(Days to Events)")

# Cigs per day x Age @ Diagnoses (Among smokers with at least 1 diagnosis)
fr_p2g <- fr_p2 %>% filter(CIGPDAY>0 & HAS_DIAGNOSIS>0)
ggplot(fr_p2f, aes(x = CIGPDAY, y = AGE, color=DIAGNOSIS)) + 
  geom_point(position = "jitter", alpha = 0.4) + 
  geom_smooth(method = "lm") + 
  labs(title = "P2 Age @ Diagnosis x Cigarettes per Day", x = "Cigarettes per Day", y = "Age")


#####################
# PERIOD 3
fr_p3 <- fr_p3 %>% filter(BPMEDS %in% c(0,1))

# No Diagnoses v. Events by Age, Sex, BMI
# Males
fr_p3z <- fr_p3 %>% filter(FLAG_VAL==1 & HAS_DIAGNOSIS==0 & SEXGROUP=="Male")
ggplot(fr_p3z, aes(x = AGE, y = log(DAYS_TO_EVENT), color = EVENT)) + 
  geom_point(position = "jitter", alpha=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "P3 Males with No Prior Diagnosis, Time to Event", x = "AGE", y = "LOG(DAYS_TO_EVENT)")

# Females
fr_p3y <- fr_p3 %>% filter(FLAG_VAL==1 & HAS_DIAGNOSIS==0 & SEXGROUP=="Female")
ggplot(fr_p3y, aes(x = AGE, y = log(DAYS_TO_EVENT), color = EVENT)) + 
  geom_point(position = "jitter", alpha=0.5) +
  geom_smooth(method = "lm") +
  labs(title = "P3 Females with No Prior Diagnosis, Time to Event", x = "AGE", y = "LOG(DAYS_TO_EVENT)")

# DIAGNOSIS, EVENT & TIME TO DEATH
fr_p3a <- fr_p3 %>% select(FLAG, FLAG_VAL, TIMEDTH, DIAGNOSIS, HAS_DIAGNOSIS) %>% filter(HAS_DIAGNOSIS==1)
ggplot(fr_p3a, aes(x = log(TIMEDTH), y = FLAG, color = DIAGNOSIS)) + 
  geom_col() + 
  facet_grid(~FLAG_VAL) + 
  labs(title = "P3 Time to Death with a prior Diagnosis by Event (1) v. No Event(0)", y = "Event Flag", x = "LOG(Days to Death)")

# HYPERTENSION and EVENTS
# NOTE: I graphed people on BP Meds separately, because there are so few of them.
fr_p3b <- fr_p3 %>% filter(BPMEDS==0 & EVENT!="Hypertension")
ggplot(fr_p3b, aes(x = EVENT, y = log(DAYS_TO_EVENT), color=HTNGROUP)) +
  geom_col() + 
  labs(title = "P3 Time to Event by Hypertension (Unmedicated)", x="Event", y = "LOG(Days to Event)")

fr_p3b2 <- fr_p3 %>% filter(BPMEDS==1 & EVENT!="Hypertension")
ggplot(fr_p3b2, aes(x = EVENT, y = log(DAYS_TO_EVENT), color=HTNGROUP)) +
  geom_col() + 
  labs(title = "P3 Time to Event by Hypertension (Medicated)", x="Event", y = "LOG(Days to Event)")

# OTHER DIAGNOSES and EVENTS

# Angina: Days to Event by BMI, Sex
fr_p3c <- fr_p3 %>% filter(FLAG=="ANGINA" & FLAG_VAL==1 | (DIAGNOSIS=="Angina (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p3c, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P3 Persons with Angina: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

# MI:
fr_p3d <- fr_p3 %>% filter(FLAG=="MI" & FLAG_VAL==1 | (DIAGNOSIS=="MI (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p3d, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P3 Persons with MI: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

# Stroke:
fr_p3e <- fr_p3 %>% filter(FLAG=="STROKE" & FLAG_VAL==1 | (DIAGNOSIS=="Stroke (Diagnosis)" & HAS_DIAGNOSIS==1)) 
ggplot(fr_p3e, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~CURSMOKE) +
  labs(title = "P3 Persons with Stroke: Time to an Event by BMI Group / Smoker", x = "Days to Event", y = "Event")

# Diabetes
fr_p3h <- fr_p3 %>% filter(DIAGNOSIS=="Diabetes" & HAS_DIAGNOSIS==1)
ggplot(fr_p3h, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~SEXGROUP) +
  labs(title = "P3 Persons with Diabetes: Time to an Event by BMI Group / Sex", x = "Days to Event", y = "Event")

ggplot(fr_p3h, aes(x = DAYS_TO_EVENT, y = EVENT, color = BMIGROUP)) + 
  geom_col() +
  facet_grid(~CURSMOKE) +
  labs(title = "P3 Persons with Diabetes: Time to an Event by BMI Group / Smoker", x = "Days to Event", y = "Event")

# Cigs per day x Days to Event (Among smokers with at least 1 event)
fr_p3f <- fr_p3 %>% filter(CIGPDAY>0 & FLAG_VAL>0)
ggplot(fr_p3f, aes(x = CIGPDAY, y = log(DAYS_TO_EVENT), color=EVENT)) + 
  geom_point(position = "jitter", alpha = 0.4) + 
  geom_smooth(method = "lm") + 
  labs(title = "P3 Days to Events x Cigarettes per Day", x = "Cigarettes per Day", y = "LOG(Days to Events)")

# Cigs per day x Age @ Diagnoses (Among smokers with at least 1 diagnosis)
fr_p3g <- fr_p3 %>% filter(CIGPDAY>0 & HAS_DIAGNOSIS>0)
ggplot(fr_p3f, aes(x = CIGPDAY, y = AGE, color=DIAGNOSIS)) + 
  geom_point(position = "jitter", alpha = 0.4) + 
  geom_smooth(method = "lm") + 
  labs(title = "P3 Age @ Diagnosis x Cigarettes per Day", x = "Cigarettes per Day", y = "Age")

###########
# Phase 2 - Plotting on the insights

