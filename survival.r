      # Survival Analysis - Framingham Data 6/16/2020 Kyle P. Rasku RN
        
        library(dplyr)
        library(ggplot2)
        library(survival)
  
        
        # USE frmgham data for period 1, at risk individuals only
        
        fr_ex <- frmgham %>% 
          filter(PERIOD==1 & PREVHYP==0)
        
        # RECODE #
        ##########
        
        # Dichotomous Alternative: CLINHTN  
        fr_ex <- fr_ex %>% mutate(CLINHTN=cut(DIABP, breaks=c(-Inf,90,Inf), labels = c(0, 1)))
        fr_ex <- fr_ex %>% filter(DIABP < 90) %>% mutate(CLINHTN=cut(SYSBP, breaks=c(-Inf,121,Inf), labels = c(0, 1)))
        
        # Recode SEX
        fr_ex <- fr_ex %>% mutate(SEXGROUP=cut(SEX, breaks=c(-Inf, 1, 2), labels=c("Male", "Female")))
        
        # Dichotomous Age
        fr_ex <- fr_ex %>% mutate(AGEGROUP=cut(AGE, breaks=c(-Inf, 45, Inf), labels=c("<45", ">45")))
        
        # Dichotomous BMI
        fr_ex <- fr_ex %>% mutate(BMI=ifelse(BMI==".", 0, as.numeric(BMI)))
        fr_ex <- fr_ex %>% mutate(BMIGROUP=ifelse(BMI < 30.1, "Normal", "Obese"))
        
        # Add CENSORED flag for survival analysis
        # CENSORED INDIVIDUALS ARE: Not Dead, Have No Flags
        fr_ex <- fr_ex %>% mutate(CENSORED=ifelse(DEATH==0 & ANGINA==0 & HOSPMI==0 & MI_FCHD==0 & ANYCHD==0 & STROKE==0 & CVD==0 & HYPERTEN==0, 1, 0))
        
        #create Survival object
        fr_ex <- fr_ex %>% mutate(TIMEHYP=as.numeric(TIMEHYP))
        survivalObj <- Surv(time = fr_ex$TIMEHYP, event = fr_ex$CENSORED)
        
        # Fit a Cox proportional hazards model
        fit.coxph <- coxph(survivalObj ~ SEXGROUP + AGEGROUP + CLINHTN + BMIGROUP + DIABETES + CURSMOKE, 
                             data = fr_ex)
        ggforest(fit.coxph, data = fr_ex)
          
