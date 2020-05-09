library(ggplot2)

ggplot(frm_sel_cvd, aes(x = SYSBP, y = AGE)) + 
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ BPMEDS) + 
  stat_smooth(method = "lm", se=F, col="red") 

ggplot(frm_sel_nocvd, aes(x = SYSBP, y = AGE)) + 
  geom_jitter(alpha = 0.6) +
  facet_grid(. ~ BPMEDS) + 
  stat_smooth(method = "lm", se=F, col="blue") 
