
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(broom)

# USE frmgham data where each person has 3 entries / periods (9618 rows / 3 = 3206 individuals)

fr_e <- frmgham %>% 
  group_by(RANDID) %>% 
  filter(n()==3)