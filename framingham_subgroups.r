  # Create frm_sel (I chose to examine PERIOD 2, to make sure I had 1 row per     
  # individual, based on criteria)

  # Also, excluded '.' values from BPMEDS column
  
  frm_sel <- frmgham %>% 
    select(RANDID, PERIOD, AGE, SEX, CVD, SYSBP, DIABP, BMI, DIABETES, HYPERTEN, CURSMOKE, BPMEDS) %>% 
      filter(SEX==1, DIABETES==0, HYPERTEN==1, CURSMOKE==0, PERIOD==2)
  
  # Subset patients with CVD vs. NO CVD
  frm_sel_cvd <- frm_sel %>% filter(CVD==1, BPMEDS!=".")
  frm_sel_nocvd <- frm_sel %>% filter(CVD==0, BPMEDS!=".")
  
  frm_sel_cvd_bpmeds <- frm_sel_cvd %>% filter(BPMEDS==1)
  frm_sel_cvd_nobpmeds <- frm_sel_cvd %>% filter(BPMEDS==0)
  frm_sel_nocvd_bpmeds <- frm_sel_nocvd %>% filter(BPMEDS==1)
  frm_sel_nocvd_nobpmeds <- frm_sel_nocvd %>% filter(BPMEDS==0)
