# Multiple table joins ----------

MBScores <- list(MB2019Asoc, MB2019Mgr, MB2019Support) %>%
  Reduce(function(dtf1,dtf2) merge(dtf1,dtf2, all=TRUE), .) %>%
  ungroup() %>% 
  right_join(.,hier)
  
 #---------------------
 MB2019Mgr <- list(hier, rev, Skill, PSD) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="EMP_ID"), .) %>%
  filter(MeritRole %in% c("1_Processing Manager")) %>% 
  ungroup() %>% 
  mutate(Merit2019 = RevPts*0.5 + AvgPSD * 0.1 + ProcKscale*0.4 ) %>%
  ungroup()
