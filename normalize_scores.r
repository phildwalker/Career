# Normalize scores by group -----------
library(rcompanion)
TND_DPT <- SummaryRC %>%  filter(!is.na(DEPTID) & !is.na(SumRC)) %>% select(DEPTID) %>% unique() #& !DEPTID %in% c("Strategic Support Program")
TND_DPT <- unique(TND_DPT$DEPTID)

for (dpt in TND_DPT){
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    
    print(paste0("Dataset set existed, now using: ", dpt))
    temp_dataset <- SummaryRC %>% filter(DEPTID %in% dpt ) #& Type == "Non-Exempt"
    temp_dataset$TUK_RC <- transformTukey(temp_dataset$SumRC, plotit = FALSE)
    temp_dataset <- temp_dataset %>% mutate(TukRCHund= (TUK_RC - min(TUK_RC, na.rm = TRUE)) * (100/(max(TUK_RC, na.rm = TRUE)-min(TUK_RC, na.rm = TRUE)))) %>% ungroup()
    
    dataset <- rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    
    print(paste0("Dataset set didn't exist starting with: ", dpt))
    dataset <- SummaryRC %>% filter(DEPTID %in% dpt) # & Type == "Non-Exempt"
    dataset$TUK_RC <- transformTukey(dataset$SumRC, plotit = FALSE)
    dataset <- dataset %>% mutate(TukRCHund= (TUK_RC - min(TUK_RC, na.rm = TRUE)) * (100/(max(TUK_RC, na.rm = TRUE)-min(TUK_RC, na.rm = TRUE)))) %>% ungroup()
  }
}
