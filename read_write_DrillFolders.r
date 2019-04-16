
# Write to sharepoint location drilling into multiple folders


CClist <- unique(AssocTeam$COST_CENTER_DESCRIPTION)

# https://sharepoint.jackson.com/sites/Opsanalytics/MB2019/Shared Documents/AssociateSkilling
setwd("//sharepoint.jackson.com/sites/Opsanalytics/MB2019/Shared Documents/AssociateSkilling")

for (CC in CClist){
  CCname <- paste0(getwd(),"/",CC)
  print(CCname)
  dir.create(file.path(CCname), showWarnings = FALSE) #here("Team_Notes","Build_Assoc_Skill_Review",CC))
  datFilt <- AssocTeam %>% filter(COST_CENTER_DESCRIPTION %in% CC)
  MgrList <- unique(datFilt$SUPERVISOR_NAME)
  
  for (Mgr in MgrList){
    datMgr <- datFilt %>% filter(SUPERVISOR_NAME %in% Mgr)
    datSpl <- split(datMgr, datMgr$LoginID)
    
    MgrLab <- unique(datMgr$MgrLast)
    FlName <- paste0(MgrLab, ".xlsx")
    
    ## Create a blank workbook
    wb <- createWorkbook()
    
    ## Loop through the list of split tables as well as their names
    ##   and add each one as a sheet to the workbook
    Map(function(data, name){
      addWorksheet(wb, name)
      writeData(wb, name, data)
      
    }, datSpl, names(datSpl))
  
    ## Save workbook to working directory
    file_CCname <- paste0(CCname,"/",FlName)
    saveWorkbook(wb, file = file_CCname, overwrite = TRUE) #"Team_Notes","Build_Assoc_Skill_Review",here( CC,FlName)
    
  }

}


#--------------------------------------------------
# Read in the multiple sheets data
#--------------------------------------------------

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X, col_types = "text"))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

AddtSkills <- c("KMT", "MENTORING", "SME", "TRAINING - TRAINING MATERIAL REVIEW", "TESTING - PROJECTS", "PSP - BENCH","PSP - BENCH", "MENTOR", 
                "POS KMT", "WELCOME AMBASSADOR", "TRAINING", "MANUAL - CRD POST IMPLEMENTATION")

# List of the associates who have skilling looked at------------------------
# read in base assoc list ----------------
hier <- read.csv(here("2_DBdata", "0BASE_AssocOverview.csv"), stringsAsFactors = FALSE) %>% 
  select(EMP_ID, LOGIN_ID, FULL_NAME, DEPTID, MeritRole) %>%
  mutate(EMP_ID = as.numeric(EMP_ID)) %>% 
  filter(MeritRole %in% c("4_Processing Associate", "5_SC Processing Associate")) %>%
  ungroup()


# Pull in list for Manager review------------
setwd("C:/Users/hvm0116/Desktop/MB2019/Tool_Demo/Data/1_InputData/_ToCompile")
finf <- file.info(list.files(getwd(),pattern =".xlsx"), extra_cols = FALSE)
finf$date <- as.Date(finf$mtime, tz = "EST")
finf$filename <- row.names(finf)
finf$fileType <- str_sub(finf$filename, -4, -1)
fileLoop <- unique(finf$filename)

for (file in fileLoop){
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    print(file)
    tabbed <- read_excel_allsheets(here("1_InputData","_ToCompile", file)) 
    temp_dataset <- bind_rows(tabbed) %>% select(1:11)
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    print(file)
    tabbed <- read_excel_allsheets(here("1_InputData","_ToCompile", file)) 
    dataset <- bind_rows(tabbed)
  }
}

AllSkills <- unique(dataset)
rm(dataset, tabbed)
AllSkills$EMP_ID <- as.numeric(AllSkills$EMP_ID)
AllSkills$SkillName <- trimws(toupper(AllSkills$SkillName))

AllSkillsAssoc <- AllSkills %>% filter(!is.na(Skilled) & !is.na(SkillName) & !SkillName %in% AddtSkills) %>% select(EMP_ID, SkillName, Inventory) %>% #EMP_ID %in% AssocLst & 
  mutate(MgrRev_EMP = EMP_ID) %>% 
  mutate(SkillName = case_when(Inventory == "NB" & SkillName == "DUPLICATES" ~ "NB ANNUITY NONDESKTOP - DUPLICATE POLICY",
                               Inventory == "RP" & SkillName == "MISMATCH" ~ "MISMATCH_RP",
                               TRUE ~SkillName))