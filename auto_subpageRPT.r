# Overview.rmd -----------------
---
title: "PSP Analysis"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: scroll
---
# Overview

`r shiny::tags$h1("Overview", style="text-align:center")`

```{r setup, include=FALSE}
library(tidyverse)
library(stringr)
library(here)
library(readxl)
library(bit64)
library(data.table)
library(ggthemes)
library(viridis)

startDt <- as.Date(c("2019-03-01"))
EndDt <- as.Date(c("2019-03-31"))

map <- read_excel(here("DSCX_DPT_MAP.xlsx"), sheet = "Sheet1") %>% mutate(DESCRIPTION_X = trimws(toupper(DESCRIPTION_X))) %>% filter(!DEPT %in% c("DCC", "NA", "SYSTEM", "UNKNOWN"))
BRDGmap <- read_excel(here("DSCX_DPT_MAP.xlsx"), sheet = "BrdgDPT")
INmap <- read_excel(here("DSCX_DPT_MAP.xlsx"), sheet = "INOUTdpt") %>% mutate(MgrDept = trimws(toupper(as.character(MgrDept)))) 

tblAssociate <- read_excel("S:/TSR_CSR/SF Management/OPS_PSD_LINKS/tblAssociate.xlsx", sheet = "tblAssociate", range = cell_cols("A:D")) %>% 
  select(EMP_ID, LOGIN_ID, EMPL_NAME) %>% 
  rename("Employ ID"=EMP_ID,LOGIN_NM=LOGIN_ID)

PSPmap <- read_excel("S:/TSR_CSR/OPS_PSD/PSD_ADMIN_DATA/PSP Monthly.xlsx", sheet = "qryPSP_Active_Assoc_for_monthly") %>% 
  left_join(.,tblAssociate) %>% 
  mutate(LOGIN_NM = trimws(toupper(LOGIN_NM)))

bench_id <- PSPmap %>% rename(ST='Service Tier') %>% filter(ST %like% 'Bench') %>% pull(LOGIN_NM)
psp_id <- PSPmap %>% rename(ST='Service Tier') %>% filter(!LOGIN_NM %in% bench_id) %>% pull(LOGIN_NM)


setwd("S:/Relationship Management/Command Center/GGN Reports/IN_OUT Weekly Analysis/INOUT_Creation")
finf <- file.info(list.files(getwd(),pattern ="INOUT_"), extra_cols = FALSE)
finf <- finf %>% mutate(date = as.Date(mtime, tz = "EST"),
                        filename = row.names(finf),
                        fileType = str_sub(filename, -3, -1))

file_list <- finf %>%  group_by(date) %>% 
  filter(fileType == "csv" & (date >= startDt & date <= EndDt)) %>% ungroup() %>% as.data.frame(.) %>% pull(filename)

for (file in file_list){
  Filepath <- paste0("S:/Relationship Management/Command Center/GGN Reports/IN_OUT Weekly Analysis/INOUT_Creation/",file)

  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    print(file)
    temp_dataset <-as.data.frame(fread(Filepath,  sep = ","))
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    print(file)
    dataset <- as.data.frame(fread(Filepath,  sep = ",")) 
  }
}

dataset <- unique(dataset)

dataset <- dataset %>%
  mutate(LOGIN_NM = trimws(toupper(LOGIN_NM))) %>%
  mutate(AssocDPT = case_when(
    MgrDept %in% c("Broker Management") ~ "BRM",
    MgrDept %in% c("Claims") ~ "CLM",
    MgrDept %in% c("Data Management - Ops") ~ "DM",
    MgrDept %in% c("New Business - Lansing") ~ "NB",
    MgrDept %in% c("Policy Owner Services-Lansing") ~ "POS",
    MgrDept %in% c("Remittance Processing-Ops") ~ "RP",
    MgrDept %in% c("Call Center") ~ "SC",
    MgrDept %in% c("Staffing - Operations Support") ~ "SSP",
    TRUE ~ "OTHER"
  )) %>%
  mutate(item_tier = as.factor(case_when(
    serv_tier %in% c("Star", "SuperStar", "Premier I", "Premier II") ~ "PSP",
    TRUE ~ "GENPOP"
  ))) %>%
  mutate(assoc_tier = as.factor(case_when(
    LOGIN_NM %in% psp_id ~ "PSP",
    LOGIN_NM %in% bench_id ~ "Bench",
    TRUE ~ "GENPOP"
  )))

INOUT <- dataset %>%
  mutate(DESCRIPTION_X = trimws(toupper(DESCRIPTION_X)), MgrDept = trimws(toupper(as.character(MgrDept)))) %>%
  left_join(., map, by = c("DESCRIPTION_X")) %>%
  select(DESCRIPTION_X, DATE, item_tier, assoc_tier, Effort_Count, EVENT_IN, EVENT_OUT, MinDiff, AssocDPT, DEPT) %>%
  left_join(., BRDGmap, by = c("DEPT")) %>%
  select(-DEPT) %>%
  ungroup() %>%
  filter(!AssocDPT %in% c("OTHER")) %>%
  ungroup() %>% 
  mutate(date = as.Date(DATE, format="%d%b%Y")) %>% 
  mutate(DOW = weekdays(date))


dept_list <- unique(INOUT$SkillDPT)
skill_list <- unique(INOUT$DESCRIPTION_X)

skill_inout <- INOUT %>% filter(DESCRIPTION_X == 'BM APPOINTMENTS') %>% filter(EVENT_OUT == 'StatusComplete')





```



Row
-----------------------------------------------------------------------

### Overall count of items split by item tier

```{r}
INOUT %>% 
  group_by(date, item_tier) %>% 
  summarise(TotalEffort = n()) %>% #sum(Effort_Count, na.rm = TRUE)
  ungroup() %>% mutate(item_tier = as.character(item_tier)) %>% 
  ungroup() %>% 
  ggplot(aes(x=date, y=TotalEffort)) +
    geom_line(aes(color=item_tier))+
    scale_color_viridis(discrete = TRUE)+
    theme_bw() +
    ylab("Effort Count") +
    xlab("Date")
```

### Bar chart of Department Volumns

```{r}
INOUT %>% 
  group_by(SkillDPT) %>% 
  summarise(TotalEffort = n()) %>%
  ungroup() %>% 
  ggplot(aes(x=SkillDPT, y=TotalEffort)) +
    geom_bar(stat="identity")+
    scale_color_viridis(discrete = TRUE)+
    theme_bw()
```

Row
-----------------------------------------------------------------------

### Department and Item Tier

```{r fig.width=15, fig.height=5}
skill_inout %>% group_by(AssocDPT, item_tier) %>% summarise(TotlAmt = n()) %>% ungroup() %>% 
  group_by(item_tier) %>% spread(AssocDPT, value=TotlAmt) %>% ungroup() %>% 
  knitr::kable()

# INOUT %>%
#   group_by(date, item_tier) %>%
#   summarise(TotalEffort = sum(Effort_Count)) %>%
#   ungroup() %>% group_by(date) %>%  spread(item_tier, value=TotalEffort) %>%
#   knitr::kable()

```

### Associate Tier and Item Tier

```{r fig.width=15, fig.height=5}
skill_inout %>% group_by(assoc_tier, item_tier) %>% summarise(TotlAmt = n()) %>% ungroup() %>% 
  group_by(item_tier) %>% spread(assoc_tier, value=TotlAmt) %>% ungroup() %>% 
  knitr::kable()

```

```{r render subpages, include=FALSE}
# Get all unique product groups for the subpages
SkillDptList <- INOUT %>% filter(!is.na(SkillDPT)) %>% select(SkillDPT) %>% unique() %>% pull(SkillDPT)

# Create variable which stores all subpages outputs
out = NULL

# Set knitr options to allow duplicate labels (needed for the subpages)
options(knitr.duplicate.label = 'allow')

# Create temporary environment which we use for knitting subpages.RMD 
subpage_env <- new.env()

for (Dpt in SkillDptList) {
  # Filter data for product group 
  subpage_data <- INOUT %>% 
    filter(SkillDPT == Dpt)
  
  # Assign filtered data and product group to subpage_env 
  assign("subpage_data", subpage_data, subpage_env)
  assign("SkillDept", Dpt, subpage_env)
  
  # Knit PSP_subpage.rmd using the subpage_env and add result to out vector
  out = c(out, knitr::knit_child('PSP_subpage.rmd', envir = subpage_env))
}

# out

```

`r paste(knitr::knit_child(text = out), collapse = '')`



#------------------------------------
# subpage.rmd -----------------
`r paste0(SkillDept,' ', '{data-navmenu="Department"}', '\n', '=====================================')`

`r shiny::tags$h1(SkillDept, style="text-align:center")`

Row
-----------------------------------------------------------------------

### Trend of Department by Item Tier

```{r}
subpage_data %>% 
  group_by(date,DOW, item_tier) %>% 
  summarise(Hours = sum(MinDiff, na.rm = TRUE)/60) %>% 
  ggplot(aes(x=date, y=Hours, color=item_tier)) +
  geom_line()+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+
  facet_grid(DOW~.)
```


### Trend of Department by Associate Tier

```{r}
# unique(subpage_data$DOW)

subpage_data %>%  
  group_by(date,DOW, assoc_tier) %>% 
  summarise(Hours = sum(MinDiff, na.rm = TRUE)/60) %>% 
  ggplot(aes(x=date, y=Hours, color=assoc_tier)) +
  geom_line()+
  scale_color_viridis(discrete = TRUE)+
  theme_bw()+
  facet_grid(DOW~.)
```












