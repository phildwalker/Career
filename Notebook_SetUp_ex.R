---
title: "NBD Queue/Txn Comparison"
output:
  html_document:
    df_print: paged
    code_folding: hide
    toc: true
    toc_float: true
date: "`r format(Sys.time(), '%d %B, %Y')`"
---

```{css, echo=FALSE}
body .main-container {
  max-width: 1280px !important;
  width: 1280px !important;
    }
body {
  max-width: 1280px !important;
    }
```


```{r read in data, results='asis', echo=FALSE, include=FALSE}
library(tidyverse)
library(readxl)
library(here)
library(tidylog)
library(ggthemes)
library(viridis)

```

## Summary of results  

For budget, we wanted to determine if certain txn_types for NB/NBD items should be excluded, or if the grouping for forecasting would at least be performed on the skill level.  

Based on the results below, it __does__ __not__ seem that ".._archive" is a duplicate of "nbdesktop_wfc" and we would recommend capturing both transactions when gathering the effort for NBD items for a forecast.   


## Data import and clean
Looking to see if there is a pattern for what sets apart the txn types that are labeled as "_archive" vs those that don't.  
Build the data from pulling "jPOD_Total" from the OA dataset (sql example below).  

```{sql, eval=FALSE}
select queue_nm, txn_type, year(date) as year, effort
from fcdata.jpod_total
where (upcase(queue_nm) contains ('NBD-') or upcase(queue_nm) contains ('NB-'))
```

Now reading in and summarizing the datasets (one is a summary by queue/txn, another split out by year)

```{r}

NBD_mix <- NBDyear %>% filter(PercentGroup < .95 & !is.na(PercentGroup) & year >=2013) %>% select(QUEUE_NM) %>% unique() %>% pull(QUEUE_NM)


```