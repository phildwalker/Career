---
title: "Customer Care Forecasted Volume"
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

    
```{r package loading, results='asis', echo=FALSE, include=FALSE}
library(tidyverse)
library(lubridate)
library(here)
library(readxl)
library(ggthemes)
library(viridis)
library(bizdays)
library(forecast)
library(tidyquant)
library(timetk)
library(sweep)

JACKHOL <- as.Date(c(
  "2013-01-01", "2013-01-21", "2013-02-18", "2013-05-18", "2013-07-04", "2013-09-02", "2013-11-28", "2013-12-25", "2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", "2014-07-04", "2014-09-01",
  "2014-11-27", "2014-12-25", "2015-01-01", "2015-01-19", "2015-02-16", "2015-04-03", "2015-05-25", "2015-07-03", "2015-09-07", "2015-11-26", "2015-12-25", "2016-01-01", "2016-01-18", "2016-02-15",
  "2016-03-25", "2016-05-30", "2016-07-04", "2016-09-05", "2016-11-24", "2016-12-26", "2017-01-02", "2017-01-16", "2017-02-20", "2017-04-14", "2017-05-29", "2017-07-04", "2017-09-04", "2017-11-23",
  "2017-12-25", "2018-01-01", "2018-01-15", "2018-02-19", "2018-02-28", "2018-03-30", "2018-07-04", "2018-09-03", "2018-11-22", "2018-12-25", "2019-01-01", "2019-01-21", "2019-02-18", "2019-04-19",
  "2019-05-27", "2019-07-04", "2019-09-02", "2019-11-28", "2019-12-25", "2020-01-01", "2020-01-20", "2020-02-17", "2020-04-10", "2020-05-25", "2020-07-03", "2020-09-07", "2020-11-26", "2020-12-25",
  "2021-01-01", "2021-01-18", "2021-02-15", "2021-04-02", "2021-05-31", "2021-07-05", "2021-09-06", "2021-11-25", "2021-12-24", "2021-12-31", "2022-01-17", "2022-02-21", "2022-04-15", "2022-05-30",
  "2022-07-04", "2022-09-05", "2022-11-24", "2022-12-26", "2023-01-02", "2023-01-16", "2023-02-20", "2023-04-07", "2023-05-29", "2023-07-04", "2023-09-04", "2023-11-23", "2023-12-25"
))
cal <- create.calendar(weekdays = c("saturday", "sunday"), holidays = JACKHOL, name = "Actual")

Today <- Sys.Date()
LastBusDay <- add.bizdays(Today, -1, cal)
```

## Build out the dataset  
Dataset is received from DM. The focus of the initial forecasting effort will be on the customer care data elements.  
From conversation, this data has been gathered manually and is tracked by management.

```{r}
setwd("C:/Users/hvm0116/Desktop/CustomerCare_Forecasting")

CCdata <- read_excel(("CC_Data.xlsx"), sheet = "Sheet1") %>%
  rename(
    CCA_Total = `CCA Total`, SSA_Proc = `SSA processed`, SSA_Per = `SSA %`,
    DM_Proc = `DM Processed`, DM_Per = `DM %`, DM_Special_Handl = `DM Special handling`, CC_Msg_Sorted = `CC Messages Sorted`
  ) %>%
  mutate(Date = as.Date(Date)) %>%
  ungroup() %>%
  mutate(DM_Special_Handl = as.numeric(DM_Special_Handl)) %>%
  ungroup() %>%
  gather(key = "Type", value = "amount", 2:8)


# unique(CCdata$Type)
# [1] "CCA_Total"        "SSA_Proc"         "SSA_Per"          "DM_Proc"          "DM_Per"           "DM_Special_Handl" "CC_Msg_Sorted"

PerGroups <- c("SSA_Per", "DM_Per")
VolGroups <- c("CCA_Total", "SSA_Proc", "DM_Proc", "DM_Special_Handl", "CC_Msg_Sorted")


Ttl <- read_excel(("CC_Data.xlsx"), sheet = "Historical") %>% select(Date, `CC Sorted`) %>% rename(TotalCC=`CC Sorted`)



```
## Visual of Customer Care work 
Looking at the two different groups of data (items expressed as percentages and items expressed as amounts).  

```{r ploting of data expressed as percentiles}

ggplot(data = filter(CCdata, Type %in% PerGroups), aes(x = Date, y = amount, color = Type)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  scale_color_viridis(discrete = TRUE)

ggplot(data = filter(CCdata, Type %in% VolGroups), aes(x = Date, y = amount, color = Type)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  scale_color_viridis(discrete = TRUE)



ggplot(data = Ttl, aes(x = Date, y = TotalCC)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  scale_color_viridis(discrete = TRUE)

```

## Testing for any structural changes/ change points for the historical data.

```{r}
CurWk <- floor_date(Sys.Date(), unit= "week")


Ttl.wk <- Ttl %>% mutate(WOY = floor_date(Date, unit= "week")) %>% group_by(WOY) %>% 
  summarise(ttlAmt = sum(TotalCC), AmtDays = n()) %>%
  filter(WOY < CurWk) %>% 
  select(-AmtDays)
totAmt <- tk_ts(Ttl.wk, start = 2019, freq = 52, silent = TRUE)


# fit_ets <- totAmt %>%
#     tbats()


library(changepoint)
library(autoplotly)
autoplotly(cpt.meanvar(totAmt))


```


## Building out a forecast of the total amount of items

The forecasting workflow involves a few basic steps:

Step 1: Coerce to a ts object class.  
Step 2: Apply a model (or set of models)  
Step 3: Forecast the models (similar to predict)  
Step 4: Use sw_sweep() to tidy the forecast.  
Note that we purposely omit other steps such as testing the series for stationarity (Box.test(type = "Ljung")) and analysis of autocorrelations (Acf, Pacf) for brevity purposes. We recommend the analyst to follow the forecasting workflow in “Forecasting: principles and practice”  



```{r total vol forecast}

CurWk <- floor_date(Sys.Date(), unit= "week")


Ttl.wk <- Ttl %>% mutate(WOY = floor_date(Date, unit= "week")) %>% group_by(WOY) %>% 
  summarise(ttlAmt = sum(TotalCC), AmtDays = n()) %>%
  filter(WOY < CurWk) %>% 
  select(-AmtDays)
totAmt <- tk_ts(Ttl.wk, start = 2019, freq = 52, silent = TRUE)
# totAmt
# has_timetk_idx(totAmt)

fit_ets <- totAmt %>%
    tbats()


sw_glance(fit_ets)





```

## Review of the residuals from the forecast

```{r}
augment_fit_ets <- sw_augment(fit_ets)

augment_fit_ets %>%
    ggplot(aes(x = index, y = .resid)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_point(color = palette_light()[[1]], alpha = 0.5) +
    geom_smooth(method = "loess") +
    scale_x_yearmon(n = 10) +
    labs(title = "Customer Care Forecasted Work: TBATS Residuals", x = "") +
    theme_tq()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

```

## Perform a decomp of the forecast

```{r}
decomp_fit_ets <- sw_tidy_decomp(fit_ets)
decomp_fit_ets 

decomp_fit_ets %>%
    gather(key = key, value = value, -index) %>%
    mutate(key = forcats::as_factor(key)) %>%
    ggplot(aes(x = index, y = value, group = key)) +
    geom_line(color = palette_light()[[2]]) +
    geom_ma(ma_fun = SMA, n = 12, size = 1) +
    facet_wrap(~ key, scales = "free_y") +
    scale_x_yearmon(n = 10) +
    labs(title = "Customer Care Forecasted Work: TBATS Decomposition", x = "") + 
    theme_tq() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


```

## Forecasting the values

```{r}
fcast_ets <- fit_ets %>%
    forecast(h = 12)

# fc_df<- sw_sweep(fcast_ets, fitted = TRUE) %>% filter(!is.na(lo.80))

sw_sweep(fcast_ets) %>%
    ggplot(aes(x = index, y = ttlAmt, color = key)) +
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95),
                fill = "#D5DBFF", color = NA, size = 0) +
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key),
                fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
    geom_line(size = 1) +
    labs(title = "Customer Care Forecasted Work, TBATS Model Forecast", x = "", y = "Effort",
         subtitle = "Regular Time Index") +
    # scale_y_continuous(labels = scales::dollar) +
    scale_x_yearmon(n = 12) + #, format = "%Y"
    scale_color_tq() +
    scale_fill_tq() +
    theme_tq()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))


```

#https://business-science.github.io/sweep/index.html

