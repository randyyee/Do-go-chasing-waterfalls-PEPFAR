## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MSD Functions for TXS
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD refactor and update from Imran Mujawar (CDC)'s original script
## CREATION DATE: 6/8/2020
## UPDATE: 9/20/2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## ==================== MSD IMPORT AND MADE LONG ====================
msd_import_long <- function(msd_txt){
  
  df <- read_delim(msd_txt, 
                   "\t", 
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_types = cols(.default = col_character(), 
                                    targets  = col_double(),
                                    qtr1 = col_double(),
                                    qtr2 = col_double(),
                                    qtr3 = col_double(),
                                    qtr4 = col_double(),
                                    cumulative = col_double())) 
  
  df <- df %>%
    filter(indicator %in% c("TX_CURR",
                            "TX_NEW",
                            "TX_ML",
                            "TX_RTT")) %>%
    filter(standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                           "Age Aggregated/Sex/HIVStatus",
                                           "Age/Sex/ARTNoContactReason/HIVStatus",
                                           "ARTNoContactReasonIIT/HIVStatus")) %>%
    select(-cumulative)
  
  df <- pivot_longer(df,
                     targets:qtr4,
                     names_to = "period",
                     values_to = "value")
  
  df <- unite(df, 
              "period", 
              c("fiscal_year", "period"),
              sep = "_", 
              remove = T)
}


## ==================== TXS LAG INDICATORS ====================
txs_lag_indicators <- function(msd_imported_long){
  
  df <- rbind(
    msd_imported_long, 
    msd_imported_long %>%
      filter(indicator %in% c("TX_CURR", "TX_NEW")) %>%
      filter(str_detect(period, "qtr")) %>%
      pivot_wider(names_from = period, values_from = value) %>%
      pivot_longer(cols = contains("qtr"), names_to = "period", values_to = "value") %>%
      pivot_wider(names_from = indicator, values_from = value) %>%
      group_by(across(c(-period, -TX_CURR, -TX_NEW))) %>% 
      mutate(TX_CURR_LAG  = dplyr::lag(TX_CURR,     order_by = period),
             TX_CURR_LAG2 = dplyr::lag(TX_CURR_LAG, order_by = period),
             TX_NEW_LAG   = dplyr::lag(TX_NEW,      order_by = period),
             TX_NEW_LAG2  = dplyr::lag(TX_NEW_LAG,  order_by = period)) %>%
      ungroup() %>%
      pivot_longer(contains("TX"), names_to = "indicator", values_to = "value") %>%
      filter(!indicator %in% c("TX_CURR", "TX_NEW")))
}


## ==================== TXS RECODE PERIOD & NEW TX INDICATOR LABELS ==================== 
recode_period_txdisagg <- function(txs_lagged_indicators, prev_r, curr_r, curr_t){
  df <- txs_lagged_indicators %>%
    mutate(period = case_when(period == prev_r ~ "prev_result",
                              period == curr_r ~ "curr_result",
                              period == curr_t ~ "curr_target",
                              TRUE ~"remove")) %>%
    mutate(indicator = ifelse(indicator %in% c("TX_ML","TX_RTT"),
                              paste(indicator, otherdisaggregate, sep="_"),
                              indicator)) %>%
    filter(period != "remove") %>%
    group_by_if(is.character) %>% 
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup() %>%
    mutate(period_range = curr_r)
}  


## ==================== TXS RECODE MOST CURRENT PRIORITIZATIONS (SITE ONLY) ==================== 
recode_prioritizations <- function(recoded_period_txdisagg){
  df <- recoded_period_txdisagg %>% 
    filter(period == "curr_result") %>%
    select(c(orgunituid, snuprioritization)) %>%
    distinct()
  
  df2 <- select(recoded_period_txdisagg, -snuprioritization)
  
  df3 <- left_join(df2, df)
}


## ==================== TXS ADD SINGLE AGE COLUMN ====================
collapse_age <- function(recoded_prioritizations){
  
  df <- recoded_prioritizations %>%
    select(-c(ageasentered, age_2019)) %>%
    rename("trendsfine" = "age_2018") %>%
    pivot_longer(cols = c("trendsfine", "trendscoarse"), 
                 names_to = "age_type", 
                 values_to = "age") %>%
    filter(!is.na(value))
}


## ==================== TXS REDO INDICATOR NAMES FOR WIDE PIVOT ====================
redo_indicator_name <- function(reformatted_age_sex){
  df <- reformatted_age_sex %>% 
    mutate(var_suffix = case_when(
      period == "curr_result"  ~ "Now_R",
      period == "curr_target"  ~ "Now_T",
      period == "prev_result"  ~ "Prev_R")) %>% 
    filter(!is.na(var_suffix)) %>%
    unite("varname", 
          c(indicator, var_suffix),
          sep = "_", 
          remove = T) %>%
    group_by_if(is.character) %>%
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== TXS REMOVE EXTRANEOUS COLUMNS & CLEAN ====================
txs_clean <- function(redone_indicator_name){
  
  lookup <- c("operatingunit",
              "country",
              "snu1",
              "snuprioritization",
              "psnu",
              "psnuuid",
              "sitetype",
              "sitename",
              "orgunituid",
              "funding_agency",
              "prime_partner_name",
              "mech_name",
              "mech_code",
              "facility",
              "indicatortype",
              "age_type",
              "age",
              "sex",
              "period_range",
              "varname",
              "value")
  
  df <- redone_indicator_name %>%
    select(any_of(lookup)) %>%
    group_by_if(is_character) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== TXS CONVERT WIDE ====================
txs_convert_wide <- function(txs_cleaned){
  df <- pivot_wider(txs_cleaned,
                    names_from = "varname",
                    values_from = "value") %>%
    na_if(0)
  
  df$SUMCOL <- rowSums(df[sapply(df, is.numeric)], na.rm = TRUE)
  
  df <- df %>%
    mutate(period = paste0("FY",substr(period_range,3,4),"Q",substr(period_range,9,9))) %>%
    filter(SUMCOL != 0) %>%
    select(-SUMCOL)
}


## ==================== WATERFALL COLUMN STANDARDIZATION ====================
waterfall_standardized <- function(txs_converted_wide){
  
  lookup1 <- c(TX_ML_Died_Now_R                               = "TX_ML_No Contact Outcome - Died_Now_R",
               `TX_ML_Interruption <3 Months Treatment_Now_R` = "TX_ML_No Contact Outcome - Interruption in Treatment <3 Months Treatment_Now_R",
               `TX_ML_Interruption 3+ Months Treatment_Now_R` = "TX_ML_No Contact Outcome - Interruption in Treatment 3+ Months Treatment_Now_R",
               `TX_ML_Interruption 3-5 Months Treatment_R`    = "TX_ML_No Contact Outcome - Interruption in Treatment 3-5 Months Treatment_Now_R",
               `TX_ML_Interruption 6+ Months Treatment_R`     = "TX_ML_No Contact Outcome - Interruption in Treatment 6+ Months Treatment_Now_R",
               `TX_ML_Refused Stopped Treatment_Now_R`        = "TX_ML_No Contact Outcome - Refused Stopped Treatment_Now_R",
               `TX_ML_Transferred Out_Now_R`                  = "TX_ML_No Contact Outcome - Transferred Out_Now_R",
               `TX_RTT_Now_R`                                 = "TX_RTT_NA_Now_R",
               `TX_RTT_ <3 Months Interruption`               = "TX_RTT_No Contact Outcome - Interruption in Treatment <3 Months Interruption_Now_R",
               `TX_RTT_3-5 Months Interruption`               = "TX_RTT_No Contact Outcome - Interruption in Treatment 3-5 Months Interruption_Now_R",
               `TX_RTT_6+ Months Interruption`                = "TX_RTT_No Contact Outcome - Interruption In Treatment 6+ Months Interruption_Now_R")
  
  df <- txs_converted_wide %>% 
    rename(any_of(lookup1))
  
  shell_col <- c("operatingunit",                                                         
                 "country",                                                           
                 "snu1",                                                                  
                 "snuprioritization",                                                     
                 "psnu",                                                                  
                 "psnuuid",                                                               
                 "sitetype",                                                              
                 "sitename",                                                              
                 "orgunituid",                                                            
                 "funding_agency",                                                         
                 "prime_partner_name",                                                          
                 "mech_name",                                                             
                 "mech_code",
                 "facility",
                 "age_type",                                                                   
                 "age",                                                                   
                 "sex", 
                 "indicatortype",
                 "period",
                 "TX_CURR_Prev_R",
                 "TX_CURR_Now_R",
                 "TX_CURR_Now_T",
                 "TX_NEW_Prev_R",
                 "TX_NEW_Now_R",
                 "TX_NEW_Now_T",
                 #"TX_ML_Now_R",
                 "TX_ML_Interruption <3 Months Treatment_Now_R",
                 "TX_ML_Interruption 3+ Months Treatment_Now_R",
                 "TX_ML_Interruption 3-5 Months Treatment_R",
                 "TX_ML_Interruption 6+ Months Treatment_R",
                 "TX_ML_Died_Now_R",
                 "TX_ML_Refused Stopped Treatment_Now_R",
                 "TX_ML_Transferred Out_Now_R",
                 "TX_RTT_Now_R",
                 "TX_RTT_ <3 Months Interruption",
                 "TX_RTT_3-5 Months Interruption",
                 "TX_RTT_6+ Months Interruption",
                 "TX_CURR_LAG_Now_R",
                 "TX_CURR_LAG2_Now_R",
                 "TX_NEW_LAG_Now_R",
                 "TX_NEW_LAG2_Now_R")
  
  
  missing <- setdiff(shell_col, names(df))
  df[missing] <- NA
  df <- df[shell_col] # Column Order
  
}


## ==================== COMPOSED FUNCTION ====================
txs_generate <- function(msd_long_df, prevR, currR, currT){
  
  df <- txs_lag_indicators(msd_long_df)
  
  df <- recode_period_txdisagg(df, prevR, currR, currT)
  
  if("orgunituid" %in% colnames(df)){
    df <- recode_prioritizations(df)
  }
  
  df <- collapse_age(df)
  
  df <- redo_indicator_name(df)
  
  df <- txs_clean(df)
  
  df <- txs_convert_wide(df)
  
  df <- waterfall_standardized(df)
  
}