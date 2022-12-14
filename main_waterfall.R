## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN WATERFALL WRANGLE
## AUTHOR: Randy Yee
## DESCRIPTION: 
##      TSD refactor and update from Imran Mujawar's original script
##      Download from DATIM with select TX indicators
##      Operating Unit: 
##      Indicator: TX_CURR, TX_ML, TX_NEW, TX_RTT
##     Fiscal Year: 2022,2021,2020,
## CREATION DATE: 6/8/2020
## UPDATE: 9/20/2022
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
#library(openxlsx)

source("waterfall.R")

period_list <- list(
  # c("2020_qtr1", "2020_qtr2", "2020_targets"),
  # c("2020_qtr2", "2020_qtr3", "2020_targets"),
  # c("2020_qtr3", "2020_qtr4", "2020_targets"),
  c("2020_qtr4", "2021_qtr1", "2021_targets"),
  
  c("2021_qtr1", "2021_qtr2", "2021_targets"),
  c("2021_qtr2", "2021_qtr3", "2021_targets"),
  c("2021_qtr3", "2021_qtr4", "2021_targets"),
  c("2021_qtr4", "2022_qtr1", "2022_targets"),
  
  c("2022_qtr1", "2022_qtr2", "2022_targets"),
  c("2022_qtr2", "2022_qtr3", "2022_targets"),
  c("2022_qtr3", "2022_qtr4", "2022_targets")
)

## ==================== Global ==================== 
message("Running PSNU Global...")

ou_df <- msd_import_long("~/Genie_PSNU_IM_Global_Frozen_2986ae63-1565-48ae-b7d5-97d2cca755dd.txt")

startTime <- Sys.time()

df <- list()

for(i in 1:length(period_list)){
  message(paste("Round", i))
  df[[i]] <- txs_generate(ou_df, period_list[[i]][1], period_list[[i]][2], period_list[[i]][3])
}

df1 <- bind_rows(df) %>%
  discard(~all(is.na(.)))

write.csv(df1,"Global_Waterfall_FY22Q4Initial.csv", na = "")

endTime <- Sys.time()
print(endTime - startTime)


## ==================== Individual Site ==================== 
message("Running Site...")
ou_df <- msd_import_long("Genie_SITE_IM_Global_Frozen_5ac1e841-14b6-4696-b574-cf3ee8ddf4ec.txt")

ou_list <- unique(ou_df$operatingunit)

startTime <- Sys.time()

for(o in ou_list){
  message(paste("Starting...", o))
  df <- list()
  
  for(i in 1:length(period_list)){
    message(paste(o, ":", i))
    df[[i]] <- txs_generate(filter(ou_df, operatingunit == o), period_list[[i]][1], period_list[[i]][2], period_list[[i]][3])
  }
  
  df1 <- bind_rows(df) %>%
    discard(~all(is.na(.)))
  
  write.csv(df1,paste0("./Individual_OUs/", o, "_Waterfall_FY22Q4Initial.csv"), na = "")
}

endTime <- Sys.time()
print(endTime - startTime)

