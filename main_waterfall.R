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
library(openxlsx)

source("waterfall.R")

# ## ==================== MAIN ====================
period_list <- list(
  # c("2020_qtr1", "2020_qtr2", "2020_targets"),
  # c("2020_qtr2", "2020_qtr3", "2020_targets"),
  # c("2020_qtr3", "2020_qtr4", "2020_targets"),
  # c("2020_qtr4", "2021_qtr1", "2021_targets"),
  
  c("2021_qtr1", "2021_qtr2", "2021_targets"),
  c("2021_qtr2", "2021_qtr3", "2021_targets"),
  c("2021_qtr3", "2021_qtr4", "2021_targets"),
  c("2021_qtr4", "2022_qtr1", "2022_targets"),
  
  c("2022_qtr1", "2022_qtr2", "2022_targets"),
  c("2022_qtr2", "2022_qtr3", "2022_targets")#,
  #c("2022_qtr3", "2022_qtr4", "2022_targets"),
)

ou_df <- msd_df("ESWATINI_SITE_22Q3.txt")

df <- list()

startTime <- Sys.time()

for(i in 1:length(period_list)){
  df[[i]] <- txs_generate(ou_df, period_list[[i]][1], period_list[[i]][2], period_list[[i]][3])
}

endTime <- Sys.time()
print(endTime - startTime)

df1 <- bind_rows(df) %>%
  discard(~all(is.na(.)))

write.xlsx(df1,"test.xlsx", na = "")



## ==================== TESTS ==================== 
# test  <- msd_import_long("ESWATINI_SITE_22Q3.txt")
# 
# test  <- txs_lag_indicators(test)
# 
# test1 <- recode_period_txdisagg(test, "2021_qtr4", "2022_qtr1", "2022_targets")
# 
# test2 <- recode_prioritizations(test1)
# 
# test3 <- collapse_age(test2)
# 
# test4 <- redo_indicator_name(test3)
# 
# test5 <- txs_clean(test4)
# 
# test6 <- txs_convert_wide(test5)
# 
# test7 <- waterfall_standardized(test6)
# 
# compose_test <- txs_generate(ou_list[3], "2020_qtr2", "2020_qtr3", "2020_targets")