## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN WATERFALL WRANGLE
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
##      Download from DATIM with select TX indicators and disaggs
## CREATION DATE: 6/8/2020
## UPDATE: 2/28/2020
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(openxlsx)
library(foreach)
library(parallel)
library(doParallel)

source("waterfall.R")

# ## ==================== MAIN ====================
n.cores    <- parallel::detectCores()
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(cl = my.cluster)

period_list <- list(
  c("2020_qtr1", "2020_qtr2", "2020_targets"),
  c("2020_qtr2", "2020_qtr3", "2020_targets"),
  c("2020_qtr3", "2020_qtr4", "2020_targets"),
  c("2020_qtr4", "2021_qtr1", "2021_targets"),

  c("2021_qtr1", "2021_qtr2", "2021_targets"),
  c("2021_qtr2", "2021_qtr3", "2021_targets"),
  c("2021_qtr3", "2021_qtr4", "2021_targets"),
  c("2021_qtr4", "2022_qtr1", "2022_targets")
)

ou_df <- msd_df("MSD.txt")

df <- foreach(i = 1:length(period_list)) %dopar%
  txs_generate(ou_df, period_list[[i]][1], period_list[[i]][2], period_list[[i]][3])

df1 <- bind_rows(df) %>%
  discard(~all(is.na(.)))

write.csv(df1,"test.csv", na = "")



# ## ==================== TESTS ==================== 
# test <- msd_import("MSD.txt")
# 
# test1 <- msd_convert_long(test)
# 
# test2 <- recode_period_txdisagg(test1, "2021_qtr4", "2022_qtr1", "2022_targets")
# 
# test3 <- recode_prioritizations(test2)
# 
# test4 <- collapse_age(test3)
# 
# test5 <- redo_indicator_name(test4)
# 
# test6 <- txs_clean(test5)
# 
# test7 <- txs_convert_wide(test6)
# 
# compose_test <- txs_generate(ou_list[3], "2020_qtr2", "2020_qtr3", "2020_targets")