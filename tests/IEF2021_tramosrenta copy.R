# Muestra AEAT / IEF 2021 (income cohorts analysis)

library(magrittr)
library(survey)
library(data.table)
rm(list = ls())

# Parameters to set (reference survey, wave, base-unit, numerical variables)
ref_survey <- "IEF" # either IEF or EFF
sel_year <- 2021 # 2020 for EFF & 2021 for IEF
ref_unit <- "IDENHOG" # Use either IDENPER for personal or IDENHOG for household levels
selected_columns <- c("RENTAB", "RENTAD")

# Import choosen dataframe (modify path if needed)
dt <- paste0("dataº") %>% fread()
