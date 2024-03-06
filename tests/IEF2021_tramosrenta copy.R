# Muestra AEAT / IEF 2021 (income cohorts analysis)

library(magrittr)
library(survey)
library(data.table)
rm(list = ls())


# Fichero principal 2021
library(magrittr)
library(readr)
library(data.table)
rm(list = ls()) # clean enviroment to avoid ram bottlenecks

# PATRIMONIO
start <- c(1, 12, 427, 447)
end <- c(11, 22, 446, 466)
col <- fwf_positions(start = start, end = end) # Use fwf_positions to define column positions
pat <- read_fwf(paste0("data/11_M151_2021.TXT") , col_positions = col) %>% data.table()
colnames(pat) <- c("IDENPER", "IDENHOG", "GENERAL", "AHORRO")

dt <- pat[, NEW_RENTA := GENERAL + AHORRO]
