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
start <- c(1, 12, 23, 43)
end <- c(11, 22, 42, 62)
col <- fwf_positions(start = start, end = end) # Use fwf_positions to define column positions
pat <- read_fwf(paste0("data/4_IRPF2021_trim.txt") , col_positions = col) %>% data.table()
colnames(pat) <- c("IDENPER", "IDENHOG", "GENERAL", "AHORRO")

dt <- pat[, NEW_RENTA := GENERAL + AHORRO]

fwrite(dt, "data/IEF-2021-new-renta.gz")


#------------------------------------
rm(list = ls())

dt <- fread("data/IEF-2021-new-renta.gz")
dt2 <- fread("data/IEF-2021-new.gz")

dt_merged <- merge(dt, dt2, by = c("IDENHOG", "IDENPER"))

fwrite(dt_merged, "data/IEF-2021-new.gz")
