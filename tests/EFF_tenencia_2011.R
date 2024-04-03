# PArtial analysis
library(magrittr)
library(survey)
library(data.table)
rm(list = ls()) # clean enviroment to avoid ram bottlenecks

# Parameters to set
ref_survey <- "EFF"
sel_year <- 2011

# Import choosen dataframe
dt <- paste0("data/", ref_survey, "-", sel_year, "-new.gz") %>% fread()

## Prepare survey object
dt_sv <- svydesign(ids = ~1, data = dt, weights = dt$facine3)

# get tenure status
tenencia <- svytable(~p2_1, subset(dt_sv, p1_2b_1 > 1985)) %>% prop.table()

# present as proportions
tenencia %>% print()
