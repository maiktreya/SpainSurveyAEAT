# Fichero principal 2020, select columns par150-caseros par36-renta
library(magrittr)
library(survey)
library(data.table)
rm(list = ls()) # clean enviroment to avoid ram bottlenecks

# Parameters to set (Encuesta de referencia, periodo de estudio, unidad de estudi, columnas seleccionadaso)
ref_survey <- "EFF" # either IEF or EFF
sel_year <- 2011 # 2020 for EFF & 2021 for IEF

# Import choosen dataframe (cambiar string inicial según ruta de los datos)
dt <- paste0("data/", ref_survey, "-", sel_year, "-new.gz") %>% fread()

## Prepare survey object from dt and set income cuts for quantiles dynamically
dt_sv <- svydesign(ids = ~1, data = dt, weights = dt$facine3) # muestra con coeficientes de elevación

# get tenure status
tenencia <- svytable(~p2_1, dt_sv) %>% prop.table()

# present as proportions
tenencia %>% print()
