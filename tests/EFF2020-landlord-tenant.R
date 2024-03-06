# Analyze households meeting  both landlord and tenant conditions from EFF 2020
library(magrittr)
library(survey)
library(data.table)
rm(list = ls())

# Parameters to set (Encuesta de referencia, periodo de estudio, unidad de estudi, columnas seleccionadaso)
ref_survey <- "EFF" # either IEF or EFF
sel_year <- 2020 # 2020 for EFF & 2021 for IEF

# Import choosen dataframe (cambiar string inicial según ruta de los datos)
dt <- paste0("data/", ref_survey, "-", sel_year, "-new.gz") %>% fread()

# Use lapply with .SDcols to specify columns and replace NA with 0
dt2 <- fread("data/eff2020databol.gz") # para importar valor de renta que coincida con EFF 2020 rev 2
dt[, RENTAD := dt2$renthog19_eur20]
dt[, CASERO := 0][RENTA_ALQ > 0 & p2_1 != 1, CASERO := 1]
dt[, CASERO_INQ := 0][RENTA_ALQ > 0 & p2_1 == 1, CASERO_INQ := 1]
dt[, INQUILINO := 0][p2_1 == 1 & CASERO_INQ == 0, INQUILINO := 1]

## Prepare survey object from dt and set income cuts for quantiles dynamically
dt_sv <- svydesign(ids = ~1, data = dt, weights = dt$FACTORCAL) # muestra con coeficientes de elevación
quantiles <- seq(.25, .75, .25) # cortes
quantiles_renta <- svyquantile(~RENTAD, design = dt_sv, quantiles = quantiles, ci = FALSE)$RENTAD # rentas asociadas a cores
table_names <- c(
    paste0("hasta ", quantiles_renta[, "0.25"]),
    paste0("entre ", quantiles_renta[, "0.25"], " y ", quantiles_renta[, "0.5"]),
    paste0("entre ", quantiles_renta[, "0.5"], " y ", quantiles_renta[, "0.75"]),
    paste0("mas de ", quantiles_renta[, "0.75"])
)

# TABLA 1-------------------------------------------------------------------------------
caseros25 <- svytable(~CASERO, subset(dt_sv, RENTAD < quantiles_renta[, "0.25"]))
caseros25to50 <- svytable(~CASERO, subset(dt_sv, RENTAD > quantiles_renta[, "0.25"] & RENTAD < quantiles_renta[, "0.5"]))
caseros50to75 <- svytable(~CASERO, subset(dt_sv, RENTAD > quantiles_renta[, "0.5"] & RENTAD < quantiles_renta[, "0.75"]))
caseros75 <- svytable(~CASERO, subset(dt_sv, RENTAD > quantiles_renta[, "0.75"]))

inquilinos25 <- svytable(~INQUILINO, subset(dt_sv, RENTAD < quantiles_renta[, "0.25"]))
inquilinos25to50 <- svytable(~INQUILINO, subset(dt_sv, RENTAD > quantiles_renta[, "0.25"] & RENTAD < quantiles_renta[, "0.5"]))
inquilinos50to75 <- svytable(~INQUILINO, subset(dt_sv, RENTAD > quantiles_renta[, "0.5"] & RENTAD < quantiles_renta[, "0.75"]))
inquilinos75 <- svytable(~INQUILINO, subset(dt_sv, RENTAD > quantiles_renta[, "0.75"]))

caseros_inquil25 <- svytable(~CASERO_INQ, subset(dt_sv, RENTAD < quantiles_renta[, "0.25"]))
caseros_inquil25to50 <- svytable(~CASERO_INQ, subset(dt_sv, RENTAD > quantiles_renta[, "0.25"] & RENTAD < quantiles_renta[, "0.5"]))
caseros_inquil50to75 <- svytable(~CASERO_INQ, subset(dt_sv, RENTAD > quantiles_renta[, "0.5"] & RENTAD < quantiles_renta[, "0.75"]))
caseros_inquil75 <- svytable(~CASERO_INQ, subset(dt_sv, RENTAD > quantiles_renta[, "0.75"]))

caseros <- data.table(rbind(caseros25, caseros25to50, caseros50to75, caseros75))[, "1"] %>% prop.table()
inquilinos <- data.table(rbind(inquilinos25, inquilinos25to50, inquilinos50to75, inquilinos75))[, "1"] %>% prop.table()
caseros_inqui <- data.table(rbind(caseros_inquil25, caseros_inquil25to50, caseros_inquil50to75, caseros_inquil75))[, "1"] %>% prop.table()
final_table <- cbind(table_names, caseros, inquilinos, caseros_inqui)
colnames(final_table) <- c("niveles", "caseros", "inquilinos", "caseros-inquilinos")


# TABLA 2--------------------------------------------------------------------
median_renta_inquili <- svyquantile(~RENTAD, subset(dt_sv, INQUILINO == 1), quantiles = .5, ci = FALSE)$RENTAD[1]
median_renta_caseros <- svyquantile(~RENTAD, subset(dt_sv, CASERO == 1), quantiles = .5, ci = FALSE)$RENTAD[1]
median_renta_propsin <- svyquantile(~RENTAD, subset(dt_sv, CASERO_INQ == 1), quantiles = .5, ci = FALSE)$RENTAD[1]
median_renta_totalpo <- svyquantile(~RENTAD, dt_sv, quantiles = .5, ci = FALSE)$RENTAD[1]

mean_renta_inquili <- svymean(~RENTAD, subset(dt_sv, INQUILINO == 1))[1]
mean_renta_caseros <- svymean(~RENTAD, subset(dt_sv, CASERO == 1))[1]
mean_renta_propsin <- svymean(~RENTAD, subset(dt_sv, CASERO_INQ == 1))[1]
mean_renta_totalpo <- svymean(~RENTAD, dt_sv)[1]

medians <- c(median_renta_caseros, median_renta_inquili, median_renta_propsin, median_renta_totalpo)
means <- c(mean_renta_caseros, mean_renta_inquili, mean_renta_propsin, mean_renta_totalpo)
renta_table <- cbind(c("caseros", "inquilinos", "caseros-inquilinos", "todos"), means, medians)
colnames(renta_table) <- c("tipo", "media", "mediana")



# Analyze results including landlord-tenants dimension
svymean(~CASERO_INQ, dt_sv) %>% print() # aroun 1.3% of total population
print(renta_table)
print(final_table)
