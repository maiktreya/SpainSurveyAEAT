# Fichero principal 2020, select columns par150-caseros par36-renta
library(magrittr)
library(survey)
library(data.table)
library(srvyr)
library(dplyr)
rm(list = ls()) # clean enviroment to avoid ram bottlenecks

# Parameters to set (Encuesta de referencia, periodo de estudio, unidad de estudi, columnas seleccionadaso)
ref_survey <- "IEF" # either IEF or EFF
sel_year <- 2021 # 2020 for EFF & 2021 for IEF
ref_unit <- "IDENPER" # Use either IDENPER for personal or IDENHOG for household levels
selected_columns <- c("RENTAD", "RENTAB", "RENTA_ALQ", "PATINMO")

# Import choosen dataframe (cambiar string inicial según ruta de los datos)
dt <- paste0("data/", ref_survey, "-", sel_year, "-new.gz") %>% fread()

# Use lapply with .SDcols to specify columns and replace NA with 0
dt[, (selected_columns) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = selected_columns]

# Main data transformation TABLA[ filter_rows , select_columns  , group_by ]
dt[TRAMO == "N", TRAMO := 8][, TRAMO := as.numeric(TRAMO)]

dt <- dt[TIPODEC %in% c("T1", "T21") & !is.na(FACTORCAL),
    .(
        RENTAB = sum(RENTAB),
        TRAMO = mean(TRAMO),
        RENTA_ALQ = sum(RENTA_ALQ),
        PAR150 = sum(PAR150),
        PATINMO = sum(PATINMO),
        FACTORCAL = mean(FACTORCAL)
    ),
    by = .(reference = get(ref_unit))
]
setnames(dt, "reference", as.character(ref_unit))

dt[, CASERO := 0][PAR150 > 0, CASERO := 1][, CASERO := factor(CASERO)]


dt <- dt %>%
    mutate(
        tramos_panzer =
            case_when(
                RENTAB <= 0 ~ "Negativo y Cero",
                RENTAB > 0 & RENTAB <= 1500 ~ "(0 - 1,5]",
                RENTAB > 1500 & RENTAB <= 6000 ~ "(1,5 - 6]",
                RENTAB > 6000 & RENTAB <= 12000 ~ "(6 - 12]",
                RENTAB > 12000 & RENTAB <= 21000 ~ "(12 - 21]",
                RENTAB > 21000 & RENTAB <= 30000 ~ "(21 - 30]",
                RENTAB > 30000 & RENTAB <= 60000 ~ "(30 - 60]",
                RENTAB > 60000 & RENTAB <= 150000 ~ "(60 - 150]",
                RENTAB > 150000 & RENTAB <= 601000 ~ "(150 - 601]",
                RENTAB > 601000 ~ "Mayor de 601",
                TRUE ~ "Sin informacion"
            )
    )


dt$tramos_panzer <- factor(dt$tramos_panzer, levels = c(
    "Negativo y Cero", "(0 - 1,5]", "(1,5 - 6]", "(6 - 12]", "(12 - 21]",
    "(21 - 30]", "(30 - 60]", "(60 - 150]", "(150 - 601]", "Mayor de 601"
))

dt <- dt %>%
    mutate(
        tramos_doc =
            case_when(
                RENTAB <= 0 ~ "Menor o igual a cero",
                RENTAB < 6000 ~ "Menos de 6 mil euros",
                RENTAB > 6000 & RENTAB <= 12000 ~ "De 6 a 12 mil euros",
                RENTAB > 12000 & RENTAB <= 22000 ~ "12 a 22 mil euros",
                RENTAB > 22000 & RENTAB <= 60000 ~ "22 a 60 mil euros",
                RENTAB > 150000 & RENTAB <= 300000 ~ "150 a 300 mil euros",
                RENTAB > 300000 ~ "Más de 300 mil euros",
                TRUE ~ "Sin información"
            )
    )

dt$tramos_doc <- factor(dt$tramos_doc, levels = c(
    "Menor o igual a cero", "Menos de 6 mil euros", "De 6 a 12 mil euros",
    "12 a 22 mil euros", "22 a 60 mil euros", "150 a 300 mil euros",
    "Más de 300 mil euros", "Sin información"
))



dt_sv <- svydesign(ids = ~1, data = dt, weights = dt$FACTORCAL) # muestra con coeficientes de elevación


##########
casero_dis <- svytable(~ TRAMO + CASERO, dt_sv) %>% as.data.frame() %>% print()

#--------------@miguel compobar start--------------
dt[, TRAMO_NEW := 0][RENTAB <= 0, TRAMO_NEW := 1][RENTAB > 0, TRAMO_NEW := 2]
dt[RENTAB >= 6000, TRAMO_NEW := 3]
dt[RENTAB >= 12000, TRAMO_NEW := 4]
dt[RENTAB >= 22000, TRAMO_NEW := 5]
dt[RENTAB >= 150000, TRAMO_NEW := 6]
dt[RENTAB >= 300000, TRAMO_NEW := 7][, TRAMO_NEW := as.factor(TRAMO_NEW)]
casero_dis_alt <- svytable(~ TRAMO_NEW + CASERO, dt_sv) %>% print()
casero_dis_alt2 <- svyby(~ CASERO,   ~TRAMO_NEW, dt_sv, svymean) %>% print()
#--------------@miguel compobar end--------------





casero <- casero_dis %>%
    filter(CASERO == 1)

no_casero <- casero_dis %>%
    filter(CASERO != 1)

distribucion <- merge(casero, no_casero, by = "TRAMO")
distribucion <- distribucion[, -c(2, 4)]
colnames(distribucion) <- c("TRAMO", "CASERO", "NO CASERO")
distribucion$total <- distribucion$`NO CASERO` + distribucion$CASERO

distribucion$CASERO_por <- round((distribucion$CASERO * 100) / sum(distribucion$CASERO), 2)
distribucion$NO_CASERO_por <- round((distribucion$`NO CASERO` * 100) / sum(distribucion$`NO CASERO`), 2)
distribucion$total_por <- round((distribucion$total * 100) / sum(distribucion$total), 2)

distribucion_TRAMO <- distribucion


######
casero_dis <- svytable(~ tramos_doc + CASERO, dt_sv) %>% as.data.frame()
casero <- casero_dis %>%
    filter(CASERO == 1)

no_casero <- casero_dis %>%
    filter(CASERO != 1)

distribucion <- merge(casero, no_casero, by = "tramos_doc")
distribucion <- distribucion[, -c(2, 4)]
colnames(distribucion) <- c("tramos_doc", "CASERO", "NO CASERO")
distribucion$total <- distribucion$`NO CASERO` + distribucion$CASERO

distribucion$CASERO_por <- round((distribucion$CASERO * 100) / sum(distribucion$CASERO), 2)
distribucion$NO_CASERO_por <- round((distribucion$`NO CASERO` * 100) / sum(distribucion$`NO CASERO`), 2)
distribucion$total_por <- round((distribucion$total * 100) / sum(distribucion$total), 2)

distribucion_tramos_doc <- distribucion

# row order
levels_order <- c(
    "Menor o igual a cero", "Menos de 6 mil euros", "De 6 a 12 mil euros",
    "12 a 22 mil euros", "22 a 60 mil euros", "150 a 300 mil euros",
    "Más de 300 mil euros", "Sin información"
)

distribucion_tramos_doc$tramos_doc <- factor(distribucion_tramos_doc$tramos_doc, levels = levels_order)
distribucion_tramos_doc <- distribucion_tramos_doc[order(distribucion_tramos_doc$tramos_doc), ]


#######

casero_dis <- svytable(~ tramos_panzer + CASERO, dt_sv) %>% as.data.frame()
casero <- casero_dis %>%
    filter(CASERO == 1)

no_casero <- casero_dis %>%
    filter(CASERO != 1)

distribucion <- merge(casero, no_casero, by = "tramos_panzer")
distribucion <- distribucion[, -c(2, 4)]
colnames(distribucion) <- c("tramos_panzer", "CASERO", "NO CASERO")
distribucion$total <- distribucion$`NO CASERO` + distribucion$CASERO

distribucion$CASERO_por <- round((distribucion$CASERO * 100) / sum(distribucion$CASERO), 2)
distribucion$NO_CASERO_por <- round((distribucion$`NO CASERO` * 100) / sum(distribucion$`NO CASERO`), 2)
distribucion$total_por <- round((distribucion$total * 100) / sum(distribucion$total), 2)

distribucion_tramos_panzer <- distribucion

# row order
levels_order <- c(
    "Negativo y Cero", "(0 - 1,5]", "(1,5 - 6]", "(6 - 12]", "(12 - 21]",
    "(21 - 30]", "(30 - 60]", "(60 - 150]", "(150 - 601]", "Mayor de 601"
)

distribucion_tramos_panzer$tramos_panzer <- factor(distribucion_tramos_panzer$tramos_panzer, levels = levels_order)
distribucion_tramos_panzer <- distribucion_tramos_panzer[order(distribucion_tramos_panzer$tramos_panzer), ]


#########

# ver las diferencias

# Check output
list(distribucion_TRAMO, distribucion_tramos_doc, distribucion_tramos_panzer) %>% print()
