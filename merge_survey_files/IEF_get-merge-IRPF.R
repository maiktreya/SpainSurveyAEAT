# Fichero principal 2021

library(data.table)
rm(list = ls()) # clean enviroment to avoid ram bottlenecks

sel_year <- 2021
dt <- fread("data/ief2021/par150.gz")

dt <- dt[, # group todas las declaraciones por persona. PAR150: ded. por alq. de viv.)
    .(
        IDENHOG = mean(IDENHOG),
        PAR150 = sum(PAR150)
    ),
    by = IDENPER
]

dt_tipo <- fread(paste0("data/ief", sel_year, "/tipodec.gz")) # tipo de declaración already grouped by IDENPER
dt <- merge(dt, dt_tipo, by = c("IDENPER", "IDENHOG"))

dt_iden <- fread(paste0("data/ief", sel_year, "/iden2021newdt.gz")) #  pesos muestrales
dt <- merge(dt, dt_iden, by = c("IDENPER", "IDENHOG"))

dt_renta <- fread(paste0("data/ief", sel_year, "/renta.gz")) # renta y alquiler
dt <- merge(dt, dt_renta, by = c("IDENPER", "IDENHOG"))

dt_pat <- fread(paste0("data/ief", sel_year, "/pat.gz")) # patrimonio inmobiliario
dt <- merge(dt, dt_pat, by = c("IDENPER", "IDENHOG"))

fwrite(dt, paste0("data/IEF-", sel_year, "-new.gz")) # exportar objeto preparado
