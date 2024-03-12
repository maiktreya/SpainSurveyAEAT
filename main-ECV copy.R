# Fichero principal 2021
library(magrittr)
library(data.table)
library(survey)

rm(list = ls()) # clean enviroment to avoid ram bottlenecks

#### OBTAINING DATA ()
hogar1 <- fread("data/ECV/esudb22h.csv")
hogar2 <- fread("data/ECV/esudb22d.csv")

# unir
ecv <- merge(hogar1, hogar2, by.x = "HB030", by.y = "DB030", all = TRUE)

ecv_sv <- svydesign(ids = ~1, data = ecv, weights = ecv$DB090)

#### calcular percentile 2% de pago de alquiler por ccaa

percentile_2_ccaa_tabla <- svyby(~HH060, ~DB040,
  design = subset(ecv_sv, HH021 == 3),
  svyquantile,
  quantiles = 0.02,
  na.rm = TRUE
) %>% data.table()

percentile_2_ccaa_tabla[, HH060 := HH060 * 12]

percentile_2_ccaa_tabla <- cbind(percentile_2_ccaa_tabla, Region = c(
  "Galicia",
  "Principado de Asturias",
  "Cantabria",
  "País Vasco",
  "Comunidad Foral de Navarra",
  "La Rioja", "Aragón",
  "Comunidad de Madrid",
  "Castilla y León",
  "Castilla-La Mancha",
  "Extremadura",
  "Cataluña",
  "Comunidad Valenciana",
  "Illes Balears",
  "Andalucía",
  "Región de Murcia",
  "Ciudad Autónoma de Ceuta",
  "Ciudad Autónoma de Melilla",
  "Canarias"
))[, se.HH060 := NULL][order(Region)] %>% print()

## is HH040G anual?



for (i in unique(ecv$DB040)) {
  ecv[DB040 == i & HY040G >= percentile_2_ccaa_tabla[DB040 == i, HH060], status := "CASERO"]
  ecv[HH021 == 1 | HH021 == 2, status := "PROPIETARIO"]
  ecv[HH021 == 3, status := "INQUILINO"]
  ecv[status %in% c("CASERO", "PROPIETARIO", "INQUILINO"), STATUS := "RESTO"]
}

ecv_sv <- svydesign(ids = ~1, data = ecv, weights = ecv$DB090)

svytable(~status, ecv_sv) %>% print()
