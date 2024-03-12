# Fichero principal 2021
library(magrittr)
library(readr)
library(data.table)
library(dplyr)
library(survey)
library(DescTools) # Para calcular la mediana ponderada

rm(list = ls()) # clean enviroment to avoid ram bottlenecks

#### OBTAINING DATA ()
hogar1 <- read.csv("data/ECV/esudb22h.csv")
hogar2 <- read.csv("data/ECV/esudb22d.csv")

# unir
ecv <- merge(hogar1, hogar2, by.x = "HB030", by.y = "DB030", all = T)

# cambiar los nombres a las CCAA
ecv <- ecv %>%
  mutate(
    ccaa = case_when(
      DB040 == "ES11" ~ "Galicia",
      DB040 == "ES12" ~ "Principado de Asturias",
      DB040 == "ES13" ~ "Cantabria",
      DB040 == "ES21" ~ "País Vasco",
      DB040 == "ES22" ~ "Comunidad Foral de Navarra",
      DB040 == "ES23" ~ "La Rioja",
      DB040 == "ES24" ~ "Aragón",
      DB040 == "ES30" ~ "Comunidad de Madrid",
      DB040 == "ES41" ~ "Castilla y León",
      DB040 == "ES42" ~ "Castilla-La Mancha",
      DB040 == "ES43" ~ "Extremadura",
      DB040 == "ES51" ~ "Cataluña",
      DB040 == "ES52" ~ "Comunidad Valenciana",
      DB040 == "ES53" ~ "Illes Balears",
      DB040 == "ES61" ~ "Andalucía",
      DB040 == "ES62" ~ "Región de Murcia",
      DB040 == "ES63" ~ "Ciudad Autónoma de Ceuta",
      DB040 == "ES64" ~ "Ciudad Autónoma de Melilla",
      DB040 == "ES70" ~ "Canarias",
      DB040 == "ESZZ" ~ "Extra-Regio",
      T ~ "Nada"
    )
  )

codes <- c(
  "ES11",
  "ES12",
  "ES13",
  "ES21",
  "ES22",
  "ES23",
  "ES24",
  "ES30",
  "ES41",
  "ES42",
  "ES43",
  "ES51",
  "ES52",
  "ES53",
  "ES61",
  "ES62",
  "ES63",
  "ES64",
  "ES70",
  "ESZZ"
)
#### calcular percentile 2% de pago de alquiler por ccaa

percentile_2_ccaa_tabla <- ecv %>%
  filter(HH021 == 3) %>%
  group_by(ccaa) %>%
  summarize(percentile_2_ccaa = Quantile(HH060 * 12, weights = DB090, probs = 0.02, na.rm = TRUE))

ecv <- merge(ecv, percentile_2_ccaa_tabla, by = "ccaa", all = TRUE)

# hacer perfiles (caseros, propietarios, inquilinos)
ecv <- ecv %>%
  mutate(
    perfil =
      case_when(
        HY040G >= percentile_2_ccaa ~ "CASERO",
        HH021 == 1 | HH021 == 2 ~ "PROPIETARIO",
        HH021 == 3 ~ "INQUILINO",
        TRUE ~ "RESTO"
      )
  )



# cambiar nombres de variables para homogeneizar
ecv <- ecv %>%
  mutate(
    RENTAD = vhRentaa,
    RENTAB = HY010,
    RENTA_ALQ = HY040G,
    RENTAB_NOAL = HY010 - HY040G,
    FACTORCAL = DB090
  )

dt <- ecv

setDT(dt)

dt[, CASERO := 0][perfil == "CASERO", CASERO := 1][, CASERO := factor(CASERO)]
dt[, INQUILINO := 0][perfil == "INQUILINO", INQUILINO := 1][, INQUILINO := factor(INQUILINO)]
dt[, PROPIETARIO := 0][perfil == "PROPIETARIO", PROPIETARIO := 1][, PROPIETARIO := factor(PROPIETARIO)]
dt[, RESTO := 0][perfil == "RESTO", RESTO := 1][, RESTO := factor(RESTO)]


## Prepare survey object from dt and set income cuts for quantiles dynamically
dt_sv <- svydesign(ids = ~1, data = dt, weights = dt$FACTORCAL) # muestra con coeficientes de elevación

quantiles <- seq(.25, .75, .25) # cortes
quantiles_renta <- Quantile(dt$RENTAB, c(.25, 0.5, 0.75), na.rm = FALSE, weight = dt$FACTORCAL)

quantiles_renta <- svyquantile(~RENTAB, design = dt_sv, quantiles = quantiles, ci = FALSE)$RENTAB # rentas asociadas a cores
table_names <- c(
  paste0("hasta ", quantiles_renta[, "0.25"]),
  paste0("entre ", quantiles_renta[, "0.25"], " y ", quantiles_renta[, "0.5"]),
  paste0("entre ", quantiles_renta[, "0.5"], " y ", quantiles_renta[, "0.75"]),
  paste0("mas de ", quantiles_renta[, "0.75"])
)


# TABLA 1-------------------------------------------------------------------------------
caseros25 <- svytable(~perfil, subset(dt_sv, perfil == "CASERO" & RENTAB < quantiles_renta[, "0.25"])) %>% as.data.frame()
caseros25to50 <- svytable(~perfil, subset(dt_sv, perfil == "CASERO" & RENTAB > quantiles_renta[, "0.25"] & RENTAB < quantiles_renta[, "0.5"])) %>% as.data.frame()
caseros50to75 <- svytable(~perfil, subset(dt_sv, perfil == "CASERO" & RENTAB > quantiles_renta[, "0.5"] & RENTAB < quantiles_renta[, "0.75"])) %>% as.data.frame()
caseros75 <- svytable(~perfil, subset(dt_sv, perfil == "CASERO" & RENTAB > quantiles_renta[, "0.75"])) %>% as.data.frame()

caseros <- bind_rows(
  caseros25 %>% mutate(Interval = "caseros25"),
  caseros25to50 %>% mutate(Interval = "caseros25to50"),
  caseros50to75 %>% mutate(Interval = "caseros50to75"),
  caseros75 %>% mutate(Interval = "caseros75")
) %>%
  filter(perfil == "CASERO") %>%
  select(-perfil, -Interval) %>%
  mutate("caseros %" = round(Freq / sum(Freq) * 100, 2))

inquilinos25 <- svytable(~perfil, subset(dt_sv, perfil == "INQUILINO" & RENTAB < quantiles_renta[, "0.25"])) %>% as.data.frame()
inquilinos25to50 <- svytable(~perfil, subset(dt_sv, perfil == "INQUILINO" & RENTAB > quantiles_renta[, "0.25"] & RENTAB < quantiles_renta[, "0.5"])) %>% as.data.frame()
inquilinos50to75 <- svytable(~perfil, subset(dt_sv, perfil == "INQUILINO" & RENTAB > quantiles_renta[, "0.5"] & RENTAB < quantiles_renta[, "0.75"])) %>% as.data.frame()
inquilinos75 <- svytable(~perfil, subset(dt_sv, perfil == "INQUILINO" & RENTAB > quantiles_renta[, "0.75"])) %>% as.data.frame()

inquilinos <- bind_rows(
  inquilinos25 %>% mutate(Interval = "inquilinos25"),
  inquilinos25to50 %>% mutate(Interval = "inquilinos25to50"),
  inquilinos50to75 %>% mutate(Interval = "inquilinos50to75"),
  inquilinos75 %>% mutate(Interval = "inquilinos75")
) %>%
  filter(perfil == "INQUILINO") %>%
  select(-perfil, -Interval) %>%
  mutate("inquilinos %" = round(Freq / sum(Freq) * 100, 2))

final_table <- cbind(table_names, caseros, inquilinos)
colnames(final_table) <- c("table_names", "caseros", "caseros %", "inquilinos", "inquilinos %")

# TABLA 2--------------------------------------------------------------------
#####
# esto solo control para una parte de la tabla siguiente, pero dejamos la otra forma

renta_mediana <- svyby(~RENTAB, ~perfil, dt_sv,
  svyquantile,
  quantiles = .5, keep.names = T, na.rm = T
)[-4, -3]


renta_mediana2 <- svyby(~RENTAB_NOAL, ~perfil, dt_sv,
  svyquantile, 0.5,
  keep.names = T, na.rm = T
)[-c(1, 3, 4), -3]


renta_mediana2$perfil <- sub("CASERO", "CASERO_SIN", renta_mediana2$perfil)
rownames(renta_mediana2)[rownames(renta_mediana2) == "CASERO"] <- "CASERO_SIN"
colnames(renta_mediana2)[colnames(renta_mediana2) == "RENTAB_NOAL"] <- "RENTAB"


renta_mediana <- rbind(renta_mediana, renta_mediana2)
renta_mediana <- renta_mediana[order(-renta_mediana$RENTAB), ]
#############
median_renta_inquili <- svyquantile(~RENTAB, subset(dt_sv, INQUILINO == 1), quantiles = .5, ci = FALSE)$RENTAB[1]
median_renta_caseros <- svyquantile(~RENTAB, subset(dt_sv, CASERO == 1), quantiles = .5, ci = FALSE)$RENTAB[1]
median_renta_propsin <- svyquantile(~RENTAB, subset(dt_sv, PROPIETARIO == 1), quantiles = .5, ci = FALSE)$RENTAB[1]
median_renta_totalpo <- svyquantile(~RENTAB, dt_sv, quantiles = .5, ci = FALSE)$RENTAB[1]
median_renta_caseros_NOAL <- svyquantile(~RENTAB_NOAL, subset(dt_sv, CASERO == 1), quantiles = .5, ci = FALSE)$RENTAB_NOAL[1]

mean_renta_inquili <- svymean(~RENTAB, subset(dt_sv, INQUILINO == 1))[1]
mean_renta_caseros <- svymean(~RENTAB, subset(dt_sv, CASERO == 1))[1]
mean_renta_caseros_NOAL <- svymean(~RENTAB_NOAL, subset(dt_sv, CASERO == 1))[1]
mean_renta_propsin <- svymean(~RENTAB, subset(dt_sv, PROPIETARIO == 1))[1]
mean_renta_totalpo <- svymean(~RENTAB, dt_sv)[1]

medians <- c(median_renta_caseros, median_renta_caseros_NOAL, median_renta_inquili, median_renta_propsin, median_renta_totalpo)
means <- c(mean_renta_caseros, mean_renta_caseros_NOAL, mean_renta_inquili, mean_renta_propsin, mean_renta_totalpo)
renta_table <- cbind(c("caseros", "caseros (excluye rentas alq.)", "inquilinos", "prop. sin rentas alq.", "todos"), means, medians)
colnames(renta_table) <- c("tipo", "media", "mediana")

# Calculate frequencies with survey weights, only including cases where the variable equals 1
CASERO_FREQ <- svytotal(~CASERO, design = dt_sv, subset = CASERO == 1)[-1]
INQUILINO_FREQ <- svytotal(~INQUILINO, design = dt_sv, subset = INQUILINO == 1)[-1]
PROPIETARIO_FREQ <- svytotal(~PROPIETARIO, design = dt_sv, subset = PROPIETARIO == 1)[-1]
RESTO_FREQ <- svytotal(~RESTO, design = dt_sv, subset = RESTO == 1)[-1]

# Create a data frame
reg_tenencia <- data.frame(
  Category = c("CASERO", "INQUILINO", "PROPIETARIO", "RESTO"),
  Frequency = c(CASERO_FREQ, INQUILINO_FREQ, PROPIETARIO_FREQ, RESTO_FREQ)
)

reg_tenencia <- reg_tenencia %>%
  mutate(Percentage = round((Frequency / sum(Frequency)) * 100, 2))


# comprobar
reg_tenencia_comprobar <- as.data.frame(svytotal(~perfil, dt_sv))[-2] %>%
  mutate(porcentaje = (total / sum(total)) * 100)



# Check output
list(final_table, renta_table, reg_tenencia_comprobar) %>% print()
