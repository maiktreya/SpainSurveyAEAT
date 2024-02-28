library(data.table)
library(survey)
library(mitools)
library(magrittr)

dt <- svy_dt <- manual_mean <- list()
replication_weights <- fread("data/replicate_weights_2020.csv")

for (i in 1:5) dt[[i]] <- paste0("data/databol_2020_csv/databol", i, ".csv") %>% fread()

# METHOD1 replicated (mitools)
svy_rep_dt <- svrepdesign(
    repweights = replication_weights,
    scale = 1,
    rscale = ncol(replication_weights),
    weights = ~facine3,
    data = imputationList(dt),
    type = "bootstrap",
    combined.weights = TRUE
)
replicated_mean <- with(svy_rep_dt, svymean(~renthog19_eur20)) %>% MIcombine()

# METHOD2 manual replication (survey)
for (i in 1:5) {
    dt[[i]] <- paste0("data/databol_2020_csv/databol", i, ".csv") %>% fread()
    pipe <- as.data.frame(dt[[i]])
    svy_dt[[i]] <- svydesign(ids = ~1, data = pipe, weights = pipe$facine3)
    manual_mean[[i]] <- svymean(~renthog19_eur20, svy_dt[[i]])
}

# METHOD3 without replication
dt_mean <- (dt[[1]] + dt[[2]] + dt[[3]] + dt[[4]] + dt[[5]]) / 5
dt_mean <- data.frame(dt_mean)
svy_mean <- svydesign(ids = ~1, data = dt_mean, weights = dt_mean$facine3)
standard_mean <- svymean(~renthog19_eur20, svy_mean)


# Three methods return same results :)

replicated_mean %>% print()
manual_mean %>%
    as.numeric() %>%
    mean() %>%
    print()
standard_mean %>% print()
