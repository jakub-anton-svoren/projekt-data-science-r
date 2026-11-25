# ============================================================
# Projekt: Analýza vzťahu nezamestnanosti a rastu HDP
# Úloha: Sťahovanie dát z Eurostatu a World Bank
# Obdobie: 2010–2024
# Územie: EÚ-27
# ============================================================


# 1. Načítanie potrebných knižníc

library(eurostat)
library(WDI)
library(dplyr)
library(tidyr)
library(ggplot2)


# 2. Definovanie krajín EÚ-27

eu27 <- c(
  "AT", "BE", "BG", "HR", "CY", "CZ", "DK",
  "EE", "FI", "FR", "DE", "EL", "HU", "IE",
  "IT", "LV", "LT", "LU", "MT", "NL", "PL",
  "PT", "RO", "SK", "SI", "ES", "SE"
)


# 3. Definovanie časového obdobia

start_year <- 2010
end_year <- 2024

# 4. Stiahnutie údajov o nezamestnanosti z Eurostatu

unemployment_raw <- get_eurostat(
  id = "une_rt_a",
  time_format = "num"
)

# Výber krajín, rokov a požadovaných kategórií
unemployment <- unemployment_raw %>%
  filter(
    geo %in% eu27,
    TIME_PERIOD >= start_year,
    TIME_PERIOD <= end_year,
    age == "Y15-74",
    sex == "T",
    unit == "PC_ACT"
  )

# Kontrola výsledných dát
head(unemployment)

# 5. Výber a premenovanie premenných

unemployment <- unemployment %>%
  select(
    geo,
    TIME_PERIOD,
    values
  ) %>%
  rename(
    iso2c = geo,
    year = TIME_PERIOD,
    unemp_rate = values
  )

# Kontrola upraveného datasetu
head(unemployment)

# Kontrola počtu pozorovaní
nrow(unemployment)

# Kontrola počtu krajín
n_distinct(unemployment$iso2c)

# Kontrola rokov
range(unemployment$year)

# Kontrola chýbajúcich hodnôt
sum(is.na(unemployment$unemp_rate))

# 6. Stiahnutie údajov o HDP z Eurostatu

gdp_raw <- get_eurostat(
  id = "nama_10_gdp",
  time_format = "num"
)

# 7. Výber reálneho rastu HDP

gdp <- gdp_raw %>%
  filter(
    freq == "A",
    na_item == "B1GQ",
    unit == "CLV_PCH_PRE",
    geo %in% eu27,
    TIME_PERIOD >= start_year,
    TIME_PERIOD <= end_year
  ) %>%
  select(
    geo,
    TIME_PERIOD,
    values
  ) %>%
  rename(
    iso2c = geo,
    year = TIME_PERIOD,
    gdp_growth = values
  )

# Kontrola výsledných dát
head(gdp)

# Kontrola rozsahu
nrow(gdp)
n_distinct(gdp$iso2c)
range(gdp$year)
sum(is.na(gdp$gdp_growth))

# 8. Stiahnutie údajov o vzdelaní z Eurostatu

education_raw <- get_eurostat(
  id = "edat_lfse_03",
  time_format = "num"
)

# 9. Výber a úprava údajov o terciárnom vzdelaní

education <- education_raw %>%
  filter(
    geo %in% eu27,
    TIME_PERIOD >= start_year,
    TIME_PERIOD <= end_year,
    age == "Y25-64",
    sex == "T",
    isced11 == "ED5-8",
    unit == "PC"
  ) %>%
  select(
    geo,
    TIME_PERIOD,
    values
  ) %>%
  rename(
    iso2c = geo,
    year = TIME_PERIOD,
    tertiary_edu = values
  )

# Kontrola výsledných dát
head(education)

# Kontrola kvality
sum(is.na(education$tertiary_edu))

# 11. Definovanie indikátorov World Bank

wb_indicators <- c(
  investment_gdp = "NE.GDI.TOTL.ZS",
  fdi_inflows = "BX.KLT.DINV.WD.GD.ZS",
  inflation = "FP.CPI.TOTL.ZG",
  industry_share = "NV.IND.TOTL.ZS",
  old_dependency = "SP.POP.DPND.OL",
  activity_rate = "SL.TLF.CACT.ZS"
)

# 12. Stiahnutie údajov z World Bank

wb_raw <- WDI(
  country = "all",
  indicator = wb_indicators,
  start = start_year,
  end = end_year,
  extra = TRUE
)

# 13. Výber krajín EÚ-27

wb_data <- wb_raw %>%
  filter(iso2c %in% c(
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK",
    "EE", "FI", "FR", "DE", "GR", "HU", "IE",
    "IT", "LV", "LT", "LU", "MT", "NL", "PL",
    "PT", "RO", "SK", "SI", "ES", "SE"
  ))

head(wb_data)
dim(wb_data)

# 14. Zjednotenie kódov krajín

unemployment <- unemployment %>%
  mutate(iso2c = if_else(iso2c == "EL", "GR", iso2c))

gdp <- gdp %>%
  mutate(iso2c = if_else(iso2c == "EL", "GR", iso2c))

education <- education %>%
  mutate(iso2c = if_else(iso2c == "EL", "GR", iso2c))

# 15. Príprava World Bank dát na spojenie

wb_data_clean <- wb_data %>%
  select(
    iso2c,
    year,
    investment_gdp,
    fdi_inflows,
    inflation,
    industry_share,
    old_dependency,
    activity_rate
  )

# 16. Spojenie všetkých dátových zdrojov

eu27_data <- unemployment %>%
  left_join(gdp, by = c("iso2c", "year")) %>%
  left_join(education, by = c("iso2c", "year")) %>%
  left_join(wb_data_clean, by = c("iso2c", "year")) %>%
  arrange(iso2c, year)

# 17. Kontrola výsledného datasetu

dim(eu27_data)

head(eu27_data)

colSums(is.na(eu27_data))

# Kontrola duplicít
eu27_data %>%
  count(iso2c, year) %>%
  filter(n > 1)

# Kontrola krajín a rokov
n_distinct(eu27_data$iso2c)
range(eu27_data$year)

# 18. Vytvorenie oneskorenej premennej rastu HDP

eu27_data <- eu27_data %>%
  group_by(iso2c) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(gdp_growth_lag1 = lag(gdp_growth, n = 1)) %>%
  ungroup()

# 19. Uloženie výsledného datasetu

datum <- Sys.Date()

# CSV snapshot s dátumom stiahnutia
write.csv(
  eu27_data,
  file = file.path(
    "data",
    paste0("eu27_nezamestnanost_", datum, ".csv")
  ),
  row.names = FALSE,
  na = ""
)

# Aktuálna verzia datasetu vo formáte RDS
saveRDS(
  eu27_data,
  file = file.path("data", "eu27_nezamestnanost_latest.rds")
)