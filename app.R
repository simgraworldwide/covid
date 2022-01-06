# Load Packages ====

source("packages.R")

# Data Import and Wrangling ====

## Hospitalisations ----

hosp_opendata.swiss <- read_html("https://opendata.swiss/de/dataset/covid-19-schweiz/resource/81f48dd5-22d5-4244-9b26-1879fdeeff69")
hosp_url <- html_attr(html_elements(hosp_opendata.swiss, "a"), "href")[21]
rm(hosp_opendata.swiss)

hosp_vaccpersons_age_raw <- read_csv(hosp_url)
hosp_vaccpersons_age_raw1 <- hosp_vaccpersons_age_raw %>%
  mutate(
    kw = as.numeric(str_trunc(date, 2, side = "left", ellipsis = "")),
    year = as.numeric(str_trunc(date, 4, side = "right", ellipsis = ""))
  ) %>%
  filter(
    altersklasse_covid19 != "Unbekannt",
    altersklasse_covid19 != "all",
    vaccination_status %in% c("fully_vaccinated", "not_vaccinated")
  ) %>%
  select(
    date,
    year,
    kw,
    altersklasse_covid19,
    vaccination_status,
    entries,
    sumTotal,
    pop,
    inz_entries
  )

### Calculate relative values ----

# population per age cohort
pop_total <- hosp_vaccpersons_age_raw1 %>%
  group_by(year, kw, altersklasse_covid19) %>%
  summarise(pop_total = sum(pop))

# entries per age cohort
entries_total <- hosp_vaccpersons_age_raw1 %>%
  group_by(year, kw, altersklasse_covid19) %>%
  summarise(entries_total = sum(entries))

### Vergleich der Inzidenzen pro Altersklasse und Woche ----

# Hilfstabellen
hosp_vaccpersons_age_unvac <- hosp_vaccpersons_age_raw1 %>%
  filter(vaccination_status == "not_vaccinated") %>%
  mutate(inz_entries_unvac = inz_entries) %>%
  select(date, year, kw, altersklasse_covid19, pop, inz_entries_unvac)
hosp_vaccpersons_age_vac <- hosp_vaccpersons_age_raw1 %>%
  filter(vaccination_status == "fully_vaccinated") %>%
  mutate(inz_entries_vac = inz_entries) %>%
  select(date, year, kw, altersklasse_covid19, pop, inz_entries_vac)

hosp_vaccpersons_age_rel <- hosp_vaccpersons_age_unvac %>%
  left_join(hosp_vaccpersons_age_vac, by = c("date", "year", "kw", "altersklasse_covid19")) %>%
  mutate(inz_entries_rel = inz_entries_unvac/inz_entries_vac)
rm(hosp_vaccpersons_age_unvac, hosp_vaccpersons_age_vac)

### Modell für alle vaxxed ----

hosp_model_fully_vaxxed <- hosp_vaccpersons_age_raw1 %>%
  filter(vaccination_status == "fully_vaccinated") %>%
  left_join(pop_total) %>%
  mutate(entries_all_vaxxed = round((inz_entries * pop_total / 100000), 0)) %>%
  select(
    date,
    year,
    kw,
    altersklasse_covid19,
    entries_all_vaxxed
  )

### Übersichtstabelle ----

hosp_vaccpersons_age <- hosp_vaccpersons_age_raw1 %>%
  left_join(hosp_vaccpersons_age_rel) %>%
  left_join(pop_total) %>%
  left_join(entries_total) %>%
  left_join(hosp_model_fully_vaxxed, by = c("date" = "date", "year" = "year", "kw" = "kw", "altersklasse_covid19" = "altersklasse_covid19")) %>%
  select(-c(
    inz_entries_unvac,
    inz_entries_vac,
    pop.x,
    pop.y)
  ) %>%
  mutate(
    rel_pop = round(pop/pop_total*100, digits = 2),
    rel_entries = round(entries/entries_total*100, digits = 2)
  )

### Vergleich mit Modell für alle vaxxed ----

hosp_comparison <- hosp_vaccpersons_age %>%
  select(date, year, kw, altersklasse_covid19, vaccination_status, entries, entries_all_vaxxed) %>%
  spread(vaccination_status, entries) %>%
  rename(entries_vaxxed = fully_vaccinated, entries_unvaxxed = not_vaccinated) %>%
  mutate(
    entries_total = entries_vaxxed + entries_unvaxxed,
    reduction = round(100 - (entries_all_vaxxed / entries_total * 100), 2)
  ) %>%
  relocate(entries_all_vaxxed, .after = entries_total)

hosp_comparison_total <- hosp_comparison %>%
  group_by(year, kw) %>%
  summarise(
    entries_vaxxed = sum(entries_vaxxed),
    entries_unvaxxed = sum(entries_unvaxxed),
    entries_total = sum(entries_total),
    entries_all_vaxxed = sum(entries_all_vaxxed)
  ) %>%
  mutate(reduction = round(100 - (entries_all_vaxxed / entries_total * 100), 2))

### Tabelle über die Zeit ----

hosp_vaccpersons_age_allweeks_raw <- hosp_vaccpersons_age %>%
  group_by(altersklasse_covid19, vaccination_status) %>%
  summarise(
    entries = sum(entries),
    pop = sum(pop),
    entries_total = sum(entries_total),
    pop_total = sum(pop_total)
  ) %>%
  mutate(
    rel_pop = round(pop/pop_total*100, digits = 2),
    rel_entries = round(entries/entries_total*100, digits = 2),
    factor = round(rel_entries/rel_pop, digits = 2)
  )

hosp_vaccpersons_age_allweeks_rel <- hosp_vaccpersons_age_allweeks_raw %>%
  select(vaccination_status, altersklasse_covid19, factor) %>%
  spread(vaccination_status, factor) %>%
  rename(factor_unvac = not_vaccinated, factor_vac = fully_vaccinated) %>%
  mutate(factor_rel = factor_unvac/factor_vac)

hosp_vaccpersons_age_allweeks <- hosp_vaccpersons_age_allweeks_raw %>%
  left_join(hosp_vaccpersons_age_allweeks_rel) %>%
  select(-c(factor_unvac, factor_vac))
rm(hosp_vaccpersons_age_allweeks_raw, hosp_vaccpersons_age_allweeks_rel)

## Deaths ----

death_opendata.swiss <- read_html("https://opendata.swiss/de/dataset/covid-19-schweiz/resource/a631c80f-89ac-4032-bc23-425a07717692")
death_url <- html_attr(html_elements(hosp_opendata.swiss, "a"), "href")[21]
rm(death_opendata.swiss)

death_vaccpersons_age_raw <- read_csv(death_url)
death_vaccpersons_age <- death_vaccpersons_age_raw %>%
  mutate(
    kw = as.numeric(str_trunc(date, 2, side = "left", ellipsis = "")),
    year = as.numeric(str_trunc(date, 4, side = "right", ellipsis = ""))) %>%
  # relocate(kw, .after = date) %>%
  filter(
    altersklasse_covid19 != "Unbekannt",
    altersklasse_covid19 != "all",
    vaccination_status %in% c("fully_vaccinated", "not_vaccinated")
  ) %>%
  select(
    date,
    year,
    kw,
    altersklasse_covid19,
    vaccination_status,
    entries,
    sumTotal,
    pop,
    # comp,
    inz_entries
  )

## Tests ----

tests_opendata.swiss <- read_html("https://opendata.swiss/de/dataset/covid-19-schweiz/resource/a82fa311-ce86-4057-a429-a972bd6d2de6")
tests_url <- html_attr(html_elements(tests_opendata.swiss, "a"), "href")[21]
rm(tests_opendata.swiss)

tests_raw <- read_csv(tests_url)
tests <- tests_raw %>%
  filter(geoRegion == "CH") %>%
  select(
    datum,
    tests = entries,
    entries_pos,
    entries_neg,
    pos_anteil,
    pop
    ) %>%
  mutate(
    tests_7dsum = RcppRoll::roll_sum(tests, 7, fill = NA, align = "right"),
    tests_14dsum = RcppRoll::roll_sum(tests, 14, fill = NA, align = "right"),
    pos_7dsum = RcppRoll::roll_sum(entries_pos, 7, fill = NA, align = "right"),
    pos_14dsum = RcppRoll::roll_sum(entries_pos, 14, fill = NA, align = "right"),
    neg_7dsum = RcppRoll::roll_sum(entries_neg, 7, fill = NA, align = "right"),
    neg_14dsum = RcppRoll::roll_sum(entries_neg, 14, fill = NA, align = "right"),
    pos_7davg = round((pos_7dsum / 7), digits = 0),
    pos_14davg = round((pos_14dsum / 14), digits = 0),
    pos_anteil_7davg = round((pos_7dsum / tests_7dsum * 100), digits = 1),
    year = year(datum),
    kw = week(datum),
    date = paste0(year, str_pad(kw, 2, pad = "0"))
  ) %>%
  relocate(pos_anteil_7davg, .after = pos_anteil) %>%
  relocate(date, year, kw, .after = datum) %>%
  select(
    -tests_14dsum,
    -pos_14dsum,
    -neg_14dsum
  )

## Quarantine/Isolation ----

# iso_opendata.swiss <- read_html("https://opendata.swiss/de/dataset/covid-19-schweiz/resource/4b080cca-8400-49b7-8679-9afe8f182c6d")
# iso_url <- html_attr(html_elements(iso_opendata.swiss, "a"), "href")[21]
#
# iso_raw <- read_csv(iso_url)

# User Interface ====

ui <- fluidPage








# Server ====

server <-







# Application ====

shinyApp(ui = ui, server = server)
