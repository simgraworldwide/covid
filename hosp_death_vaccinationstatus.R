# Load packages ----
source("packages.R")


# Plot COVID related hospitalisations, grouped by vaccination status AND age ----
# Data from https://opendata.swiss/de/dataset/covid-19-schweiz

## Load and wrangle data ----
hosp_opendata.swiss <- read_html("https://opendata.swiss/de/dataset/covid-19-schweiz/resource/81f48dd5-22d5-4244-9b26-1879fdeeff69")
hosp_url <- html_attr(html_elements(hosp_opendata.swiss, "a"), "href")[21]

hosp_vaccpersons_age_raw <- read_csv(hosp_url)
hosp_vaccpersons_age_raw1 <- hosp_vaccpersons_age_raw %>%
  mutate(kw = as.numeric(str_trunc(date, 2, side = "left", ellipsis = ""))) %>%
  relocate(kw, .after = date) %>%
  # group_by(altersklasse_covid19, kw) %>%
  # mutate(comp = max(inz_entries)/min(inz_entries)) %>%
  filter(
    # kw >= 30, # use only data after week 29
    altersklasse_covid19 != "Unbekannt",
    altersklasse_covid19 != "all",
    vaccination_status %in% c("fully_vaccinated", "not_vaccinated")
    ) %>%
  select(
    kw,
    altersklasse_covid19,
    vaccination_status,
    entries,
    sumTotal,
    pop,
    # comp,
    inz_entries
  )


### Calculate relative values ----
# population per age cohort
pop_total <- hosp_vaccpersons_age_raw1 %>%
  group_by(kw, altersklasse_covid19) %>%
  summarise(pop_total = sum(pop))

# entries per age cohort
entries_total <- hosp_vaccpersons_age_raw1 %>%
  group_by(kw, altersklasse_covid19) %>%
  summarise(entries_total = sum(entries))

# Hilfstabellen für die Inzidenzen von geimpften und ungeimpften
hosp_vaccpersons_age_unvac <- hosp_vaccpersons_age_raw1 %>%
  filter(vaccination_status == "not_vaccinated") %>%
  mutate(inz_entries_unvac = inz_entries) %>%
  select(kw, altersklasse_covid19, pop, inz_entries_unvac)

hosp_vaccpersons_age_vac <- hosp_vaccpersons_age_raw1 %>%
  filter(vaccination_status == "fully_vaccinated") %>%
  mutate(inz_entries_vac = inz_entries) %>%
  select(kw, altersklasse_covid19, pop, inz_entries_vac)

# Vergleich der Inzidenzen pro Altersklasse und Woche
hosp_vaccpersons_age_rel <- hosp_vaccpersons_age_unvac %>%
  left_join(hosp_vaccpersons_age_vac, by = c("kw", "altersklasse_covid19")) %>%
  mutate(inz_entries_rel = inz_entries_unvac/inz_entries_vac)

# Modell für alle vaxxed
hosp_model_fully_vaxxed <- hosp_vaccpersons_age_raw1 %>%
  filter(vaccination_status == "fully_vaccinated") %>%
  mutate(entries_all_vaxxed = round((inz_entries * pop / 100000), 0)) %>%
  select(
    kw,
    altersklasse_covid19,
    entries_all_vaxxed
  )

# Übersichtstabelle
hosp_vaccpersons_age <- hosp_vaccpersons_age_raw1 %>%
  left_join(hosp_vaccpersons_age_rel) %>%
  left_join(pop_total) %>%
  left_join(entries_total) %>%
  left_join(hosp_model_fully_vaxxed, by = c("kw" = "kw", "altersklasse_covid19" = "altersklasse_covid19")) %>%
  select(-c(inz_entries_unvac, inz_entries_vac, -pop.x, -pop.y)) %>%
  mutate(
    rel_pop = round(pop/pop_total*100, digits = 2),
    rel_entries = round(entries/entries_total*100, digits = 2)
  )

hc1 <- hosp_vaccpersons_age %>%
  filter(vaccination_status == "fully_vaccinated") %>%
  select(kw, altersklasse_covid19, entries_vaxxed = entries, entries_all_vaxxed)
hc2 <- hosp_vaccpersons_age %>%
  filter(vaccination_status == "not_vaccinated") %>%
  select(kw, altersklasse_covid19, entries_unvaxxed = entries)
hosp_comparison <- hc1 %>%
  left_join(hc2) %>%
  mutate(
    entries_total = entries_vaxxed + entries_unvaxxed,
    reduction = round(100 - (entries_all_vaxxed / entries_total * 100), 2)
    ) %>%
  relocate(entries_all_vaxxed, .after = entries_total)

hosp_comparison_total <- hosp_comparison %>%
  group_by(kw) %>%
  summarise(
    entries_vaxxed = sum(entries_vaxxed),
    entries_unvaxxed = sum(entries_unvaxxed),
    entries_total = sum(entries_total),
    entries_all_vaxxed = sum(entries_all_vaxxed)
  ) %>%
  mutate(reduction = round(100 - (entries_all_vaxxed / entries_total * 100), 2))

# Tabelle über die ZEit
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

# Hilfstabellen
hosp_vaccpersons_age_allweeks_unvac <- hosp_vaccpersons_age_allweeks_raw %>%
  filter(vaccination_status == "not_vaccinated") %>%
  mutate(factor_unvac = factor) %>%
  select(altersklasse_covid19, factor_unvac)

hosp_vaccpersons_age_allweeks_vac <- hosp_vaccpersons_age_allweeks_raw %>%
  filter(vaccination_status == "fully_vaccinated") %>%
  mutate(factor_vac = factor) %>%
  select(altersklasse_covid19, factor_vac)

hosp_vaccpersons_age_allweeks_rel <- hosp_vaccpersons_age_allweeks_unvac %>%
  left_join(hosp_vaccpersons_age_allweeks_vac) %>%
  mutate(factor_rel = factor_unvac/factor_vac)

# Übersichtstabelle über Zeit
hosp_vaccpersons_age_allweeks <- hosp_vaccpersons_age_allweeks_raw %>%
  left_join(hosp_vaccpersons_age_allweeks_rel) %>%
  select(-c(factor_unvac, factor_vac))


## Create plots ----
hosp_vaccpersons_age %>%
  filter(kw >= 30) %>%
  ggplot(
    # data = hosp_vaccpersons_age,
    mapping = aes(
      x = kw,
      y = inz_entries,
      group = vaccination_status,
      colour = vaccination_status)) +
    labs(
      title = "Hospitalisierungsinzidenz nach Alter und Impfstatus",
      y = "Hospitalisierungen/100'000",
      x = "Kalenderwoche Jahr 2021",
      colour = "Impfstatus",
      linetype = "Impfstatus"
      ) +
    # geom_point() +
    geom_line(aes(
      colour = vaccination_status,
      linetype = vaccination_status
      )) +
    # geom_line(
    #   data = hosp_vaccpersons_age[!is.na(hosp_vaccpersons_age$inz_entries_rel),],
    #   mapping = aes(
    #     x = kw,
    #     y = inz_entries_rel
    #     )
    #   ) +
    facet_wrap(
      altersklasse_covid19 ~ .
      # scales = "free"
      ) +
    # scale_color_brewer(palette = "Dark2") +
    # scale_color_discrete() +
    scale_colour_manual(values = c(
      "fully_vaccinated" = "#00797B",
      "not_vaccinated" = "#B01657"
      )) +
    # theme(legend.position = c(0.65, 0.14)) +
    theme_minimal()

hosp_vaccpersons_age %>%
  filter(kw %in% c((max(kw)-11):max(kw))) %>%
  mutate(id = "all_vaccinated") %>%
  ggplot(
    mapping = aes(
      x = altersklasse_covid19,
      y = entries)
    ) +
  labs(
    title = "Hospitalisierungen nach Alter und Impfstatus mit theoretischen Hospitalisierungen bei Durchimpfung",
    y = "Hospitalisierungen",
    x = "Altersklasse",
    fill = "Impfstatus",
    alpha = "Durchimpfung"
    ) +
  geom_col(aes(
    fill = vaccination_status
  )) +
  geom_col( # mässig elegant...
    aes(
      y = (entries_all_vaxxed / 2), # Daten werden sonst doppelt hinzugefügt, da bei beiden Impfstatus dabei
      fill = id
      ),
    alpha = 0.4
    ) +
  facet_wrap(
    vars(kw),
    nrow = 3
    ) +
  scale_fill_manual(values = c(
    "not_vaccinated" = "#B01657",
    "fully_vaccinated" = "#00797B",
    "all_vaccinated" = "#000000"
  )) +
  # theme(legend.position = c(0.65, 0.14)) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45))

hosp_model_fully_vaxxed %>%
  filter(kw %in% c((max(kw)-11):max(kw))) %>%
  ggplot(
    mapping = aes(
      x = altersklasse_covid19,
      y = entries_all_vaxxed
      )) +
  labs(
    title = "Theoretische Hospitalisierungen bei Durchimpfung nach Alter",
    y = "Hospitalisierungen",
    x = "Altersklasse"
    ) +
  geom_col() +
  facet_wrap(
    vars(kw),
    nrow = 3
    # ncol = 4
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 45))


# Plot COVID related deaths, grouped by vaccination status AND age ----


## Load and wrangle data ----
death_opendata.swiss <- read_html("https://opendata.swiss/de/dataset/covid-19-schweiz/resource/a631c80f-89ac-4032-bc23-425a07717692")
death_url <- html_attr(html_elements(hosp_opendata.swiss, "a"), "href")[21]

death_vaccpersons_age_raw <- read_csv(death_url)
death_vaccpersons_age <- death_vaccpersons_age_raw %>%
  mutate(kw = as.numeric(str_trunc(date, 2, side = "left", ellipsis = ""))) %>%
  relocate(kw, .after = date) %>%
  # group_by(altersklasse_covid19, kw) %>%
  # mutate(comp = max(inz_entries)/min(inz_entries)) %>%
  filter(
    altersklasse_covid19 != "Unbekannt",
    altersklasse_covid19 != "all",
    vaccination_status %in% c("fully_vaccinated", "not_vaccinated")
  ) %>%
  select(
    kw,
    altersklasse_covid19,
    vaccination_status,
    entries,
    sumTotal,
    pop,
    # comp,
    inz_entries
  )

## Create plots ----
ggplot(
  data = death_vaccpersons_age,
  mapping = aes(
    x = kw,
    y = inz_entries,
    group = vaccination_status,
    colour = vaccination_status)) +
  labs(
    title = "Sterbeinzidenz nach Alter und Impfstatus",
    y = "Todesfälle/100'000",
    x = "Kalenderwoche Jahr 2021",
    colour = "Impfstatus",
    linetype = "Impfstatus") +
  # geom_point() +
  geom_line(aes(
    colour = vaccination_status,
    linetype = vaccination_status
  )) +
  facet_wrap(altersklasse_covid19 ~ ., scales = "free") +
  # scale_color_brewer(palette = "Dark2") +
  # scale_color_discrete() +
  scale_colour_manual(values = c(
    "not_vaccinated" = "#B01657",
    "fully_vaccinated" = "#00797B"
  )) +
  # theme(legend.position = c(0.65, 0.14)) +
  theme_minimal()


## Create plot to compare relative status and relative entries
hosp_vaccpersons_age_allweeks %>% gather(condition, measurement, rel_pop:rel_entries, factor_key = TRUE) %>%
  arrange(altersklasse_covid19) %>%
  # mutate(condition1 = case_when(
  #   condition = rel_entries ~ "Anteil Hospitalisierungen",
  #   condition = rel_pop ~ "Anteil Bevölkerung"
  #   )
  # ) %>%
  # as.character(condition) %>%
  mutate(condition = ifelse(condition == "rel_entries", "Anteile Hospitalisierungen", "Anteile Bevölkerung")) %>%
  mutate(xvar = str_c(altersklasse_covid19, condition, sep = ": ")) %>%
  ggplot() +
  labs(
    title = "Relative Anteile in der Bevölkerung und in den Hospitalisierungen nach Impfstatus",
    y = "Anteile in Prozent",
    x = "",
    fill = "Impfstatus"
    ) +
  geom_bar(
    mapping = aes(
      # group = condition,
      # x = altersklasse_covid19,
      x = xvar,
      y = measurement,
      fill = vaccination_status
    ),
    # position = "dodge",
    stat = "identity",
    width = 0.7
    ) +
  scale_fill_manual(values = c(
    "not_vaccinated" = "#B01657",
    "fully_vaccinated" = "#00797B"
  )) +
  theme_minimal() +
  theme(axis.text.x = element_text(
    angle = 90))

