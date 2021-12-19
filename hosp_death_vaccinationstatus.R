# Load packages ----
source("packages.R")


# Plot COVID related hospitalisations, grouped by vaccination status AND age ----


## Load and wrangle data ----
hosp_vaccpersons_age_raw <- read_csv("https://www.covid19.admin.ch/api/data/20211217-drih76wp/sources/COVID19Hosp_vaccpersons_AKL10_w.csv")
hosp_vaccpersons_age_raw1 <- hosp_vaccpersons_age_raw %>%
  mutate(kw = as.numeric(str_trunc(date, 2, side = "left", ellipsis = ""))) %>%
  relocate(kw, .after = date) %>%
  # group_by(altersklasse_covid19, kw) %>%
  # mutate(comp = max(inz_entries)/min(inz_entries)) %>%
  filter(
    kw >= 30, # use only data after week 29
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

# Übersichtstabelle
hosp_vaccpersons_age <- hosp_vaccpersons_age_raw1 %>%
  left_join(hosp_vaccpersons_age_rel) %>%
  left_join(pop_total) %>%
  left_join(entries_total) %>%
  select(-c(inz_entries_unvac, inz_entries_vac)) %>%
  mutate(
    rel_pop = round(pop/pop_total*100, digits = 2),
    rel_entries = round(entries/entries_total*100, digits = 2)
  )

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
ggplot(
  data = hosp_vaccpersons_age,
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
    linetype = "Impfstatus") +
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
  facet_wrap(altersklasse_covid19 ~ ., scales = "free") +
  # scale_color_brewer(palette = "Dark2") +
  # scale_color_discrete() +
  scale_colour_manual(values = c(
    "not_vaccinated" = "#B01657",
    "fully_vaccinated" = "#00797B"
    )) +
  # theme(legend.position = c(0.65, 0.14)) +
  theme_minimal()


# Plot COVID related deaths, grouped by vaccination status AND age ----


## Load and wrangle data ----
death_vaccpersons_age_raw <- read_csv("https://www.covid19.admin.ch/api/data/20211115-ew84xikb/sources/COVID19Death_vaccpersons_AKL10_w.csv")
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

