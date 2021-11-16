# Load packages ----
source(here("packages.R"))

# Plot COVID related hospitalisations, grouped by vaccination status AND age ----

## Load and wrangle data ----
hosp_vaccpersons_age_raw <- read_csv("https://www.covid19.admin.ch/api/data/20211115-ew84xikb/sources/COVID19Hosp_vaccpersons_AKL10_w.csv")
hosp_vaccpersons_age <- hosp_vaccpersons_age_raw %>%
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
