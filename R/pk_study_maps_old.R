library(tidyverse)
library(readxl)
library(terra)
library(tidyterra)
library(geodata)
library(tidyr)

# get spatial data
# include other countries in admin 0 to make map look sensible
admin_0 <- gadm(
  country = c("IDN", "MYS", "PHL", "THA", "KHM", "VNM"),
  level = 0,
  path = "data/downloads/",
  resolution = 2
)

admin_1 <- gadm(
  country = c("IDN", "MYS", "PHL"),
  level = 1,
  path = "data/downloads/",
  resolution = 2
)

admin_2 <- gadm(
  country = c("IDN", "MYS", "PHL"),
  level = 2,
  path = "data/downloads/",
  resolution = 2
)

admin_3 <- gadm(
  country = c("IDN"), # doesn't exist for Malaysia or Philippines
  level = 3,
  path = "data/downloads/",
  resolution = 2
)

# organise records data

# NB: if sheet is updated must update range or will miss data

study_areas <- admin_1 |>
  as.data.frame() |>
  as_tibble() |>
  select(COUNTRY, NAME_1) |>
  arrange(COUNTRY, NAME_1) |>
  rename(
    country = COUNTRY,
    name = NAME_1
  ) |>
  mutate(
    study_area = case_when(
      country == "Malaysia" ~ TRUE,
      name == "Palawan" ~ TRUE,
      name == "Kalimantan Barat" ~ TRUE,
      name == "Kalimantan Selatan" ~ TRUE,
      name == "Kalimantan Tengah" ~ TRUE,
      name == "Kalimantan Timur" ~ TRUE,
      name == "Kalimantan Utara" ~ TRUE,
      name == "Sumatera Barat" ~ TRUE,
      name == "Sumatera Selatan" ~ TRUE,
      name == "Sumatera Utara" ~ TRUE,
      name == "Aceh" ~ TRUE,
      name == "Jambi" ~ TRUE,
      name == "Lampung" ~ TRUE,
      name == "Riau" ~ TRUE,
      .default = FALSE
    )
  )


dat <- read_excel(
  path = "data/Study locations.xlsx",
  #sheet = "Study locations",
  range = "B5:H50",
  na = "N/A"
) |>
  # tidy column names
  rename(
    country = `Country total`,
    region = `Region total`,
    admin1 = `State or Province`,
    admin2 = `Division or Regency`,
    admin3 = District,
    n = `Number of studies`
  ) |>
  # get rid of subcounts in parenthesess to tidy country names
  mutate(
    country = sub(
      pattern = "\\\r.*",
      replacement = "",
      x = country
    ),
    region = sub(
      pattern = "\\\r.*",
      replacement = "",
      x = region
    )
  ) |>
  # fill country down col and up for Indonesia as
  # phantom empty non merged row above
  # this will need checking if sheet is updated
  fill(
    country,
    .direction = "down"
  ) |>
  fill(
    country,
    .direction = "up"
  ) |>
  fill(
    region,
    .direction = "down"
  ) |>
  select(-Reference) |>
  filter(!is.na(n)) |>
  # collapse wider admin levels into two cols with data and get rid of NAs
  pivot_longer(
    cols = starts_with("admin"),
    names_to = "admin_level",
    values_to = "name"
  ) |>
  filter(!is.na(name))|>
  # in Bornean Malaysia, 'divisions' form an administrative level between 1 and 2
  # "districts" form level 2 and are uniform over the country
  # https://en.wikipedia.org/wiki/List_of_districts_in_Malaysia
  # therefore we classify all 'districts' in malaysia as admin 3,
  # any studies at 'division' level are booted upwards to state
  mutate(
    admin_level = sub(
      pattern = "admin",
      replacement = "",
      x = admin_level
    ) |>
      as.numeric(),
    name = case_when(
      name == "Tawau" & admin_level == 2 ~ "Sabah",
      name == "Kapit" & admin_level == 2 ~ "Sarawak",
      # also can't find admin 3 for Philippines, so booting this upward
      name == "3rd District" ~ "Palawan",
      .default = name
    )
  ) |>
  # retotal so only one entry for each name
  group_by(country, region, name) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  # get rid of entries with zeroes
  filter(n != 0) |>
  # correct names to match admin data
  mutate(
    name = case_when(
      # translate English to Bahasa Indonesia
      name == "Central Kalimantan" ~ "Kalimantan Tengah",
      name == "North Kalimantan" ~ "Kalimantan Utara",
      name == "South Kalimantan" ~ "Kalimantan Selatan",
      name == "West Kalimantan" ~ "Kalimantan Barat",
      # I think this is a typo or alternate spelling
      name == "Kua Musang" ~ "Gua Musang",
      # this is a newly designated district, previously part of Batang Padang
      # the current admin 2 data does not have this so am lumping into old district
      name == "Muallim" ~ "Batang Padang",
      .default = name
    )
  )



# create table of admin names to join with count of studies and then bind directly back to spatvector

# admin 1
a1_n <- tibble(name = admin_1$NAME_1) |>
  left_join(
    y = dat |>
      select(name, n),
    by = "name"
  ) |>
  mutate(
    n = ifelse(is.na(n), 0, n)
  )

a1_plot <- bind_spat_cols(admin_1, a1_n) |>
  select(NAME_1, name, n)


# admin 2
a2_n <- tibble(name = admin_2$NAME_2) |>
  left_join(
    y = dat |>
      select(name, n),
    by = "name"
  ) |>
  mutate(
    n = ifelse(is.na(n), 0, n)
  )


a2_plot <- bind_spat_cols(admin_2, a2_n) |>
  select(NAME_2, name, n)


# admin 3
a3_n <- tibble(name = admin_3$NAME_3) |>
  left_join(
    y = dat |>
      select(name, n),
    by = "name"
  ) |>
  mutate(
    n = ifelse(is.na(n), 0, n)
  )

a3_plot <- bind_spat_cols(admin_3, a3_n) |>
  select(NAME_3, name, n)


# study area layer

study_areas_plot <- bind_spat_cols(
  admin_1,
  study_areas
)

## check everything in dat has a match, and the number of points tally
# across the admin plot thingies
# should return empty df if all fine
dat |>
  left_join(
    y = a1_n |>
      rename(n1 = n),
    by = "name"
  ) |>
  left_join(
    y = a2_n |>
      rename(n2 = n),
    by = "name"
  ) |>
  left_join(
    y = a3_n |>
      rename(n3 = n),
    by = "name"
  ) |>
  rowwise() |>
  mutate(
    sumn = sum(n1, n2, n3, na.rm = TRUE),
    flag = n != sumn
  ) |>
  filter(flag)

# hooray


# plots

library(idpalette)

ggplot() +
  geom_spatvector(
    data = admin_0,
    fill = "grey50"
  ) +
  theme_void() +
  geom_spatvector(
    data = a1_plot,
    aes(fill = n),
    colour = NA
  ) +
  geom_spatvector(
    data = study_areas_plot,
    aes(colour = study_area),
  ) +
 scale_colour_discrete(palette = c(NA, "white")) +
  geom_spatvector(
    data = a2_plot,
    aes(fill = n),
    colour = NA
  ) +
  geom_spatvector(
    data = a3_plot,
    aes(fill = n),
    colour = NA
  ) +
  scale_id_continuous(
    cols = c(NA, colorRampPalette(colors = c("white", "darkorchid"))(14)[2:12]),
    aesthetics = "fill"
  ) +
  theme(
    panel.background = element_rect(fill = "grey90")
  ) +
  coord_sf(
    xlim = c(94, 121),
    ylim = c(-8,13)
  )




