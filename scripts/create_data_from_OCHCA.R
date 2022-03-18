library(tidyverse)
library(lubridate)
library(data.table)
library(dtplyr)
library(fs)
library(scales)
library(ggrepel)
library(cowplot)
source("src/functions.R")
# Setup -------------------------------------------------------------------
line_list_path <- "/Users/damon/Documents/uci_covid_modeling2/data/from_OCHCA/3.10.22 release to UCI team.csv"
negative_line_list_path <- "/Users/damon/Documents/uci_covid_modeling2/data/from_OCHCA/All PCR tests updated 3.3.22.csv"

# Create Daily Zip Data ---------------------------------------------------
neg_line_list <-
  fread(negative_line_list_path,
        select = c("unique_num", "Specimen.Collected.Date", "TestResult", "Zip"),
        na.strings = "") %>%
  select(id = unique_num, date = Specimen.Collected.Date, zip = Zip, test_result = TestResult) %>%
  filter(!(is.na(id) | is.na(test_result) | is.na(zip))) %>%
  mutate(test_result = fct_collapse(str_to_lower(test_result),
                                    negative = "negative",
                                    positive = "positive",
                                    other = c("invalid", "inconclusive"))) %>%
  filter(date >= lubridate::ymd("2020-01-01")) %>%
  mutate(zip = zip %>%
           str_sub(end = 5) %>%
           as.integer()) %>%
  filter(zip %in% oc_zips) %>%
  count(date, zip, test_result) %>%
  pivot_wider(names_from = test_result, values_from = n, values_fill = 0)

deaths_tbl <-
  fread(line_list_path, select = c("DtDeath", "Date.death.posted", "DeathDueCOVID", "Zip")) %>%
  filter(DeathDueCOVID == "Y") %>%
  select(zip = Zip, date = DtDeath) %>%
  filter(zip %in% oc_zips) %>%
  count(date, zip, name = "deaths")

all_dates <-
  neg_line_list %>%
  pull(date) %>%
  range() %>%
  {seq(from = .[1], to = .[2], by = 1)}

oc_covid_data_by_date_zip <-
  full_join(neg_line_list, deaths_tbl) %>%
  right_join(crossing(date = all_dates,
                     zip = oc_zips)) %>%
  arrange(zip, date) %>%
  replace_na(list(
    other = 0L,
    negative = 0L,
    positive = 0L,
    deaths = 0L)) %>%
  mutate(tests = other + negative + positive) %>%
  as_tibble()

dir_create("data")
write_csv(oc_covid_data_by_date_zip, "data/oc_covid_data_by_date_zip.csv")

oc_covid_data_by_date_zip <- read_csv("data/oc_covid_data_by_date_zip.csv", col_types = cols(date = col_date(), .default = col_integer()))

# Create Weekly County Data -----------------------------------------------
oc_covid_data_weekly <-
  oc_covid_data_by_date_zip %>%
  select(-zip) %>%
  group_by(date) %>%
  summarize(across(everything(), sum)) %>%
  filter(date >= ymd("2020-03-15"),
         date < ymd("2022-02-01")) %>%
  group_by(week = as.integer(floor((date - min(date)) / 7) + 1L)) %>%
  summarize(date = max(date),
            across(where(is.integer), sum)) %>%
  mutate(test_positivity = positive / tests) %>%
  select(date, test_positivity, deaths, cases = positive, tests) %>%
  pivot_longer(-date)

write_csv(oc_covid_data_weekly, "data/oc_covid_data_weekly.csv")

waves_tbl_plot <-
  waves_tbl %>%
  crossing(name = c("deaths", "test_positivity", "cases")) %>%
  mutate(across(ends_with("week"),
                ~if_else(condition = name == "deaths",
                         true = . + 14,
                         false = .))) %>%
  mutate(date = map2_dbl(start_week, end_week, ~mean(c(.x, .y))) %>%
           as_date()) %>%
  left_join(oc_covid_data_weekly %>%
              group_by(name) %>%
              summarize(value = min(value) * 0.05))

write_csv(waves_tbl_plot, "data/waves_tbl_plot.csv")


# Create Weekly Zip Data --------------------------------------------------
oc_covid_data_zip_weekly <-
  oc_covid_data_by_date_zip %>%
  filter(date >= ymd("2020-03-15"),
         date < ymd("2022-02-01")) %>%
  group_by(week = as.integer(floor((date - min(date)) / 7) + 1L),
           zip) %>%
  summarize(start_date = min(date),
            end_date = max(date),
            across(where(is.integer), sum),
            .groups = "drop") %>%
  mutate(test_positivity = positive / tests) %>%
  select(week, start_date, end_date, zip, tests, cases = positive, deaths)


write_csv(oc_covid_data_zip_weekly, "data/oc_covid_data_zip_weekly.csv")

# Create Wave Zip Data ----------------------------------------------------
wave_date_tbl <-
  oc_covid_data_by_date_zip %>%
  distinct(date) %>%
  mutate(wave = map_chr(date, wave_date_classifier))

oc_covid_data_by_wave_zip <-
  oc_covid_data_by_date_zip %>%
  left_join(wave_date_tbl) %>%
  drop_na() %>%
  mutate(wave = fct_inorder(wave)) %>%
  group_by(zip, wave) %>%
  summarize(across(where(is.integer), sum),
            .groups = "drop") %>%
  mutate(test_positivity = positive / tests) %>%
  select(zip, wave, tests, cases = positive, deaths, test_positivity)

write_csv(oc_covid_data_by_wave_zip, "data/oc_covid_data_by_wave_zip.csv")
