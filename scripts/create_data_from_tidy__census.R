source("src/functions")

census_api_key("975bb79e5ddf62e44f5850c04ad60ce030fc4f14", install = T)
options(tigris_use_cache = TRUE)

# sVarNames <- load_variables(2019, "acs5/subject", cache = TRUE)
# pVarNames <- load_variables(2019, "acs5/profile", cache = TRUE)
# otherVarNames <- load_variables(2019, "acs5", cache = TRUE)


oc_land_area <-
  read_table("data/2021_Gaz_zcta_national.txt") %>%
  select(zip = GEOID, land_area = ALAND_SQMI) %>%
  mutate(zip = as.integer(zip)) %>%
  filter(zip %in% oc_zips)

oc_census_data <-
  get_acs(state = "CA",
          geography = "zcta",
          variables = c(
            percent_65_or_older = "DP05_0024P",
            percent_85_and_older = "DP05_0017P",
            percent_bachelors_degree_or_higher = "DP02_0068P",
            percent_health_insurance = "DP03_0096P",
            percent_hispanic_or_latino = "DP05_0071P",
            median_household_income = "DP03_0062",
            total_population = 	"DP05_0001P"),
          output = "wide",
          geometry = T) %>%
  mutate(GEOID = as.integer(GEOID)) %>%
  filter(GEOID %in% oc_zips)

tmp <-
  oc_census_data %>%
  select(-ends_with("E"), NAME) %>% poly2nb() %>%
  nb2mat(style = "B")

oc_census_data <-
  oc_census_data %>%
  select(zip = GEOID, ends_with("E"), -NAME) %>%
  rename_all(~str_remove(., "E$")) %>%
  left_join(oc_land_area) %>%
  mutate(density = total_population / land_area) %>%
  st_sf()

oc_nb <- poly2nb(oc_census_data)
nb2mat(neighbours = oc_nb, style = "B")

write_rds(oc_census_data, "data/oc_census_data.rds")
# oc_census_data <- read_rds("data/oc_census_data.rds")

oc_nb <- poly2nb(oc_census_data)
