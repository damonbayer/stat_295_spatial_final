source("src/functions.R")

iter <- 2000
warmup <- 1000
chains <- 4

oc_census_data <-
  read_rds("data/oc_census_data.rds") %>%
  arrange(zip) %>%
  mutate(density = density / 10000,
         median_household_income = median_household_income / 10000)

oc_nb_mat <-
  oc_census_data %>%
  poly2nb() %>%
  nb2mat(style = "B") %>%
  `rownames<-`(oc_census_data$zip) %>%
  `colnames<-`(oc_census_data$zip)

oc_covid_data_by_wave_zip <-
  read_csv("data/oc_covid_data_by_wave_zip.csv",
         col_types = cols(.default = col_integer(),
                          wave = col_character(),
                          test_positivity = col_skip())) %>%
  mutate(wave = fct_inorder(wave)) %>%
  arrange(zip)


coefficient_center_scale_key <-
  oc_census_data %>%
  as_tibble() %>%
  select(percent_65_or_older, percent_85_and_older,
         percent_bachelors_degree_or_higher, percent_health_insurance,
         percent_hispanic_or_latino, median_household_income, density) %>%
  pivot_longer(everything()) %>%
  group_by(name) %>%
  summarize(center = attr(scale(value), "scaled:center"),
            scale = attr(scale(value), "scaled:scale")) %>%
  rename(.variable = name) %>%
  add_row(.variable = "Intercept", center = 0, scale = 1)


data_for_modeling <-
  oc_census_data %>%
  as_tibble() %>%
  right_join(oc_covid_data_by_wave_zip) %>%
  select(zip, wave, tests, cases, deaths, everything()) %>%
  arrange(zip) %>%
  mutate(zip = as.character(zip)) %>%
  mutate(across(c(percent_65_or_older, percent_85_and_older,
                  percent_bachelors_degree_or_higher, percent_health_insurance,
                  percent_hispanic_or_latino, median_household_income, density),
                ~scale(.)[,1]))

# Fit Models --------------------------------------------------------------
fit_pos <-
  brm(formula = cases | trials(tests) ~
        (1 | zip) +
        wave +
        percent_65_or_older * wave +
        percent_85_and_older * wave +
        percent_bachelors_degree_or_higher * wave +
        percent_health_insurance * wave +
        percent_hispanic_or_latino * wave +
        median_household_income * wave +
        density * wave,
      data = data_for_modeling,
      family = binomial(),
      prior = NULL,
      data2 = list(W = oc_nb_mat),
      chains = chains,
      iter = iter,
      warmup = warmup,
      control = list(max_treedepth = 12),
      file = "results/fit_pos",
      seed = 1)

fit_pos_exact_sparse_CAR <-
  brm(formula = cases | trials(tests) ~
        wave +
        percent_65_or_older * wave +
        percent_85_and_older * wave +
        percent_bachelors_degree_or_higher * wave +
        percent_health_insurance * wave +
        percent_hispanic_or_latino * wave +
        median_household_income * wave +
        density * wave  +
        car(W, gr = zip, type = "escar"),
      data = data_for_modeling,
      family = binomial(),
      prior = NULL,
      data2 = list(W = oc_nb_mat),
      chains = chains,
      iter = iter,
      warmup = warmup,
      control = list(max_treedepth = 12),
      file = "results/fit_pos_exact_sparse_CAR",
      seed = 1)

fit_pos_exact_sparse_ICAR <-
  brm(formula = cases | trials(tests) ~
        wave +
        percent_65_or_older * wave +
        percent_85_and_older * wave +
        percent_bachelors_degree_or_higher * wave +
        percent_health_insurance * wave +
        percent_hispanic_or_latino * wave +
        median_household_income * wave +
        density * wave  +
        car(W, gr = zip, type = "esicar"),
      data = data_for_modeling,
      family = binomial(),
      prior = NULL,
      data2 = list(W = oc_nb_mat),
      chains = chains,
      iter = iter,
      warmup = warmup,
      control = list(max_treedepth = 12),
      file = "results/fit_pos_exact_sparse_ICAR",
      seed = 1)

fit_death <-
  brm(formula = deaths | rate(total_population) ~
        (1 | zip) +
        wave +
        percent_65_or_older * wave +
        percent_85_and_older * wave +
        percent_bachelors_degree_or_higher * wave +
        percent_health_insurance * wave +
        percent_hispanic_or_latino * wave +
        median_household_income * wave +
        density * wave,
      data = data_for_modeling,
      family = poisson(),
      prior = NULL,
      data2 = list(W = oc_nb_mat),
      chains = chains,
      iter = iter,
      warmup = warmup,
      control = list(max_treedepth = 12),
      file = "results/fit_death",
      seed = 1)

fit_death_exact_sparse_CAR <-
  brm(formula = deaths | rate(total_population) ~
        wave +
        percent_65_or_older * wave +
        percent_85_and_older * wave +
        percent_bachelors_degree_or_higher * wave +
        percent_health_insurance * wave +
        percent_hispanic_or_latino * wave +
        median_household_income * wave +
        density * wave  +
        car(W, gr = zip, type = "escar"),
      data = data_for_modeling,
      family = poisson(),
      prior = NULL,
      data2 = list(W = oc_nb_mat),
      chains = chains,
      iter = iter,
      warmup = warmup,
      control = list(max_treedepth = 12),
      file = "results/fit_death_exact_sparse_CAR",
      seed = 1)

fit_death_exact_sparse_ICAR <-
  brm(formula = deaths | rate(total_population) ~
        wave +
        percent_65_or_older * wave +
        percent_85_and_older * wave +
        percent_bachelors_degree_or_higher * wave +
        percent_health_insurance * wave +
        percent_hispanic_or_latino * wave +
        median_household_income * wave +
        density * wave  +
        car(W, gr = zip, type = "esicar"),
      data = data_for_modeling,
      family = poisson(),
      prior = NULL,
      data2 = list(W = oc_nb_mat),
      chains = chains,
      iter = iter,
      warmup = warmup,
      control = list(max_treedepth = 12),
      file = "results/fit_death_exact_sparse_ICAR",
      seed = 1)
