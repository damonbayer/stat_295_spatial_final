source("src/functions.R")

# coefficient_labels ------------------------------------------------------
coefficient_labels <- c(
  `Intercept` = "Intercept",
  `percent_65_or_older` = "Age 65+ (%)",
  `percent_85_and_older` = "Age 85+ (%)",
  `percent_bachelors_degree_or_higher` = "Have Bachelor's Degree (%)",
  `percent_health_insurance` = "Have Health Insurance (%)",
  `percent_hispanic_or_latino` = "Hispanic or Latino (%)",
  `median_household_income` = "Med. Household Income ($10k)",
  `density` = "Population Density (10k/sq mi)"
)

exploratory_labels <- c(
  `percent_65_or_older` = "Age 65+",
  `percent_85_and_older` = "Age 85+",
  `percent_bachelors_degree_or_higher` = "Have Bach. Degree",
  `percent_health_insurance` = "Have Health Insur.",
  `percent_hispanic_or_latino` = "Hispanic or Latino",
  `median_household_income` = "Med. House Income",
  `density` = "Pop. Density"
)

exploratory_labels_legend <- c(
  `percent_65_or_older` = "%",
  `percent_85_and_older` = "%",
  `percent_bachelors_degree_or_higher` = "%",
  `percent_health_insurance` = "%",
  `percent_hispanic_or_latino` = "%",
  `median_household_income` = "$",
  `density` = "People/sq. mi"
)

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

date_labeller <- function(x) x %>% format("%m/%y") %>% str_remove("^0")

oc_census_data <- read_rds("data/oc_census_data.rds")
oc_covid_data_by_wave_zip <-
  read_csv("data/oc_covid_data_by_wave_zip.csv",
           col_types = cols(.default = col_integer(),
                            wave = col_character(),
                            test_positivity = col_skip())) %>%
  mutate(wave = fct_inorder(wave)) %>%
  arrange(zip)
oc_covid_data_weekly <- read_csv("data/oc_covid_data_weekly.csv")
waves_tbl_plot <- read_csv("data/waves_tbl_plot.csv")

death_ICAR <- read_rds("results/fit_death_exact_sparse_ICAR.rds")
pos_ICAR <- read_rds("results/fit_pos_exact_sparse_ICAR.rds")

prep_coefficients_for_plotting_pos <- prep_coefficients_for_plotting(pos_ICAR)
prep_coefficients_for_plotting_death <- prep_coefficients_for_plotting(death_ICAR)

# Weekly Data -------------------------------------------------------------
make_weekly_plot <- function(data_name, y_name, y_label, title, date_breaks = "1 month") {
  ggplot(data = oc_covid_data_weekly %>%
           filter(name == data_name),
         mapping = aes(x = date, y = value)) +
    geom_rect(data = waves_tbl_plot %>%
                filter(name == data_name),
              mapping = aes(xmin = start_week, xmax = end_week,
                            ymin = -Inf, ymax = Inf,
                            fill = wave),
              show.legend = F) +
    geom_label(data = waves_tbl_plot %>%
                 filter(name == data_name),
               mapping = aes(label = wave), size = 3) +
    geom_line() +
    geom_point() +
    scale_y_continuous(name = y_name, labels = y_label) +
    scale_x_date(name = "Date",
                 date_breaks = date_breaks,
                 labels = date_labeller) +
    ggtitle(title) +
    theme_cowplot(font_size = 11)
}

weekly_data_plot <-
  plot_grid(make_weekly_plot(data_name = "test_positivity", y_name = "Test Positivity", y_label = ~percent(., accuracy = 1), title = "Weekly COVID-19 Test Positivity in Orange County", date_breaks = "2 months"),
            make_weekly_plot(data_name = "deaths", y_name = "Deaths", y_label = comma, title = "Weekly Deaths due to COVID-19 in Orange County", date_breaks = "2 months"), align = "hv",
            nrow = 2,
            ncol = 1)

write_rds(weekly_data_plot, "figures/weekly_data_plot.rds")


# Exploratory plots -------------------------------------------------------
make_exploratory_plot <- function(var_to_plot) {
  oc_census_data %>%
    select(names(exploratory_labels)) %>%
    pivot_longer(-geometry) %>%
    filter(name == var_to_plot) %>%
    mutate(value = value / if_else(str_starts(var_to_plot, "percent"), 100, 1)) %>%
    ggplot(aes(fill = value)) +
    geom_sf(color = "NA") +
    coord_sf(crs = 26911) +
    scale_fill_viridis_c(name = exploratory_labels_legend[[var_to_plot]],
                         option = "magma",
                         label = ifelse(str_starts(var_to_plot, "percent"), percent, comma))  +
    ggtitle(exploratory_labels[[var_to_plot]]) +
    theme_map(font_size = 11, rel_large = 1) +
    theme(legend.position = "bottom") +
    guides(fill = guide_colorbar(barwidth = 7, title.position = "top"))
}

exploratory_plot <- plot_grid(plotlist = map(names(exploratory_labels), make_exploratory_plot), ncol = 4, align = "hv")
write_rds(exploratory_plot, "figures/exploratory_plot.rds")


exploratory_response_labels <- c("test_positivity" = "Test Positivity",
                                 "death_rate" = "Death Rate per 100,000")


make_exploratory_response_plot <- function(var_to_plot, ncol = 1) {
  oc_covid_data_by_wave_zip %>%
    left_join(oc_census_data %>%
                as_tibble() %>%
                select(zip, total_population)) %>%
    mutate(test_positivity = cases / tests,
           death_rate = deaths / total_population * 100000) %>%
    select(zip, wave, death_rate, test_positivity) %>%
    pivot_longer(c(death_rate, test_positivity)) %>%
    filter(name == var_to_plot) %>%
    left_join(oc_census_data %>%
                select(zip, geometry)) %>%
    st_sf() %>%
    ggplot(aes(fill = value)) +
    facet_wrap(. ~ wave, ncol = ncol) +
    geom_sf(color = "NA") +
    coord_sf(crs = 26911) +
    scale_fill_viridis_c(name = exploratory_response_labels[[var_to_plot]],
                         option = "magma",
                         label = ifelse(var_to_plot == "test_positivity", percent, comma))  +
    ggtitle(exploratory_response_labels[[var_to_plot]]) +
    theme_map(font_size = 11) +
    theme(legend.position = "right")
}


exploratory_response_plot <- plot_grid(plotlist = map(c("test_positivity", "death_rate"), make_exploratory_response_plot), align = "hv")
write_rds(exploratory_response_plot, "figures/exploratory_response_plot.rds")

exploratory_response_plot_test_positivity <- make_exploratory_response_plot("test_positivity", 2)
write_rds(exploratory_response_plot_test_positivity, "figures/exploratory_response_plot_test_positivity.rds")

exploratory_response_plot_death_rate <- make_exploratory_response_plot("death_rate", 2)
write_rds(exploratory_response_plot_death_rate, "figures/exploratory_response_plot_death_rate.rds")



# Results Plots -----------------------------------------------------------
tmp_pos <-
  pos_ICAR %>%
  augment_model_fit() %>%
  prep_augmented_model_fit_for_plotting()

tmp_pos %>%
  filter(name == ".residual") %>%
  pull(value) %>%
  ecdf() %>%
  plot()

tmp_pos2 <-
  pos_ICAR %>%
  augment_model_fit() %>%
  prep_augmented_model_fit_for_plotting()

unique(tmp_pos$name)

tmp_pos %>%
  filter(name == ".epred") %>%
  ggplot(aes(fill = value)) +
  facet_wrap(. ~ wave) +
  geom_sf(color = "NA") +
  coord_sf(crs = 26911) +
  scale_fill_viridis_c(name = "bla",
                       option = "magma") +
  theme_map(font_size = 11) +
  theme(legend.position = "right")


# Coefficient Plots -------------------------------------------------------


plot_coefficients <- function(prepped_coefficients_for_plotting) {
  prepped_coefficients_for_plotting %>%
    filter(.variable != "Intercept") %>%
    ggplot(aes(value, wave)) +
    facet_wrap(. ~ .variable,
               scales = "free_x",
               labeller = as_labeller(coefficient_labels)) +
    stat_halfeye(aes(fill = stat(x > 1)),
                 normalize = "panels", show.legend = F) +
    scale_y_discrete("Wave", limits = rev) +
    scale_fill_viridis_d(option = "D", begin = 0.25, end = 0.75) +
    scale_x_continuous(str_c(prepped_coefficients_for_plotting$value_type[1], " for One Unit Increase")) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    ggtitle(str_c("Posterior Distributions of Regression Coefficients in", if_else(prepped_coefficients_for_plotting$value_type[1] == "Odds Ratio", "Test Positivity", "Death"), "Model", sep = " ")) +
    theme_cowplot(font_size = 11, rel_large = 1)
}


coefficients_pos_plot <- plot_coefficients(prep_coefficients_for_plotting_pos)
write_rds(coefficients_pos_plot, "figures/coefficients_pos_plot.rds")

coefficients_death_plot <- plot_coefficients(prep_coefficients_for_plotting_death)
write_rds(coefficients_death_plot, "figures/coefficients_death_plot.rds")
