library(tidyverse)
library(tidybayes)
library(lubridate)
library(sf)
library(spdep)
library(brms)
library(cmdstanr)
library(tidycensus)
library(cowplot)
library(scales)
options(brms.backend = "cmdstanr",
        mc.cores=parallel::detectCores())

# round_any ---------------------------------------------------------------
round_any <- function (x, accuracy, f = round) f(x/accuracy) * accuracy

# draw_to_iteration -------------------------------------------------------
draw_to_iteration <- function(draw) {
  iteration_raw <- draw %% (iter - warmup)
  if_else(iteration_raw == 0, iter - warmup, iteration_raw)
}

# draw_to_chain -----------------------------------------------------------
draw_to_chain <- function(draw) {
  which(draw <= (iter - warmup) * 1:chains)[1]
}

# waves_tbl ---------------------------------------------------------------
waves_tbl <-
  tribble(
    ~wave,          ~start_week,    ~end_week,
    "First Wave",   "2020-06-13",   "2020-08-08",
    "Winter Wave", "2020-11-07",   "2021-02-20",
    "Delta Wave",   "2021-07-03",   "2021-09-11",
    "Omicron Wave", "2021-12-18",   "2022-01-15"
  ) %>%
  mutate(across(ends_with("week"), lubridate::ymd))

# wave_date_classifier ----------------------------------------------------
wave_date_classifier <- function(date) {
  wave <- waves_tbl$wave[date <= waves_tbl$end_week & date >= waves_tbl$start_week - 6]
  if (length(wave) == 0) wave <- NA_character_
  wave
}

# oc_zips -----------------------------------------------------------------
oc_zips <- c(
  90620L, 90621L, 90623L, 90630L, 90631L, 90680L, 90720L, 90740L, 90742L,
  92602L, 92603L, 92604L, 92606L, 92610L, 92612L, 92614L, 92617L, 92618L,
  92620L, 92624L, 92625L, 92626L, 92627L, 92629L, 92630L, 92637L, 92646L,
  92647L, 92648L, 92649L, 92651L, 92653L, 92655L, 92656L, 92657L, 92660L,
  92661L, 92662L, 92663L, 92672L, 92673L, 92675L, 92677L, 92679L, 92683L,
  92688L, 92691L, 92692L, 92694L, 92701L, 92703L, 92704L, 92705L, 92706L,
  92707L, 92708L, 92780L, 92782L, 92801L, 92802L, 92804L, 92805L, 92806L,
  92807L, 92808L, 92821L, 92823L, 92831L, 92832L, 92833L, 92835L, 92840L,
  92841L, 92843L, 92844L, 92845L, 92861L, 92865L, 92866L, 92867L, 92868L,
  92869L, 92870L, 92886L, 92887L)


# plot_neighbors ----------------------------------------------------------
plot_neighbors <- function(root, oc_census_data, oc_nb) {
  oc_census_data %>%
    mutate(nb = case_when(
      row_number() == root ~ "root",
      row_number() %in% oc_nb[[root]] ~ "neighbor",
      TRUE ~ "other")
    ) %>%
    ggplot(aes(fill = nb)) +
    geom_sf(color = NA) +
    coord_sf(crs = 26911) +
    cowplot::theme_map()
}

# augment_model_fit -------------------------------------------------------
augment_model_fit <- function(model_fit) {
  augmented_model_fit <- list(
    epred_draws(model_fit, model_fit$data),
    predicted_draws(model_fit, model_fit$data),
    residual_draws(model_fit, model_fit$data)
  ) %>%
    reduce(left_join) %>%
    mutate(.chain = draw_to_chain(.draw),
           .iteration = draw_to_iteration(.draw))
  if (model_fit$family$family == "binomial") {
    augmented_model_fit <-
      augmented_model_fit %>%
      mutate(across(c(.epred, .prediction, .residual), ~`/`(., tests)))
  } else if (model_fit$family$family == "poisson") {
    augmented_model_fit <-
      augmented_model_fit %>%
      mutate(across(c(.epred, .prediction, .residual), ~`/`(., total_population)))
  }
  augmented_model_fit
}


# prep_augmented_model_fit_for_plotting -----------------------------------
prep_augmented_model_fit_for_plotting <- function(augmented_model_fit) {
  augmented_model_fit %>%
    pivot_longer(c(.epred, .prediction, .residual)) %>%
    group_by(name, .add = T) %>%
    median_qi() %>%
    left_join(oc_census_data %>%
                mutate(zip = as.character(zip)) %>%
                select(zip, geometry)) %>%
    st_sf()
}

# prep_coefficients_for_plotting ------------------------------------------
prep_coefficients_for_plotting <- function(model_fit) {
  model_fit %>%
    gather_draws(`b_.*`, regex = T) %>%
    mutate(.variable = str_sub(.variable, 3)) %>%
    separate(col = .variable, into = c("wave", ".variable"), sep = "\\:") %>%
    mutate(tmp_wave = if_else(str_starts(wave, "wave"), wave, "waveFirstWave")) %>%
    mutate(.variable = case_when(
      is.na(.variable) & str_starts(wave, "wave") ~ "Intercept",
      is.na(.variable) & str_starts(wave, "wave", negate = T) ~ wave,
      TRUE ~ .variable)) %>%
    select(-wave) %>%
    rename(wave = tmp_wave) %>%
    mutate(wave = str_sub(wave, 5, - 5)) %>%
    pivot_wider(names_from = wave, values_from = .value) %>%
    mutate(across(c(Winter, Delta, Omicron), ~`+`(., First))) %>%
    pivot_longer(c(First, Winter, Delta, Omicron), names_to = "wave") %>%
    left_join(coefficient_center_scale_key) %>%
    mutate(value = exp(value / scale)) %>%
    mutate(wave = fct_inorder(wave)) %>%
    mutate(value_type = case_when(
      model_fit$family$family == "binomial" ~ "Odds Ratio",
      model_fit$family$family == "poisson" ~ "Rate Ratio",
      TRUE ~ "Exp(Identitiy)"))
}
