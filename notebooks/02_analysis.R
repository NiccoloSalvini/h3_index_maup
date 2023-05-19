

# Example of fitting a spatial autoregressive model
model <- spdep::lagsarlm(
  formula = value ~ 1,
  data = hexagons,
  listw = hex_to_hex_2d(list(hexagons$h3_id)),
  method = "BM",
  tol.solve = 1e-6,
  zero.policy = TRUE
)

summary(model)

# Example of incorporating uncertainty analysis
# we may think of having different level of spatial autocorelation
simulations <- replicate(10, hexagons %>% mutate(value = rnorm(n(), value, sd = 0.1 * value)))


aggregate_and_summarise <- function(.data, col_grouping, col_value) {
  .data %>%
    group_by(col_grouping) %>%
    summarise(mean_value = mean(col_value), sd_value = sd(col_value)) %>%
    return()
}


simulation_summary <- simulations %>%
  map_dfr(~aggregate_and_summarise(.x, col_grouping = h3_id, col_value = value))

group_by(h3_id) %>%
  summarise(
    mean_value = mean(mean_value),
    sd_value = mean(sd_value)
  ) %>%
  mutate(coeff_var = sd_value / mean_value)


simulation_summary <- simulations %>%
  map_dfr(~ .x %>%
            group_by(h3_id) %>%
            summarise(mean_value = mean(value), sd_value = sd(value)))
  group_by(h3_id) %>%
  summarise(
    mean_value = mean(mean_value),
    sd_value = mean(sd_value)
  ) %>%
  mutate(coeff_var = sd_value / mean_value)














## take inspiration for MAUP here: https://andrewmaclachlan.github.io/CASA0005repo_20192020/advanced-r-maup-and-more-regression.html
