# ==============================================================================
# >> 1. Load Libraries and Setup
# ==============================================================================
library(tidyverse)
library(nls.multstart)
library(showtext)
# Optionally enable automatic font handling for plots:
# showtext_auto()

# ==============================================================================
# >> 2. Data Import & Preprocessing
# ==============================================================================
data <- read.csv2("koda_data.csv", na.strings = "NA") %>%
  mutate(
    non_human_members = total_members - total_human_members,  # Non-human members
    total_human_members = zoo::na.approx(total_human_members, na.rm = FALSE),  # Impute missing values
    female_members = round(total_human_members * (female_members_pct / 100), 0),
    departed_members = abs(total_members - lag(total_members, default = NA) - new_members),
    new_human_members = total_human_members - lag(total_human_members, default = NA),
    non_human_pct = non_human_members / total_members,
    departed_human_members_estimated = round(departed_members - (non_human_pct * departed_members), 0)
  )

# ==============================================================================
# >> 3. Fit Logistic Model
# ==============================================================================
logistic_model <- nls_multstart(
  new_female_members_pct ~ SSlogis(year, Asym, xmid = 2020, scal),
  data = data,
  iter = 100,
  start_lower = c(Asym = 40, scal = 15),
  start_upper = c(Asym = 60, scal = 25),
  supp_errors = 'Y'
)

# ==============================================================================
# >> 4. Simulation & Future Predictions
# ==============================================================================
future_years <- data.frame(year = seq(2013, 3000, by = 1))
set.seed(123); n_sim <- 1000
sim_params <- tibble(Asym = runif(n_sim, 40, 60), scal = coef(logistic_model)["scal"])
sim_results <- sim_params %>%
  rowwise() %>%
  mutate(predictions = list(
    tibble(
      year = future_years$year,
      new_female_members_pct = Asym / (1 + exp(-(future_years$year - 2020) / scal))
    )
  )) %>%
  unnest(predictions)

# ==============================================================================
# >> 5. Historical Analysis & Confidence Intervals
# ==============================================================================
historical_data <- data %>%
  filter(!is.na(new_female_members_pct)) %>%
  summarise(
    median_obs = median(new_female_members_pct, na.rm = TRUE),
    max_deviation = max(abs(new_female_members_pct - median_obs) / median_obs, na.rm = TRUE)
  )
baseline_deviation <- historical_data$max_deviation * 2
target_year <- 2040; threshold <- 0.95
k <- -log(1 - threshold) / (target_year - 2013)

ci_results <- sim_results %>%
  group_by(year) %>%
  summarise(
    lower_raw = quantile(new_female_members_pct, 0.05),
    upper_raw = quantile(new_female_members_pct, 0.95),
    median = median(new_female_members_pct),
    .groups = "drop"
  ) %>%
  mutate(
    scale_factor = pmin(baseline_deviation + (0.95 - baseline_deviation) * (0.95 - exp(-k * (year - 2013))), 0.95),
    lower = median + scale_factor * (lower_raw - median),
    upper = median + scale_factor * (upper_raw - median)
  )

# ==============================================================================
# >> 6. Plot Logistic Model Predictions
# ==============================================================================
ggplot() +
  geom_point(data = data, aes(x = year, y = new_female_members_pct), color = "black", size = 0.75) +
  geom_rect(aes(xmin = 2013, xmax = 2070, ymin = 40, ymax = 60), fill = "#2a7f62", alpha = 0.1) +
  geom_ribbon(data = ci_results %>% filter(year <= 2070),
              aes(x = year, ymin = lower, ymax = upper), fill = "#df2935", alpha = 0.2) +
  geom_line(data = ci_results %>% filter(year <= 2070),
            aes(x = year, y = median), color = "#df2935", linetype = "dotted", linewidth = 0.5) +
  geom_hline(yintercept = c(40, 60), linetype = "dashed", color = "#2a7f62") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(15, 60)) +
  scale_x_continuous(breaks = seq(2013, 2070, 5), limits = c(2013, 2070), expand = c(0.01, 0)) +
  labs(title = "Fremskrivning af nyindmeldte, kvindelige Koda-medlemmer",
       x = "År", y = "Nyindmeldte, kvindelige koda-medlemmer (%)") +
  theme(panel.grid.major = element_line(color = "#eaeaea"), panel.grid.minor = element_blank())

# Print year in which upper bound hits 50 % female representation (best case)
ci_results %>% 
  filter(upper > 50) %>%
  pull(year) %>%
  head(1)

# ==============================================================================
# >> 7. Forecast Total Female Membership
# ==============================================================================
target_percent_female <- 45
current_year <- max(data$year)
avg_new_human_members <- round(mean(data$new_human_members, na.rm = TRUE))
projected_human_members_leaving <- data %>% filter(year != "2019") %>% pull(departed_human_members_estimated) %>% mean(na.rm = TRUE) %>% round(0)
projected_brutto_new_human_members <- avg_new_human_members - projected_human_members_leaving

data <- data %>% mutate(
  female_members_median = female_members,
  female_members_lower  = female_members,
  female_members_upper  = female_members
)

while ((tail(data$female_members_median, 1) / tail(data$total_human_members, 1)) * 100 < target_percent_female) {
  current_year <- current_year + 1
  print(current_year)
  projection <- list()
  for (i in c("lower", "median", "upper")) {
    projected_percent_new_female <- ci_results %>% filter(year == current_year) %>% pull(i)
    projection[[i]][["projected_percent_new_female"]] <- projected_percent_new_female
    projection[[i]][["new_female_members"]] <- round(avg_new_human_members * (projected_percent_new_female / 100), 0)
    projection[[i]][["new_male_members"]]   <- avg_new_human_members - projection[[i]][["new_female_members"]]
    projection[[i]][["female_departures"]] <- round(
      projected_human_members_leaving * (tail(data[[paste0("female_members_", i)]], 1) / tail(data$total_human_members, 1)), 0)
    projection[[i]][["male_departures"]] <- round(
      projected_human_members_leaving * ((tail(data$total_human_members, 1) - tail(data[[paste0("female_members_", i)]], 1)) / tail(data$total_human_members, 1)), 0)
    projection[[i]][["female_members"]] <- tail(data[[paste0("female_members_", i)]], 1) +
      projection[[i]][["new_female_members"]] - projection[[i]][["female_departures"]]
    projection[[i]][["female_members_percent"]] <- projection[[i]][["female_members"]] /
      (tail(data$total_human_members, 1) + projected_brutto_new_human_members) * 100
  }
  data <- bind_rows(data, data.frame(
    year = current_year,
    total_human_members = tail(data$total_human_members, 1) + projected_brutto_new_human_members,
    female_members_pct_lower = projection$lower$female_members_percent,
    female_members_pct       = projection$median$female_members_percent,
    female_members_pct_upper = projection$upper$female_members_percent,
    new_human_members        = avg_new_human_members,
    brutto_new_human_members = projected_brutto_new_human_members,
    departed_human_members   = projected_human_members_leaving,
    female_members_lower     = projection$lower$female_members,
    female_members_median    = projection$median$female_members,
    female_members_upper     = projection$upper$female_members
  ))
}

# Print year in which upper bound hits 50 % female representation (best case)
data %>%
  filter(female_members_pct_upper > 50) %>%
  pull(year) %>%
  head(1)

# ==============================================================================
# >> 8. Final Visualization: Historical vs. Predicted Membership
# ==============================================================================
data <- data %>% mutate(Status = ifelse(year <= 2023, "Historical", "Predicted"))
ggplot() +
  geom_rect(aes(xmin = 2013, xmax = year_50_percent_female, ymin = 40, ymax = 60), fill = "#2a7f62", alpha = 0.1) +
  geom_line(data = data, aes(x = year, y = female_members_pct, linetype = Status, color = "Medlemmer (total)"),
            size = 0.75, linetype = "dotted") +
  geom_point(data = filter(data, Status == "Historical"),
             aes(x = year, y = female_members_pct), color = "#2E0854", size = 0.5, alpha = 0.5) +
  geom_ribbon(data = data, aes(x = year, ymin = female_members_pct_lower, ymax = female_members_pct_upper),
              fill = "purple", alpha = 0.2) +
  geom_point(data = data, aes(x = year, y = new_female_members_pct),
             color = "black", size = 0.5, alpha = 0.2) +
  geom_ribbon(data = ci_results %>% filter(year <= year_50_percent_female),
              aes(x = year, ymin = lower, ymax = upper), fill = "#df2935", alpha = 0.05) +
  geom_line(data = ci_results %>% filter(year <= year_50_percent_female),
            aes(x = year, y = median, color = "Nyindmeldte medlemmer"),
            linetype = "dotted", size = 1, alpha = 0.2) +
  geom_hline(yintercept = c(40, 60), linetype = "dashed", color = "#2a7f62") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(2013, year_50_percent_female, 20),
                     limits = c(2013, year_50_percent_female),
                     expand = c(0.01, 0)) +
  scale_color_manual(values = c("Medlemmer (total)" = "purple", "Nyindmeldte medlemmer" = "#df2935")) +
  labs(title = "Fremskrivning af kvindelige Koda-medlemmer over tid",
       x = "År", y = "Kvindelige Koda-medlemmer (%)", color = "") +
  scale_linetype_manual(values = c(Historical = "solid", Predicted = "dashed")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "bottom")
