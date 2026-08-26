#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(DBI)
library(duckdb)
library(dbplyr)
#
#
#
con <- dbConnect(duckdb(), "data/seda_2025.duckdb", read_only = TRUE)
dbplyr_dist <- tbl(con, "district_scores")
#
#
#
#| cache: true
#| cache.extra: !expr file.mtime("data/seda_2025.duckdb")
scores_2015 <- dbplyr_dist |>
  filter(year == 2015, !is.na(rla_score)) |>
  select(district_id, district_name, rla_2015 = rla_score)
scores_2023 <- dbplyr_dist |>
  filter(year == 2023, !is.na(rla_score)) |>
  select(district_id, rla_2023 = rla_score)
score_change <- scores_2015 |>
  inner_join(scores_2023, by = "district_id") |>
  mutate(score_change = rla_2023 - rla_2015) |>
  collect() |>
  arrange(district_id)
dbplyr_state <- tbl(con, "state_scores")
state_scores_2015 <- dbplyr_state |>
  filter(year == 2015, !is.na(rla_score)) |>
  select(stateabb, state_name, rla_2015 = rla_score)
state_scores_2023 <- dbplyr_state |>
  filter(year == 2023, !is.na(rla_score)) |>
  select(stateabb, rla_2023 = rla_score)
state_change <- state_scores_2015 |>
  inner_join(state_scores_2023, by = "stateabb") |>
  mutate(state_change = rla_2023 - rla_2015) |>
  collect() |>
  arrange(stateabb)
#
#
#
rla_2023 <- dbplyr_dist |>
  filter(year == 2023, !is.na(rla_score)) |>
  collect()

ggplot(rla_2023, aes(x = rla_score)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Distribution of 2023 District Reading Scores",
    subtitle = paste("Number of districts:", nrow(rla_2023)),
    x = "Reading score",
    y = "Number of districts",
    caption = "Source: SEDA 2025 district_scores table"
  )
#
#
#
con_atus <- dbConnect(duckdb(), "data/atus.duckdb", read_only = TRUE)
#
#
#
score_change_plot <- score_change |>
  mutate(
    rla_change = score_change,
    change_direction = case_when(
      rla_change > 0 ~ "Positive",
      rla_change < 0 ~ "Negative",
      TRUE ~ "No change"
    )
  )

ggplot(score_change_plot, aes(x = rla_change, fill = change_direction)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(
    values = c(
      Positive = "#2a9d8f",
      Negative = "#e76f51",
      `No change` = "#6c757d"
    )
  ) +
  labs(
    title = "Distribution of District Reading Score Changes",
    subtitle = paste("Number of districts:", nrow(score_change_plot)),
    x = "Change in reading score",
    y = "Number of districts",
    fill = "Change direction",
    caption = "Source: SEDA 2025 district_scores table; change from 2015 to 2023"
  )
#
#
#
state_change <- state_change |>
  mutate(rla_change = state_change) |>
  arrange(rla_change)
print(state_change)
#
#
#
state_arrow_data <- state_change |>
  arrange(rla_2023) |>
  mutate(
    state_name = factor(state_name, levels = state_name),
    change_direction = case_when(
      rla_change > 0 ~ "Positive",
      rla_change < 0 ~ "Negative",
      TRUE ~ "No change"
    )
  )

ggplot(state_arrow_data, aes(y = state_name)) +
  geom_segment(
    aes(x = rla_2015, xend = rla_2023, yend = state_name, color = change_direction),
    arrow = grid::arrow(length = grid::unit(0.12, "inches"), type = "closed")
  ) +
  geom_point(aes(x = rla_2015), color = "gray50", size = 2) +
  scale_color_manual(
    values = c(
      Positive = "#2a9d8f",
      Negative = "#e76f51",
      `No change` = "#6c757d"
    )
  ) +
  labs(
    title = "State-Level Reading Score Changes",
    subtitle = "Arrows show reading scores from 2015 to 2023",
    x = "Reading score",
    y = NULL,
    color = "Change direction",
    caption = "Source: SEDA 2025 state_scores table"
  )
#
#
#
dbplyr_act   <- tbl(con_atus, "activities")
dbplyr_resp  <- tbl(con_atus, "respondents")
dbplyr_codes <- tbl(con_atus, "activity_codes")
person_activity_minutes <- dbplyr_resp |>
  filter(
    day_type == "Weekday",
    employment_status == "Not in labor force"
  ) |>
  inner_join(dbplyr_act, by = "tucaseid") |>
  inner_join(
    dbplyr_codes |>
      select(activity_code, major_name),
    by = "activity_code"
  ) |>
  group_by(tucaseid, sex, major_name) |>
  summarise(minutes = sum(duration_min, na.rm = TRUE)) |>
  collect()
activity_avg <- person_activity_minutes |>
  group_by(sex, major_name) |>
  summarise(mean_minutes = mean(minutes)) |>
  arrange(sex, major_name)
print(activity_avg)
#
#
#
keep_cats <- c(
  "Personal Care Activities",
  "Household Activities",
  "Caring For & Helping Household (HH) Members",
  "Socializing, Relaxing, and Leisure"
)

hourly_avg <- dbplyr_act |>
  mutate(hour = start_hhmm %/% 100L) |>
  filter(hour >= 6L, hour <= 23L) |>
  left_join(dbplyr_codes |> select(activity_code, major_name),
            by = "activity_code") |>
  filter(major_name %in% keep_cats) |>
  inner_join(dbplyr_resp |>
               filter(str_starts(employment_status, "Not"),
                      day_type == "Weekday") |>
               select(tucaseid, sex),
             by = "tucaseid") |>
  group_by(sex, hour, major_name) |>
  summarise(avg_min = mean(duration_min, na.rm = TRUE), .groups = "drop") |>
  collect()
print(hourly_avg)
#
#
#
activity_avg_plot <- activity_avg |>
  filter(major_name != "Data Codes") |>
  mutate(major_name = fct_reorder(major_name, mean_minutes, .fun = mean))

ggplot(activity_avg_plot, aes(x = major_name, y = mean_minutes, fill = sex)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Average Daily Minutes by Major Activity Category",
    subtitle = "Weekday respondents not in the labor force, by sex",
    x = "Major activity category",
    y = "Average daily minutes",
    fill = "Sex",
    caption = "Source: American Time Use Survey (ATUS)"
  )
#
#
#
#
