library(dplyr)
library(stringr)
library(ggplot2)

# Combine TailNum columns from all years
all_tailnums <- bind_rows(
  df_2004 %>% select(TailNum),
  df_2005 %>% select(TailNum),
  df_2006 %>% select(TailNum),
  df_2007 %>% select(TailNum),
  df_2008 %>% select(TailNum)
) %>%
  filter(!is.na(TailNum)) %>%
  distinct()

# Filter aircraft that actually flew
actuallyflew_all <- planedata %>%
  filter(tailnum %in% all_tailnums$TailNum)

# Add TotalDelay to each year's df
df_2004 <- df_2004 %>% mutate(TotalDelay = rowSums(across(c(ArrDelay, DepDelay)), na.rm = TRUE))
df_2005 <- df_2005 %>% mutate(TotalDelay = rowSums(across(c(ArrDelay, DepDelay)), na.rm = TRUE))
df_2006 <- df_2006 %>% mutate(TotalDelay = rowSums(across(c(ArrDelay, DepDelay)), na.rm = TRUE))
df_2007 <- df_2007 %>% mutate(TotalDelay = rowSums(across(c(ArrDelay, DepDelay)), na.rm = TRUE))
df_2008 <- df_2008 %>% mutate(TotalDelay = rowSums(across(c(ArrDelay, DepDelay)), na.rm = TRUE))

# Combine all years into one
all_data <- bind_rows(df_2004, df_2005, df_2006, df_2007, df_2008) %>%
  mutate(TailNum = str_trim(TailNum))

# Compute average delay per TailNum
avg_delay_by_tail <- all_data %>%
  group_by(TailNum) %>%
  summarise(avg_total_delay = mean(TotalDelay, na.rm = TRUE), .groups = "drop")

# Helper function for processing per year
process_year_data <- function(year, actuallyflew_data) {
  actuallyflew_data %>%
    mutate(
      year = as.numeric(as.character(year)),
      age_y = year - as.numeric(as.character(year)),
      tailnum = str_trim(tailnum)
    ) %>%
    mutate(age_y = year - year) %>%  # Will result in 0 — replace below
    mutate(age_y = year - as.numeric(as.character(year))) %>%
    left_join(avg_delay_by_tail, by = c("tailnum" = "TailNum")) %>%
    filter(age_y >= 0, age_y <= 48, !is.na(avg_total_delay)) %>%
    select(tailnum, age_y, Total_Delay = avg_total_delay) %>%
    mutate(flight_year = year)
}

# Run for all years
plot_data <- bind_rows(
  process_year_data(2004, actuallyflew_2004),
  process_year_data(2005, actuallyflew_2005),
  process_year_data(2006, actuallyflew_2006),
  process_year_data(2007, actuallyflew_2007),
  process_year_data(2008, actuallyflew_2008)
)

# Group into 5-year age bins
plot_data <- plot_data %>%
  mutate(age_group = cut(age_y, breaks = seq(0, 50, by = 5), right = FALSE))

# Final plot
ggplot(plot_data, aes(x = age_group, y = Total_Delay)) +
  geom_boxplot() +
  labs(
    x = "Aircraft Age Group (Years)",
    y = "Average Total Delay (Minutes)",
    title = "Distribution of Delay by Aircraft Age Group (2004–2008)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


