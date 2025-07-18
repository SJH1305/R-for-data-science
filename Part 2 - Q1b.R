str(planedata)

colnames(df_2004)

# Identifying planes which flew in 2004-08 from plane_data
library(dplyr)

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

# Filter plane_data for matching tailnum
actuallyflew_all <- planedata %>%
  filter(tailnum %in% all_tailnums$TailNum)

# Making the age variable based on year 
actuallyflew_2004 <- actuallyflew_2004 %>%
  mutate(year = as.numeric(as.character(year)),
         age_y = 2004 - year) %>%
  filter(age_y >= 0) %>%
  select(tailnum, year, age_y)

actuallyflew_2005 <- actuallyflew_2005 %>%
  mutate(year = as.numeric(as.character(year)),
         age_y = 2005 - year) %>%
  filter(age_y >= 0) %>%
  select(tailnum, year, age_y)

actuallyflew_2006 <- actuallyflew_2006 %>%
  mutate(year = as.numeric(as.character(year)),
         age_y = 2006 - year) %>%
  filter(age_y >= 0) %>%
  select(tailnum, year, age_y)

actuallyflew_2007 <- actuallyflew_2007 %>%
  mutate(year = as.numeric(as.character(year)),
         age_y = 2007 - year) %>%
  filter(age_y >= 0) %>%
  select(tailnum, year, age_y)

actuallyflew_2008 <- actuallyflew_2008 %>%
  mutate(year = as.numeric(as.character(year)),
         age_y = 2008 - year) %>%
  filter(age_y >= 0) %>%
  select(tailnum, year, age_y)


# viewing bad dates - year
# View bad year-based ages
neg_count <- data.frame(
  year = 2004:2008,
  neg_age_count = c(
    sum(actuallyflew_2004$age_y < 0, na.rm = TRUE),
    sum(actuallyflew_2005$age_y < 0, na.rm = TRUE),
    sum(actuallyflew_2006$age_y < 0, na.rm = TRUE),
    sum(actuallyflew_2007$age_y < 0, na.rm = TRUE),
    sum(actuallyflew_2008$age_y < 0, na.rm = TRUE)
  )
)

print(neg_count)

# Overall average 
# getting averages and visualising 
library(dplyr)

# Combine all years' data
all_data <- bind_rows(df_2004, df_2005, df_2006, df_2007, df_2008)

# Compute average TotalDelay
avg_delay <- all_data %>%
  summarise(avg_delay = mean(TotalDelay, na.rm = TRUE))

print(avg_delay)

# identified additional bad data - year is 0? 
# Arrange by age_y descending and select columns
actuallyflew_2004 %>%
  arrange(year) %>%
  select(age_y, year)

library(dplyr)
library(stringr)

# 1. Compute average delay by TailNum across all years once
all_data <- bind_rows(df_2004, df_2005, df_2006, df_2007, df_2008) %>%
  mutate(
    TotalDelay = rowSums(across(c(ArrDelay, DepDelay)), na.rm = TRUE),
    TailNum = str_trim(TailNum)
  )

avg_delay_by_tail <- all_data %>%
  group_by(TailNum) %>%
  summarise(avg_total_delay = mean(TotalDelay, na.rm = TRUE), .groups = "drop")

# 2. For each year, join avg delay and filter:

actuallyflew_2004_clean <- actuallyflew_2004 %>%
  mutate(tailnum = str_trim(tailnum)) %>%
  left_join(avg_delay_by_tail, by = c("tailnum" = "TailNum")) %>%
  filter(age_y >= 0, age_y <= 48, !is.na(avg_total_delay)) %>%
  select(tailnum, age_y, Total_Delay = avg_total_delay)

actuallyflew_2005_clean <- actuallyflew_2005 %>%
  mutate(tailnum = str_trim(tailnum)) %>%
  left_join(avg_delay_by_tail, by = c("tailnum" = "TailNum")) %>%
  filter(age_y >= 0, age_y <= 48, !is.na(avg_total_delay)) %>%
  select(tailnum, age_y, Total_Delay = avg_total_delay)

actuallyflew_2006_clean <- actuallyflew_2006 %>%
  mutate(tailnum = str_trim(tailnum)) %>%
  left_join(avg_delay_by_tail, by = c("tailnum" = "TailNum")) %>%
  filter(age_y >= 0, age_y <= 48, !is.na(avg_total_delay)) %>%
  select(tailnum, age_y, Total_Delay = avg_total_delay)

actuallyflew_2007_clean <- actuallyflew_2007 %>%
  mutate(tailnum = str_trim(tailnum)) %>%
  left_join(avg_delay_by_tail, by = c("tailnum" = "TailNum")) %>%
  filter(age_y >= 0, age_y <= 48, !is.na(avg_total_delay)) %>%
  select(tailnum, age_y, Total_Delay = avg_total_delay)

actuallyflew_2008_clean <- actuallyflew_2008 %>%
  mutate(tailnum = str_trim(tailnum)) %>%
  left_join(avg_delay_by_tail, by = c("tailnum" = "TailNum")) %>%
  filter(age_y >= 0, age_y <= 48, !is.na(avg_total_delay)) %>%
  select(tailnum, age_y, Total_Delay = avg_total_delay)

library(ggplot2)

# Create age groups (bins of 5 years from 0 to 45)
plot_data <- plot_data %>%
  mutate(age_group = cut(age_y, breaks = seq(0, 50, by = 5), right = FALSE))

# Boxplot
ggplot(plot_data, aes(x = age_group, y = Total_Delay2004)) +
  geom_boxplot() +
  labs(
    x = "Aircraft Age Group (Years)",
    y = "Average Total Delay (Minutes)",
    title = "Distribution of Delay by Aircraft Age Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



