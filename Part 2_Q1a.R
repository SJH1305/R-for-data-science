# Reading in the files 
df_2004 <- read.csv("2004.csv.bz2")
df_2005 <- read.csv("2005.csv.bz2")
df_2006 <- read.csv("2006.csv.bz2")
df_2007 <- read.csv("2007.csv.bz2")
df_2008 <- read.csv("2008.csv.bz2")
airports <- read.csv("airports.csv")
carriers <- read.csv("carriers.csv")
planedata <- read.csv("plane-data.csv")
variabledescriptions <- read.csv("variable-descriptions.csv")

library(tidyverse)

# Understanding the data:
#str(df_2004)
#variabledescriptions

# Creating new variable - Totaldelay
df_2004$TotalDelay <- rowSums(df_2004[, c("ArrDelay", "DepDelay")], na.rm = TRUE)
df_2005$TotalDelay <- rowSums(df_2005[, c("ArrDelay", "DepDelay")], na.rm = TRUE)
df_2006$TotalDelay <- rowSums(df_2006[, c("ArrDelay", "DepDelay")], na.rm = TRUE)
df_2007$TotalDelay <- rowSums(df_2007[, c("ArrDelay", "DepDelay")], na.rm = TRUE)
df_2008$TotalDelay <- rowSums(df_2008[, c("ArrDelay", "DepDelay")], na.rm = TRUE)

# Insight 1: Average delay - Days of the week
library(dplyr)

avg_dailydelay_2004 <- df_2004 %>%
  group_by(DayOfWeek) %>%
  summarise(avg_TotalDelay = mean(TotalDelay, na.rm = TRUE))

avg_dailydelay_2005 <- df_2005 %>%
  group_by(DayOfWeek) %>%
  summarise(avg_TotalDelay = mean(TotalDelay, na.rm = TRUE))

avg_dailydelay_2006 <- df_2006 %>%
  group_by(DayOfWeek) %>%
  summarise(avg_TotalDelay = mean(TotalDelay, na.rm = TRUE))

avg_dailydelay_2007 <- df_2007 %>%
  group_by(DayOfWeek) %>%
  summarise(avg_TotalDelay = mean(TotalDelay, na.rm = TRUE))

avg_dailydelay_2008 <- df_2008 %>%
  group_by(DayOfWeek) %>%
  summarise(avg_TotalDelay = mean(TotalDelay, na.rm = TRUE))

# Add a 'Year' column to each summary first
avg_dailydelay_2004 <- avg_dailydelay_2004 %>% mutate(Year = 2004)
avg_dailydelay_2005 <- avg_dailydelay_2005 %>% mutate(Year = 2005)
avg_dailydelay_2006 <- avg_dailydelay_2006 %>% mutate(Year = 2006)
avg_dailydelay_2007 <- avg_dailydelay_2007 %>% mutate(Year = 2007)
avg_dailydelay_2008 <- avg_dailydelay_2008 %>% mutate(Year = 2008)

# Combine all into one table
avg_dailydelay_all <- bind_rows(
  avg_dailydelay_2004,
  avg_dailydelay_2005,
  avg_dailydelay_2006,
  avg_dailydelay_2007,
  avg_dailydelay_2008
)

# Reshape into wide format: DayOfWeek as rows, years as columns
avg_dailydelay_wide <- avg_dailydelay_all %>%
  pivot_wider(names_from = Year, values_from = avg_TotalDelay)

# Print the table
print(avg_dailydelay_wide)

# Additional method 
library(ggplot2)
library(dplyr)

# Add year column to each dataframe
df_2004 <- df_2004 %>% mutate(Year = 2004)
df_2005 <- df_2005 %>% mutate(Year = 2005)
df_2006 <- df_2006 %>% mutate(Year = 2006)
df_2007 <- df_2007 %>% mutate(Year = 2007)
df_2008 <- df_2008 %>% mutate(Year = 2008)

# Combine all data
df_all <- bind_rows(df_2004, df_2005, df_2006, df_2007, df_2008)

# Convert DayOfWeek to factor with labels
df_all <- df_all %>%
  mutate(
    DayOfWeek = factor(DayOfWeek, levels = 1:7,
                       labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  )

# Plot using facet_wrap for each year
ggplot(df_all, aes(x = DayOfWeek, y = TotalDelay)) +
  geom_boxplot(outlier.size = 0.4, fill = "steelblue") +
  facet_wrap(~ Year, ncol = 2) +
  labs(title = "Total Delay by Day of Week (2004–2009)",
       x = "Day of Week", y = "Total Delay (mins)") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

# Insight 2: Times of the week 
# Getting delay averages in terms of scheduled departure 
# Same, previously calculated total delay but now ranked according to CRS dep time
library(dplyr)

avg_timedelay_2004 <- df_2004 %>%
  group_by(CRSDepTime) %>%
  summarise(AvgTotalDelay = mean(TotalDelay, na.rm = TRUE))

avg_timedelay_2005 <- df_2005 %>%
  group_by(CRSDepTime) %>%
  summarise(AvgTotalDelay = mean(TotalDelay, na.rm = TRUE))

avg_timedelay_2006 <- df_2006 %>%
  group_by(CRSDepTime) %>%
  summarise(AvgTotalDelay = mean(TotalDelay, na.rm = TRUE))

avg_timedelay_2007 <- df_2007 %>%
  group_by(CRSDepTime) %>%
  summarise(AvgTotalDelay = mean(TotalDelay, na.rm = TRUE))

avg_timedelay_2008 <- df_2008 %>%
  group_by(CRSDepTime) %>%
  summarise(AvgTotalDelay = mean(TotalDelay, na.rm = TRUE))

library(dplyr)
library(tidyr)

# Add year column to each summary
avg_timedelay_2004 <- avg_timedelay_2004 %>% mutate(Year = 2004)
avg_timedelay_2005 <- avg_timedelay_2005 %>% mutate(Year = 2005)
avg_timedelay_2006 <- avg_timedelay_2006 %>% mutate(Year = 2006)
avg_timedelay_2007 <- avg_timedelay_2007 %>% mutate(Year = 2007)
avg_timedelay_2008 <- avg_timedelay_2008 %>% mutate(Year = 2008)

# Combine all years
avg_timedelay_all <- bind_rows(
  avg_timedelay_2004,
  avg_timedelay_2005,
  avg_timedelay_2006,
  avg_timedelay_2007,
  avg_timedelay_2008
)

# Reshape to wide format: CRSDepTime as rows, years as columns
avg_timedelay_wide <- avg_timedelay_all %>%
  pivot_wider(names_from = Year, values_from = AvgTotalDelay)

# View result
print(avg_timedelay_wide)

# Count of unique CRS flights 
length(unique(df_2004$CRSDepTime))


# Line plot - visualising and understanding if flights are the same throughout the week by count 
# Create CRSHour by integer division (floor division) of CRSDepTime by 100
df_2004 <- df_2004 %>%
  mutate(CRSHour = CRSDepTime %/% 100)  # integer division

# Count flights by DayOfWeek and CRSHour
hourly_counts <- df_2004 %>%
  group_by(DayOfWeek, CRSHour) %>%
  summarise(FlightCount = n(), .groups = "drop")

# Optional: convert DayOfWeek to factor with labels for nicer legend
hourly_counts <- hourly_counts %>%
  mutate(DayOfWeek = factor(DayOfWeek, levels = 1:7,
                            labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

# Plot
ggplot(hourly_counts, aes(x = CRSHour, y = FlightCount, color = DayOfWeek)) +
  geom_line(size = 1) +
  labs(title = "Scheduled Flights per Hour by Day of Week (2004)",
       x = "Hour of Day",
       y = "Number of Flights",
       color = "Day of Week") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

