#Load libraries
library(ggplot2)
library(tidyr)
library(reshape2)
library(dplyr)
library(scales)
library(gridExtra)
library(grid)
library(patchwork)
library(stringr)
library(cowplot)

#Read data - leading cause of death
data_M <- read.csv("Male Leading Cause of Death.csv", check.names = FALSE)
data_F <- read.csv("Female Leading Cause of Death.csv", check.names = FALSE)

#Convert data from wide to long format
data_long_M <- gather(data_M, Year, Deaths, -Cause)
data_long_F <- gather(data_F, Year, Deaths, -Cause)

#Convert some year and deaths to numeric
data_long_M$Year <- as.numeric(data_long_M$Year)
data_long_M$Deaths <- as.numeric(gsub(",", "", data_long_M$Deaths))
data_long_F$Year <- as.numeric(data_long_F$Year)
data_long_F$Deaths <- as.numeric(gsub(",", "", data_long_F$Deaths))

#Plot with formatted y-axis, x-axis, and legend below
Plot_M <- ggplot(data_long_M, aes(x = Year, y = Deaths, color = Cause, group = Cause)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) +  # Show y-axis in 1000s
  scale_x_continuous(breaks = seq(min(data_long_M$Year), max(data_long_M$Year), by = 4)) +  # More x-axis ticks
  labs(title = "Male Leading Causes of Death",
       x = "Year", y = "Number of Deaths (in Thousands)",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",  # Move legend to bottom left
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),  # Bold and centered title
        axis.title = element_text(face = "bold", size = 10),   # Bold axis titles
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face="bold",size = 10),
        legend.key.size = unit(0.5, "lines"))
Plot_F <- ggplot(data_long_F, aes(x = Year, y = Deaths, color = Cause, group = Cause)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, suffix = "K")) +  # Show y-axis in 1000s
  scale_x_continuous(breaks = seq(min(data_long_F$Year), max(data_long_F$Year), by = 4)) +  # More x-axis ticks
  labs(title = "Female Leading Causes of Death",
       x = "Year", y = "Number of Deaths (in Thousands)",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",  # Move legend to bottom left
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5),  # Bold and centered title
        axis.title = element_text(face = "bold", size = 10),   # Bold axis titles
        axis.text = element_text(face = "bold", size = 10),
        legend.text = element_text(face="bold", size = 10),
        legend.key.size = unit(0.5, "lines"))

combined <- Plot_F + Plot_M
#Save a ggplot
ggsave("Death_Trends.png",combined, width = 8, height = 3.5, dpi = 300)

#Read the CSV file - Lee-Carter mx for both genders
dataF <- read.csv("LCmxF.csv", check.names = FALSE)
dataM <- read.csv("LCmxM.csv", check.names = FALSE)

#Convert data to long format
data_longF <- gather(dataF, Year, Value, -Age)
data_longM <- gather(dataM, Year, Value, -Age)

#Convert columns to numeric
data_longF$Year <- as.numeric(data_longF$Year)
data_longF$Value <- as.numeric(data_longF$Value)
data_longM$Year <- as.numeric(data_longM$Year)
data_longM$Value <- as.numeric(data_longM$Value)

#Identify start and end points for labeling
start_pointsF <- data_longF %>% group_by(Age) %>% filter(Year == min(Year))
end_pointsF <- data_longF %>% group_by(Age) %>% filter(Year == max(Year))
label_pointsF <- bind_rows(start_pointsF, end_pointsF)
start_pointsM <- data_longM %>% group_by(Age) %>% filter(Year == min(Year))
end_pointsM <- data_longM %>% group_by(Age) %>% filter(Year == max(Year))
label_pointsM <- bind_rows(start_pointsM, end_pointsM)

#Create the plots
Plot_F <- ggplot(data_longF, aes(x = Year, y = Value, color = factor(Age), group = Age)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 3), nsmall = 3))
  })+
  scale_x_continuous(breaks = seq(min(data_longF$Year), max(data_longF$Year), by = 4)) +
  labs(
    title = "LC: Female Projected Mortality Rates",
    x = "Year",
    y = "Mortality Rates",
    color = "Age"
  ) +
  geom_text(
    data = label_pointsF,
    aes(label = format(round(Value, 4), nsmall = 4)),
    hjust = ifelse(label_pointsF$Year == min(label_pointsF$Year), -0.2, 1.2),
    nudge_y = case_when(
      label_pointsF$Age == 75 ~ -0.002,
      TRUE ~ 0.001  # push others up
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))
#ggsave("mxFemale.png", Plot_F, width = 6, height = 4, dpi = 300)

Plot_M <- ggplot(data_longM, aes(x = Year, y = Value, color = factor(Age), group = Age)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 3), nsmall = 3))
  })+
  scale_x_continuous(breaks = seq(min(data_longM$Year), max(data_longM$Year), by = 4)) +
  labs(
    title = "LC: Male Projected Mortality Rates",
    x = "Year",
    y = "Mortality Rates",
    color = "Age"
  ) +
  geom_text(
    data = label_pointsM,
    aes(label = format(round(Value, 4), nsmall = 4)),
    hjust = ifelse(label_pointsM$Year == min(label_pointsM$Year), -0.2, 1.2),
    nudge_y = case_when(
      label_pointsF$Age == 75 ~ -0.0025,
      TRUE ~ 0.0015  # push others up
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))
#ggsave("mxMale.png", Plot_M, width = 6, height = 4, dpi = 300)
# Save a ggplot
combined <- Plot_F + Plot_M
ggsave("LCmx.png",combined, width = 8, height = 4, dpi = 300)

#Read the CSV file - Lee-Carter ex for both genders
dataF <- read.csv("LCexF.csv", check.names = FALSE)
dataM <- read.csv("LCexM.csv", check.names = FALSE)

#Convert data to long format
data_longF <- dataF %>%
  pivot_longer(cols = -Year, names_to = "Age", values_to = "Value")
data_longM <- dataM %>%
  pivot_longer(cols = -Year, names_to = "Age", values_to = "Value")

#Convert to numeric
data_longF$Year <- as.numeric(data_longF$Year)
data_longF$Value <- as.numeric(data_longF$Value)
data_longM$Year <- as.numeric(data_longM$Year)
data_longM$Value <- as.numeric(data_longM$Value)

#Identify start and end points
start_pointsF <- data_longF %>% group_by(Age) %>% filter(Year == min(Year))
end_pointsF   <- data_longF %>% group_by(Age) %>% filter(Year == max(Year))
label_pointsF <- bind_rows(start_pointsF, end_pointsF)
start_pointsM <- data_longM %>% group_by(Age) %>% filter(Year == min(Year))
end_pointsM   <- data_longM %>% group_by(Age) %>% filter(Year == max(Year))
label_pointsM <- bind_rows(start_pointsM, end_pointsM)

#Create the plots
Plot_F <- ggplot(data_longF, aes(x = Year, y = Value, color = factor(Age), group = Age)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 0), nsmall = 0))
  })+
  scale_x_continuous(breaks = seq(min(data_longF$Year), max(data_longF$Year), by = 4)) +
  labs(
    title = "LC: Female Projected Life Expectancies",
    x = "Year",
    y = "Life Expectancies",
    color = "Age"
  ) +
  geom_text(
    data = label_pointsF,
    aes(label = format(round(Value, 2), nsmall = 2)),
    hjust = ifelse(label_pointsF$Year == min(label_pointsF$Year), -0.2, 1.2),
    nudge_y = case_when(
      label_pointsF$Age == 35 ~ -1.3,
      TRUE ~ 1.3  # push others up
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))
#ggsave("exFemale.png", Plot_F, width = 6, height = 4, dpi = 300)

Plot_M <- ggplot(data_longM, aes(x = Year, y = Value, color = factor(Age), group = Age)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 0), nsmall = 0))
  })+
  scale_x_continuous(breaks = seq(min(data_longM$Year), max(data_longM$Year), by = 4)) +
  labs(
    title = "LC: Male Projected Life Expectancies",
    x = "Year",
    y = "Life Expectancies",
    color = "Age"
  ) +
  geom_text(
    data = label_pointsM,
    aes(label = format(round(Value, 2), nsmall = 2)),
    hjust = ifelse(label_pointsM$Year == min(label_pointsM$Year), -0.2, 1.2),
    nudge_y = case_when(
      label_pointsF$Age == 35 ~ -1.3,
      TRUE ~ 1.3  # push others up
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))
#ggsave("exMale.png", Plot_M, width = 6, height = 4, dpi = 300)

#Save a ggplot
combined <- Plot_F + Plot_M
ggsave("LCex.png",combined, width = 8, height = 4, dpi = 300)

#Read the CSV file - DGM ex for both gender
data35 <- read.csv("DGM35.csv", check.names = FALSE)
data55 <- read.csv("DGM55.csv", check.names = FALSE)
data75 <- read.csv("DGM75.csv", check.names = FALSE)

#Convert data to long format
data_long35 <- gather(data35, Category, Value, -Year)
data_long55 <- gather(data55, Category, Value, -Year)
data_long75 <- gather(data75, Category, Value, -Year)

#Convert year to numeric
data_long35$Year <- as.numeric(data_long35$Year)
data_long55$Year <- as.numeric(data_long55$Year)
data_long75$Year <- as.numeric(data_long75$Year)

#Split datasets for separate plots
life_expectancy_data35 <- data_long35 %>%
  filter(Category %in% c("Female Life Expectancy", "Male Life Expectancy", "Best Practice Life Expectancy"))
gap_data35 <- data_long35 %>%
  filter(Category %in% c("Sex Gap", "Best Practice Gap"))
life_expectancy_data55 <- data_long55 %>%
  filter(Category %in% c("Female Life Expectancy", "Male Life Expectancy", "Best Practice Life Expectancy"))
gap_data55 <- data_long55 %>%
  filter(Category %in% c("Sex Gap", "Best Practice Gap"))
life_expectancy_data75 <- data_long75 %>%
  filter(Category %in% c("Female Life Expectancy", "Male Life Expectancy", "Best Practice Life Expectancy"))
gap_data75 <- data_long75 %>%
  filter(Category %in% c("Sex Gap", "Best Practice Gap"))

#Identify start and end points for bold labels
life_expectancy_labels35 <- life_expectancy_data35 %>%
  group_by(Category) %>%
  filter(Year == min(Year) | Year == max(Year))
gap_labels35 <- gap_data35 %>%
  group_by(Category) %>%
  filter(Year == min(Year) | Year == max(Year))
life_expectancy_labels55 <- life_expectancy_data55 %>%
  group_by(Category) %>%
  filter(Year == min(Year) | Year == max(Year))
gap_labels55 <- gap_data55 %>%
  group_by(Category) %>%
  filter(Year == min(Year) | Year == max(Year))
life_expectancy_labels75 <- life_expectancy_data75 %>%
  group_by(Category) %>%
  filter(Year == min(Year) | Year == max(Year))
gap_labels75 <- gap_data75 %>%
  group_by(Category) %>%
  filter(Year == min(Year) | Year == max(Year))

#Identify last data point for each category to label the category name below the line
life_expectancy_end_labels35 <- life_expectancy_data35 %>%
  group_by(Category) %>%
  filter(Year == max(Year))
gap_end_labels35 <- gap_data35 %>%
  group_by(Category) %>%
  filter(Year == max(Year))
life_expectancy_end_labels55 <- life_expectancy_data55 %>%
  group_by(Category) %>%
  filter(Year == max(Year))
gap_end_labels55 <- gap_data55 %>%
  group_by(Category) %>%
  filter(Year == max(Year))
life_expectancy_end_labels75 <- life_expectancy_data75 %>%
  group_by(Category) %>%
  filter(Year == max(Year))
gap_end_labels75 <- gap_data75 %>%
  group_by(Category) %>%
  filter(Year == max(Year))

#Define colors
life_expectancy_colors <- c("Female Life Expectancy" = "pink", 
                            "Male Life Expectancy" = "blue", 
                            "Best Practice Life Expectancy" = "green")

gap_colors <- c("Sex Gap" = "purple", "Best Practice Gap" = "orange")


#Plot life expectancy
Plot_ex35 <- ggplot(life_expectancy_data35, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(size = 1) +
  scale_color_manual(values = life_expectancy_colors,
                     labels = c("Female Life Expectancy" = "Female", "Best Practice Life Expectancy" = "Best Practice", "Male Life Expectancy" = "Male")) +
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 0), nsmall = 0))
  })+
  scale_x_continuous(breaks = seq(min(life_expectancy_data35$Year), max(life_expectancy_data35$Year), by = 4)) +
  labs(
    title = "DGM: Age 35 Projected Life Expectancies",
    x = "Year",
    y = "Life Expectancies",
    color = "Category"
  ) +
  geom_text(
    data = life_expectancy_labels35,
    aes(label = format(round(Value, 2), nsmall = 2)),
    hjust = ifelse(life_expectancy_labels35$Year == min(life_expectancy_labels35$Year), -0.2, 1.2),
    nudge_y = case_when(
      life_expectancy_labels35$Value == 52.98 ~ -0.7,
      life_expectancy_labels35$Value == 48.96 ~ -0.4,
      life_expectancy_labels35$Value == 49.08 ~ 0.65,
      life_expectancy_labels35$Value == 45.37 ~ -0.3,
      TRUE ~ 0.3  # push others up
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))

Plot_ex55 <- ggplot(life_expectancy_data55, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(size = 1) +
  scale_color_manual(values = life_expectancy_colors,
                     labels = c("Female Life Expectancy" = "Female", "Best Practice Life Expectancy" = "Best Practice", "Male Life Expectancy" = "Male")) +
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 0), nsmall = 0))
  })+
  scale_x_continuous(breaks = seq(min(life_expectancy_data55$Year), max(life_expectancy_data55$Year), by = 4)) +
  labs(
    title = "DGM: Age 55 Projected Life Expectancies",
    x = "Year",
    y = "Life Expectancies",
    color = "Category"
  ) +
  geom_text(
    data = life_expectancy_labels55,
    aes(label = format(round(Value, 2), nsmall = 2)),
    hjust = ifelse(life_expectancy_labels55$Year == min(life_expectancy_labels55$Year), -0.2, 1.2),
    nudge_y = case_when(
      life_expectancy_labels55$Value == 29.92 ~ 0.65,
      life_expectancy_labels55$Value == 29.69 ~ -0.2,
      life_expectancy_labels55$Value == 32.82 ~ -0.6,
      life_expectancy_labels55$Value == 26.98 ~ -0.3,
      TRUE ~ 0.2  # push others up
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))

Plot_ex75 <- ggplot(life_expectancy_data75, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(size = 1) +
  scale_color_manual(values = life_expectancy_colors,
                     labels = c("Female Life Expectancy" = "Female", "Best Practice Life Expectancy" = "Best Practice", "Male Life Expectancy" = "Male")) +
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 0), nsmall = 0))
  })+
  scale_x_continuous(breaks = seq(min(life_expectancy_data75$Year), max(life_expectancy_data75$Year), by = 4)) +
  labs(
    title = "DGM: Age 75 Projected Life Expectancies",
    x = "Year",
    y = "Life Expectancies",
    color = "Category"
  ) +
  geom_text(
    data = life_expectancy_labels75,
    aes(label = format(round(Value, 2), nsmall = 2)),
    hjust = ifelse(life_expectancy_labels75$Year == min(life_expectancy_labels75$Year), -0.2, 1.2),
    nudge_y = case_when(
      life_expectancy_labels75$Value == 12.95 ~ -0.15,
      life_expectancy_labels75$Value == 14.78 ~ -0.4,
      life_expectancy_labels75$Value == 11.50 ~ -0.3,
      TRUE ~ 0.2  # push others up
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))

#Plot gender and best practice gaps
Plot_gap35 <- ggplot(gap_data35, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = gap_colors,
    labels = c("Sex Gap" = "Sex", "Best Practice Gap" = "Best Practice")
  )+
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 0), nsmall = 0))
  })+
  scale_x_continuous(breaks = seq(min(gap_data35$Year), max(gap_data35$Year), by = 4)) +
  labs(
    title = "DGM: Age 35 Projected Best and Gender Gaps",
    x = "Year",
    y = "Gap",
    color = "Category"
  ) +
  geom_text(
    data = gap_labels35,
    aes(label = format(round(Value, 2), nsmall = 2),
        hjust = ifelse(Year == min(Year), -0.2, 1.2)),
    nudge_y = case_when(
      gap_labels35$Value == 0.12 ~ 0.25,  # push down
      TRUE ~ 0.15
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))

Plot_gap55 <- ggplot(gap_data55, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = gap_colors,
    labels = c("Sex Gap" = "Sex", "Best Practice Gap" = "Best Practice")
  )+
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 0), nsmall = 0))
  })+
  scale_x_continuous(breaks = seq(min(gap_data55$Year), max(gap_data55$Year), by = 4)) +
  labs(
    title = "DGM: Age 55 Projected Best and Gender Gaps",
    x = "Year",
    y = "Gap",
    color = "Category"
  ) +
  geom_text(
    data = gap_labels55,
    aes(label = format(round(Value, 2), nsmall = 2),
        hjust = ifelse(Year == min(Year), -0.2, 1.2)),
    nudge_y = case_when(
      gap_labels55$Value == 0.12 ~ 0.25,  # push down
      TRUE ~ 0.15
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))

Plot_gap75 <- ggplot(gap_data75, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(size = 1) +
  scale_color_manual(
    values = gap_colors,
    labels = c("Sex Gap" = "Sex", "Best Practice Gap" = "Best Practice")
  )+
  scale_y_continuous(labels = function(x) {
    ifelse(x == 0, "0", format(round(x, 0), nsmall = 0))
  })+
  scale_x_continuous(breaks = seq(min(gap_data75$Year), max(gap_data75$Year), by = 4)) +
  labs(
    title = "DGM: Age 75 Projected Best and Gender Gaps",
    x = "Year",
    y = "Gap",
    color = "Category"
  ) +
  geom_text(
    data = gap_labels75,
    aes(label = format(round(Value, 2), nsmall = 2),
        hjust = ifelse(Year == min(Year), -0.2, 1.2)),
    nudge_y = case_when(
      gap_labels75$Value == -0.31 ~ 0.2,
      TRUE ~ 0.15
    ),
    size = 3.5,
    fontface = "bold",
    show.legend = FALSE
  )+
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(face = "bold", size = 10),
    legend.key.size = unit(0.5, "lines"))

#save ggplots
combined35 <- Plot_ex35 + Plot_gap35
combined55 <- Plot_ex55 + Plot_gap55
combined75 <- Plot_ex75 + Plot_gap75

ggsave("DGM35.png",combined35, width = 8, height = 4, dpi = 300)
ggsave("DGM55.png",combined55, width = 8, height = 4, dpi = 300)
ggsave("DGM75.png",combined75, width = 8, height = 4, dpi = 300)
