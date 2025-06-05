# Descriptive Statistics --------------------------------------------------

#Loading Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(fixest)
library(ggfixest)
library(bacondecomp)
library(ggplot2)
library(htmlwidgets)
library(webshot2)
getwd()

my_data <- readRDS("/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/cleaned_objects.rds")
fs <- my_data$fs
reg_data <- my_data$reg_data
#summary table for food scarcity
fs_summary_table <- reg_data %>%
  filter(!is.na(Avg_FoodScarcity_Percent)) %>%
  group_by(ea_treatment, calendar_year) %>%
  summarise(
    Mean_FS = round(mean(Avg_FoodScarcity_Percent, na.rm = TRUE), 2),
    SD_FS = round(sd(Avg_FoodScarcity_Percent, na.rm = TRUE), 2),
    Min_FS = round(min(Avg_FoodScarcity_Percent, na.rm = TRUE), 2),
    Max_FS = round(max(Avg_FoodScarcity_Percent, na.rm = TRUE), 2),
    n = n(),
    .groups = "drop"
  )

#summary table for the controls - food assistance, job loss and median household income
summary_combined_controls <- reg_data %>%
  group_by(calendar_year) %>%
  summarise(
    FA_Mean = round(mean(FOODASSIS_RATE_avg, na.rm = TRUE), 2),
    FA_SD   = round(sd(FOODASSIS_RATE_avg, na.rm = TRUE), 2),
    JL_Mean = round(mean(JOBLOSS_RATE_avg, na.rm = TRUE), 2),
    JL_SD   = round(sd(JOBLOSS_RATE_avg, na.rm = TRUE), 2),
    Income_Mean = round(mean(Median_Household_Income, na.rm = TRUE), 0),
    Income_SD   = round(sd(Median_Household_Income, na.rm = TRUE), 0),
    .groups = "drop"
  ) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

#Plot food scarcity trend for each group of states that ended EA within the given year

#Create grouped trends for each EA end year
trend_data <- reg_data %>%
  filter(ea_end_year %in% c(2021, 2022, 2023)) %>%
  group_by(ea_end_year, calendar_year) %>%
  summarise(Avg_FS = mean(Avg_FoodScarcity_Percent, na.rm = TRUE), .groups = "drop") %>%
  mutate(group_label = paste0("EA Ended in ", ea_end_year)) %>%
  complete(calendar_year = 2020:2024, nesting(ea_end_year, group_label))

# Plot all three lines in one graph
plot <- ggplot(trend_data, aes(x = calendar_year, y = Avg_FS, color = group_label)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2021, linetype = "dashed", color = "#00583D") +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "#F58220") +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "#6950A1") +
  annotate("text", x = 2020, y = max(trend_data$Avg_FS, na.rm = TRUE), label = "EA Starts", vjust = -0.5, color = "black", size = 4) +
  annotate("text", x = 2021, y = max(trend_data$Avg_FS, na.rm = TRUE), label = "EA Ends (2021)", vjust = -0.5, color = "#00583D", size = 4) +
  annotate("text", x = 2022, y = max(trend_data$Avg_FS, na.rm = TRUE), label = "EA Ends (2022)", vjust = -0.5, color = "#F58220", size = 4) +
  annotate("text", x = 2023, y = max(trend_data$Avg_FS, na.rm = TRUE), label = "EA Ends (2023)", vjust = -0.5, color = "#6950A1", size = 4) +
  labs(
    title = "Food Scarcity Trends for States by EA End Year",
    x = "Year",
    y = "Food Scarcity (%)",
    color = "Treatment Group"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 2020:2024) +
  scale_color_manual(values = c(
    "EA Ended in 2021" = "#00583D",
    "EA Ended in 2022" = "#F58220",
    "EA Ended in 2023" = "#6950A1"
  )) +
  theme(legend.position = "bottom")

#Save objects and plot
saveRDS(list(fs_summary_table = fs_summary_table, summary_combined_controls = summary_combined_controls, plot = plot ), "descriptive_visuals.rds")
ggsave("plot_fs_trend.png", plot, width = 10, height = 6, dpi = 300)


# Regressions and Estimation ----------------------------------------------

event_study <- feols(Avg_FoodScarcity_Percent ~ i(rel_year, ea_ended, ref = -1) + `Total Cost` + `No. of Persons` + Median_Household_Income | State + calendar_year,
                     data = reg_data,
                     cluster = ~State)

summary(event_study)

event_study_plot <- iplot(event_study,
      main = "Effect of Early EA Expiration on Food Scarcity",
      xlab = "Years since EA ended",
      ylab = "Estimated Effect on Food Scarcity (%)")

#Goodman Bacon Decomposition
df_bacon <- bacon(Avg_FoodScarcity_Percent ~ ea_ended,
                  data = joined_data,
                  id_var = "State",
                  time_var = "calendar_year")

coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
coef_bacon

bacon <- ggplot(df_bacon) +
  aes(x = weight, y = estimate, color = factor(type), shape = factor(type)) +
  geom_point(size=4) + 
  geom_hline(yintercept = coef_bacon, lty  = 2) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  labs(color    = "Type", 
       shape    = "Type") +
  theme_minimal() +
  scale_colour_manual(values = c("#00583D", "#F58220", "#6950A1"))

saveRDS(event_study, "event_study.rds")
ggsave("bacon.png", bacon, width = 10, height = 6, dpi = 300)
saveWidget(event_study_plot, "event_study_plot.html", selfcontained = TRUE)
webshot("event_study_plot.html", file = "event_study_plot.png", vwidth = 1000, vheight = 700)

