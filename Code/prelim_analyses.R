# Panic Model Data Exploration and Prelim Analyses
# Updated 2025-02-05

library(here)
library(tidyverse)
library(ggplot2)
library(qgraph)
library(bootnet)
library(psychonetrics)

# things to include:
# number of participants on x axis by week, group
# look at 17 weeks
# spaghetti plot with individual trajectories in different colors to show variability around main effect
# look at people with panic who did some exposure - look to see what the breakdown of packages was for red line, can show that people got exposure and that it didn't help for panic (potentially) - descriptive statitics on the red line to show differences
# qualitative data from the modules on 

# source processing script
source('/Users/natechoukas/Documents/Research/Panic Model x RainFrog/Code/panic_alac_data_cleaning.R')

# to do
# look at histograms of weeks and put N participants on x axis - set some sort of cutoff
# look at plots with all individual trajectories
# look at qualitative data from the modules
# bl contemporaneous network with panic sx

# 1. Characterize Sample
panic_data_st <- panic_data %>% filter(week <17)
ggplot(panic_data_st, aes(x = week, y = pdss_total, color = factor(panic_tx))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "PDSS Total Over Time by Treatment Group",
       x = "Weeks", y = "PDSS Total", color = "Panic Treatment") +
  theme_minimal()

ggplot(panic_data, aes(x = week, y = fq_total, color = factor(panic_tx))) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "FQ Total Over Time by Treatment Group",
       x = "Weeks", y = "FQ Total", color = "Panic Treatment") +
  theme_minimal()

# define panic ids
panic_ids <- unique(panic_data$study_id_mspss)
panic_tx_ids <- unique(panic_data$study_id_mspss[panic_data$panic_tx == 1])

# summary data for all ids above panic threshold
module_summary_data <- module_summary_data %>%
  filter(study_id_mspss %in% panic_ids)

module_summary_statistics <- module_summary_data %>%
  mutate(
    percentage_completed = total_modules_completed / total_modules_started
  ) %>%
  summarise(
    mean_modules_started = mean(total_modules_started, na.rm = TRUE),
    mean_modules_completed = mean(total_modules_completed, na.rm = TRUE),
    mean_percentage_completed = mean(percentage_completed, na.rm = TRUE)
  )
print(module_summary_statistics)

# bar graph
# Example mean values from module_summary_statistics
mean_modules_completed <- module_summary_statistics$mean_modules_completed
mean_percentage_completed <- module_summary_statistics$mean_percentage_completed

# Bar graph with additional labels
ggplot(module_summary_data, aes(x = factor(total_modules_completed))) +
  geom_bar(fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, color = "black", size = 8) +  # Add count labels
  labs(title = "Total Modules Completed",
       x = "Total Modules Completed",
       y = "Number of Participants") +
  scale_x_discrete(limits = as.character(0:8)) +
  ylim(0, 25) +
  theme(
    axis.title.x = element_text(size = 20),    # Increase x-axis label size
    axis.title.y = element_text(size = 20),    # Increase y-axis label size
    axis.text.x = element_text(size = 16),     # Increase x-axis text size
    axis.text.y = element_text(size = 16),     # Increase y-axis text size
    plot.title = element_text(size = 20),      # Increase title size
    legend.text = element_text(size = 16),     # Increase legend text size (if applicable)
    legend.title = element_text(size = 18)     # Increase legend title size (if applicable)
  ) +
  # Add mean labels
  annotate(
    "text",
    x = 3,  # Adjust x to position the mean label (center of the graph)
    y = 24, # Adjust y to place it above the bars
    label = paste0("Mean Modules Completed: ", round(mean_modules_completed, 2)),
    size = 6,
    color = "black"
  ) +
  annotate(
    "text",
    x = 3,  # Align with the first annotation
    y = 23, # Adjust y for the second annotation
    label = paste0("Mean Percentage Completed: ", round(mean_percentage_completed*100, 2), "%"),
    size = 6,
    color = "black"
  )

# summary data for panic tx ids
module_summary_data <- module_summary_data %>%
  filter(study_id_mspss %in% panic_tx_ids)

module_summary_statistics <- module_summary_data %>%
  mutate(
    percentage_completed = total_modules_completed / total_modules_started
  ) %>%
  summarise(
    mean_modules_started = mean(total_modules_started, na.rm = TRUE),
    mean_modules_completed = mean(total_modules_completed, na.rm = TRUE),
    mean_percentage_completed = mean(percentage_completed, na.rm = TRUE)
  )
print(module_summary_statistics)

# bar graph
# Example mean values from module_summary_statistics
mean_modules_completed <- module_summary_statistics$mean_modules_completed
mean_percentage_completed <- module_summary_statistics$mean_percentage_completed

# Bar graph with additional labels
ggplot(module_summary_data, aes(x = factor(total_modules_completed))) +
  geom_bar(fill = "blue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, color = "black", size = 8) +  # Add count labels
  labs(title = "Total Modules Completed",
       x = "Total Modules Completed",
       y = "Number of Participants") +
  scale_x_discrete(limits = as.character(0:8)) +
  ylim(0, 25) +
  theme(
    axis.title.x = element_text(size = 20),    # Increase x-axis label size
    axis.title.y = element_text(size = 20),    # Increase y-axis label size
    axis.text.x = element_text(size = 16),     # Increase x-axis text size
    axis.text.y = element_text(size = 16),     # Increase y-axis text size
    plot.title = element_text(size = 20),      # Increase title size
    legend.text = element_text(size = 16),     # Increase legend text size (if applicable)
    legend.title = element_text(size = 18)     # Increase legend title size (if applicable)
  ) +
  # Add mean labels
  annotate(
    "text",
    x = 3,  # Adjust x to position the mean label (center of the graph)
    y = 24, # Adjust y to place it above the bars
    label = paste0("Mean Modules Completed: ", round(mean_modules_completed, 2)),
    size = 6,
    color = "black"
  ) +
  annotate(
    "text",
    x = 3,  # Align with the first annotation
    y = 23, # Adjust y for the second annotation
    label = paste0("Mean Percentage Completed: ", round(mean_percentage_completed*100, 2), "%"),
    size = 6,
    color = "black"
  ) # adherence looks better! - show this plot next to the module diagrams to show who did what


# see if there are any panic tx / panic threshold people in alacrity


####

panic_summary <- panic_data %>%
  group_by(panic_tx) %>%
  summarise(unique_participants = n_distinct(study_id_mspss)) %>%
  arrange(desc(unique_participants))

print(panic_summary)








# 2. Symptom Trajectory Plots





#### BL contemporaneous network model ####
bl_pdss <- data_panic %>% 
  filter(week == 0) %>% 
  select(
    study_id_mspss,
    pdss_q1_bl,
    pdss_q2_bl,
    pdss_q3_bl,
    pdss_q4_bl,
    pdss_q5_bl,
    pdss_q6_bl,
    pdss_q7_bl
  ) %>% 
  na.omit()

obsvars <- c(
  "pdss_q1_bl",
  "pdss_q2_bl",
  "pdss_q3_bl",
  "pdss_q4_bl",
  "pdss_q5_bl",
  "pdss_q6_bl",
  "pdss_q7_bl"
)

# form GGM modle
ggm1 <- ggm(bl_pdss, vars = obsvars)

ggm1 <- ggm1 %>% runmodel # run model

ggm1 %>% parameters # check parameters

# prune to significant edges
ggm1 <- ggm1 %>% 
  prune(adjust = "fdr", alpha = 0.01)

# check modification indices:
ggm1 %>% MIs

# stepup estimation:
ggm1 <- ggm1 %>% stepup(criterion = "bic", alpha = 0.05)

# inspect fit
ggm1 %>% fit

# obtain network
net1 <- getmatrix(ggm1, "omega")

# Plot:
qgraph(net1, 
       layout = "spring", 
       theme = "colorblind",
       labels = obsvars)




