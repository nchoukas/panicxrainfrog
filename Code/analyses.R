# Panic x RF Analyses

# load libraries
library(here)
library(qgraph)
library(bootnet)
library(vars)
library(psychonetrics)
library(kableExtra)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# # source data cleaning
# source('/Users/natechoukas/Documents/Research/Panic Model x RainFrog/Code/panic_alac_data_cleaning.R')

data <- read.csv('/Users/natechoukas/Documents/Research/Panic Model x RainFrog/Data/panic_data.csv')

bl_data <- read.csv('/Users/natechoukas/Documents/Research/Panic Model x RainFrog/Data/bl_data_panic.csv')

module_1_data <- read.csv('/Users/natechoukas/Documents/Research/Panic Model x RainFrog/Data/panic_module_1.csv')

# subset data further
data <- data %>% 
  select(
    study_id_mspss,
    panic_pkg,
    started_tx,
    total_pdss_obs,
    week,
    pdss_q1,
    pdss_q2,
    pdss_q3,
    pdss_q4,
    pdss_q5,
    pdss_q6,
    pdss_q7,
    pdss_total
  ) %>%
  filter(started_tx == 1)

bl_data <- bl_data %>% 
  filter(started_tx == 1)

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# Summarize demographic data
demo_summary <- bl_data %>%
  group_by(panic_pkg) %>%
  summarise(
    N = n(),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    n_male = sum(gender == "Male", na.rm = TRUE),
    n_female = sum(gender == "Female", na.rm = TRUE),
    n_transgender = sum(gender == "Transgender", na.rm = TRUE),
    n_other_gender = sum(gender == "Do not identify as male, female or transgender", na.rm = TRUE),
    n_prefer_not_gender = sum(gender == "Prefer not to answer", na.rm = TRUE),
    n_white = sum(race == "White", na.rm = TRUE),
    n_black = sum(race == "Black or African American", na.rm = TRUE),
    n_asian = sum(race == "Asian", na.rm = TRUE),
    n_native = sum(race == "American Indian or Alaska Native", na.rm = TRUE),
    n_pacific_islander = sum(race == "Native Hawaiian or Other Pacific Islander", na.rm = TRUE),
    n_multiracial = sum(race == "More than one race", na.rm = TRUE),
    n_other_race = sum(race == "Other", na.rm = TRUE),
    n_hispanic = sum(ethnicity == "Hispanic/Latino", na.rm = TRUE),
    n_non_hispanic = sum(ethnicity == "Not Hispanic/Latino", na.rm = TRUE),
    .groups = "drop"
  )

# Reshape and format the summary
demo_summary_formatted <- demo_summary %>%
  mutate(
    group = ifelse(panic_pkg == 1, "Panic", "Other"),
    age = sprintf("%.2f (%.2f)", mean_age, sd_age),
    male = sprintf("%d (%.2f%%)", n_male, n_male / N * 100),
    female = sprintf("%d (%.2f%%)", n_female, n_female / N * 100),
    transgender = sprintf("%d (%.2f%%)", n_transgender, n_transgender / N * 100),
    other_gender = sprintf("%d (%.2f%%)", n_other_gender, n_other_gender / N * 100),
    prefer_not_gender = sprintf("%d (%.2f%%)", n_prefer_not_gender, n_prefer_not_gender / N * 100),
    white = sprintf("%d (%.2f%%)", n_white, n_white / N * 100),
    black = sprintf("%d (%.2f%%)", n_black, n_black / N * 100),
    asian = sprintf("%d (%.2f%%)", n_asian, n_asian / N * 100),
    native = sprintf("%d (%.2f%%)", n_native, n_native / N * 100),
    pacific_islander = sprintf("%d (%.2f%%)", n_pacific_islander, n_pacific_islander / N * 100),
    multiracial = sprintf("%d (%.2f%%)", n_multiracial, n_multiracial / N * 100),
    other_race = sprintf("%d (%.2f%%)", n_other_race, n_other_race / N * 100),
    hispanic = sprintf("%d (%.2f%%)", n_hispanic, n_hispanic / N * 100),
    non_hispanic = sprintf("%d (%.2f%%)", n_non_hispanic, n_non_hispanic / N * 100)
  ) %>%
  select(group, age, male, female, transgender, other_gender, prefer_not_gender,
         white, black, asian, native, pacific_islander, multiracial, other_race,
         hispanic, non_hispanic) %>%
  pivot_longer(-group, names_to = "Measure", values_to = "Value") %>%
  pivot_wider(names_from = group, values_from = Value)

# Create and display the table
demo_table <- demo_summary_formatted %>%
  kbl(col.names = c("Measure", "Panic (N=13)", "Other (N=38)"), align = "lcc") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Print the table
print(demo_table)

## Clinical Characteristics at Baseline
clinical_summary <- bl_data %>%
  group_by(panic_pkg) %>%
  summarise(
    N = n(),
    
    # Mean and SD of n_disorders_above_threshold
    mean_n_disorders = mean(n_disorders_above_thresh_bl, na.rm = TRUE),
    sd_n_disorders = sd(n_disorders_above_thresh_bl, na.rm = TRUE),
    
    # Mean and SD of total_modules_completed
    mean_total_modules = mean(total_modules, na.rm = TRUE),
    sd_total_modules = sd(total_modules, na.rm = TRUE),
    
    # Mean and SD of cnt_coach_session
    mean_coaching_sess = mean(cnt_coach_sessions, na.rm = TRUE),
    sd_coaching_sess = sd(cnt_coach_sessions, na.rm = TRUE),
    
    # Mean and SD of pdss_total
    mean_pdss_total = mean(pdss_total_bl, na.rm = TRUE),
    sd_pdss_total = sd(pdss_total_bl, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -panic_pkg, names_to = "Measure", values_to = "Value") %>%
  mutate(
    panic_pkg = case_when(
      panic_pkg == 1 ~ "Panic",
      panic_pkg == 0 ~ "Other",
      TRUE ~ as.character(panic_pkg)
    )
  ) %>%
  pivot_wider(names_from = panic_pkg, values_from = Value)

# Merge Mean and SD into M(SD) format while avoiding NAs
clinical_summary_formatted <- clinical_summary %>%
  arrange(Measure) %>%
  mutate(
    `Panic (N=13)` = case_when(
      grepl("mean_", Measure) ~ sprintf("%.2f (%.2f)", `Panic`, lead(`Panic`)),  # Use lead() instead of match()
      TRUE ~ as.character(`Panic`)
    ),
    `Other (N=38)` = case_when(
      grepl("mean_", Measure) ~ sprintf("%.2f (%.2f)", `Other`, lead(`Other`)),
      TRUE ~ as.character(`Other`)
    )
  ) %>%
  select(Measure, `Panic (N=13)`, `Other (N=38)`) %>%
  filter(!grepl("^sd_", Measure))  # Remove SD rows (now in M(SD))

# ==== Create Formatted Table ====
clinical_table <- clinical_summary_formatted %>%
  kable(format = "html", align = "c",
        col.names = c("Measure", "Panic (N=13)", "Other (N=38)"),
        caption = "Clinical Summary") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

# Display in R console and save as an HTML file
print(clinical_table)
writeLines(clinical_table, "clinical_summary_table.html")
browseURL("clinical_summary_table.html")



## Exploring Number of PDSS observations

# summary table
data_summary <- data %>%
  group_by(study_id_mspss) %>%  # Group by unique participant ID
  summarise(
    total_pdss_obs = sum(!is.na(pdss_total)), # Count non-missing PDSS observations
    panic_pkg = first(panic_pkg), # Keep treatment group for filtering
    started_tx = first(started_tx)
  ) %>%
  filter(started_tx == 1) %>% 
  ungroup()

# mean and SD
pdss_obs_summary <- data_summary %>% 
  group_by(panic_pkg) %>% 
  summarise(
    N = n(),
    mean_pdss_obs = mean(total_pdss_obs, na.rm = TRUE),
    sd_pdss_obs = sd(total_pdss_obs, na.rm = TRUE)
  )

# distribution (panic tx group)
pdss_obs_panic_pkg <- data_summary %>%
  filter(panic_pkg == 1)

ggplot(pdss_obs_panic_pkg, aes(x = total_pdss_obs)) +
  geom_histogram(binwidth = 1, fill = "navy", color = "black") +
  scale_x_continuous(breaks = seq(0, 21, by = 1)) +
  coord_cartesian(xlim = c(0, 21)) +  
  labs(
    title = "Total Number of PDSS Observations (Panic Tx)",
    x = "Number PDSS Observations",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# distribution (other tx group)
pdss_obs_oth_pkg <- data_summary %>%
  filter(panic_pkg == 0)

ggplot(pdss_obs_oth_pkg, aes(x = total_pdss_obs)) +
  geom_histogram(binwidth = 1, fill = "firebrick3", color = "black") +
  scale_x_continuous(breaks = seq(0, 21, by = 1)) +
  scale_y_continuous(labels = function(x) floor(x)) +  # Remove decimals from y-axis
  coord_cartesian(xlim = c(0, 21)) +  
  labs(
    title = "Total Number of PDSS Observations (Other Tx)",
    x = "Number PDSS Observations",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# total modules modules (panic tx)
bl_data_panix_tx <- bl_data %>% filter(panic_pkg == 1)

# Automatically determine the x-axis range
x_limits_panic <- as.character(seq(min(bl_data_panix_tx$total_modules, na.rm = TRUE), 
                                   max(bl_data_panix_tx$total_modules, na.rm = TRUE)))

# Compute mean
mean_modules_completed_panic <- mean(bl_data_panix_tx$total_modules, na.rm = TRUE)

# Plot
ggplot(bl_data_panix_tx, aes(x = factor(total_modules))) +
  geom_bar(fill = "navy", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, color = "black", size = 8) +  
  labs(title = "Total Modules Completed (Panic Treatment)",
       x = "Modules",
       y = "Participants") +
  scale_x_discrete(limits = x_limits_panic) + 
  ylim(0, 7) +
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) +
  annotate(
    "text",
    x = 6.5,
    y = 7,
    label = paste0("M=", round(mean_modules_completed_panic, 2)),
    size = 6,
    color = "black"
  )

# Filter for other treatment
bl_data_oth_tx <- bl_data %>% filter(panic_pkg == 0)

# Automatically determine the x-axis range based on the unique values present
x_limits_oth <- sort(unique(bl_data_oth_tx$total_modules))

# Compute mean
mean_modules_completed_oth <- mean(bl_data_oth_tx$total_modules, na.rm = TRUE)

# Plot
ggplot(bl_data_oth_tx, aes(x = factor(total_modules))) +
  geom_bar(fill = "firebrick3", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, color = "black", size = 8) +  
  labs(
    title = "Total Modules Completed (Other Treatment)",
    x = "Modules",
    y = "Participants"
  ) +
  scale_x_discrete(limits = as.character(x_limits_oth)) + 
  scale_y_continuous(breaks = seq(0, 10, by = 2)) +  # Ensure y-axis ticks are whole numbers
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18)
  ) +
  annotate(
    "text",
    x = 8.5,
    y = 10,
    label = paste0("M=", round(mean_modules_completed_oth, 2)),
    size = 6,
    color = "black"
  )

# total coaching sessions (panic tx)
mean_coach_sess_panic <- mean(bl_data_panix_tx$cnt_coach_sessions, na.rm = TRUE)

ggplot(bl_data_panix_tx, aes(x = cnt_coach_sessions)) +
  geom_bar(fill = "navy", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, color = "black", size = 8) +  # Add count labels
  labs(title = "Total Coaching Sessions",
       x = "Coaching Sessions",
       y = "Participants") +
  scale_x_discrete(limits = as.character(0:6)) +
  ylim(0, 7) +
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
    x = 6.5,  # Adjust x to position the mean label (center of the graph)
    y = 7, # Adjust y to place it above the bars
    label = paste0("M=", round(mean_coach_sess_panic, 2)),
    size = 6,
    color = "black"
  )

# total modules modules (other tx)
mean_coach_sess_oth <- sprintf("%.2f", mean(bl_data_oth_tx$cnt_coach_sessions, na.rm = TRUE))

ggplot(bl_data_oth_tx, aes(x = factor(cnt_coach_sessions))) +
  geom_bar(fill = "firebrick3", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, color = "black", size = 8) +  # Add count labels
  labs(title = "Total Coaching Sessions",
       x = "Coaching Sessions",
       y = "Participants") +
  scale_x_discrete(limits = as.character(0:9)) +
  ylim(0, 20) +
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
    x = 9.5,  # Adjust x to position the mean label (center of the graph)
    y = 20,    # Adjust y to place it above the bars
    label = paste0("M=", mean_coach_sess_oth),  # Ensures it prints "3.00" instead of "3"
    size = 6,
    color = "black"
  )

## Symptom Trajectory Plots
### tx phase ###
# Filter data for participants who started treatment
panic_data_tx_phase <- data %>%
  filter(week < 17, started_tx == 1)

# Count unique participants per week where pdss_total is NOT NA, split by panic package status
weekly_participant_counts <- panic_data_tx_phase %>%
  filter(!is.na(pdss_total)) %>%
  group_by(week, panic_pkg) %>%
  summarise(n_participants = n_distinct(study_id_mspss), .groups = "drop")

# Separate counts for text placement
weekly_panic_counts <- weekly_participant_counts %>%
  filter(panic_pkg == 1) %>%
  mutate(y_pos = 15)  # Position at the top

weekly_nonpanic_counts <- weekly_participant_counts %>%
  filter(panic_pkg == 0) %>%
  mutate(y_pos = 0.2)  # Position at the bottom

# Create the plot
ggplot(panic_data_tx_phase, aes(x = week, y = pdss_total, color = factor(panic_pkg))) +
  geom_smooth(method = "lm", se = TRUE) +  # Linear model smoothing
  geom_hline(yintercept = 8, linetype = "dashed", color = "black") +  # Clinical cutoff at PDSS = 8
  geom_text(data = weekly_panic_counts,  # Panic package participant counts
            aes(x = week, y = y_pos, label = n_participants),
            color = "navy", size = 4, vjust = 0) +
  geom_text(data = weekly_nonpanic_counts,  # Non-panic package participant counts
            aes(x = week, y = y_pos, label = n_participants),
            color = "firebrick3", size = 4, vjust = 0) +
  scale_color_manual(values = c("0" = "firebrick3", "1" = "navy")) +  # Set colors for panic_pkg
  labs(title = "PDSS Total Over Time by Treatment Group",
       subtitle = "Numbers indicate participants with PDSS observations per week\nDashed line indicates clinical cutoff",
       x = "Weeks", y = "PDSS Total", color = "Panic Treatment") +
  theme_minimal()


### Modeling
# bl contemporaneous network model 
bl_pdss <- bl_data %>% 
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
  rename(
    p1 = pdss_q1_bl,
    p2 = pdss_q2_bl,
    p3 = pdss_q3_bl,
    p4 = pdss_q4_bl,
    p5 = pdss_q5_bl,
    p6 = pdss_q6_bl,
    p7 = pdss_q7_bl,
  ) %>% 
  na.omit()

obsvars <- c(
  "p1",
  "p2",
  "p3",
  "p4",
  "p5",
  "p6",
  "p7"
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
layout(matrix(c(1, 2), nrow = 2), heights = c(7, 2))  # 2 rows: plot (7) and legend (2)

qgraph(net1,
       layout = "spring",  
       theme = "colorblind", 
       labels = obsvars)  

par(mar = c(0, 0, 0, 0))
plot.new()

legend("left", 
       legend = c(
         "p1 = Frequency of panic attacks during past week",
         "p2 = Distress during panic attacks",
         "p3 = Anxiety about future attacks",
         "p4 = Avoidance of places/situations associated with panic attacks",
         "p5 = Avoidance of physical sensations associated with panic attacks",
         "p6 = Interference with daily life",
         "p7 = Interference with social life"
       ),
       cex = 0.8, 
       bty = "n"
)

## PAI at baseline
# filter datasets
pai_pre <- module_1_data %>%
  filter(final_package_name == "Panic or Agoraphobia") %>%
  select(
    study_id_mspss, 
    pPAI2_pre_q1_z,
    pPAI2_pre_q2_z,
    pPAI2_pre_q3_z,
    pPAI2_pre_q4_z,
    pPAI2_pre_q5_z,
    pPAI2_pre_q6_z,
    pPAI2_pre_q7_z,
    pPAI2_pre_q8_z,
    pPAI2_pre_q9_z,
    pPAI2_pre_q10_z,
    pPAI2_pre_q11_z,
    pPAI2_pre_q12_z,
    pPAI2_pre_q13_z,
    pPAI2_pre_q14_z,
    pPAI2_pre_q15_z
  ) %>%
  rename(
    h.attack = pPAI2_pre_q1_z,
    faint = pPAI2_pre_q2_z,
    stroke = pPAI2_pre_q3_z,
    suffocate = pPAI2_pre_q4_z,
    die = pPAI2_pre_q5_z,
    stare = pPAI2_pre_q6_z,
    laugh = pPAI2_pre_q7_z,
    embars = pPAI2_pre_q8_z,
    scene = pPAI2_pre_q9_z,
    weird = pPAI2_pre_q10_z,
    insane = pPAI2_pre_q11_z,
    hyster = pPAI2_pre_q12_z,
    scream = pPAI2_pre_q13_z,
    uncnt.sense = pPAI2_pre_q14_z,
    uncnt.act = pPAI2_pre_q15_z
  )

pai_post <- module_1_data %>%
  filter(final_package_name == "Panic or Agoraphobia") %>%
  select(
    study_id_mspss,
    pPAI2_post_q1_z,
    pPAI2_post_q2_z,
    pPAI2_post_q3_z,
    pPAI2_post_q4_z,
    pPAI2_post_q5_z,
    pPAI2_post_q6_z,
    pPAI2_post_q7_z,
    pPAI2_post_q8_z,
    pPAI2_post_q9_z,
    pPAI2_post_q10_z,
    pPAI2_post_q11_z,
    pPAI2_post_q12_z,
    pPAI2_post_q13_z,
    pPAI2_post_q14_z,
    pPAI2_post_q15_z
  ) %>%
  rename(
    h.attack = pPAI2_post_q1_z,
    faint = pPAI2_post_q2_z,
    stroke = pPAI2_post_q3_z,
    suffocate = pPAI2_post_q4_z,
    die = pPAI2_post_q5_z,
    stare = pPAI2_post_q6_z,
    laugh = pPAI2_post_q7_z,
    embars = pPAI2_post_q8_z,
    scene = pPAI2_post_q9_z,
    weird = pPAI2_post_q10_z,
    insane = pPAI2_post_q11_z,
    hyster = pPAI2_post_q12_z,
    scream = pPAI2_post_q13_z,
    uncnt.sense = pPAI2_post_q14_z,
    uncnt.act = pPAI2_post_q15_z
  )

# Function to compute means for visualization
compute_means <- function(data, timepoint) {
  data %>%
    select(-study_id_mspss) %>%
    summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "mean") %>%
    mutate(Timepoint = timepoint)
}

# Compute means separately for pre and post
pai_pre_means <- compute_means(pai_pre, "PAI Pre-Module")
pai_post_means <- compute_means(pai_post, "PAI Post-Module")

# Combine both into one dataframe
pai_means <- bind_rows(pai_pre_means, pai_post_means)

# Bar plot for pre vs post means
ggplot(pai_means, aes(x = variable, y = mean, fill = Timepoint)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of PAI Item Means (Pre vs. Post)",
       x = "Item",
       y = "Mean Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("PAI Pre-Module" = "steelblue", "PAI Post-Module" = "darkorange"))


# VAR model PDSS
# subset data
pdss_long <- data %>% 
  select(
    study_id_mspss,
    week,
    total_pdss_obs,
    pdss_q1,
    pdss_q2,
    pdss_q3,
    pdss_q4,
    pdss_q5,
    pdss_q6,
    pdss_q7
  ) %>% 
  na.omit()

unique_ids <- unique(pdss_long$study_id_mspss)

alac_0406 <- pdss_long %>% 
  filter(study_id_mspss == "alac_0406")

alac_0406 <- alac_0406 %>% 
  select(-study_id_mspss, -week, -total_pdss_obs)

data_ts <- as.matrix(alac_0406)  # Convert to numeric matrix

var_model <- VAR(alac_0406, p = 1, type = "const")

# Extract coefficient matrix
coef_matrix <- do.call(cbind, lapply(var_model$varresult, coef))
coef_matrix <- coef_matrix[-1, ]

# Convert to adjacency matrix (set small coefficients to zero for clarity)
adj_matrix <- ifelse(abs(coef_matrix) > 0.1, coef_matrix, 0)


edge_colors <- ifelse(adj_matrix > 0, "blue", "red")

par(mar = c(5, 4, 4, 10))  # Right margin increased to 10

# Create the directed network plot
qgraph(adj_matrix, 
       layout = "spring", 
       directed = TRUE, 
       labels = colnames(alac_0406),  
       label.cex = 1.2,
       edge.color = edge_colors,  # Color edges based on sign
       theme = "gray",
       title = "VAR Directed Network for ID: alac_0316")


### PAI Networks ###
# define list of pre-vars for network model
pai_pre_vars <- c(
  "h.attack",
  "faint",
  "stroke",
  "suffocate",
  "die",
  "stare",
  "laugh",
  "embars",
  "scene",
  "weird",
  "insane",
  "hyster",
  "scream",
  "uncnt.sense",
  "uncnt.act"
)

pai_post_vars <- pai_pre_vars

# form GGM modle
ggm2 <- ggm(pai_pre, vars = pai_pre_vars)

ggm2 <- ggm2 %>% runmodel # run model

ggm2 %>% parameters # check parameters

# prune to significant edges
ggm2 <- ggm2 %>% 
  prune(adjust = "fdr", alpha = 0.01)

# check modification indices:
ggm2 %>% MIs

# stepup estimation:
ggm2 <- ggm2 %>% stepup(criterion = "bic", alpha = 0.05)

# inspect fit
ggm2 %>% fit

# obtain network
net2 <- getmatrix(ggm2, "omega")

# Plot:
layout(matrix(c(1, 2), nrow = 2), heights = c(7, 2))  # 2 rows: plot (7) and legend (2)

qgraph(net2,  
       layout = "spring",  
       theme = "colorblind", 
       labels = pai_pre_vars)  

par(mar = c(0, 0, 0, 0))
plot.new()

# form GGM modle
ggm3 <- ggm(pai_post, vars = pai_post_vars)

ggm3 <- ggm3 %>% runmodel # run model

ggm3 %>% parameters # check parameters

# prune to significant edges
ggm3 <- ggm3 %>% 
  prune(adjust = "fdr", alpha = 0.01)

# check modification indices:
ggm3 %>% MIs

# stepup estimation:
ggm3 <- ggm3 %>% stepup(criterion = "bic", alpha = 0.05)

# inspect fit
ggm3 %>% fit

# obtain network
net3 <- getmatrix(ggm3, "omega")

# Plot:
layout(matrix(c(1, 2), nrow = 2), heights = c(7, 2))  # 2 rows: plot (7) and legend (2)

qgraph(net3,  
       layout = "spring",  
       theme = "colorblind", 
       labels = pai_post_vars)  

par(mar = c(0, 0, 0, 0))
plot.new()

write.csv(demo_summary_formatted, "demographics.csv", row.names = F)
write.csv(clinical_summary_formatted, "bl_clinical.csv", row.names = F)