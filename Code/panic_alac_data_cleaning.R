# RainFrog Data for Panic Model

# Load required libraries
library(tidyverse)
library(ggplot2)
library(here)

# bl_data
bl_data <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/REDCap Data/alacrity_bl_data_2025_02_06.csv')

data <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/REDCap Data/alacrity_longitudinal_data_2025_02_06.csv')

# module data (and IDs)
module_data_all_ids <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Pre-Processed/json_alac2025_02_04.csv')

user_id_matching_matrix <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Pre-Processed/user_id_matching_matrix_alac2025_02_04.csv')

# merge with user_id_matching_matrix and extract list of IDs who started a module
module_data_all_ids <- module_data_all_ids %>%
  filter(
    str_detect(source, "Module") | 
      variable %in% c("module_started_at", "module_completed_at", "module_duration")
  )


started_module_ids <- unique(module_data_all_ids$participant_id)
length(started_module_ids)

dim(module_data_all_ids)
table(duplicated(user_id_matching_matrix$presc_cont_oktaid))

user_id_matching_matrix <- user_id_matching_matrix %>%
  distinct(presc_cont_oktaid, .keep_all = TRUE) # remove duplicates

# merge data
module_data_all_ids <- merge(module_data_all_ids, user_id_matching_matrix, 
                     by.x = "participant_id", 
                     by.y = "presc_cont_oktaid", 
                     all.x = FALSE, all.y = FALSE)

module_data_all_ids <- module_data_all_ids %>% 
  select(
    study_id_mspss,
    final_package_name,
    source,
    variable,
    value,
    recordedAt
  )

started_module_ids <- unique(module_data_all_ids$study_id_mspss)

# panic package module data
module_1_data <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/Module Data/mind_body2025_02_05.csv')
module_2_data <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/Module Data/breathing_relaxation2025_02_05.csv')
module_3_data <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/Module Data/balanced_thinking2025_02_05.csv')
module_4_data <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/Module Data/dealing_directly_phys_a2025_02_05.csv')
module_5_data <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/Module Data/dealing_directly_phys_b2025_02_05.csv')
module_6_data <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/Module Data/dealing_directly_sit2025_02_05.csv')

# baseline module
baseline_module <- read.csv('/Users/natechoukas/Documents/Research/RainFrog Analyses/Data and Code/Data/Clean/Module Data/setting_your_baseline2025_02_05.csv')

### Prepare Longitudinal Data ###
# clean redcap data
phq_vars <- c("phq_q1", "phq_q2a", "phq_q2b", "phq_q3a", "phq_q3b", "phq_q4", "phq_q5a", "phq_q5b", "phq_q6", 
              "phq_q7", "phq_q8a", "phq_q8b", "phq_q11_irritable", "phq_q12_libido", "pseudo_phq9_total")

gad7_vars <- c("gad7_q1", "gad7_q2", "gad7_q3", "gad7_q4", "gad7_q5", "gad7_q6", "gad7_q7", "gad7_total")

pcl5_vars <- c("pcl5_q1", "pcl5_q2", "pcl5_q3", "pcl5_q4", "pcl5_q5", "pcl5_q6", "pcl5_q7", "pcl5_q8", 
               "pcl5_q9", "pcl5_q10", "pcl5_q11", "pcl5_q12", "pcl5_q13", "pcl5_q14", "pcl5_q15", 
               "pcl5_q16", "pcl5_q17", "pcl5_q18", "pcl5_q19", "pcl5_q20", "pcl5_total", "pcl5_8itemproxy_total")

sadd_vars <- c("sadd_q1", "sadd_q2", "sadd_q3", "sadd_q4", "sadd_q5", "sadd_q6", "sadd_q7", "sadd_q8", 
               "sadd_q9", "sadd_q10", "sadd_total")

pdss_vars <- c("pdss_q1", "pdss_q2", "pdss_q3", "pdss_q4", "pdss_q5", "pdss_q6", "pdss_q7", "pdss_total")

fq_vars <- c("fq_q1", "fq_q2", "fq_q3", "fq_q4", "fq_q5", "fq_total")

isi_vars <- c("isi_q1", "isi_q2", "isi_q3", "isi_q4", "isi_q5", "isi_q6", "isi_q7", "sleep_isi_total")

catmh_dep_vars <- c("catmh_mdd_diagnosis", "catmh_mdd_confidence", "catmh_dep_severity", "catmh_dep_category",
                    "catmh_dep_precision", "catmh_dep_prob", "catmh_dep_percentile")

catmh_anx_vars <- c("catmh_anx_severity", "catmh_anx_category", "catmh_anx_precision", "catmh_anx_prob", "catmh_anx_percentile")


# subset data
data <- data %>% 
  select(
    study_id_mspss,
    week,
    all_of(phq_vars),
    all_of(gad7_vars),
    all_of(pcl5_vars),
    all_of(sadd_vars),
    all_of(pdss_vars),
    all_of(fq_vars),
    all_of(isi_vars),
    all_of(catmh_dep_vars),
    all_of(catmh_anx_vars)
  )

# create total_pdss_obs variable
data <- data %>%
  group_by(study_id_mspss) %>%
  mutate(total_pdss_obs = sum(!is.na(pdss_total)))

data %>%
  group_by(study_id_mspss) %>%
  summarise(total_pdss_obs = first(total_pdss_obs)) %>%
  count(total_pdss_obs)

### Prepare Baseline Data ###
# final packages for all IDs who engaged with modules
final_package_summary <- module_data_all_ids %>%
  filter(!is.na(final_package_name)) %>%  # Exclude rows where final_package_name is NA
  group_by(study_id_mspss) %>%
  summarise(final_package_name = first(final_package_name), .groups = "drop")

dim(bl_data)

# merge final_package_name with bl_data
bl_data <- bl_data %>% 
  left_join(final_package_summary, by = "study_id_mspss")

# calculate total modules completed for IDs who completed at least 1 module
completed_modules <- module_data_all_ids %>%
  filter(variable == "module_completed_at",  # Only keep rows where the module was completed
         source != "Setting Your Baseline (Module)") %>%  # Exclude the baseline module
  group_by(study_id_mspss) %>%
  summarise(total_modules = n_distinct(source), .groups = "drop")  # Count unique sources (modules) per ID

# assign 0 to IDs who did not complete a module
total_modules <- module_data_all_ids %>%
  select(study_id_mspss) %>%  # Select unique IDs
  distinct() %>%
  left_join(completed_modules, by = "study_id_mspss") %>%  # Join with completed modules data
  mutate(total_modules = replace_na(total_modules, 0))  # Replace NA with 0 for IDs with no completed modules

dim(bl_data)

# merge total_modules with bl_data
bl_data <- bl_data %>% 
  left_join(total_modules, by = "study_id_mspss")

dim(bl_data)

# filter bl_data and data to only include start_module_ids
bl_data <- bl_data %>% 
  mutate(started_tx = ifelse(study_id_mspss %in% started_module_ids, 1, 0))

table(bl_data$started_tx)

data <- data %>% 
  mutate(started_tx = ifelse(study_id_mspss %in% started_module_ids, 1, 0))

table(data$started_tx)

# update baseline module to only panic pkg
baseline_module_panic <- baseline_module %>% 
  filter(final_package_name == "Panic or Agoraphobia")

panic_tx_ids <- unique(baseline_module_panic$study_id_mspss) # save these ids as a list

# create panic_pkg variable in bl_data and data
bl_data <- bl_data %>%
  mutate(
    panic_pkg = if_else(study_id_mspss %in% panic_tx_ids, 1, 0)
  )

data <- data %>%
  mutate(
    panic_pkg = if_else(study_id_mspss %in% panic_tx_ids, 1, 0)
  )

# baseline summary table
bl_data <- bl_data %>% 
  mutate(
    anx_target = ifelse(str_detect(bl_symptom_descrip_text, "panic|agoraphobia|worry|social anxiety"), 1, 0),
    panic_target = ifelse(str_detect(bl_symptom_descrip_text, "panic"), 1, 0),
    panic_agoraphobia_target = ifelse(str_detect(bl_symptom_descrip_text, "panic|agoraphobia"), 1, 0)
  )

# demographic variables
bl_data <- bl_data %>% 
  mutate(
    age = presc_cont_age,
    gender = presc_cont_gender.factor,
    race = presc_cont_race.factor,
    ethnicity = presc_cont_eth.factor
  )

# filter for anxiety disorders
bl_data_anx <- bl_data %>% 
  filter(anx_target == 1)

# filter for panic only
bl_data_panic <- bl_data %>% 
  filter(panic_target == 1)

# filter for panic or agoraphobia
bl_data_panic_agoraphobia <- bl_data %>% 
  filter(panic_agoraphobia_target == 1)

# pull unique IDs for each subset
anx_ids <- unique(bl_data_anx$study_id_mspss) # N=536
panic_ids <- unique(bl_data_panic$study_id_mspss) # N=162
panic_agoraphobia_ids <- unique(bl_data_panic_agoraphobia$study_id_mspss) # N=366

# subset data for different targets
data_anx <- data %>% filter(study_id_mspss %in% anx_ids)
data_panic <- data %>% filter(study_id_mspss %in% panic_ids)
data_panic_agoraphobia <- data %>% filter(study_id_mspss %in% panic_agoraphobia_ids)

# explore how many IDs started packages vs. not
length(unique(data$study_id_mspss)) # N=841
length(unique(data$study_id_mspss[data$started_tx==1])) # N=265

# panic or agoraphobia
length(unique(data_panic_agoraphobia$study_id_mspss)) # N=366
length(unique(data_panic_agoraphobia$study_id_mspss[data_panic_agoraphobia$started_tx==1])) # N=132

# panic only
length(unique(data_panic$study_id_mspss)) # N=162
length(unique(data_panic$study_id_mspss[data_panic$started_tx==1])) # N=51

# save clean datasets
setwd('/Users/natechoukas/Documents/Research/Panic Model x RainFrog/Data')

# all data
file_name <- "data.csv"
write.csv(data, file = file_name, row.names = FALSE)

# above threshold panic/agoraphobia (e.g., eligible for panic pkg)
file_name <- "panic_agora_data.csv"
write.csv(data_panic_agoraphobia, file = file_name, row.names = FALSE)

# above threshold panic only (e.g., repeated assessments pdss)
file_name <- "panic_data.csv"
write.csv(data_panic, file = file_name, row.names = FALSE)

# bl_summary all data
file_name <- "bl_data.csv"
write.csv(bl_data, file = file_name, row.names = FALSE)

# bl_summary panic/agoraphobia (e.g., eligible for panic pkg)
file_name <- "bl_data_panic_agoraphobia.csv"
write.csv(bl_data_panic_agoraphobia, file = file_name, row.names = FALSE)

# bl_summary all data
file_name <- "bl_data_panic.csv"
write.csv(bl_data_panic, file = file_name, row.names = FALSE)

# panic pkg dataframes
file_name <- "bl_panic_module.csv"
write.csv(baseline_module_panic, file = file_name, row.names = FALSE) # baseline module

file_name <- "panic_module_1.csv"
write.csv(module_1_data, file = file_name, row.names = FALSE) # module 1

file_name <- "panic_module_2.csv"
write.csv(module_2_data, file = file_name, row.names = FALSE) # module 2

file_name <- "panic_module_3.csv"
write.csv(module_3_data, file = file_name, row.names = FALSE) # module 3

file_name <- "panic_module_4.csv"
write.csv(module_4_data, file = file_name, row.names = FALSE) # module 4

file_name <- "panic_module_5.csv"
write.csv(module_5_data, file = file_name, row.names = FALSE) # module 5

file_name <- "panic_module_6.csv"
write.csv(module_6_data, file = file_name, row.names = FALSE) # module 6

# all module start and stop times (all participants)
file_name <- "module_summary.csv"
write.csv(module_data_all_ids, file = file_name, row.names = FALSE) 




