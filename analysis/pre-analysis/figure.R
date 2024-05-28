setwd("C:/Users/mz16609/OpenSAFELY_IWP/ImpactWinterPressures/local_data")
install.packages("readr")
install.packages("ggplot2")
install.packages("dplyr")
library(dplyr)
library(readr)
library(ggplot2)
# Read the CSV file with the full path
data <- read_csv("C:/Users/mz16609/OpenSAFELY_IWP/ImpactWinterPressures/local_data/workforce_quar.csv")
str(data)
data$date_collect <- as.Date(data$date_collect, format = "%d%b%Y")
data$time_num <- as.Date(data$time_num, format = "%d%b%Y")
data$time_num <- as.character(data$time_num)
head(data)

##### FTE (GP)#####
gp_data <- data %>% filter(gp_patient <= 200)
set.seed(123)  # Set seed for reproducibility
unique_ids <- unique(gp_data$prac_code)
sampled_ids <- sample(unique_ids, size = length(unique_ids) * 0.05)
sampled_data_gp <- gp_data %>% filter(prac_code %in% sampled_ids)
sampled_data_gp <- sampled_data_gp %>% arrange(prac_code, time_num)
head(sampled_data_gp)
str(sampled_data_gp)
## Plot the sub sample
ggplot(sampled_data_gp, aes(x = time_num, y = gp_patient, group = prac_code, color = as.factor(prac_code))) +
  geom_line() +
  labs(title = "Trajectories of FTE (GP) Over Time",
       x = "Time",
       y = "FTE(GP) per 100,000 patients",
       color = "prac_code") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
legend.position = "none")
# Save the plot as a PNG file
ggsave("trajectories_ggplot_GP.png", width = 10, height = 6)

## Plot all data
ggplot(gp_data, aes(x = time_num, y = gp_patient, group = prac_code, color = as.factor(prac_code))) +
  geom_line() +
  labs(title = "Trajectories of FTE (GP) Over Time",
       x = "Time",
       y = "FTE(GP) per 100,000 patients",
       color = "prac_code") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
# Save the plot as a PNG file
ggsave("trajectories_ggplot_GP_all.png", width = 10, height = 6)



##### FTE (nurse)#####
nurse_data <- data %>% filter(nurse_patient <= 200)
head(nurse_data)
set.seed(123)  # Set seed for reproducibility
unique_ids <- unique(nurse_data$prac_code)
sampled_ids <- sample(unique_ids, size = length(unique_ids) * 0.05)
sampled_data_nurse <- nurse_data %>% filter(prac_code %in% sampled_ids)
sampled_data_nurse <- sampled_data_nurse %>% arrange(prac_code, time_num)
head(sampled_data_nurse)
## Plot the sub sample
ggplot(sampled_data_nurse, aes(x = time_num, y = nurse_patient, group = prac_code, color = as.factor(prac_code))) +
  geom_line() +
  labs(title = "Trajectories of FTE (nurse) Over Time",
       x = "Time",
       y = "FTE(nurse) per 100,000 patients",
       color = "prac_code") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
# Save the plot as a PNG file
ggsave("trajectories_ggplot_nurse.png", width = 10, height = 6)
## Plot all data
ggplot(nurse_data, aes(x = time_num, y = nurse_patient, group = prac_code, color = as.factor(prac_code))) +
  geom_line() +
  labs(title = "Trajectories of FTE (nurse) Over Time",
       x = "Time",
       y = "FTE(nurse) per 100,000 patients",
       color = "prac_code") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
# Save the plot as a PNG file
ggsave("trajectories_ggplot_nurse_all.png", width = 10, height = 6)

##### FTE (dpc)#####
dpc_data <- data %>% filter(dpc_patient <= 200)
head(dpc_data)
set.seed(123)  # Set seed for reproducibility
unique_ids <- unique(dpc_data$prac_code)
sampled_ids <- sample(unique_ids, size = length(unique_ids) * 0.05)
sampled_data_dpc <- dpc_data %>% filter(prac_code %in% sampled_ids)
sampled_data_dpc <- sampled_data_dpc %>% arrange(prac_code, time_num)
head(sampled_data_dpc)
## Plot the sub sample
ggplot(sampled_data_dpc, aes(x = time_num, y = dpc_patient, group = prac_code, color = as.factor(prac_code))) +
  geom_line() +
  labs(title = "Trajectories of FTE (dpc) Over Time",
       x = "Time",
       y = "FTE(dpc) per 100,000 patients",
       color = "prac_code") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
# Save the plot as a PNG file
ggsave("trajectories_ggplot_dpc.png", width = 10, height = 6)

## Plot all data
ggplot(dpc_data, aes(x = time_num, y = dpc_patient, group = prac_code, color = as.factor(prac_code))) +
  geom_line() +
  labs(title = "Trajectories of FTE (dpc) Over Time",
       x = "Time",
       y = "FTE(dpc) per 100,000 patients",
       color = "prac_code") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
# Save the plot as a PNG file
ggsave("trajectories_ggplot_dpc_all.png", width = 10, height = 6)
