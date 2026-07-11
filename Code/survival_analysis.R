# =============================================================================
# Comprehensive Analysis: 
# Part 1: Survival Analysis (Time to 50% COVID-19 Vaccine Uptake)
# Part 2: Hypothesis Testing (AMC vs Non-AMC Disparities)
# =============================================================================

# =============================================================================
# Load Required Packages & Global Options
# =============================================================================

library(survival)
library(ggsurvfit)
library(gtsummary)
library(flexsurv)
library(tidyverse)
library(lubridate)
library(readxl)

# Disable scientific notation for clear p-value reading
options(scipen = 999) 

# =============================================================================
# -----------------------------------------------------------------------------
# PART 1: SURVIVAL ANALYSIS (TIME TO 50% UPTAKE)
# -----------------------------------------------------------------------------
# =============================================================================

# 1.1 Data Import and Preparation
# ---------------------------------------------------------
all_ctry_full_df <- read.csv("survival_analysis_data.csv")

# Create a clear factor variable for AMC status plotting
all_ctry_full_df$AMC_Factor <- factor(
  all_ctry_full_df$AMC_IND, 
  levels = c(0, 1), 
  labels = c("Non-AMC", "AMC")
)

# Define Survival Object
surv_obj <- Surv(all_ctry_full_df$time, all_ctry_full_df$status)

# 1.2 Between-Group Significance Tests (Log-Rank Test)
# ---------------------------------------------------------
print("--- Log-Rank Test (AMC vs Non-AMC) ---")
survdiff(surv_obj ~ AMC_IND, data = all_ctry_full_df)

# 1.3 Kaplan-Meier Curve & Risk Table
# ---------------------------------------------------------
km_plot <- survfit2(Surv(time, status) ~ AMC_Factor, data = all_ctry_full_df) %>%
  ggsurvfit(linetype_aes = TRUE) +   
  labs(
    x = "Day",
    y = "Probability of Not Reaching \n50% Vaccine Uptake"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("Non-AMC" = "red", "AMC" = "blue"),
    labels = c("Non-AMC" = "Non-AMC", "AMC" = "AMC Supported")
  ) +
  scale_linetype_manual(
    values = c("Non-AMC" = "solid", "AMC" = "dashed"),
    labels = c("Non-AMC" = "Non-AMC", "AMC" = "AMC Supported")
  ) +
  guides(
    color = guide_legend(override.aes = list(fill = NA)),
    linetype = guide_legend(override.aes = list(fill = NA)),
    fill = "none"  
  ) +
  theme_classic(base_family = "Times") +
  theme(
    plot.margin = unit(c(40, 10, 10, 10), "pt"),
    legend.position = "bottom",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18, margin = margin(r = 10)),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 15)
  ) +
  add_risktable(
    risktable_stats = "{n.risk} ({cum.event})",
    risktable_height = 0.25, # Ensures correct proportion in updated ggsurvfit
    theme = theme_risktable_default(
      axis.text.y.size = 12,
      plot.title.size = 14
    ), 
    size = 4.5
  ) 

# Explicitly print the plot to ensure rendering
print(km_plot)

# Export Plots
ggsave("survival_analysis_probability_not_to_reach_50_all_country_plot_risktable.pdf", plot = km_plot, width = 8, height = 6, dpi = 300, units = "in")
ggsave("survival_analysis_probability_not_to_reach_50_all_country_plot.svg", plot = km_plot, width = 8, height = 6, dpi = 300, units = "in", device = "svg")

# 1.4 Survival Probabilities at Specific Time Points (1 Year)
# ---------------------------------------------------------
print("--- 1-Year (365.25 Days) Survival Probability ---")
summary(survfit(surv_obj ~ AMC_IND, data = all_ctry_full_df), times = 365.25)

survfit(surv_obj ~ AMC_IND, data = all_ctry_full_df) %>% 
  tbl_survfit(
    times = 365.25,
    label_header = "**50% unreach (95% CI)**"
  )

# 1.5 Cox Proportional Hazards Model & Assumptions Check
# ---------------------------------------------------------
print("--- Univariate Cox Model (AMC_IND) ---")
cox_model <- coxph(surv_obj ~ AMC_IND, data = all_ctry_full_df)
summary(cox_model)

# Test Proportional Hazards (PH) assumption
ph_test <- cox.zph(cox_model)
print("--- Proportional Hazards Assumption Test ---")
print(ph_test)

# Custom Schoenfeld Residuals Plot
resid_data <- data.frame(
  time = ph_test$x,   
  residuals = ph_test$y[, "AMC_IND"]  
)

min_t <- 2
max_t <- max(all_ctry_full_df$time[all_ctry_full_df$status == 1])
labels_real <- c(0, 180, 360, 540, 720)
labels_scaled <- (labels_real - min_t) / (max_t - min_t)

ph_assumption_plot <- ggplot(resid_data, aes(x = time, y = residuals)) +
  geom_point(aes(color = "Residuals", shape = "Residuals", linetype = "Residuals", fill = "Residuals"), size = 3) +
  geom_smooth(aes(color = "Smoothed estimate with 95% CI", shape = "Smoothed estimate with 95% CI", linetype = "Smoothed estimate with 95% CI", fill = "Smoothed estimate with 95% CI"), 
              se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(
    name = "Time since first vaccine uptake (days)",
    breaks = labels_scaled,         
    labels = labels_real           
  ) +
  scale_y_continuous(
    name = "Scaled Schoenfeld residuals",
    breaks = -3:3
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Residuals" = "black", "Smoothed estimate with 95% CI" = "blue")
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("Residuals" = 1, "Smoothed estimate with 95% CI" = NA)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Residuals" = "blank", "Smoothed estimate with 95% CI" = "solid")
  ) +
  scale_fill_manual(
    name = NULL,
    values = c("Residuals" = NA, "Smoothed estimate with 95% CI" = "lightblue")
  ) +
  theme_minimal(base_size = 20, base_family = "Times") +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 18, color = "black"),
    axis.text.y = element_text(hjust = 1),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid = element_blank()
  )

print(ph_assumption_plot)

# 1.6 Royston-Parmar Flexible Parametric Survival Model
# ---------------------------------------------------------
print("--- Royston-Parmar Model (flexsurvspline) ---")
rp_model <- flexsurvspline(
  surv_obj ~ AMC_IND, 
  data = all_ctry_full_df, 
  k = 2,               
  scale = "hazard"     
)
print(rp_model)


# =============================================================================
# -----------------------------------------------------------------------------
# PART 2: HYPOTHESIS TESTING (AMC VS NON-AMC DISPARITIES)
# -----------------------------------------------------------------------------
# =============================================================================

# 2.1 Load and Prepare Base Data
# ---------------------------------------------------------
# Population Data
pop <- read.csv("population.csv") %>%
  rename(ENTITY = Entity, POPULATION = Population...Sex..all...Age..all...Variant..estimates, YYYY = Year) %>%
  mutate(ENTITY = toupper(ENTITY)) %>%
  filter(YYYY %in% c(2021, 2022, 2023))

# AMC Group Data
amc <- read_excel("WHO/WHO MS COVAX AMC/COVAX_AMC.xlsx") %>%
  rename(ENTITY = Entity, COVAX_GROUP = `COVAX GROUP`) %>%
  mutate(ENTITY = toupper(ENTITY)) %>%
  select(ENTITY, COVAX_GROUP)

# 2.2 Vaccine Accessibility Testing (Wilcoxon Rank Sum Test)
# ---------------------------------------------------------
vacc_dvry <- read.csv("Covid_Vaccine_Delievry_All_Country.csv") %>% 
  rename(ENTITY = Country.territory, TOTAL_DOSES_DELIVERED = Total.Doses.Delivered, MMYYYY = mmm.Year) %>%
  mutate(DATE = parse_date_time(MMYYYY, orders = "my"), YYYY = year(DATE), ENTITY = toupper(ENTITY))

# 2021 Vaccine Coverage
vacc_dvry_2021 <- vacc_dvry %>% filter(YYYY == 2021) %>%
  group_by(ENTITY, DATE) %>%
  summarise(TOTAL_DOSES_DELIVERED = sum(as.numeric(gsub(",", "", TOTAL_DOSES_DELIVERED)), na.rm = TRUE)) %>%  
  group_by(ENTITY) %>% slice_max(DATE, n = 1) %>% mutate(YYYY = year(DATE)) %>%
  inner_join(pop, by = c('ENTITY', 'YYYY')) %>%
  left_join(amc, by = 'ENTITY') %>%
  mutate(VACCINE_COVERAGE_PROPORTION = TOTAL_DOSES_DELIVERED / POPULATION,
         COVAX_GROUP = coalesce(COVAX_GROUP, 'NON-AMC'))

# 2023 Vaccine Coverage
vacc_dvry_latest <- vacc_dvry %>%
  group_by(ENTITY, DATE) %>%
  summarise(TOTAL_DOSES_DELIVERED = sum(as.numeric(gsub(",", "", TOTAL_DOSES_DELIVERED)), na.rm = TRUE)) %>%  
  group_by(ENTITY) %>% slice_max(DATE, n = 1) %>% mutate(YYYY = year(DATE)) %>%
  inner_join(pop, by = c('ENTITY', 'YYYY')) %>%
  left_join(amc, by = 'ENTITY') %>%
  mutate(VACCINE_COVERAGE_PROPORTION_2023 = TOTAL_DOSES_DELIVERED / POPULATION,
         COVAX_GROUP = coalesce(COVAX_GROUP, 'NON-AMC'))

print("--- Wilcoxon Test: Vaccine Coverage (2021) ---")
wilcox.test(VACCINE_COVERAGE_PROPORTION ~ COVAX_GROUP, data = vacc_dvry_2021)

print("--- Wilcoxon Test: Vaccine Coverage (End of Data Collection / 2023) ---")
wilcox.test(VACCINE_COVERAGE_PROPORTION_2023 ~ COVAX_GROUP, data = vacc_dvry_latest)

# 2.3 Cases and Deaths Testing (With VACC_RATIO Filtering)
# ---------------------------------------------------------
owid_death_url <- "https://catalog.ourworldindata.org/garden/covid/latest/cases_deaths/cases_deaths.csv"
owid_death <- read_csv(owid_death_url, show_col_types = FALSE) %>%
  select(country, date, total_cases_per_million, total_deaths_per_million) %>%
  rename_with(toupper) %>%
  rename(ENTITY = COUNTRY) %>%
  mutate(YYYY = year(DATE), ENTITY = toupper(ENTITY)) %>% 
  filter(YYYY %in% c(2021, 2022, 2023)) %>%
  group_by(ENTITY, YYYY) %>% slice_max(DATE, n = 1)

vacc <- read.csv("share-of-people-who-received-at-least-one-dose-of-covid-19-vaccine.csv") %>% 
  rename(ENTITY = Entity, VACC_RATIO = `People.vaccinated..cumulative..per.hundred.`, CODE = Code) %>%
  mutate(ENTITY = toupper(ENTITY), YYYY = year(Day)) %>%
  filter(YYYY %in% c(2021, 2022, 2023), CODE != "") %>%
  group_by(ENTITY, YYYY) %>% slice_max(Day, n = 1) %>% 
  filter(!is.na(CODE)) %>% select(-Day, -CODE)

owid_death <- owid_death %>% 
  left_join(vacc, by = c('ENTITY', 'YYYY')) %>%
  left_join(amc, by = c('ENTITY')) %>%
  mutate(COVAX_GROUP = coalesce(COVAX_GROUP, 'NON-AMC')) %>% 
  filter(!is.na(VACC_RATIO))

data_2021 <- owid_death %>% filter(YYYY == 2021)
data_2023 <- owid_death %>% group_by(ENTITY) %>% filter(YYYY == 2023 | (YYYY == 2022 & !any(YYYY == 2023))) %>% ungroup()

print("--- Wilcoxon Test: Total Cases Per Million (2021) ---")
wilcox.test(TOTAL_CASES_PER_MILLION ~ COVAX_GROUP, data = data_2021)

print("--- Wilcoxon Test: Total Deaths Per Million (2021) ---")
wilcox.test(TOTAL_DEATHS_PER_MILLION ~ COVAX_GROUP, data = data_2021)

print("--- Wilcoxon Test: Total Cases Per Million (End of Data Collection) ---")
wilcox.test(TOTAL_CASES_PER_MILLION ~ COVAX_GROUP, data = data_2023)

print("--- Wilcoxon Test: Total Deaths Per Million (End of Data Collection) ---")
wilcox.test(TOTAL_DEATHS_PER_MILLION ~ COVAX_GROUP, data = data_2023)