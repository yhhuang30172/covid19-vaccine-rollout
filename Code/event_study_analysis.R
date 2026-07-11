# =============================================================================
# Event Study Analysis: Effect of AMC Introduction on Vaccine Uptake
# =============================================================================

# Load Required Packages
library(tidyverse)
library(fixest)
library(zoo)
library(broom)

# Disable scientific notation for clear reading
options(scipen = 999)

# =============================================================================
# 1. Data Import and Preparation
# =============================================================================
# Load Main Delivery Timing Data
all_ctry_full_df <- read.csv("amc_delivery_timing_full_data.csv")

all_ctry_full_df$VACCINED_DATE <- as.Date(all_ctry_full_df$VACCINED_DATE)
all_ctry_full_df <- all_ctry_full_df %>%
  filter(!is.na(CODE), CODE != "") 

# Load and Format GHS Index Data
ghs <- read.csv("GHS/2021_GHS_Index_April_2022.csv")
colnames(ghs) <- toupper(colnames(ghs))

# Subset main question columns containing ".."
col_names <- colnames(ghs) 
main_questions <- c("COUNTRY", "YEAR", "OVERALL.SCORE", 
                    grep("^X\\d+\\.\\.", col_names, value = TRUE))

ghs <- ghs[, main_questions, drop = FALSE]
ghs$COUNTRY <- toupper(ghs$COUNTRY)

# =============================================================================
# 2. Descriptive Trend Plots
# =============================================================================

# Clean data: Keep only the 1st day of each month and remove invalid zero rates
all_ctry_full_df1 <- all_ctry_full_df %>%
  filter(format(VACCINED_DATE, "%d") == "01") %>% 
  arrange(ENTITY, VACCINED_DATE) %>%
  group_by(ENTITY) %>%
  mutate(
    first_positive_date = VACCINED_DATE[which(PARTIAL_VACCINED_RATE > 0)[1]],
    invalid_zero = VACCINED_DATE > first_positive_date & PARTIAL_VACCINED_RATE == 0
  ) %>%
  filter(!invalid_zero) %>%
  ungroup()

# Calculate monthly average uptake rates
trend_df_clean <- all_ctry_full_df1 %>% 
  group_by(VACCINED_DATE, AMC_IND) %>%
  summarise(PARTIAL_VACCINED_RATE = mean(PARTIAL_VACCINED_RATE, na.rm = TRUE), .groups = "drop")

# Count AMC observations to differentiate solid (n >= 15) and dashed (n < 15) lines
amc_obs_count <- all_ctry_full_df1 %>%
  filter(AMC_IND == 1) %>%
  group_by(VACCINED_DATE) %>%
  summarise(n = n(), .groups = "drop")

trend_df_amc <- trend_df_clean %>%
  filter(AMC_IND == 1) %>%
  left_join(amc_obs_count, by = "VACCINED_DATE")

amc_solid_df  <- trend_df_amc %>% filter(n >= 15)
amc_dashed_df <- trend_df_amc %>% filter(n < 15)
non_amc_df    <- trend_df_clean %>% filter(AMC_IND == 0)

# --- Plot 1: Overall Vaccine Uptake Over Time (AMC vs. Non-AMC) ---
ggplot() +
  geom_line(data = non_amc_df, aes(x = VACCINED_DATE, y = PARTIAL_VACCINED_RATE, color = "Non-AMC"), linewidth = 1.5) +
  geom_point(data = non_amc_df, aes(x = VACCINED_DATE, y = PARTIAL_VACCINED_RATE, color = "Non-AMC", shape = "Non-AMC"), size = 3, fill = "white", stroke = 1.2) +
  geom_line(data = amc_solid_df, aes(x = VACCINED_DATE, y = PARTIAL_VACCINED_RATE, color = "AMC"), linewidth = 1.5) +
  geom_point(data = amc_solid_df, aes(x = VACCINED_DATE, y = PARTIAL_VACCINED_RATE, color = "AMC", shape = "AMC"), size = 3, fill = "white", stroke = 1.2) +
  geom_line(data = amc_dashed_df, aes(x = VACCINED_DATE, y = PARTIAL_VACCINED_RATE, color = "AMC"), linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c("Non-AMC" = "red", "AMC" = "blue")) +
  scale_shape_manual(values = c("Non-AMC" = 21, "AMC" = 22)) +
  labs(
    title = "Vaccine Uptake Over Time: AMC vs. Non-AMC Countries",
    x = "Year", y = "Partial Vaccination Rate",
    color = "Group", shape = "Group"
  ) +
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black"),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 13),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.position = c(0.13, 0.85),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "grey70"),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12)
  ) +
  annotate("text", x = as.Date("2021-10-01"), y = 90, label = "Dashed = AMC (n < 15)", size = 4, family = "Times")

# --- Plot 2: Vaccine Uptake Over Time by WHO Region ---
trend_df_region <- all_ctry_full_df1 %>%
  filter(!is.na(WHO_REGION), WHO_REGION != "") %>%
  group_by(VACCINED_DATE, AMC_IND, WHO_REGION) %>%
  summarise(PARTIAL_VACCINED_RATE = mean(PARTIAL_VACCINED_RATE, na.rm = TRUE), .groups = "drop")

ggplot(trend_df_region, aes(x = VACCINED_DATE, y = PARTIAL_VACCINED_RATE, color = factor(AMC_IND), shape = factor(AMC_IND))) +
  geom_line(linewidth = 0.8, alpha = 1) +
  geom_point(size = 1.2, fill = "white", stroke = 0.8) + 
  labs(
    title = "Vaccine Uptake Over Time: AMC vs. Non-AMC Countries",
    x = "Year", y = "Partial Vaccination Rate",
    color = "AMC Indicator", shape = "AMC Indicator"
  ) +
  scale_color_manual(values = c("red", "blue"), labels = c("Non-AMC", "AMC")) +
  scale_shape_manual(values = c(21, 22), labels = c("Non-AMC", "AMC")) + 
  theme_minimal(base_size = 14, base_family = "Times") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title = element_text(size = 16, family = "Times"),
    axis.text = element_text(size = 10, family = "Times"),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "Times"),
    legend.title = element_text(size = 10, family = "Times"),
    legend.text = element_text(size = 9, family = "Times")
  ) +
  facet_wrap(~ WHO_REGION, scales = "free_y")  + 
  scale_y_continuous(limits = c(-5, 105)) 

ggsave("AMC_vaccine_uptake_rate_by_WHO_Region_plot.pdf", width = 8, height = 6, dpi = 300, units = "in")

# =============================================================================
# 3. Event Study Analysis 
# =============================================================================

# --- 3.1 Base Model: All AMC Countries ---
event_study_monthly_df <- all_ctry_full_df %>%
  filter(!is.na(CODE), CODE != "", !is.na(YEARMONTH)) %>%
  mutate(
    FIRST_AMC_DT  = as.Date(FIRST_AMC_DT),
    obs_mon   = as.yearmon(as.character(YEARMONTH), format = "%Y%m"),
    first_mon = as.yearmon(FIRST_AMC_DT)
  ) %>%
  mutate(EVENT_TIME = round((obs_mon - first_mon) * 12)) %>%
  arrange(ENTITY, VACCINED_DATE) %>%
  group_by(ENTITY, EVENT_TIME) %>%
  summarise(
    PARTIAL_VACCINED_RATE = last(PARTIAL_VACCINED_RATE),
    YEARMONTH = first(YEARMONTH),
    .groups = "drop"
  ) %>%
  filter(EVENT_TIME >= -5 & EVENT_TIME <= 12)

print("--- Base Event Study Model ---")
event_model <- feols(PARTIAL_VACCINED_RATE ~ i(EVENT_TIME, ref = -1) | ENTITY + YEARMONTH,
                     data = event_study_monthly_df, 
                     cluster = ~ENTITY)
print(summary(event_model))

# Extract coefficients and append reference group (-1)
coef_df <- broom::tidy(event_model) %>%
  filter(grepl("EVENT_TIME::", term)) %>%
  mutate(Event_Time = as.numeric(gsub("EVENT_TIME::", "", term))) %>%
  bind_rows(tibble(term = "EVENT_TIME::-1", estimate = 0, std.error = 0, Event_Time = -1))

ggplot(coef_df, aes(x = Event_Time, y = estimate)) +
  geom_point(size = 2, color = "blue") +
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), width = 0.2, color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +   
  geom_hline(yintercept = 0, color = "black") + 
  labs(
    title = "Effect of AMC Introduction on Vaccine Uptake",
    x = "Months Since AMC Introduction",
    y = "Effect on Vaccine Uptake"
  ) +
  theme_classic(base_size = 16, base_family = "Times") +
  theme(
    panel.grid.major.x = element_line(color = "gray80", linetype = "dotted"), 
    panel.grid.major.y = element_line(color = "gray80", linetype = "dotted"), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7), 
    axis.line = element_blank(), 
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

ggsave("event_study_all_countries_iplot.pdf", width = 8, height = 6, dpi = 300, units = "in")

# --- 3.2 Interaction Model: Does lag vary by WHO Region? ---
event_study_who_region_df <- all_ctry_full_df %>%
  filter(!is.na(CODE), CODE != "", !is.na(YEARMONTH)) %>%
  mutate( 
    FIRST_AMC_DT  = as.Date(FIRST_AMC_DT),
    obs_mon   = as.yearmon(VACCINED_DATE),
    first_mon = as.yearmon(FIRST_AMC_DT) 
  ) %>%
  mutate(EVENT_TIME = round((obs_mon - first_mon) * 12)) %>%
  arrange(ENTITY, VACCINED_DATE) %>%
  group_by(ENTITY, EVENT_TIME) %>%
  summarise( 
    PARTIAL_VACCINED_RATE = last(PARTIAL_VACCINED_RATE),
    YEARMONTH = first(YEARMONTH),
    WHO_REGION = first(WHO_REGION),
    AMC_IND = first(AMC_IND),
    CODE = first(CODE),
    .groups = "drop"
  ) %>%
  filter(EVENT_TIME >= -5 & EVENT_TIME <= 12) %>%
  filter(WHO_REGION != "", !is.na(WHO_REGION), AMC_IND == 1) %>%
  mutate(WHO_REGION = as.factor(WHO_REGION))

print("--- Interaction Model: WHO Region ---")
event_model1 <- feols(PARTIAL_VACCINED_RATE ~ i(EVENT_TIME, WHO_REGION, ref = -1) | ENTITY + YEARMONTH, 
                      data = event_study_who_region_df, 
                      cluster = ~ENTITY)
print(summary(event_model1))

# Extract interaction coefficients for plotting
coef_df_region <- as.data.frame(summary(event_model1)$coeftable) %>%
  mutate(Term = rownames(.)) %>%
  filter(grepl("EVENT_TIME", Term)) %>%
  mutate(
    Region = gsub(".*WHO_REGION", "", Term),
    Color = factor(Region, levels = unique(Region)),
    Event_Time = as.numeric(gsub("EVENT_TIME::(-?\\d+).*", "\\1", Term))
  )

ggplot(coef_df_region, aes(x = Event_Time, y = Estimate, color = Region, shape = Region, group = Region)) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 1.5) +  
  geom_point(fill = "white", size = 3, stroke = 1.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "brown")) +  
  scale_shape_manual(values = c(21, 22, 24, 25, 23, 8), labels = c("::AFRO", "::AMRO", "::EMRO", "::EURO", "::SEARO", "::WPRO")) +  
  labs(
    title = "Effect of AMC Introduction on Vaccine Uptake by Region",
    x = "Months Since AMC Introduction",
    y = "Effect on Vaccine Uptake"
  ) +
  theme_classic(base_size = 16, base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Times"),
    axis.title = element_text(size = 16, family = "Times"),
    axis.text = element_text(size = 16, family = "Times"),
    legend.position = c(0.12, 0.24),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "grey60", linewidth = 0.5),
    legend.title = element_text(size = 13, family = "Times"),
    legend.text = element_text(size = 12, family = "Times")
  )

ggsave("event_study_WHO_Region_plot.pdf", width = 8, height = 6, dpi = 300, units = "in")

# --- 3.3 Interaction Model: Does lag vary by GHS Index? ---
event_study_ghs_df <- event_study_who_region_df %>% # Reusing the base cleaned data for AMC countries
  left_join(
    ghs %>%
      filter(YEAR == 2021) %>%
      rename(ENTITY = COUNTRY, GHS_SCORE = OVERALL.SCORE) %>%  
      dplyr::select(ENTITY, GHS_SCORE) %>%
      mutate(
        GHS_GROUP = case_when(
          GHS_SCORE >= 0.0  & GHS_SCORE <= 20.0  ~ "0.0-20.0",
          GHS_SCORE >= 20.1 & GHS_SCORE <= 40.0  ~ "20.1-40.0",
          GHS_SCORE >= 40.1 & GHS_SCORE <= 60.0  ~ "40.1-60.0",
          GHS_SCORE >= 60.1 & GHS_SCORE <= 80.0  ~ "60.1-80.0",
          GHS_SCORE >= 80.1 & GHS_SCORE <= 100.0 ~ "80.1-100.0",
          TRUE ~ NA_character_
        )
      ),
    by = "ENTITY"
  )

print("--- Interaction Model: GHS Index Group ---")
event_model_ghs <- feols(PARTIAL_VACCINED_RATE ~ i(EVENT_TIME, GHS_GROUP, ref = -1) | ENTITY + YEARMONTH, 
                         data = event_study_ghs_df, 
                         cluster = ~ENTITY)
print(summary(event_model_ghs))

# Extract interaction coefficients for plotting
coef_df_ghs <- as.data.frame(summary(event_model_ghs)$coeftable) %>%
  mutate(Term = rownames(.)) %>%
  filter(grepl("EVENT_TIME", Term)) %>%
  mutate(
    GHS = gsub(".*GHS_GROUP", "", Term),
    Color = factor(GHS, levels = unique(GHS)),
    Event_Time = as.numeric(gsub("EVENT_TIME::(-?\\d+).*", "\\1", Term))
  )

ggplot(coef_df_ghs, aes(x = Event_Time, y = Estimate, color = GHS, shape = GHS, group = GHS)) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 1.5) +  
  geom_point(fill = "white", size = 3, stroke = 1.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "brown")) +  
  scale_shape_manual(values = c(21, 22, 24, 25, 23, 8)) +  
  labs(
    title = "Effect of AMC Introduction on Vaccine Uptake by GHS Group",
    x = "Months Since AMC Introduction",
    y = "Effect on Vaccine Uptake"
  ) +
  theme_classic(base_size = 16, base_family = "Times") +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),  
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Times"),
    axis.title = element_text(size = 16, family = "Times"),
    axis.text = element_text(size = 16, family = "Times"),
    legend.position = c(0.14, 0.2),
    legend.background = element_rect(fill = alpha("white", 0.8), color = "grey60", linewidth = 0.5),
    legend.title = element_text(size = 13, family = "Times"),
    legend.text = element_text(size = 12, family = "Times")
  )

ggsave("event_study_GHS_Group_plot.pdf", width = 8, height = 6, dpi = 300, units = "in")