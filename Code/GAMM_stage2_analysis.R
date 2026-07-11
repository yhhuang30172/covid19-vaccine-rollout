# =============================================================================
# Stage 2: GAMM Analysis for COVID-19 Vaccine Uptake (2023 Data)
# =============================================================================

# Load required packages (Maintained original list to prevent function masking)
library(tidyverse)
library(mgcv)
library(lmtest)
library(olsrr)

set.seed(12121)
options(scipen = 999)

# =============================================================================
# 1. Data Import and Formatting
# =============================================================================
household <- read.csv('Country_Agg_Final.csv', header = TRUE)
data <- read.csv("0301check_full_model_data.csv", header = TRUE)

# Format numeric and categorical variables
data$POPULATION_DENSITY <- as.numeric(data$POPULATION_DENSITY)
data$V1 <- as.factor(data$V1)
data$V4 <- as.factor(data$V4)
data$LIFE_EXPECTANCY_AT_BIRTH <- as.numeric(data$LIFE_EXPECTANCY_AT_BIRTH)

# Adjust extreme values for quasibinomial distribution (bound between 0.001 and 0.999)
data <- data %>% filter(!is.na(PARTIALLY_VACCINATED))
data$PARTIALLY_VACCINATED_ADJ <- ifelse(
  data$PARTIALLY_VACCINATED / 100 == 0, 0.001,
  ifelse(data$PARTIALLY_VACCINATED / 100 == 1, 0.999, data$PARTIALLY_VACCINATED / 100)
)

# Handle missing values: Fill "0" for categorical, keep NA for numeric
categorical_columns <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
data[, categorical_columns] <- lapply(data[, categorical_columns], function(x) ifelse(is.na(x), "0", x))

numeric_columns <- setdiff(names(data), categorical_columns)
data[, numeric_columns] <- lapply(data[, numeric_columns], function(x) ifelse(is.na(x), NA, x))

# =============================================================================
# 2. Data Merging and Cleaning
# =============================================================================
# Merge datasets using household as the base table (left join)
data <- household %>% 
  left_join(data, by="CODE") %>% 
  rename(ENTITY = NAME, WHO_REGION = WORLD.REGIONS.ACCORDING.TO.OWID)

data$ENTITY <- toupper(data$ENTITY)

# Filter missing values for the selected predictors
cleaned_data_covax <- data %>%
  mutate(WHO_REGION = as.factor(WHO_REGION)) %>%
  drop_na(PARTIALLY_VACCINATED_ADJ, AGRI_OWNERSHIP_MEAN, HHTYPE_MODE, HHSIZE_CAT_MODE, 
          EDUC_MODE, POPULATION, POPULATION_DENSITY, StringencyIndex, HEALTH_EXPENDITURE, 
          MATERNAL_MORTALITY_RATIO, PUBLIC_SPENDING_ON_EDUCATION_AS_A_SHARE_OF_GDP, 
          URBAN_POPULATION, WHO_REGION) %>%
  mutate(HHTYPE_MODE = as.factor(HHTYPE_MODE))

# =============================================================================
# 3. Final BAM Model (with Interaction Terms)
# =============================================================================
model_partial_distinct <- bam(
  PARTIALLY_VACCINATED_ADJ ~ 
    AGRI_OWNERSHIP_MEAN + 
    HHTYPE_MODE + 
    log(HHSIZE_CAT_MODE) + 
    log(EDUC_MODE) + 
    log(URBAN_POPULATION+1) +
    # Interaction terms
    log(sqrt(POPULATION_DENSITY+1)) * log(HEALTH_EXPENDITURE+1) +
    log(sqrt(POPULATION_DENSITY+1)) * log(StringencyIndex+1) +
    sqrt(PUBLIC_SPENDING_ON_EDUCATION_AS_A_SHARE_OF_GDP) * log(RURAL_MEAN+1) +
    # Random effects
    s(HHTYPE_MODE, WHO_REGION, bs='re'),
  data = cleaned_data_covax,
  family = quasibinomial(link = "logit"),
  method = "fREML",
  select = TRUE
)

# =============================================================================
# 4. Model Diagnostics and Outputs
# =============================================================================
options(width = 400) 
print("--- Model Summary ---")
summary(model_partial_distinct)

print("--- Heteroscedasticity Check ---")
bptest(model_partial_distinct)

print("--- Normality Check ---")
shapiro.test(residuals(model_partial_distinct, type = "deviance"))

print("--- Multicollinearity Check ---")
ols_vif_tol(model_partial_distinct)

# Export GAM check plots to PDF for appendix
pdf("appendix_2023.pdf", width=8, height=8)
par(mfrow = c(2, 2))
gam.check(model_partial_distinct)
dev.off()