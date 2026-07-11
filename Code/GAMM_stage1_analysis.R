# =============================================================================
# Stage 1: GAMM Analysis for COVID-19 Vaccine Uptake (2021 Data)
# =============================================================================

# Load required packages
library(lme4)
library(tidyverse)
library(lmerTest)
library(dplyr)
library(mgcv)
library(MASS)
library(lmtest)
library(olsrr)

set.seed(12121)
options(scipen = 999)

# =============================================================================
# 1. Data Import and Preprocessing
# =============================================================================
data <- read.csv("202107_full_data.csv", header = TRUE)

data$POPULATION_DENSITY <- as.numeric(data$POPULATION_DENSITY)
data$V1 <- as.factor(data$V1)
data$V4 <- as.factor(data$V4)

# Adjust extreme values for quasibinomial distribution (bound between 0.001 and 0.999)
data <- data %>% filter(!is.na(PARTIAL_VACC_RATE))
data$PARTIALLY_VACCINATED_ADJ <- ifelse(
  data$PARTIAL_VACC_RATE / 100 == 0, 0.001,
  ifelse(data$PARTIAL_VACC_RATE / 100 == 1, 0.999, data$PARTIAL_VACC_RATE / 100)
)

# Handle missing values: Fill "0" for categorical, keep NA for numeric
categorical_columns <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
data[, categorical_columns] <- lapply(data[, categorical_columns], function(x) ifelse(is.na(x), "0", x))

numeric_columns <- setdiff(names(data), categorical_columns)
data[, numeric_columns] <- lapply(data[, numeric_columns], function(x) ifelse(is.na(x), NA, x))

data$ENTITY <- toupper(data$ENTITY)

# =============================================================================
# 2. Diagnostic Plot Function
# =============================================================================
check_beta_model <- function(model) {
  par(mfrow = c(2, 3))
  residuals_beta <- residuals(model, type = "pearson")
  fitted_beta <- fitted(model)
  
  plot(fitted_beta, residuals_beta, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted", pch = 20)
  abline(h = 0, col = "red", lwd = 2)
  hist(residuals_beta, breaks = 30, main = "Histogram of Residuals", col = "lightblue")
  qqnorm(residuals_beta)
  qqline(residuals_beta, col = "red")
  
  cooks_d <- cooks.distance(model)
  plot(cooks_d, type = "h", main = "Cook's Distance", ylab = "Cook's Distance")
  abline(h = 4 / length(residuals_beta), col = "red", lty = 2)
  
  leverage_vals <- hatvalues(model)
  plot(leverage_vals, main = "Leverage Plot", ylab = "Leverage Values")
  abline(h = 2 * mean(leverage_vals), col = "red")
  
  bp_test <- bptest(model)
  legend("topright", legend = paste("BP p-value:", round(bp_test$p.value, 4)), bty = "n")
  par(mfrow = c(1,1))
}

# =============================================================================
# 3. Create Cleaned Dataset
# =============================================================================
# Retain original valid cases for modeling to guarantee identical degrees of freedom
cleaned_data <- data %>%
  mutate(WHO_REGION = as.factor(WHO_REGION)) %>%
  drop_na(PARTIALLY_VACCINATED_ADJ, AGRI_OWNERSHIP_MEAN, HHTYPE_MODE, HHSIZE_CAT_MODE, 
          EDUC_MODE, POPULATION, POPULATION_DENSITY, STRINGENCYINDEX_AVERAGE, HEALTH_EXPENDITURE, 
          MATERNAL_MORTALITY_RATIO, PUBLIC_SPENDING_ON_EDUCATION_AS_A_SHARE_OF_GDP, 
          URBAN_POPULATION, WHO_REGION)

# =============================================================================
# 4. PCA Dimension Reduction for COVAX Metrics
# =============================================================================
# Perform PCA on selected policy/delivery indicators
pca_data <- cleaned_data %>%
  dplyr::select(POST_AMC, AMC_IND, CUMULATED_COVAX_LAG3) %>%
  mutate(log_CUMULATED_COVAX_LAG3 = log1p(CUMULATED_COVAX_LAG3)) %>%
  na.omit()

pca_data_scaled <- scale(pca_data)
pca_result <- prcomp(pca_data_scaled, center = TRUE, scale. = TRUE)

# Map the first principal component (PC1) back to the modeling dataset
cleaned_data$PC1 <- pca_result$x[, 1]

print("--- PCA Summary (Variance Explained) ---")
summary(pca_result)

print("--- PCA Loadings (Variable Weights on PC1) ---")
pca_result$rotation

# =============================================================================
# 5. Final BAM Model
# =============================================================================
model_partial_distinct <- bam(
  PARTIALLY_VACCINATED_ADJ ~ 
    AGRI_OWNERSHIP_MEAN + 
    log(HHSIZE_CAT_MODE+1) + 
    log(EDUC_MODE+1) + 
    log(POPULATION+1) + 
    log(sqrt(POPULATION_DENSITY+1)) + 
    log(STRINGENCYINDEX_AVERAGE+1) + 
    MATERNAL_MORTALITY_RATIO +
    sqrt(PUBLIC_SPENDING_ON_EDUCATION_AS_A_SHARE_OF_GDP) + 
    log(URBAN_POPULATION+1) +
    V1 + 
    PC1 +  
    s(WHO_REGION, bs='re'),
  data = cleaned_data,
  family = quasibinomial(link = "logit"),
  method = "fREML",  
  select = TRUE
)

# =============================================================================
# 6. Model Diagnostics and Outputs
# =============================================================================
options(width = 400) 
print("--- Model Summary ---")
summary(model_partial_distinct)

print("--- Multicollinearity Check ---")
ols_vif_tol(model_partial_distinct)

print("--- Heteroscedasticity Check ---")
bptest(model_partial_distinct)

print("--- Normality Check ---")
shapiro.test(residuals(model_partial_distinct, type = "deviance"))

# Export GAM check plots to PDF for appendix
pdf("appendix_2021.pdf", width=8, height=8)
par(mfrow = c(2, 2))
gam.check(model_partial_distinct)
dev.off()