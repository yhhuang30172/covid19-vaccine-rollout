"""
=============================================================================
Data Preprocessing Script for Country-Level Covariates

Usage Guide:
This script generates aggregated datasets for Stage 1. 
It can also be easily adapted to generate the dataset for Stage 2 by disabling 
specific time-filtering conditions.

How to generate the Stage 2 dataset:
1. Search the script for all `.query()` methods that filter by specific time periods 
   (e.g., `.query('YYYYMM == 202107 ...')`, `.query('YYYY == 2021')`, or `.query('YYYY == 2021 | YYYY == 2020')`).
2. Comment out all these time-filtering lines.
3. In the final export section (bottom of the script), switch the output filename to:
   `merge_df.to_csv('0301check_full_model_data.csv', index=False)`

For Stage 1, keep all temporal queries active and export as '202107_full_data.csv'.
=============================================================================
"""

import pandas as pd
import numpy as np

# ==========================================
# 1. Import and Process Individual Datasets
# ==========================================

# Process partial vaccination rate (keep the latest record per country)
share_ppl_part_vacinced = pd.read_csv("share-of-people-who-received-at-least-one-dose-of-covid-19-vaccine.csv")
share_ppl_part_vacinced = share_ppl_part_vacinced.rename(columns={'People vaccinated (cumulative, per hundred)': 'Partial_Vacc_Rate'})
share_ppl_part_vacinced.columns = share_ppl_part_vacinced.columns.str.upper()

share_ppl_part_vacinced = (
    share_ppl_part_vacinced
    .rename(columns={'COUNTRYNAME': 'ENTITY', 'COUNTRYCODE': 'CODE'})
    .assign(
        DAY=lambda df: pd.to_datetime(df['DAY']),
        YYYYMM=lambda df: df['DAY'].dt.strftime('%Y%m').astype(int),
        YYYY=lambda df: df['DAY'].dt.strftime('%Y'),
        ENTITY=lambda df: df['ENTITY'].str.upper()
    )
    .sort_values(by=['ENTITY', 'DAY'], ascending=True)
    .sort_values(by=['ENTITY', 'YYYY'], ascending=True)
    .drop_duplicates(subset=['ENTITY'], keep='last')
    .drop_duplicates(subset=['ENTITY', 'YYYYMM'], keep='last')
    .query('YYYYMM == 202107')
    [['ENTITY', 'CODE', 'YYYYMM', 'YYYY', 'PARTIAL_VACC_RATE']]
)

# Process Stringency Index (extract national total metrics)
stringency_index = pd.read_csv("OxCGRT/OxCGRT_simplified_v1.csv", low_memory=False)
stringency_index.columns = stringency_index.columns.str.upper()

stringency_index = (
    stringency_index
    .rename(columns={'COUNTRYNAME': 'ENTITY', 'COUNTRYCODE': 'CODE', 'V1..SUMMARY.': 'V1', 'V4..SUMMARY.': 'V4'})
    .assign(
        DATE=lambda df: pd.to_datetime(df['DATE'], format='%Y%m%d'),
        YYYYMM=lambda df: pd.to_datetime(df['DATE'], format='%Y%m%d').dt.strftime('%Y%m').astype(int),
        YYYY=lambda df: pd.to_datetime(df['DATE'], format='%Y%m%d').dt.strftime('%Y'),
        ENTITY=lambda df: df['ENTITY'].str.upper()
    )
    .sort_values(by=['ENTITY', 'DATE'], ascending=True)
    .query('YYYYMM == 202107 & JURISDICTION == "NAT_TOTAL"')
    .sort_values(by=['ENTITY', 'YYYY'], ascending=True)
    .drop_duplicates(subset=['ENTITY'], keep='last')
    [['ENTITY', 'CODE', 'YYYYMM', 'YYYY', 'STRINGENCYINDEX_AVERAGE', 'V1', 'V4']]
)

# Process Health Expenditure (keep the latest available year)
health_expenditure = pd.read_csv("OWID/annual-healthcare-expenditure-per-capita.csv")
health_expenditure.columns = health_expenditure.columns.str.upper()

health_expenditure = (
    health_expenditure
    .rename(columns={'CURRENT HEALTH EXPENDITURE PER CAPITA, PPP (CURRENT INTERNATIONAL $)': 'HEALTH_EXPENDITURE',
                     'YEAR': 'YYYY'})
    .assign(ENTITY=lambda df: df['ENTITY'].str.upper())
    .query('YYYY == 2021')
    .sort_values(by=['ENTITY', 'YYYY'], ascending=True)
    .drop_duplicates(subset=['ENTITY'], keep='last')
)

# Process Maternal Mortality Ratio (2020/2021 estimates)
maternal_mortality = pd.read_csv("WORLD_BANK/maternal-mortality.csv")
maternal_mortality.columns = maternal_mortality.columns.str.upper()

maternal_mortality = (
    maternal_mortality
    .rename(columns={'MATERNAL MORTALITY RATIO': 'MATERNAL_MORTALITY_RATIO', 'YEAR': 'YYYY'})
    .assign(ENTITY=lambda df: df['ENTITY'].str.upper())
    .query('YYYY == 2021 | YYYY == 2020')
    [['ENTITY', 'CODE', 'YYYY', 'MATERNAL_MORTALITY_RATIO']]
)

# Process Education Spending
education_spending = pd.read_csv("WORLD_BANK/total-government-expenditure-on-education-gdp.csv")
education_spending.columns = education_spending.columns.str.upper()

education_spending = (
    education_spending
    .rename(columns={'PUBLIC SPENDING ON EDUCATION AS A SHARE OF GDP': 'PUBLIC_SPENDING_ON_EDUCATION_AS_A_SHARE_OF_GDP',
                     'YEAR': 'YYYY'})
    .assign(ENTITY=lambda df: df['ENTITY'].str.upper())
    .query('YYYY == 2021')
    .sort_values(by=['ENTITY', 'YYYY'], ascending=True)
    .drop_duplicates(subset=['ENTITY'], keep='last')
)

# Process Urban Population Share
urban_population = pd.read_csv("WORLD_BANK/share-of-population-urban.csv")
urban_population.columns = urban_population.columns.str.upper()

urban_population = (
    urban_population
    .rename(columns={'URBAN POPULATION (% OF TOTAL POPULATION)': 'URBAN_POPULATION', 'YEAR': 'YYYY'})
    .assign(ENTITY=lambda df: df['ENTITY'].str.upper())
    .query('YYYY == 2021')
    .sort_values(by=['ENTITY', 'YYYY'], ascending=True)
    .drop_duplicates(subset=['ENTITY'], keep='last')
)

# Process Total Population
population = pd.read_csv('population.csv')
population.columns = population.columns.str.upper()

population = (
    population
    .rename(columns={'POPULATION - SEX: ALL - AGE: ALL - VARIANT: ESTIMATES': 'POPULATION', 'YEAR': 'YYYY'})
    .assign(ENTITY=lambda df: df['ENTITY'].str.upper())
    .query('YYYY == 2021')
    .sort_values(by=['ENTITY', 'YYYY'], ascending=True)
    .drop_duplicates(subset=['ENTITY'], keep='last')
    [['ENTITY', 'YYYY', 'POPULATION']]
)

# Process Population Density (baseline 2021)
population_density = pd.read_csv('OWID/population-density.csv')
population_density.columns = population_density.columns.str.upper()

population_density = (
    population_density
    .rename(columns={'POPULATION DENSITY': 'POPULATION_DENSITY', 'YEAR': 'YYYY'})
    .assign(ENTITY=lambda df: df['ENTITY'].str.upper())
    .query('YYYY == 2021')
    [['ENTITY', 'CODE', 'YYYY', 'POPULATION_DENSITY']]
)

# Load Household Size and WHO Region Indicators
household = pd.read_csv('Country_Agg_Final.csv')

who_region = pd.read_csv('WHO/WHO MS COVAX AMC/WHO-vaccination-data.csv', usecols=['ISO3', 'WHO_REGION'])
who_region.rename(columns={'ISO3': 'CODE'}, inplace=True)

# Process Vaccine Delivery Timing
amc_delivery = pd.read_csv('amc_delivery_timing_full_data.csv')
amc_delivery = (
    amc_delivery
    .rename(columns={'YEARMONTH': 'YYYYMM'})
    .query('YYYYMM==202107 & CODE.notnull()')
    .sort_values(by=['ENTITY', 'VACCINED_DATE'], ascending=True)
    .drop_duplicates(subset='ENTITY', keep='last')
    [['ENTITY', 'YYYYMM', 'CODE', 'AMC_IND', 'CUMULATED_COVAX', 'CUMULATED_DELIVERED',
      'POST_AMC', 'CUMULATED_COVAX_LAG1', 'CUMULATED_COVAX_LAG2', 'CUMULATED_COVAX_LAG3',
     'CUMULATED_DELIVERED_LAG1', 'CUMULATED_DELIVERED_LAG2', 'CUMULATED_DELIVERED_LAG3']]
)

# ==========================================
# 2. Merge Datasets
# ==========================================

# Standardize Year formats to string for smooth merging
for df in [health_expenditure, population_density, population, urban_population, education_spending]:
    df['YYYY'] = df['YYYY'].astype(str)

# Aggregate all covariates at the country level
merge_df = (
    share_ppl_part_vacinced.merge(stringency_index, how='left', on=['ENTITY', 'CODE', 'YYYY', 'YYYYMM'])
    .merge(health_expenditure.drop(columns=['YYYY']), how='left', on=['ENTITY', 'CODE'])
    .merge(maternal_mortality.drop(columns=['YYYY']), how='left', on=['ENTITY', 'CODE']) 
    .merge(population.drop(columns=['YYYY']), how='left', on=['ENTITY'])
    .merge(population_density.drop(columns=['YYYY']), how='left', on=['ENTITY', 'CODE'])
    .merge(urban_population.drop(columns=['YYYY']), how='left', on=['ENTITY', 'CODE'])
    .merge(education_spending.drop(columns=['YYYY']), how='left', on=['ENTITY', 'CODE'])
    .merge(household, how='left', on=['CODE'])
    .merge(who_region, how='left', on=['CODE'])
    .merge(amc_delivery, how='left', on=['ENTITY', 'CODE', 'YYYYMM', 'CODE'])
    .query("WHO_REGION.notnull()") # Exclude territories not belonging to any WHO region
    .fillna(0)
)

# ==========================================
# 3. Export the final aggregated dataset
# ==========================================
# Note: Uncomment the line corresponding to the stage you are processing.

# --- For Stage 1 ---
merge_df.to_csv('202107_full_data.csv', index=False)

# --- For Stage 2 ---
# merge_df.to_csv('0301check_full_model_data.csv', index=False)