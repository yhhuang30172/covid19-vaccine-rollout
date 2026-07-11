"""
=============================================================================
AMC Delivery Timing & Lag Calculation Script

Description:
This script processes raw COVAX delivery data and vaccine uptake rates to 
generate a complete monthly panel dataset. It calculates cumulative vaccine 
deliveries, first AMC delivery dates, and constructs 1- to 3-month lag 
variables for the subsequent modeling analysis.

Output:
- amc_delivery_timing_full_data.csv (Used as input for covariate merging)
=============================================================================
"""

import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

# ==========================================
# 1. Import Raw Datasets
# ==========================================
delivery = pd.read_excel('COVID19_Vaccine_Delievery.xlsx', usecols=['Country/territory', 'mmm Year', 'COVAX', 'Total Doses Delivered'])
vaccine_rate = pd.read_csv('share-people-vaccinated-covid.csv', usecols=['Entity', 'Code', 'People vaccinated (cumulative, per hundred)', 'Day'])
amc = pd.read_csv("who_owd_amc.csv", usecols=['Entity' ,'COVAX GROUP'])
who_region = pd.read_csv('WHO/WHO MS COVAX AMC/WHO-vaccination-data.csv', usecols=['ISO3', 'WHO_REGION'])

# ==========================================
# 2. Date Formatting and Standardization
# ==========================================
# Reformat date to capture vaccine rate of the last day of each month
vaccine_rate['Day'] = pd.to_datetime(vaccine_rate['Day'])
vaccine_rate['YearMonth'] = vaccine_rate['Day'].dt.strftime('%Y%m')

delivery['mmm Year'] = pd.to_datetime(delivery['mmm Year'])
delivery['YearMonth'] = delivery['mmm Year'].dt.strftime('%Y%m')

# Keep only the latest record per month for each entity
last_day_per_month = vaccine_rate.loc[vaccine_rate.groupby(["Entity", "YearMonth"])["Day"].idxmax()]
delivery = delivery.loc[delivery.groupby(["Country/territory", "YearMonth"])["mmm Year"].idxmax()]

# Trim whitespace and convert to uppercase for standard merging
vaccine_rate["Entity"] = vaccine_rate["Entity"].str.strip().str.upper()
delivery["Country/territory"] = delivery["Country/territory"].str.strip().str.upper()
amc["Entity"] = amc["Entity"].str.strip().str.upper()

vaccine_rate.columns = vaccine_rate.columns.str.upper()
delivery.columns = delivery.columns.str.upper()
amc.columns = amc.columns.str.upper()

delivery['COVAX'] = pd.to_numeric(delivery["COVAX"], errors="coerce").fillna(0)

# ==========================================
# 3. Time-Series Expansion and Lag Calculation
# ==========================================
# Ensure all timelines start from Jan 2020
full_dates = (
    vaccine_rate.groupby('ENTITY')['DAY']
    .agg(['min', 'max'])
    .assign(min=pd.to_datetime('2020-01-01')) 
)

# Create a complete monthly panel for each entity
vaccine_rate_full = (
    full_dates.apply(lambda x: pd.date_range(start=x['min'], end=x['max'], freq='MS'), axis=1)
    .explode()
    .reset_index()
    .rename(columns={0: 'BASE_VACC_DATE'})
    .assign(
        YEARMONTH=lambda x: pd.to_datetime(x["BASE_VACC_DATE"]).dt.strftime('%Y%m')
    )
    .merge(vaccine_rate, how='left', on=['ENTITY', 'YEARMONTH'])
    .assign(
        DAY=lambda x: x['DAY'].fillna(x['BASE_VACC_DATE']),
        # Forward fill partial vaccination rates within each entity
        PEOPLE_VACCINATED_CUMULATIVE=lambda x: x.groupby('ENTITY')['PEOPLE VACCINATED (CUMULATIVE, PER HUNDRED)']
        .transform(lambda g: g.ffill().fillna(0))
    )
    .rename(columns={
        'PEOPLE VACCINATED (CUMULATIVE, PER HUNDRED)': 'PARTIAL_VACCINED_RATE', 
        'DAY': 'VACCINED_DATE', 
        'TOTAL DOSES DELIVERED': 'TOTAL_DOES_DELIVERED'
    })
    .merge(delivery, how='left', left_on=['ENTITY', 'YEARMONTH'], right_on=['COUNTRY/TERRITORY', 'YEARMONTH'])
    .merge(amc, how='left', on='ENTITY')
    .assign(
        AMC_IND=lambda x: x['COVAX GROUP'].apply(lambda y: 1 if y == 'AMC' else 0),
        DELIVER_DT=lambda x: x['MMM YEAR'],
        COVAX=lambda x: x['COVAX'].fillna(0),
        TOTAL_DOES_DELIVERED=lambda x: x['TOTAL_DOES_DELIVERED'].fillna(0),
        CODE=lambda x: x.groupby('ENTITY')['CODE'].transform(lambda g: g.ffill().bfill())
    )
    .assign(
        DELIVER_DT=lambda x: x['DELIVER_DT'].fillna(
            x['YEARMONTH'].astype(str).apply(lambda y: pd.to_datetime(y + '01'))
        ), 
        PARTIAL_VACCINED_RATE=lambda x: x['PARTIAL_VACCINED_RATE'].fillna(0),
    )
    .sort_values(by=['ENTITY', 'YEARMONTH']) 
    .groupby('ENTITY')
    .apply(lambda group: group.assign(
        FIRST_AMC_DT=group.loc[group['COVAX'] > 0, 'DELIVER_DT'].min(),
        CUMULATED_COVAX=group['COVAX'].cumsum(),
        CUMULATED_DELIVERED=group['TOTAL_DOES_DELIVERED'].cumsum(),
    ))
    .reset_index(drop=True)
    .sort_values(by=['ENTITY', 'YEARMONTH', 'DELIVER_DT']) 
    .assign(
        POST_AMC=lambda df: (df["FIRST_AMC_DT"] < df['VACCINED_DATE']).astype(int),
        # Create 1- to 3-month lags for cumulative vaccine deliveries
        CUMULATED_COVAX_LAG1=lambda df: df.groupby('ENTITY')['CUMULATED_COVAX'].shift(1).ffill(),
        CUMULATED_COVAX_LAG2=lambda df: df.groupby('ENTITY')['CUMULATED_COVAX'].shift(2).ffill(),
        CUMULATED_COVAX_LAG3=lambda df: df.groupby('ENTITY')['CUMULATED_COVAX'].shift(3).ffill(),
        CUMULATED_DELIVERED_LAG1=lambda df: df.groupby('ENTITY')['CUMULATED_DELIVERED'].shift(1).ffill(),
        CUMULATED_DELIVERED_LAG2=lambda df: df.groupby('ENTITY')['CUMULATED_DELIVERED'].shift(2).ffill(),
        CUMULATED_DELIVERED_LAG3=lambda df: df.groupby('ENTITY')['CUMULATED_DELIVERED'].shift(3).ffill(),
    )
    .merge(who_region, how='left', left_on=['CODE'], right_on=['ISO3'])
    .drop(columns=[
        'COUNTRY/TERRITORY', 'MMM YEAR', 'COVAX GROUP', 'BASE_VACC_DATE', 
        'PEOPLE_VACCINATED_CUMULATIVE', 'ISO3'
    ])
)

# Correct specific country AMC status
vaccine_rate_full.loc[vaccine_rate_full['ENTITY'] == 'DOMINICAN REPUBLIC', 'AMC_IND'] = 1

# ==========================================
# 4. Export Final Data
# ==========================================
vaccine_rate_full.to_csv('amc_delivery_timing_full_data.csv', index=False)