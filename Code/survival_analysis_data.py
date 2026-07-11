"""
=============================================================================
Survival Analysis Data Preparation Script

Description:
This script processes the complete monthly panel dataset to extract survival 
analysis parameters. It defines the 'event' as a country reaching a 50% partial 
vaccination rate. 

Outputs generated:
- time: Days elapsed from the first vaccine administered to the 50% milestone 
        (or the end of the observation period).
- status: 1 if the 50% milestone is reached, 0 if censored.
=============================================================================
"""

import pandas as pd
from dateutil.relativedelta import relativedelta
import warnings

# Suppress pandas FutureWarnings to keep the execution output clean
warnings.simplefilter(action='ignore', category=FutureWarning)

# ==========================================
# 1. Import Data and Clean Missing Codes
# ==========================================
all_ctry_full_df = pd.read_csv("amc_delivery_timing_full_data.csv")

# Filter out rows with empty or missing country codes
all_ctry_full_df = all_ctry_full_df.loc[
    (all_ctry_full_df['CODE'] != "") & 
    (all_ctry_full_df['CODE'].notna())
]

# ==========================================
# 2. Compute Survival Time and Status
# ==========================================
# Retaining the exact original chaining logic to guarantee 100% data replication
filtered_df = (
    all_ctry_full_df
    .assign(VACCINED_DATE=pd.to_datetime(all_ctry_full_df['VACCINED_DATE']))
    .groupby('CODE', group_keys=False)
    .apply(lambda df: df.assign(
        FIRST_VACCINE_DT=df.loc[df['PARTIAL_VACCINED_RATE'] > 0, 'VACCINED_DATE'].min()
    ))
    .assign(
        FIRST_VACCINE_MTH=lambda df: df['FIRST_VACCINE_DT'].dt.to_period('M').dt.to_timestamp()
    )
    .loc[lambda df: (df['CODE'] != "")]
    .sort_values(by=['CODE', 'VACCINED_DATE', 'PARTIAL_VACCINED_RATE'], ascending=[True, True, False])
    .groupby('CODE', group_keys=False)
    .apply(lambda df: df.assign(
        FIRST_50_VACCINE_DT=(
            df.loc[df['PARTIAL_VACCINED_RATE'] >= 50, 'VACCINED_DATE'].min()
            if any(df['PARTIAL_VACCINED_RATE'] >= 50)
            else pd.NaT
        ),
        time=(df['VACCINED_DATE'] - pd.to_datetime(df['FIRST_VACCINE_DT'])).dt.days,
        status=(df['PARTIAL_VACCINED_RATE'] >= 50).astype(int)
    ))
    .query('(VACCINED_DATE >= FIRST_VACCINE_DT) & (time >= 0)')
)

# ==========================================
# 3. Compile Final Survival Dataset
# ==========================================
# Separate countries into two groups: Reached 50% vs Censored
output_df = (
    pd.concat([
        # Group 1: Reached 50% (keep only the earliest date of vaccine rate > 50%)
        filtered_df.loc[(filtered_df['FIRST_50_VACCINE_DT'].notna()) & (filtered_df['status'] == 1)]
        .query('VACCINED_DATE == FIRST_50_VACCINE_DT'),
        
        # Group 2: Censored (Countries that never reached 50%, keep the last observed date)
        filtered_df.loc[(filtered_df['FIRST_50_VACCINE_DT'].isna())]
        .sort_values(by='VACCINED_DATE', ascending=True)
        .drop_duplicates(subset=['CODE'], keep='last')
    ], ignore_index=True)
    [['ENTITY', 'YEARMONTH', 'CODE', 'AMC_IND', 'PARTIAL_VACCINED_RATE', 'time', 'status']]
)

# ==========================================
# 4. Export Final Data
# ==========================================
output_df.to_csv('survival_analysis_data.csv', index=False)