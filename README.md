# COVID-19 Vaccine Uptake and COVAX AMC Evaluation

This repository contains the replication code and aggregated country-level datasets for the analysis of COVID-19 vaccine uptake, specifically focusing on the disparities between COVAX AMC and non-AMC countries.

## Repository Structure
* `/Code`: Contains all Python preprocessing scripts and R analysis scripts (GAMM, Survival Analysis, and Event Study).
* `/Data`: Contains the aggregated, de-identified country-level datasets necessary to replicate the findings. 

## Data Availability Statement & Sources
To comply with the data redistribution policies of the original providers, micro-level datasets and raw third-party data are not hosted in this repository. 

Researchers can fully reconstruct the analysis by downloading the following raw datasets from their original sources and running the preprocessing scripts provided in the `/Code` folder:

* **GLOPOP-S (Global Population Subnational Data):** Micro-level data (including underlying LIS and DHS survey data) cannot be redistributed. Please refer to the original publication for data access: Ton, M.J., Ingels, M.W., de Bruijn, J.A. et al. (2024). A global dataset of 7 billion individuals with socio-economic characteristics. Scientific Data 11, 1096. https://doi.org/10.1038/s41597-024-03864-2. The dataset is publicly available at Harvard Dataverse: https://doi.org/10.7910/DVN/KJC3RH. Accessed on `Dec 15, 2024`.
* **Our World in Data (OWID):** Vaccine rollout, cases, and deaths data. Accessed on `Feb 26, 2025`. Available at: https://ourworldindata.org/covid-vaccinations
* **World Health Organization (WHO) & COVAX:** AMC grouping classification and COVID-19 vaccine delivery data. 
  * Vaccine delivery data: WHO COVID-19 Dashboard (https://data.who.int/dashboards/covid19/vaccines)
  * AMC Grouping: Gavi COVAX Facility documentation (https://www.gavi.org/covax-facility)
* **World Bank:** Health expenditure, maternal mortality, education spending, and urban population data. Accessed on `Dec 31, 2024`. Available at: https://data.worldbank.org/
  * *Maternal Mortality Ratio:* Indicator `SH.STA.MMRT` (https://data.worldbank.org/indicator/SH.STA.MMRT)
  * *Urban Population (% of total):* Indicator `SP.URB.TOTL.IN.ZS` (https://data.worldbank.org/indicator/SP.URB.TOTL.IN.ZS)
  * *Government Expenditure on Education (% of GDP):* Indicator `SE.XPD.TOTL.GD.ZS` (https://data.worldbank.org/indicator/SE.XPD.TOTL.GD.ZS)
* **OxCGRT (Oxford COVID-19 Government Response Tracker):** Stringency index data. Accessed on `Dec 31, 2024`. Available at: https://github.com/OxCGRT/covid-policy-tracker
* **Global Health Security (GHS) Index:** 2021 GHS Index data. Accessed on `Mar 10, 2025`. Available at: https://www.ghsindex.org/
