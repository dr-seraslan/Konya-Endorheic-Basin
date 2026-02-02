Spatiotemporal Assessment of Thermal Anomalies Under Global Warming (2003–2024): A Multi-Scale Analysis of Konya Endorheic Basin, Türkiye 
This repository contains the statistical analysis scripts and datasets for the study focusing on Land Surface Temperature (LST) dynamics in the Konya Closed Basin (KCB), Türkiye's largest endorheic system. The research utilizes high-temporal-resolution satellite data to unmask localized thermal trajectories that are often obscured by basin-wide averages.

📝 
Project Overview
Semi-arid endorheic basins act as climatic hotspots. This project investigates the spatiotemporal evolution of LST over the last 22 years (2003–2024) using a hierarchical modeling framework to quantify the driving effects of topography and land cover.

📁 
Repository Structure
data/: Contains sub-basin-based time series datasets (CSV format) for hydrological units including Aksaray, Altınekin, Beyşehir, Cihanbeyli, Ereğli, Karaman, Konya, Melendiz, and Şereflikoçhisar.

scripts/: Contains the R Markdown workflow and scripts (Konya_LST_analyses.R) used for data processing and statistical modeling.

🛠 
Methodology & Tools
The analysis was performed using the following workflow:


Data Processing: Daily MODIS (MYD21) data processed via Google Earth Engine (GEE).


Trend Analysis: Monotonic trends were calculated using Mann-Kendall tests and Theil-Sen slope estimators.


Statistical Modeling: A Linear Mixed-Effects Model (LMM) was implemented in R to account for spatial autocorrelation and hierarchical data structures.


Key Variables: The model accounts for Elevation, Aspect, and Land Use/Land Cover (LULC) impacts.

💻 
Environment
All statistical analyses were conducted within the R statistical computing environment. Required packages include:


tidyverse (Data manipulation) 


trend & Kendall (Time series analysis) 


nlme (Mixed-effects modeling) 


emmeans (Post-hoc comparisons)
