# Spatiotemporal Assessment of Thermal Anomalies Under Global Warming (2003-2024): A Multi-Scale Analysis of the Semi-Arid Konya Endorheic Basin, Türkiye

This repository contains the datasets, Google Earth Engine scripts, and R analysis codes used for the study:

**Scale-Dependent Spatial Masking of Land Surface Temperature Trends in the Semi-Arid Konya Endorheic Basin, Türkiye**

The study investigates the spatiotemporal dynamics of daytime Land Surface Temperature (LST) in the Konya Endorheic Basin (KEB), Türkiye, over the period 2003–2024. It uses daily Aqua MODIS LST observations, land-cover data, topographic variables, and hierarchical statistical modeling to examine how basin-wide thermal signals may mask divergent sub-basin-scale thermal trajectories.

---

## Study Overview

Semi-arid endorheic basins are highly sensitive to hydro-climatic stress, land degradation, and surface energy balance changes. However, analyses based only on basin-wide averages may obscure localized warming or cooling patterns.

This study addresses this problem by combining:

- daily Aqua MODIS daytime LST observations,
- land-cover information from MODIS MCD12Q1,
- topographic variables derived from NASA SRTM,
- sub-basin-scale spatial stratification,
- Mann-Kendall and Theil-Sen trend analyses,
- False Discovery Rate correction,
- Linear Mixed-Effects Modeling.

The main objective is to quantify scale-dependent thermal patterns and identify the bio-physical drivers of LST variability in the Konya Endorheic Basin.

---

## Study Area

The Konya Endorheic Basin is located in Central Anatolia, Türkiye. It is the largest closed basin in the country and represents a semi-arid hydro-climatological system characterized by limited precipitation, high evaporation, intensive agricultural water use, and land degradation risk.

The basin was divided into nine sub-basin units:

1. Beyşehir-Kaşaklı  
2. Konya-Çumra-Karapınar  
3. Karaman-Ayrancı-Akçaşehir  
4. Ereğli-Bor  
5. Melendiz  
6. Aksaray  
7. Şereflikoçhisar  
8. Cihanbeyli-Yeniceoba-Kulu  
9. Altınekin  

---

## Data Sources

| Dataset | Product | Spatial Resolution | Temporal Coverage | Purpose |
|---|---|---:|---:|---|
| Land Surface Temperature | Aqua MODIS MYD21A1D V061 | 1 km | 2003–2024 | Daily daytime LST |
| Land Cover | MODIS MCD12Q1 V061 | 500 m | Annual | Land-cover classification |
| Topography | NASA SRTM V003 | 30 m | Static | Elevation and aspect |
| Basin/Sub-basin boundaries | Hydrological boundary data | Vector | Static | Spatial stratification |

---

## Methodological Summary

### 1. Google Earth Engine Processing

The remote sensing preprocessing was performed in Google Earth Engine.

Main steps:

- quality-control masking of MYD21A1D LST pixels,
- Kelvin to Celsius conversion,
- spatial harmonization to the native 1 km MODIS LST grid,
- aggregation of all valid daily Aqua daytime LST observations within each calendar year,
- generation of annual median daytime LST composites for 2003–2024,
- extraction of zonal statistics for sub-basin, elevation, aspect, and land-cover combinations.

No seasonal filtering was applied. The analysis represents the annual daytime thermal regime based on valid Aqua MODIS daytime observations.

### 2. Spatial Harmonization

All datasets were harmonized to the native 1 km MODIS LST grid to avoid pseudo-replication and artificial inflation of spatial detail.

- SRTM-derived elevation and aspect variables were aggregated to the 1 km analytical grid.
- MODIS land-cover classes were resampled using the dominant class approach.
- Water bodies were excluded from the terrestrial LST analysis.

### 3. Statistical Analysis in R

The statistical analysis was conducted in R.

Main analyses included:

- Exploratory Data Analysis,
- Mann-Kendall trend test,
- Theil-Sen slope estimator,
- False Discovery Rate correction,
- Linear Mixed-Effects Model,
- Estimated Marginal Means and pairwise comparisons.

The Linear Mixed-Effects Model was used to estimate the independent effects of elevation, aspect, land cover, and year while accounting for sub-basin-level spatial dependency.

---

## Repository Structure

```text
Konya-Endorheic-Basin/
│
├── data/
│   ├── processed datasets used in the statistical analysis
│   └── sub-basin and bio-physical stratification tables
│
├── scripts/
│   ├── GEE_LST_Extraction_Workflow.js
│   └── Konya_LST_analyses.R
│
└── README.md
