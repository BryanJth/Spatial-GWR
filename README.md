# GWR Poverty Rate – East Java (2022)

**Project:** Analysis of the Percentage of Poor Population by District/City in East Java Province (2022) using **Geographically Weighted Regression (GWR)**  
**Date:** Apr 2025  
**Tech Stack:** R — `GWmodel`, `sf`, `sp`, `ggplot2`

## Overview
This project analyzes spatial variation in the **percentage of poor population** across **38 districts/cities** in **East Java (2022)**.  
We compare **OLS (global regression)** vs **GWR (local regression)** to capture **spatial heterogeneity**, validate model performance, and visualize geographic patterns for interpretation and reporting.

## Key Findings
- **Best-performing model:** **GWR (Bisquare kernel)** outperform OLS (AIC **157.26** vs **166.38**; R² **0.823** vs **0.808**).
- **Lower prediction error:** Residual range is narrower in GWR Bisquare (**−4.06 to 3.71**) compared to OLS (**−4.48 to 4.33**).
- **Robust predictors:** All four predictors are **statistically significant across all districts/cities** (p < 0.05).
- **Spatial heterogeneity:** Effect sizes vary by location; some areas show stronger relationships (e.g., for cigarette consumption and basic service/education variables).

## Dataset
Unit of analysis: **District/City (Kabupaten/Kota)** in East Java, year **2022**.

**Variables**
- **Y:** Percentage of poor population
- **X1:** Open Unemployment Rate (TPT)
- **X2:** Access to decent drinking water
- **X3:** Expected years of schooling (Harapan Lama Sekolah)
- **X4:** Cigarette consumption (age 30–39)

## Methodology
1. **Data integration**
   - Join socio-economic indicators (Excel) with administrative boundary shapefile.
   - Ensure consistent naming/join keys and CRS handling.
2. **Global model (OLS)**
   - Fit `lm(Y ~ X1 + X2 + X3 + X4)` as baseline.
   - Run assumption checks and baseline diagnostics.
3. **Local model (GWR)**
   - Select bandwidth using CV.
   - Compare kernels (Gaussian, Bisquare, Tricube).
   - Choose best model using **AIC** and **R²** (final: **Bisquare kernel**).
4. **Diagnostics & visualization**
   - Coefficient maps (local effects), residual maps/plots, significance clustering.

## Repository Structure
