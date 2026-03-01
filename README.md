# Automated Exploratory Data Analysis (EDA) in R

### 📊 Comprehensive Descriptive Statistics & Visualizations for Biological and Agronomic Data

## Overview
This repository contains an R script designed to automate the process of **Exploratory Data Analysis (EDA)**. It is specifically tailored to handle multi-variable datasets common in agronomy, entomology (e.g., *Spodoptera littoralis* leaf damage studies), and biological research.

The script iterates through a dataset, automatically identifying numerical variables, calculating key descriptive statistics, classifying data quality based on variability, and generating essential visualizations.

### 👥 Collaboration and Origin
This tool was developed as a specialized support component in collaboration with research partners for the computerization and standardization of bio-agronomic surveys.

---

## Key Features

The R script performs the following tasks automatically for every numerical column in the input dataframe (`dati`):

### 1. Robust Descriptive Statistics
It generates a summary table including:
* **Mean & Median** (Central tendency)
* **Standard Deviation & Variance** (Dispersion)
* **Coefficient of Variation (CV%)** (Relative variability)

### 2. Automatic Data Quality Classification
The script classifies each variable based on its CV%:
* **Optimal** (CV < 10%)
* **Good** (10% ≤ CV < 30%)
* **Problematic** (30% ≤ CV < 40%)
* **Critical** (CV ≥ 40%)

### 3. Smart Outlier Detection (P-Value Method)
Unlike simple "2SD" rules, this script calculates a **Z-Score** and an associated **P-Value** for *every* data point. If data points are identified with a probability of occurrence less than 1% ($P < 0.01$) under a normal assumption, they are immediately flagged and printed in the console for investigation.

### 4. Comprehensive Diagnostic Plots
It automatically generates a 2x2 graphical panel for each variable:
* **Histogram with Normal Curve Overlay** (For assessing distribution shape)
* **Boxplot** (For visualizing dispersion, median, and potential outliers)
* **Q-Q Plot** (Quantile-Quantile plot for formally testing normality assumption)
  [Diagnostic Plots] (3d6e516e-1380-468e-a3f8-8e1d7548e357.png)

---

## Required R Packages
The script relies solely on R's base graphics and stats packages (`stats`, `graphics`), ensuring high compatibility. No extra installations are typically required.

---

## How to Use

1.  **Prepare Data:** Load your data into an R dataframe named `dati`.
    ```R
    dati <- read.csv("your_data.csv") # Example
    # OR create dummy data
    set.csv(123)
    dati <- data.frame(
      Treatment_A = rnorm(50, mean=100, sd=5),
      Treatment_B = rnorm(50, mean=110, sd=25)
    )
    ```
2.  **Run the Script:** Copy and paste the provided R code into your RStudio console or script file and run it.
3.  **Inspect Output:**
    * **Console:** Check for outlier flags ([!!!] ATTENZIONE) and general summaries.
    * **Viewer:** Two interactive tables (`tabella` and `risultati`) will open automatically for detailed inspection.
    * **Plots:** Diagnostic charts will appear in the plots pane.

---

## Contact & Credits
This methodology was developed and computerized by:
**[Your Name/Username]**

---
*Date: March 1, 2026. This README was generated following technical consultancy on data digitalization.*
