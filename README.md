# 🚗 Used Car Market Analysis — Predictive Price Modeling & Depreciation Insights

![Team Project](https://img.shields.io/badge/Team-6%20Members-blue)
![Focus Area](https://img.shields.io/badge/Subset-Economy%20Cars-green)
![Tools](https://img.shields.io/badge/Skills-Data%20Wrangling%20%7C%20Modeling%20%7C%20EDA-yellow)

## 🔗 [Presentation Slides](https://docs.google.com/presentation/d/1CwUQrjEGFacXS4JR28WxDYuPm0lReYUdFFNDIlLE8Zs/edit?usp=sharing)

---

## 📘 Project Overview

This project investigates the **used car market** through the lens of data science, with the goal of uncovering:
- Which manufacturers’ cars depreciate the least over time
- Market insights tailored to different budget tiers (economy, mid-range, luxury)
- Actionable **predictive price models (PPMs)** for resale investment decisions

The team of six divided the dataset into brand tiers. I focused on the **Economy car subset**, conducting:
- Data cleaning
- Outlier removal
- Model development
- Final investment recommendations

---

## 🧹 Data Wrangling & Preprocessing

### ✅ Team-Wide Preprocessing
- Removed vehicles with mileage = 0 or > 1,000,000
- Removed cars with prices of $0 or extreme values (e.g. $1B)
- Standardized fields (e.g., "All-wheel drive" → "AWD")
- Filtered cars from year ≥ 2010
- Added `log_price` and `log_mileage` columns

### 🔍 Subset-Specific Filtering (Economy)
- Removed upper-end outliers that didn’t align with economy classification
- Logged variables for visual clarity and model accuracy
- Addressed QQ plot skew via manual outlier removal

---

## 📊 Predictive Price Model (Economy Subset)

**Model Formula:**  
```python
log_price ~ manufacturer * year + log_mileage
```

**Model Performance:**
- **R²** = 0.64
- **RMSE** = $4,724
- **p-value** < 2e-6
- Residuals followed a mostly normal distribution (validated via QQ plot)

**Use Case:**  
If a car’s listing price is below its predicted price, it may be **undervalued** and worth pursuing.

> 🔍 Example:  
> `Chrysler_price_hat = 10^(-76.66 + 0.0404 * year - 0.1177 * log_mileage)`

---

## 📉 Depreciation Insights — Economy Subset

| Brand     | 2022 vs 2019 Depreciation |
|-----------|-----------------------------|
| Mitsubishi | **18.7%** (Best performer) |
| Subaru     | **23.9%** (Worst performer) |

**Conclusion:**
- ✅ **Buy Mitsubishi** — holds value best among economy brands
- ❌ **Avoid Subaru** — depreciates more aggressively

---

## 🧠 Industry Implications

For businesses such as rental agencies or dealerships:

### ✅ Recommendations
- Use depreciation data to guide **fleet purchases**
- Leverage predictive models to assess undervalued listings
- Prioritize low-depreciation brands like Mitsubishi in the economy segment

### ⚠️ Risks to Consider
- Depreciation varies across **specific models**, even within brands
- Models are based on **historical data** — cannot predict macroeconomic events or disasters
- **Repairs and upkeep** play a significant role in long-term depreciation

---

## 📈 Summary of Predictive Price Models by Subgroup

### ⚙️ Economy Cars
- **Model:** `log_price ~ manufacturer * year + log_mileage`
- **R²:** 0.64  
- **RMSE:** $4,724  
- **Top Pick:** Mitsubishi  
- **Avoid:** Subaru

### ⚙️ Mid-Range Cars
- **R²:** 0.596  
- **RMSE:** $6,594  
- **Top Pick:** Jaguar  
- **Avoid:** Ford  
- **Depreciation:**  
  - Jaguar: 18.2%  
  - Ford: 29.3%

### ⚙️ Luxury Cars
- **R²:** 0.724  
- **RMSE:** $7,627  
- **Depreciation Patterns:**  
  - Audi depreciates less than BMW or Cadillac  
  - Value holds up to ~52k miles, then drops steadily

---

## 📂 Repository Structure

```bash
📁 economy_analysis/          # Subset-specific modeling (my focus area)
📁 team_notebooks/            # Shared initial EDA and preprocessing steps
📁 visualizations/            # Graphs and QQ plots
📄 used_car_ppm.py           # Predictive price model pipeline
📄 README.md
```

---

## 🧱 Challenges Faced

- **Outlier Management:** Some economy cars were priced too high and skewed the model
- **QQ Plot Issues:** Tail skew required manual review and adjustments
- **Causality Limitations:** Difficult to isolate causes of depreciation beyond mileage → solved by using price-based comparisons

---

## 📈 Skills Applied

- Data Wrangling (`pandas`, `numpy`)
- Exploratory Visualization (`matplotlib`, `seaborn`)
- Regression Modeling (`statsmodels`, `sklearn`)
- Business Reasoning from Quantitative Analysis
- Collaborative Subset Management & GitHub Version Control

---

## 🙋 About the Project

This project was completed in collaboration with a **6-member team** as part of BUAD 312 at USC.  
All shared preprocessing was done as a group. I personally handled the **Economy car** subset, and any work not authored by me is clearly labeled.

---

## 📜 License

This repository is intended for academic and research purposes. For commercial use, please contact the project owner.

---
