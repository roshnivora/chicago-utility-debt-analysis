# Assessing Racial Injustices and Income Gaps in Chicago through Water Utility Debt

This project investigates whether racial composition and income levels predict water utility debt burdens across ZIP codes in Cook County, Illinois, using data from 2022–2024.

## Paper

[Assessing Racial Injustices and Income Gaps in Chicago through Water Utility Debt.pdf](Assessing%20Racial%20Injustices%20and%20Income%20Gaps%20in%20Chicago%20through%20Water%20Utility%20Debt.pdf)

## Data Sources

- **Chicago Department of Water Management** — Past Due Utility Debt Reports (2022, 2023, 2024): ZIP-level outstanding balances by service class
- **U.S. Census Bureau ACS** — Race/ethnicity and median household income by ZIP code

## Repository Structure

```
├── preprocessing_actual.py          # Extracts debt data from PDFs, cleans census data, builds regression dataset
├── processed_debt_data/
│   ├── preprocessing.R              # R translation of the full pipeline including regression models
│   ├── preprocessing_geofile.R      # Joins regression dataset to TIGER/Line ZCTA boundaries → GeoJSON
│   ├── regression_dataset.csv       # Final merged dataset used for modeling
│   ├── cook_county_debt.geojson     # Cook County ZIP boundaries with debt + demographic variables
│   ├── cook_lisa.geojson            # Local Indicators of Spatial Association (LISA) results
│   ├── chicago_boundary.geojson     # Chicago city boundary
│   ├── LISA_map.png                 # LISA cluster map
│   ├── GIS Final Project.qmd        # Quarto source for GIS analysis
│   └── GIS Final Project.html       # Rendered GIS report
```

## Methods

**Data pipeline (Python / R):** PDF reports are parsed with `pdfplumber` / `pdftools` to extract ZIP-level debt balances. Census race and income variables are cleaned and merged into a single regression dataset. Debt is normalized as log debt per capita.

**Regression models (OLS):**

| Model | Predictors |
|-------|-----------|
| Model 0 | % Black |
| Model 1 | % Black + log median income |
| Final Model | % Black + % Asian + log median income |

**GIS analysis:** ZIP code boundaries (TIGER/Line 2020 ZCTAs) are joined to the regression dataset and exported as GeoJSON. LISA (Local Moran's I) is used to identify spatial clusters of high and low debt burden across Cook County.

## Key Variables

| Variable | Description |
|----------|-------------|
| `log_debt_per_capita` | Log of total past-due water debt divided by ZIP population |
| `pct_black`, `pct_asian`, `pct_white` | Racial composition shares from ACS |
| `pct_nonwhite` | 1 − pct_white |
| `log_income` | Log of median household income |

## Dependencies

**Python:** `pandas`, `pdfplumber`, `statsmodels`, `stargazer`, `altair`

**R:** `pdftools`, `dplyr`, `sf`, `tigris`, `stargazer`, `modelsummary`, `ggplot2`
