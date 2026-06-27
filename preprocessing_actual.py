import re
from pathlib import Path
import pandas as pd
import pdfplumber

# =========================================================
# DATA DIRECTORY (YOUR PATH)
# =========================================================

DATA_DIR = Path("/Users/roshnivora/Desktop/School/Year2Classes/WinterQuarter/BDD/BDD_data")

PDF_FILES = {
    2022: DATA_DIR / "2022 Past Due Debt Report.pdf",
    2023: DATA_DIR / "2023 Past Due Debt Report.pdf",
    2024: DATA_DIR / "Outstanding Past Due Utility Debt - 2024.pdf",
}

OUTPUT_DIR = DATA_DIR / "processed_debt_data"
OUTPUT_DIR.mkdir(exist_ok=True)

# =========================================================
# COLUMN STRUCTURES
# =========================================================

BALANCE_COLS = [
    "zip",
    "domestic_residential_occupancy",
    "single_family_dwellings",
    "two_unit_residential_structures",
    "three_unit_residential_structures",
    "four_unit_residential_structures",
    "five_unit_residential_structures",
    "six_to_12_unit_residential_structures",
    "more_than_12_unit_residential_structures",
    "combination_residential_commercial_structures",
    "commercial_structures",
    "industrial_structures",
    "grand_total",
    "year",
]

AGED_COLS = [
    "zip",
    "service_class",
    "account_count",
    "days_30",
    "days_60",
    "days_90",
    "days_180",
    "days_365_plus",
    "total_past_due",
    "year",
]

SERVICE_CLASS_MAP = {
    "domestic residential occupancy": "domestic_residential_occupancy",
    "single-family dwellings": "single_family_dwellings",
    "two-unit residential structures": "two_unit_residential_structures",
    "three-unit residential structures": "three_unit_residential_structures",
    "four-unit residential structures": "four_unit_residential_structures",
    "five-unit residential structures": "five_unit_residential_structures",
    "six to 12 unit residential structures": "six_to_12_unit_residential_structures",
    "more than 12 unit residential structures": "more_than_12_unit_residential_structures",
    "combination of residential and commercial structures": "combination_residential_commercial_structures",
    "commercial structures": "commercial_structures",
    "industrial structures": "industrial_structures",
}

# =========================================================
# CLEANING FUNCTIONS
# =========================================================

def normalize_text(text):
    if text is None:
        return ""
    text = text.replace("\n", " ")
    text = re.sub(r"\s+", " ", text).strip()
    return text


def clean_money(value):

    if pd.isna(value):
        return pd.NA

    s = str(value).strip()

    if s in {"", "-", "$ -", "$-", "$"}:
        return 0.0

    s = s.replace("$", "").replace(",", "").strip()

    if re.fullmatch(r"\(.*\)", s):
        s = "-" + s[1:-1]

    try:
        return float(s)
    except:
        return pd.NA


def clean_count(value):

    if pd.isna(value):
        return pd.NA

    s = str(value).replace(",", "").strip()

    if s in {"", "-", "nan"}:
        return 0

    try:
        return int(float(s))
    except:
        return pd.NA


def is_zip(value):

    return bool(re.fullmatch(r"\d{5}", str(value).strip()))


# =========================================================
# EXTRACT TEXT LINES FROM PDF
# =========================================================

def extract_lines(pdf_path):

    lines = []

    with pdfplumber.open(pdf_path) as pdf:

        for page in pdf.pages:

            text = page.extract_text()

            if text:
                page_lines = [normalize_text(l) for l in text.split("\n")]
                page_lines = [l for l in page_lines if l]

                lines.extend(page_lines)

    return lines


# =========================================================
# BALANCE TABLE PARSER
# =========================================================

def parse_balance_row(line):

    line = normalize_text(line)

    if not re.match(r"^\d{5}\b", line):
        return None

    zip_code = line[:5]

    money_pattern = r"\$\s*-|\$\s*[\d,]+\.\d+|\([\d,]+\.\d+\)|-?[\d,]+\.\d+"
    values = re.findall(money_pattern, line)

    if len(values) < 12:
        return None

    values = values[:12]

    return {
        "zip": zip_code,
        "domestic_residential_occupancy": clean_money(values[0]),
        "single_family_dwellings": clean_money(values[1]),
        "two_unit_residential_structures": clean_money(values[2]),
        "three_unit_residential_structures": clean_money(values[3]),
        "four_unit_residential_structures": clean_money(values[4]),
        "five_unit_residential_structures": clean_money(values[5]),
        "six_to_12_unit_residential_structures": clean_money(values[6]),
        "more_than_12_unit_residential_structures": clean_money(values[7]),
        "combination_residential_commercial_structures": clean_money(values[8]),
        "commercial_structures": clean_money(values[9]),
        "industrial_structures": clean_money(values[10]),
        "grand_total": clean_money(values[11]),
    }


def extract_balance_df(pdf_path, year):

    lines = extract_lines(pdf_path)

    rows = []
    in_section = False

    for line in lines:

        low = line.lower()

        if "year end past due balances by zip code and service class" in low:
            in_section = True
            continue

        if "aged past due balance" in low:
            in_section = False

        if not in_section:
            continue

        parsed = parse_balance_row(line)

        if parsed:
            parsed["year"] = year
            rows.append(parsed)

    df = pd.DataFrame(rows)

    df = df.drop_duplicates(subset=["zip"])

    df["zip"] = df["zip"].astype(str)

    df = df[BALANCE_COLS]

    return df


# =========================================================
# AGED DEBT PARSER
# =========================================================

def parse_aged_row(line):

    line = normalize_text(line)

    m = re.match(r"^(\d{5})\s+([\d,]+)\s+(.*)$", line)

    if not m:
        return None

    zip_code = m.group(1)
    count = clean_count(m.group(2))
    rest = m.group(3)

    money_pattern = r"\$\s*-|\$\s*[\d,]+\.\d+|\([\d,]+\.\d+\)|-?[\d,]+\.\d+"
    values = re.findall(money_pattern, rest)

    if len(values) < 6:
        return None

    values = values[:6]

    return {
        "zip": zip_code,
        "account_count": count,
        "days_30": clean_money(values[0]),
        "days_60": clean_money(values[1]),
        "days_90": clean_money(values[2]),
        "days_180": clean_money(values[3]),
        "days_365_plus": clean_money(values[4]),
        "total_past_due": clean_money(values[5]),
    }


def extract_aged_df(pdf_path, year):

    lines = extract_lines(pdf_path)

    rows = []
    current_class = None

    for line in lines:

        low = line.lower()

        if "aged past due balance" in low and not low.startswith("zip code"):

            for key in SERVICE_CLASS_MAP:
                if key in low:
                    current_class = SERVICE_CLASS_MAP[key]

        if current_class is None:
            continue

        parsed = parse_aged_row(line)

        if parsed:
            parsed["service_class"] = current_class
            parsed["year"] = year
            rows.append(parsed)

    df = pd.DataFrame(rows)

    df["zip"] = df["zip"].astype(str)

    df = df[AGED_COLS]

    return df


# =========================================================
# RUN PROCESSING
# =========================================================

balance_list = []
aged_list = []

for year, pdf_path in PDF_FILES.items():

    print("Processing", year)

    balance_df = extract_balance_df(pdf_path, year)
    aged_df = extract_aged_df(pdf_path, year)

    balance_df.to_csv(OUTPUT_DIR / f"balances_{year}.csv", index=False)
    aged_df.to_csv(OUTPUT_DIR / f"aged_debt_{year}.csv", index=False)

    balance_list.append(balance_df)
    aged_list.append(aged_df)

balances_all = pd.concat(balance_list, ignore_index=True)
aged_all = pd.concat(aged_list, ignore_index=True)

balances_all.to_csv(OUTPUT_DIR / "balances_all_years.csv", index=False)
aged_all.to_csv(OUTPUT_DIR / "aged_all_years.csv", index=False)

print("Done")

print(balances_all.head())
print(aged_all.head())

# =========================================================
# CLEAN CENSUS DATA
# =========================================================

census = pd.read_csv(DATA_DIR / "cencus race data.csv")

# drop metadata row
census = census.iloc[1:].copy()

# extract 5-digit ZIP from GEO_ID
census["zip"] = census["GEO_ID"].str.extract(r"(\d{5})$")

# make zip a 5-digit string
census["zip"] = census["zip"].astype(str).str.zfill(5)

# convert non-ID columns to numeric
for col in census.columns:
    if col not in ["GEO_ID", "NAME", "zip"]:
        census[col] = pd.to_numeric(census[col], errors="coerce")

# rename race columns
census = census.rename(columns={
    "B02001_001E": "total_population",
    "B02001_002E": "white",
    "B02001_003E": "black",
    "B02001_004E": "native_american",
    "B02001_005E": "asian",
    "B02001_006E": "pacific_islander",
    "B02001_007E": "other_race",
    "B02001_008E": "two_or_more_races"
})

# create race share variables
census["pct_white"] = census["white"] / census["total_population"]
census["pct_black"] = census["black"] / census["total_population"]
census["pct_asian"] = census["asian"] / census["total_population"]
census["pct_native_american"] = census["native_american"] / census["total_population"]
census["pct_pacific_islander"] = census["pacific_islander"] / census["total_population"]
census["pct_other_race"] = census["other_race"] / census["total_population"]
census["pct_two_or_more_races"] = census["two_or_more_races"] / census["total_population"]

# keep useful columns
census_clean = census[[
    "zip",
    "NAME",
    "total_population",
    "white",
    "black",
    "native_american",
    "asian",
    "pacific_islander",
    "other_race",
    "two_or_more_races",
    "pct_white",
    "pct_black",
    "pct_asian",
    "pct_native_american",
    "pct_pacific_islander",
    "pct_other_race",
    "pct_two_or_more_races"
]].copy()

census_clean.to_csv(OUTPUT_DIR / "census_clean.csv", index=False)

print("\nCensus cleaned")
print(census_clean.head())


# =========================================================
# SUBSET TO COOK COUNTY ZIP CODES
# =========================================================

cook_zipcodes = [
    "60004","60005","60007","60008","60016","60018","60025","60053","60056","60062",
    "60104","60106","60107","60131","60153","60154","60155","60160","60162","60163",
    "60164","60165","60171","60176","60177","60179","60181","60193",
    "60201","60202","60203","60301","60302","60304","60305",
    "60402","60406","60409","60411","60412","60419","60422","60423","60425","60426",
    "60428","60429","60430","60438","60439","60443","60445","60452","60453","60455",
    "60456","60457","60458","60459","60461","60462","60463","60464","60465","60466",
    "60467","60469","60471","60472","60473","60475","60476","60477","60478","60480",
    "60482","60501","60513","60521","60523","60525","60526","60534","60546","60558",
    "60601","60602","60603","60604","60605","60606","60607","60608","60609","60610",
    "60611","60612","60613","60614","60615","60616","60617","60618","60619","60620",
    "60621","60622","60623","60624","60625","60626","60628","60629","60630","60631",
    "60632","60633","60634","60636","60637","60638","60639","60640","60641","60642",
    "60643","60644","60645","60646","60647","60649","60651","60652","60653","60654",
    "60655","60656","60657","60659","60660","60706","60707","60712","60714"
]

balances_cook = balances_all[balances_all["zip"].isin(cook_zipcodes)].copy()
aged_cook = aged_all[aged_all["zip"].isin(cook_zipcodes)].copy()
census_cook = census_clean[census_clean["zip"].isin(cook_zipcodes)].copy()

balances_cook.to_csv(OUTPUT_DIR / "balances_cook_county.csv", index=False)
aged_cook.to_csv(OUTPUT_DIR / "aged_cook_county.csv", index=False)
census_cook.to_csv(OUTPUT_DIR / "census_cook_county.csv", index=False)

print("\nCook County subsets created")
print("balances_cook shape:", balances_cook.shape)
print("aged_cook shape:", aged_cook.shape)
print("census_cook shape:", census_cook.shape)


# =========================================================
# MERGE BALANCES + CENSUS FOR REGRESSION DATASET
# =========================================================

regression_df = balances_cook.merge(census_cook, on="zip", how="left")

regression_df.to_csv(OUTPUT_DIR / "regression_dataset.csv", index=False)

print("\nRegression dataset created")
print(regression_df.head())
print("regression_df shape:", regression_df.shape)


# =========================================================
# OPTIONAL: QUICK CHECKS
# =========================================================

print("\nUnique ZIPs in balances_cook:", balances_cook["zip"].nunique())
print("Unique ZIPs in aged_cook:", aged_cook["zip"].nunique())
print("Unique ZIPs in census_cook:", census_cook["zip"].nunique())
print("Unique ZIPs in regression_df:", regression_df["zip"].nunique())

print("\nYears in regression_df:")
print(regression_df["year"].value_counts().sort_index())

print("\nMissing values in key vars:")
print(regression_df[["grand_total", "pct_black", "pct_white", "pct_asian"]].isna().sum())

# =========================================================
# CREATE REGRESSION DATASET
# =========================================================

census_clean = pd.read_csv(OUTPUT_DIR / "census_clean.csv", dtype={"zip": str})

regression_df = balances_all.merge(
    census_clean,
    on="zip",
    how="left"
)

# ---------------------------------------------------------
# CREATE PER CAPITA DEBT VARIABLE
# ---------------------------------------------------------

regression_df["debt_per_capita"] = (
    regression_df["grand_total"] /
    regression_df["total_population"]
)

# optional: also create debt per household proxy
regression_df["debt_per_1000_people"] = regression_df["debt_per_capita"] * 1000

# save dataset
regression_df.to_csv(OUTPUT_DIR / "regression_dataset.csv", index=False)

print("Regression dataset created")

print(regression_df[[
    "zip",
    "year",
    "grand_total",
    "total_population",
    "debt_per_capita"
]].head())

regression_df.to_csv(OUTPUT_DIR / "regression_dataset.csv", index=False)


# =========================================================
# CLEAN CENSUS INCOME DATA
# =========================================================

income = pd.read_csv(DATA_DIR / "census income data.csv")

# drop ACS metadata row
income = income.iloc[1:].copy()

# extract ZIP code from GEO_ID
income["zip"] = income["GEO_ID"].str.extract(r"(\d{5})$")
income["zip"] = income["zip"].astype(str).str.zfill(5)

# rename income column
income = income.rename(columns={
    "B19013_001E": "median_income"
})

# convert to numeric
income["median_income"] = pd.to_numeric(income["median_income"], errors="coerce")

# keep only needed columns
income_clean = income[["zip","median_income"]].copy()

# save cleaned income dataset
income_clean.to_csv(OUTPUT_DIR / "income_clean.csv", index=False)

print("Income cleaned")
print(income_clean.head())


# =========================================================
# MERGE INCOME INTO REGRESSION DATASET RUN FROM HERE
# =========================================================

# reload regression dataset
regression_df = pd.read_csv(OUTPUT_DIR / "regression_dataset.csv", dtype={"zip":str})

# merge income
regression_df = regression_df.merge(
    income_clean,
    on="zip",
    how="left"
)

# save updated dataset
regression_df.to_csv(OUTPUT_DIR / "regression_dataset.csv", index=False)

print("Income merged into regression dataset")

print(regression_df[[
    "zip",
    "year",
    "debt_per_capita",
    "median_income"
]].head())

import numpy as np
regression_df["log_debt_per_capita"] = np.log(regression_df["debt_per_capita"] + 1)
regression_df["log_income"] = np.log(regression_df["median_income"])


regression_df["pct_nonwhite"] = 1 - regression_df["pct_white"]
regression_df.to_csv(OUTPUT_DIR / "regression_dataset.csv", index=False)

regression_df.head()

regression_df = regression_df.drop(columns = [['median_income_x', 'median_income_y', ]])

regression_df.columns


regression_df.head()
regression_df["median_income"].isna().sum()
regression_df = regression_df.dropna(subset=["median_income"])


#running regression models 
import statsmodels.formula.api as smf

# Model 0
model0 = smf.ols(
    "log_debt_per_capita ~ pct_black",
    data=regression_df
).fit()

# Model 1
model1 = smf.ols(
    "log_debt_per_capita ~ pct_black + log_income",
    data=regression_df
).fit()

# Final model
model2 = smf.ols(
    "log_debt_per_capita ~ pct_black + pct_asian + log_income",
    data=regression_df
).fit()

print(model0.summary())
print(model1.summary())
print(model2.summary())


from statsmodels.iolib.summary2 import summary_col

table = summary_col(
    [model0, model1, model2],
    stars=True,
    model_names=["Model 0", "Model 1", "Final Model"],
    info_dict={
        "N": lambda x: f"{int(x.nobs)}",
        "R²": lambda x: f"{x.rsquared:.3f}"
    }
)

print(table)

with open("regression_results_2.txt", "w") as f:
    f.write(table.as_text())

from stargazer.stargazer import Stargazer

stargazer = Stargazer([model0, model1, model2])

stargazer.title("Regression Results: Utility Debt and Racial Composition")
stargazer.custom_columns(["Model 0", "Model 1", "Final Model"], [1,1,1])
stargazer.dependent_variable_name("Log Debt Per Capita")

html = stargazer.render_html()

with open("regression_table_2.html", "w") as f:
    f.write(html)

import altair as alt

scatter = alt.Chart(regression_df).mark_circle(size=60).encode(
    x=alt.X(
        "pct_nonwhite:Q",
        title="Percent Nonwhite",
        scale=alt.Scale(domain=[0,1])
    ),
    y=alt.Y(
        "log_debt_per_capita:Q",
        title="log Debt Per Capita ($)"
    ),
    tooltip=[
        "zip",
        "year",
        "pct_nonwhite",
        "debt_per_capita",
        "median_income"
    ]
)

scatter


scatter_yr = alt.Chart(regression_df).mark_circle(size=60).encode(
    x="pct_nonwhite:Q",
    y="log_debt_per_capita:Q",
    color="year:N",
    tooltip=["zip","year","debt_per_capita"]
)

scatter_yr

# import altair as alt

# binscatter = alt.Chart(regression_df).mark_circle(size=120).encode(
#     x=alt.X(
#         "pct_nonwhite:Q",
#         bin=alt.Bin(maxbins=20),
#         title="Percent Nonwhite"
#     ),
#     y=alt.Y(
#         "mean(debt_per_capita):Q",
#         title="Average Debt Per Capita ($)"
#     ),
#     tooltip=[
#         alt.Tooltip("mean(debt_per_capita):Q", title="Avg Debt"),
#         alt.Tooltip("count():Q", title="ZIPs in bin")
#     ]
# )

# binscatter

# print(regression_df["zip"].nunique())
# print(regression_df["year"].value_counts())
# print(regression_df.shape)

# set_2022 = set(regression_df[regression_df["year"] == 2022]["zip"])
# set_2024 = set(regression_df[regression_df["year"] == 2024]["zip"])

# missing_2024 = set_2022 - set_2024

# print(len(missing_2024))
# print(missing_2024)


# import altair as alt

hist = alt.Chart(regression_df).mark_bar().encode(
    x=alt.X(
        "pct_nonwhite:Q",
        bin=alt.Bin(maxbins=25),
        title="Percent Nonwhite"
    ),
    y=alt.Y("count()", title="Number of ZIP-Year Observations"),
    tooltip=["count()"]
)

hist


zip_df = regression_df.groupby("zip").mean(numeric_only=True).reset_index()

scatter = alt.Chart(zip_df).mark_circle(size=80, opacity=0.6).encode(
    x=alt.X("pct_nonwhite:Q", title="Percent Nonwhite"),
    y=alt.Y("log_debt_per_capita:Q", title="Log Debt Per Capita"),
    tooltip=["zip","pct_nonwhite","debt_per_capita","median_income"]
)

trend = alt.Chart(zip_df).transform_regression(
    "pct_nonwhite",
    "log_debt_per_capita"
).mark_line(color="red", size=3)

scatter + trend