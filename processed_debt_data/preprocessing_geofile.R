library(tigris)
library(sf)

# Download all ZIP boundaries (automatic!)
zips <- zctas(year = 2022)

# Load and merge your data
df <- read.csv("regression_dataset.csv")
df$zip <- sprintf("%05d", as.numeric(df$zip))
spatial_data <- merge(zips, df, by.x = "ZCTA5CE20", by.y = "zip")

# Export
st_write(spatial_data, "cook_county_debt.geojson")
getwd()
View(spatial_data)
names(spatial_data)
