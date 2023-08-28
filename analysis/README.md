1. `compile-data.R` compiles all the tracking data used
2. `movement-models.R` fits the movement models to each tracking dataset
3. `nc-weather-data-to-rds.R` exports all historical weather data from the Copernicus website (https://cds.climate.copernicus.eu/) from `.nc` format to `.tif` format
4. `download-dem.R` downloads the Digital Elevation Models (DEMs) necessary for downloading climate projections and for the Resource Selection Functions (RSFs)
5. `download-climate-proejctions.R` uses the DEMs to download climate projections via the `climatenaR` package
6. `add-speeds-and-weather.R` adds the estimated temperature, precipitation, snow depth, and movement speeds to each tracking data point
7. `weather-regressions.R` models the relationships between movement speed and weather
8. `rsfs.R` models animals' space use as a function of weather and habitat type
