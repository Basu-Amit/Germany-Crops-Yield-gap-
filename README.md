# Germany crop Yield Gap Dashboard 🌾

This repository contains R code which uses crop yield gaps data for Germany from open source data. The crops include Barley, Wheat, Maize, Potato, Pea, Soyabean, Faba Bean. Basically it produces Interactive R Shiny dashboard visualizing crop yield gaps in Germany
using open data from the Global Yield Gap Atlas (GYGA).

## Features
- Station-level yield gap map
- Area-weighted national KPIs
- Climate-zone yield gap comparison
- Interactive exploration with Shiny & Leaflet

## Data
- Source: Global Yield Gap Atlas (GYGA)
- Crop: Barley, Wheat, Maize, Potato, Pea, Soyabean, Faba Bean
- Country: Germany
- License: CC BY-NC-SA 4.0 (data)

## Run locally
```r
install.packages(c(
  "shiny", "shinydashboard", "leaflet", "plotly",
  "readxl", "dplyr", "sf", "lwgeom"
))
shiny::runApp()
