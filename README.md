# Seasonal Food Purchasing Analysis (Tesco Grocery MSOA Project)

This repository contains all code and datasets for a spatial and statistical analysis of seasonal food purchasing patterns across London Middle Layer Super Output Areas (MSOAs), using the Tesco Grocery 1.0 dataset.  
The project forms part of a research investigation for the Master of Data Science program at the University of Adelaide.

The repository contains:
EDA file which contains all the code
Dec_msoa_grocery.csv,Apr_msoa_grocery.csv,year_msoa_grocery.csv for seasonal and energy density analysis 
msoa_grocery.csv for socio economical analysis
A shapefile to create choropleth maps (MSOA_2011_London_gen_MHW.shp)

Make sure to add all the csv files into a folder called dats in your desktop for better management.
while loading the datasets,makesure to replace my filepath with your own file path.
eg: "C:/Users/sheba/OneDrive/Documents/Research project A/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/MSOA_2011_London_gen_MHW.shp"
this is my file path for the shapefile. Add your filepath here.

Required packages:
library(tidyverse)
library(sf)
library(tmap)
library(spdep)
library(car)
library(janitor)
