#Holiday Food Purchase Patterns And Socioeconomic Inequalities In London MSOAs

# Core packages
library(tidyverse)
library(sf)
library(tmap)
library(car)      # for VIF
library(spdep)    # for neighbours / spatial analysis

#1
# Year, December, April data
year_data <- read_csv("data/year_msoa_grocery .csv")
dec_data  <- read_csv("data/Dec_msoa_grocery.csv")
apr_data  <- read_csv("data/Apr_msoa_grocery.csv")

# 2 Quick check
glimpse(year_data)
glimpse(dec_data)
glimpse(apr_data)


# Food categories in the dataset
categories <- c(
  "f_beer", "f_dairy", "f_eggs", "f_fats_oils", "f_fish",
  "f_fruit_veg", "f_grains", "f_meat_red", "f_readymade", "f_sauces",
  "f_soft_drinks", "f_spirits", "f_sweets", "f_tea_coffee",
  "f_water", "f_wine", "f_poultry"
)
#3

#December/Year ratios 
dec_cat_ratios <- map_dfr(categories, function(cat) {
  tibble(
    category  = cat,
    year_mean = mean(year_data[[cat]], na.rm = TRUE),
    dec_mean  = mean(dec_data[[cat]],  na.rm = TRUE)
  ) %>%
    mutate(dec_to_year_ratio = dec_mean / year_mean)
})

# View all, sorted
dec_cat_ratios %>%
  arrange(desc(dec_to_year_ratio)) %>%
  print(n = Inf)

# Top 10 December categories
top10_dec<- dec_cat_ratios %>%
  arrange(desc(dec_to_year_ratio)) %>%
  slice_head(n = 10)
view(top10_dec)




#April/Year ratios
apr_cat_ratios <- map_dfr(categories, function(cat) {
  tibble(
    category   = cat,
    year_mean  = mean(year_data[[cat]], na.rm = TRUE),
    april_mean = mean(apr_data[[cat]],  na.rm = TRUE)
  ) %>%
    mutate(april_to_year_ratio = april_mean / year_mean)
})

apr_cat_ratios %>%
  arrange(desc(april_to_year_ratio)) %>%
  print(n = Inf)

top10_apr <- apr_cat_ratios %>%
  arrange(desc(april_to_year_ratio)) %>%
  slice_head(n = 10)
view(top10_apr)



#4 MSOA ratios for multiple food category, helped by a function

# Helper to compute month/year ratios by MSOA for one category
compute_ratio <- function(month_df, year_df, category_col) {
  month_df %>%
    select(area_id, month_value = all_of(category_col)) %>%
    left_join(
      year_df %>% select(area_id, year_value = all_of(category_col)),
      by = "area_id"
    ) %>%
    mutate(ratio = month_value / year_value) %>%
    mutate(ratio = ifelse(is.infinite(ratio) | year_value == 0, NA_real_, ratio))
}

#April/Year ratios for specific categories
ratio_fish_apr<- compute_ratio(apr_data, year_data, "f_fish")
ratio_wine_apr<- compute_ratio(apr_data, year_data, "f_wine")
ratio_poultry_apr<- compute_ratio(apr_data, year_data, "f_poultry")
ratio_fruitveg_apr<- compute_ratio(apr_data, year_data, "f_fruit_veg")
ratio_grains_apr<- compute_ratio(apr_data, year_data, "f_grains")

#December/Year ratios for specific categories
ratio_wine_dec<- compute_ratio(dec_data, year_data, "f_wine")
ratio_spirits_dec<- compute_ratio(dec_data, year_data, "f_spirits")
ratio_redmeat_dec<- compute_ratio(dec_data, year_data, "f_meat_red")
ratio_sauces_dec<- compute_ratio(dec_data, year_data, "f_sauces")
ratio_sweets_dec<- compute_ratio(dec_data, year_data, "f_sweets")

#5 Loading MSOA shapefile and joining ratios

# Load MSOA shapefile
msoas <- st_read(
  "C:/Users/sheba/OneDrive/Documents/Research project A/statistical-gis-boundaries-london/statistical-gis-boundaries-london/ESRI/MSOA_2011_London_gen_MHW.shp"
) %>%
  rename(msoa_code = MSOA11CD, msoa_name = MSOA11NM)

# Prepare ratio data frames with clearer names

#April
fish_apr_df<- ratio_fish_apr %>%
  rename(msoa_code = area_id, f_fish_apr_ratio = ratio)

wine_apr_df<- ratio_wine_apr %>%
  rename(msoa_code = area_id, f_wine_apr_ratio = ratio)

poultry_apr_df<- ratio_poultry_apr %>%
  rename(msoa_code = area_id, f_poultry_apr_ratio = ratio)

fruitveg_apr_df<- ratio_fruitveg_apr %>%
  rename(msoa_code = area_id, f_fruitveg_apr_ratio = ratio)

grains_apr_df<- ratio_grains_apr %>%
  rename(msoa_code = area_id, f_grains_apr_ratio = ratio)

# December
wine_dec_df<- ratio_wine_dec %>%
  rename(msoa_code = area_id, f_wine_dec_ratio = ratio)

spirits_dec_df<- ratio_spirits_dec %>%
  rename(msoa_code = area_id, f_spirits_dec_ratio = ratio)

redmeat_dec_df<- ratio_redmeat_dec %>%
  rename(msoa_code = area_id, f_redmeat_dec_ratio = ratio)

sauces_dec_df<- ratio_sauces_dec %>%
  rename(msoa_code = area_id, f_sauces_dec_ratio = ratio)

sweets_dec_df<- ratio_sweets_dec %>%
  rename(msoa_code = area_id, f_sweets_dec_ratio = ratio)

# Join everything into one big object
msoa_ratio_map <- msoas %>%
  left_join(fish_apr_df,     by = "msoa_code") %>%
  left_join(wine_apr_df,     by = "msoa_code") %>%
  left_join(poultry_apr_df,  by = "msoa_code") %>%
  left_join(fruitveg_apr_df, by = "msoa_code") %>%
  left_join(grains_apr_df,   by = "msoa_code") %>%
  left_join(wine_dec_df,     by = "msoa_code") %>%
  left_join(spirits_dec_df,  by = "msoa_code") %>%
  left_join(redmeat_dec_df,  by = "msoa_code") %>%
  left_join(sauces_dec_df,   by = "msoa_code") %>%
  left_join(sweets_dec_df,   by = "msoa_code")

#6 tmap holiday ratio maps (April&December)

tmap_mode("plot")

# Breaks with 1 in the middle band. More breaks, more gradient in maps
brks <- c(0, 0.5, 0.75, 0.9, 1, 1.1, 1.25, 1.5, Inf)

#April maps (RdBu) (Red to Blue transition)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_fish_apr_ratio",
              palette = "RdBu",
              breaks  = brks,
              title   = "April / Year (Fish)") +
  tm_layout(title = "MSOA Fish Purchase Ratio (April vs Year)",
            frame  = FALSE)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_wine_apr_ratio",
              palette = "RdBu",
              breaks  = brks,
              title   = "April / Year (Wine)") +
  tm_layout(title = "MSOA Wine Purchase Ratio (April vs Year)",
            frame  = FALSE)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_poultry_apr_ratio",
              palette = "RdBu",
              breaks  = brks,
              title   = "April / Year (Poultry)") +
  tm_layout(title = "MSOA Poultry Purchase Ratio (April vs Year)",
            frame  = FALSE)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_fruitveg_apr_ratio",
              palette = "RdBu",
              breaks  = brks,
              title   = "April / Year (Fruit & Veg)") +
  tm_layout(title = "MSOA Fruit & Veg Purchase Ratio (April vs Year)",
            frame  = FALSE)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_grains_apr_ratio",
              palette = "RdBu",
              breaks  = brks,
              title   = "April / Year (Grains)") +
  tm_layout(title = "MSOA Grains Purchase Ratio (April vs Year)",
            frame  = FALSE)


#DECEMBER maps (RdYlGn)(Red to Green transition)
tmap_mode("plot")  # use tmap_mode("view") for interactive maps and to knwo the details of each MSOA when hovering on it.
tm_shape(msoa_ratio_map) +
  tm_polygons("f_wine_dec_ratio",
              palette = "-RdYlGn",
              breaks  = brks,
              title   = "December / Year (Wine)") +
  tm_layout(title = "MSOA Wine Purchase Ratio (December vs Year)",
            frame  = FALSE)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_spirits_dec_ratio",
              palette = "RdYlGn",
              breaks  = brks,
              title   = "December / Year (Spirits)") +
  tm_layout(title = "MSOA Spirits Purchase Ratio (December vs Year)",
            frame  = FALSE)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_redmeat_dec_ratio",
              palette = "RdYlGn",
              breaks  = brks,
              title   = "December / Year (Red Meat)") +
  tm_layout(title = "MSOA Red Meat Purchase Ratio (December vs Year)",
            frame  = FALSE)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_sauces_dec_ratio",
              palette = "RdYlGn",
              breaks  = brks,
              title   = "December / Year (Sauces)") +
  tm_layout(title = "MSOA Sauces Purchase Ratio (December vs Year)",
            frame  = FALSE)

tm_shape(msoa_ratio_map) +
  tm_polygons("f_sweets_dec_ratio",
              palette = "RdYlGn",
              breaks  = brks,
              title   = "December / Year (Sweets)") +
  tm_layout(title = "MSOA Sweets Purchase Ratio (December vs Year)",
            frame  = FALSE)

#7 Energy density ratios + maps

#Energy density Dec & Apr vs Year.

year_density<- year_data %>%
  select(area_id, energy_density_year = energy_density)

dec_density<- dec_data %>%
  select(area_id, energy_density_dec = energy_density)

apr_density<- apr_data %>%
  select(area_id, energy_density_apr = energy_density)

density_ratios<- year_density %>%
  left_join(dec_density, by = "area_id") %>%
  left_join(apr_density, by = "area_id") %>%
  mutate(
    ratio_dec = energy_density_dec / energy_density_year,
    ratio_apr = energy_density_apr / energy_density_year
  ) %>%
  mutate(
    ratio_dec = ifelse(is.infinite(ratio_dec) | is.nan(ratio_dec), NA, ratio_dec),
    ratio_apr = ifelse(is.infinite(ratio_apr) | is.nan(ratio_apr), NA, ratio_apr)
  )

# Join with MSOAs
msoas_energy <- msoas %>%
  left_join(density_ratios, by = c("msoa_code" = "area_id"))

# Reusing same breaks
brks_energy <- c(0, 0.75, 0.9, 1, 1.1, 1.25, 1.5, Inf)

tmap_mode("plot")

tm_shape(msoas_energy) +
  tm_polygons("ratio_dec",
              palette = "RdBu",
              breaks  = brks_energy,
              title   = "Energy Density Ratio (Dec / Year)") +
  tm_layout(title = "MSOA Energy Density: December vs Year",
            frame  = FALSE)

tm_shape(msoas_energy) +
  tm_polygons("ratio_apr",
              palette = "RdBu",
              breaks  = brks_energy,
              title   = "Energy Density Ratio (Apr / Year)") +
  tm_layout(title = "MSOA Energy Density: April vs Year",
            frame  = FALSE)


#9 Simple regressions: December f_spirits & f_wine vs age


regression_data<- dec_data %>%
  select(f_spirits, f_wine, age_0_17, age_18_64, age_65_plus = `age_65+`)

summary(regression_data)

model_spirits<-lm(f_spirits ~ age_0_17 + age_18_64 + age_65_plus,
                    data = regression_data)
summary(model_spirits)
vif(model_spirits)

model_wine<-lm(f_wine ~ age_0_17 + age_18_64 + age_65_plus,
                 data = regression_data)
summary(model_wine)
vif(model_wine)


#morans I
# 10. Moran’s I: spatial clustering


# Combine your category-ratio map with the energy-density ratios so we can use ONE sf object for all Moran's I tests
msoas_all<- msoa_ratio_map %>%
  left_join(density_ratios, by = c("msoa_code" = "area_id"))

glimpse(msoas_all)

# 10.1 Build neighbour list (which MSOAs touch each other?)
nb_msoa <- poly2nb(msoas_all, queen = TRUE, snap = 1e-6)
nb_msoa

# Convert neighbours into spatial weights (row-standardised)
listw_msoa <- nb2listw(nb_msoa, style = "W", zero.policy = TRUE)

# 10.2 Global Moran's I: December energy-density ratio (ratio_dec)
moran_dec_energy <- moran.test(
  msoas_all$ratio_dec,
  listw = listw_msoa,
  zero.policy = TRUE
)
moran_dec_energy

# 10.3 Global Moran's I: April energy-density ratio (ratio_apr)
moran_apr_energy <- moran.test(
  msoas_all$ratio_apr,
  listw = listw_msoa,
  zero.policy = TRUE,
  na.action= na.exclude
)
moran_apr_energy

# 10.4 Global Moran's I: December wine ratio (f_wine_dec_ratio)
moran_dec_wine <- moran.test(
  msoas_all$f_wine_dec_ratio,
  listw = listw_msoa,
  zero.policy = TRUE
)
moran_dec_wine

# 10.5 Global Moran's I: December spirits ratio (f_spirits_dec_ratio)
moran_dec_spirits <- moran.test(
  msoas_all$f_spirits_dec_ratio,
  listw = listw_msoa,
  zero.policy = TRUE
)
moran_dec_spirits



#9 Socio -Economc factors
# Load MSOA socio-demographic data


library(readr)
library(dplyr)

# 1. Read the MSOA socio-economic file
msoa_info <- read.csv(
  "data/msoa-data.csv",
  check.names = FALSE,      # keep original long column names
  fileEncoding = "latin1"   # or "Windows-1252" if you prefer
)
view(msoa_info)
dim(msoa_info)      # should give 984 207
colnames(msoa_info)
msoa_info <- msoa_info %>% janitor::clean_names()

colnames(msoa_info)[1:30]
colnames(msoa_info)[140:180]

library(dplyr)


msoa_clean <- msoa_info %>%
  transmute(
    # ID + label
    area_id   = middle_super_output_area,
    msoa_name = msoa_name,
    
    # Income
    income_mean   = household_income_estimates_2011_12_total_mean_annual_household_income,
    income_median = household_income_estimates_2011_12_total_median_annual_household_income,
    
    # Deprivation & obesity
    income_depriv = income_deprivation_2010_percent_living_in_income_deprived_households_reliant_on_means_tested_benefit,
    obesity_child = obesity_percent_of_measured_children_in_year_6_who_were_classified_as_obese_2009_10_2011_12,
    obesity_adult = obesity_percentage_of_the_population_aged_16_with_a_bmi_of_30_modelled_estimate_2006_2008,
    
    # Population density from the atlas 
    pop_density   = population_density_persons_per_hectare_2012
  ) %>%
  # converting to numeric format
  mutate(
    across(
      c(income_mean, income_median, income_depriv,
        obesity_child, obesity_adult, pop_density),
      as.numeric
    )
  )
view(msoa_clean)



# Demographic columns from Tesco yearly file
demo_from_tesco <- year_data %>%
  select(
    area_id,
    male, female,
    age_0_17, age_18_64, `age_65+`,
    population, avg_age,
    people_per_sq_km
  ) %>%
  mutate(
    age_65_plus = `age_65+`,
    male_ratio  = male / (male + female)
  ) %>%
  select(-`age_65+`)   # remove old name, keep age_65_plus

analysis_full <- density_ratios %>%
  left_join(demo_from_tesco, by = "area_id") %>%
  left_join(msoa_clean,       by = "area_id")

glimpse(analysis_full)

#a) Does income explain higher energy-dense purchases at Christmas?
model_income_dec <- lm(
  ratio_dec ~ income_mean + income_depriv + pop_density,
  data = analysis_full
)
summary(model_income_dec)



