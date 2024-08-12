# THIS SCRIPT IS USED FOR TESTING SPATIAL AUTOCORRELATION, # 
# RUNNING OLS REGRESSION, AND RUNNING SAR REGRESSION. DATA #
#  CREATION WAS COMPLETED IN THE SCRIPT NAMED tidycensus   #

# Load necessary libraries
library(spdep)
library(sf)
library(spatialreg)

### SET UP 

# Import final dataset generated in tidycensus script
sf <- final_joined_data

# Project dataset in NAD 83 UTM Zone 18N (units are in meters)
sf <- st_transform(sf, 26918) 

# MAKE SELECTION HERE: Deal with nulls in Median Income 
#sf <- sf %>% mutate(medn_incm = replace_na(medn_incm, 0)) # Replace NAs in income with 0s
sf <- sf %>% filter(!is.na(medn_incm)) # Drop 0s 

# MAKE SELECTION HERE: Deal with BGs with zero brownfields, to remove them or no?
sf <- sf %>% filter(BrwnFl_500 > 0)
sf <- sf %>% filter(BrwnFl_200 > 0)
sf <- sf %>% filter(BrwnFl_1k > 0)

#################################################################################
### CREATE SPATIAL WEIGHTS AND TEST MORANS I

## SELECT WEIGHTING APPROACH AND CREATE MATRIX 

# Get centroid coordinates
coords <- st_centroid(sf) %>% st_coordinates()

# # Queen contiguity (neighbors sharing a border or vertex)
# nb <- poly2nb(sf)
# weights <- nb2listw(nb, style = "W") # Row standardized

# # Inverse Distance Weighting
# nb <- dnearneigh(coords, d1 = 0, d2 = 100000)  # Adjust d2 for max threshold
# weights <- nb2listw(nb, glist = 1 / unlist(nbdists(nb, coords)), style = "W")

# # Fixed Distance Bands
# nb <- dnearneigh(coords, d1 = 0, d2 = 2000)  # Set d2 to a distance that covers gaps
# weights <- nb2listw(nb, style = "W")
# 
# K-Nearest Neighbors
nb <- knn2nb(knearneigh(coords, k = 4))  # Adjust k for the number of neighbors
weights <- nb2listw(nb, style = "W")

## TEST MORANS I 

#  Moran's I to test for spatial autocorrelation
moran.test(sf$BrwnFl_500, weights)
moran.plot(sf$BrwnFl_500, 
           listw=weights,  
           xlab = 'X', 
           ylab = 'Y')

#################################################################################
### SELECT AND RUN OLS AND SAR MODEL

## 500
# Set regression model formula
model <- BrwnFl_500 ~ insur_rate + medn_incm + educ_rate + PoC + EJStress

# Check Linearity 
scatter.smooth(sf$BrwnFl_500 ~ sf$insur_rate)
scatter.smooth(sf$BrwnFl_500 ~ sf$medn_incm)
scatter.smooth(sf$BrwnFl_500 ~ sf$educ_rate)
scatter.smooth(sf$BrwnFl_500 ~ sf$PoC)
scatter.smooth(sf$BrwnFl_500 ~ sf$EJStress)

## 200
# Set regression model formula
model <- BrwnFl_200 ~ insur_rate + medn_incm + educ_rate + PoC + EJStress

# Check Linearity 
scatter.smooth(sf$BrwnFl_200 ~ sf$insur_rate)
scatter.smooth(sf$BrwnFl_200 ~ sf$medn_incm)
scatter.smooth(sf$BrwnFl_200 ~ sf$educ_rate)
scatter.smooth(sf$BrwnFl_200 ~ sf$PoC)
scatter.smooth(sf$BrwnFl_200 ~ sf$EJStress)

## 1,000
# Set regression model formula
model <- BrwnFl_1k ~ insur_rate + medn_incm + educ_rate + PoC + EJStress

# Check Linearity 
scatter.smooth(sf$BrwnFl_1k ~ sf$insur_rate)
scatter.smooth(sf$BrwnFl_1k ~ sf$medn_incm)
scatter.smooth(sf$BrwnFl_1k ~ sf$educ_rate)
scatter.smooth(sf$BrwnFl_1k ~ sf$PoC)
scatter.smooth(sf$BrwnFl_1k ~ sf$EJStress)


## Running Models

# Lagrange Multiplier Test for Spatial Lag and Spatial Error models
lm_tests <- lm.RStests(lm(model, data = sf), listw = weights, test = "all")
print(lm_tests)

# Standard OLS Model
lm_model <- lm(model, data = sf)
summary(lm_model)

# Spatial Autoregressive Lag Model (SAR) (based on results from LM test)
sar_model <- lagsarlm(model, data = sf, listw = weights)
summary(sar_model)
err_model <- errorsarlm(model, data = sf, listw = weights)
summary(err_model)



