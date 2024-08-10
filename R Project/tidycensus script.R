
# Load necessary libraries
library(tidycensus)
library(tidyverse)
library(sf)
library(dplyr)
library(stringr)
library(ggplot2)

# Set Census API key (only needed once)
#census_api_key("api", install = TRUE)

# Check all variables to select most appropriate  
v22 <- load_variables(2022, "acs5", cache = TRUE)
hi <- v22 %>% filter(str_detect(name,"B27010")) # Health insurance 
inc <- v22 %>% filter(str_detect(name, "B19013")) # Income
ed <- v22 %>% filter(str_detect(name, "B15003")) # Education
rc <- v22 %>% filter(str_detect(name, "B03002")) # Race


################################################################################
### GENERATE VARIABLES 

## HEALTH INSURANCE
# Health Insurance: Total population without health insurance
no_health_insurance <- get_acs(
  geography = "block group",
  variables = c(none_under19 = "B27010_017",
                none_19to34 = "B27010_033",
                none_35to64 = "B27010_050",
                none_65andover = "B27010_066"),
  state = "NJ",
  year = 2022,
  survey = "acs5",
  geometry = TRUE
) %>%  group_by(GEOID) %>% 
  summarize(total_no_health_insurance = sum(estimate)) 

# Health Insurance: Total population for whom health insurance status is determined
total_population_health_insurance <- get_acs(
  geography = "block group",
  variables = c(total = "B27010_001"),
  state = "NJ",
  year = 2022,
  survey = "acs5",
  geometry = FALSE
) 

# Percentage of the population with insurance
health_insur_percentage <- no_health_insurance %>%
  inner_join(total_population_health_insurance, by = "GEOID") %>%
  mutate(insur_rate = (estimate - total_no_health_insurance) / estimate) %>% 
  select(GEOID, geometry, insur_rate)



## MEDIAN INCOME 
# Median Household Income
median_income <- get_acs(
  geography = "block group",
  variables = c(med_income = "B19013_001"),
  state = "NJ",
  year = 2022,
  survey = "acs5",
  geometry = FALSE
) %>% select(GEOID, estimate) 

incm_max <- max(median_income$estimate, na.rm = TRUE)

median_income <- median_income %>% 
  group_by(GEOID) %>% 
  summarize(medn_incm = estimate/incm_max)  # INCOME IS MAX STANDARDIZED TO BE ON SAME SCALE AS OTHER VARS



## EDUCATION
# Population with Bachelor's Degree or Higher
bachelors_plus <- get_acs(
  geography = "block group",
  variables = c(bachelor = "B15003_022", 
                masters = "B15003_023", 
                prof = "B15003_024", 
                doctoral = "B15003_025"),
  state = "NJ",
  year = 2022,
  survey = "acs5",
  geometry = FALSE
) %>%
  group_by(GEOID) %>%
  summarize(bachelors_degree_or_higher = sum(estimate)) 

# Total population for bachelor's calculation
total_population_bachelors <- get_acs(
  geography = "block group",
  variables = c(total = "B15003_001"),
  state = "NJ",
  year = 2022,
  survey = "acs5",
  geometry = FALSE
)

# Percentage of the population with a bachelor's degree or higher
bachelors_plus_percentage <- bachelors_plus %>%
  inner_join(total_population_bachelors, by = "GEOID") %>%
  mutate(educ_rate = bachelors_degree_or_higher / estimate) %>% 
  select(GEOID, educ_rate)



## RACE
# Non-Hispanic White Population
non_hispanic_white <- get_acs(
  geography = "block group",
  variables = c(NonHspnc_White = "B03002_003"),
  state = "NJ",
  year = 2022,
  survey = "acs5",
  geometry = FALSE
)

# Total Population for Race/Ethnicity Calculation
total_population_race <- get_acs(
  geography = "block group",
  variables = c(total = "B03002_001"),
  state = "NJ",
  year = 2022,
  survey = "acs5",
  geometry = FALSE
)

# Calculate percentage of population that is NOT non-Hispanic White
non_white_percentage <- non_hispanic_white %>%
  inner_join(total_population_race, by = "GEOID") %>%
  mutate(PoC = (estimate.y - estimate.x) / estimate.y) %>%
  select(GEOID, PoC)



################################################################################
## MERGE
# Merge all datasets
final_data <- health_insur_percentage %>%
  left_join(median_income, by = "GEOID") %>%
  left_join(bachelors_plus_percentage, by = "GEOID") %>%
  left_join(non_white_percentage, by = "GEOID")

# Inspect the final dataset
head(final_data)


################################################################################
### VISUALIZE 

# Insurance
ggplot(final_data) +
  geom_sf(aes(fill = insur_rate)) +
  #scale_fill_gradient(low = "white", high = "black") +  # Grayscale gradient
  scale_fill_viridis_c(option = "A") +  # Viridis color scale
  theme_minimal() +
  labs(title = "New Jersey (2022)",
       fill = "Insurance Rate")


# Median Household Income
ggplot(final_data) +
  geom_sf(aes(fill = medn_incm)) +
  #scale_fill_gradient(low = "white", high = "black") +  # Grayscale gradient
  scale_fill_viridis_c(option = "B") +  # Viridis color scale
  theme_minimal() +
  labs(title = "New Jersey (2022)",
       fill = "Medn Hshld Income")

# Education Rate
ggplot(final_data) +
  geom_sf(aes(fill = educ_rate)) +
  #scale_fill_gradient(low = "white", high = "black") +  # Grayscale gradient
  scale_fill_viridis_c(option = "C") +  # Viridis color scale
  theme_minimal() +
  labs(title = "New Jersey (2022)",
       fill = "Education Bachelors or Above")

# People of Color
ggplot(final_data) +
  geom_sf(aes(fill = PoC)) +
  #scale_fill_gradient(low = "white", high = "black") +  # Grayscale gradient
  scale_fill_viridis_c(option = "D") +  # Viridis color scale
  theme_minimal() +
  labs(title = "New Jersey (2022)",
       fill = "Percentage of People of Color")

################################################################################
### EXPORT SHAPEFILE

# Export the combined data as a shapefile
st_write(final_data, "nj_insurance_income_education_race_2022.shp")
