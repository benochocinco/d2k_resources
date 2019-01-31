# Load packages with load_dependencies.R script
source("load_dependencies.R")

# Create clean_drinks() function
clean_drinks <- function(){
  
  # Read in drinks data set
  drinks <- read.csv("data/drinks.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Refactor country names for join with life expectancy data
  drinks$country <- gsub("&", "and", drinks$country)
  
  # Replace ? values with NA
  drinks[drinks == "?"] <- NA
  
  # Convert columns from character to numeric
  drinks$beer_servings <- as.numeric(drinks$beer_servings)
  drinks$spirit_servings <- as.numeric(drinks$spirit_servings)
  drinks$wine_servings <- as.numeric(drinks$wine_servings)
  
  
  # Calculate liters of alcohol per country
  # https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption/what-standard-drink
  drinks$total_litres_of_pure_alcohol <- (drinks$beer_servings + drinks$spirit_servings + drinks$wine_servings)*0.017744
  
  # Replace missing data from research
  # https://www.who.int/gho/countries/en/
  drinks$total_litres_of_pure_alcohol[drinks$country == "Bahamas"] <- 6.9
  drinks$total_litres_of_pure_alcohol[drinks$country == "Denmark"] <- 11.4
  drinks$total_litres_of_pure_alcohol[drinks$country == "Macedonia"] <- 6.7
  
  # Engineer feature to create indicator variable of whether consumption is above or below 7 drinks per week
  # https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption/moderate-binge-drinking
  # Healthy consumption threshold = 0.017744 x 7 x 52 = 6.458816
  drinks$unhealthy_consumption <- 0
  drinks$unhealthy_consumption[drinks$total_litres_of_pure_alcohol > 6.458816] <- 1
  
  # Return data
  return(drinks)
}


# Create clean_life_exp() function
clean_life_exp <- function(){
  
  # Read in life expectancy data set
  life_exp <- read.csv("data/lifeexpectancy.csv", header = TRUE, stringsAsFactors = FALSE)
  
  # Keep only most recent measures from year 2013
  life_exp <- life_exp[life_exp$YearCode == 2013,]
  
  # Initialize vector of all unique countries
  countries <- unique(life_exp$CountryDisplay)
  
  # Initialize reshaped data frame with columns for each measure of life expectancy 
  life_exp_rshp <- data.frame(matrix(nrow = length(countries), ncol = 11))
  
  # Initialize and assign column names
  ler_names <- c("country", "male_birth", "male_healthy", "male_60", 
                 "female_birth", "female_healthy", "female_60",
                 "both_birth", "both_healthy", "both_60", "income")
  colnames(life_exp_rshp) <- ler_names
  
  # Iterate through countries and transform data to create a tidy dataset with one row for each country and one column for each measure of life expectancy 
  for (i in 1:length(countries)){
    # Assign countries to "country" column
    life_exp_rshp$country[i] <- countries[i]
    # Assign male life expectancy values for different lifetime measures
    life_exp_rshp$male_birth[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Male" & life_exp$GhoDisplay == "Life expectancy at birth (years)"]
    life_exp_rshp$male_healthy[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Male" & life_exp$GhoDisplay == "Healthy life expectancy (HALE) at birth (years)"]
    life_exp_rshp$male_60[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Male" & life_exp$GhoDisplay == "Life expectancy at age 60 (years)"]
    # Assign female life expectancy values for different lifetime measures
    life_exp_rshp$female_birth[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Female" & life_exp$GhoDisplay == "Life expectancy at birth (years)"]
    life_exp_rshp$female_healthy[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Female" & life_exp$GhoDisplay == "Healthy life expectancy (HALE) at birth (years)"]
    life_exp_rshp$female_60[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Female" & life_exp$GhoDisplay == "Life expectancy at age 60 (years)"]
    # Assign life expectancy values for both sexes for different lifetime measures
    life_exp_rshp$both_birth[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Both sexes" & life_exp$GhoDisplay == "Life expectancy at birth (years)"]
    life_exp_rshp$both_healthy[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Both sexes" & life_exp$GhoDisplay == "Healthy life expectancy (HALE) at birth (years)"]
    life_exp_rshp$both_60[i] <- life_exp$Numeric[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Both sexes" & life_exp$GhoDisplay == "Life expectancy at age 60 (years)"]
    # Assign income-level to country
    life_exp_rshp$income[i] <- life_exp$WorldBankIncomeGroupDisplay[life_exp$CountryDisplay == countries[i] & life_exp$SexDisplay == "Both sexes" & life_exp$GhoDisplay == "Healthy life expectancy (HALE) at birth (years)"]
  }
  
  ### Refactor country names to join with drinks dataset ###
  
  # Replace underscores with dashes
  life_exp_rshp$country <- gsub("_", "-", life_exp_rshp$country)
  
  # Remove parentheses, contents within, and space before
  life_exp_rshp$country <- gsub("\\s*\\([^\\)]+\\)", "", life_exp_rshp$country)
  
  # Replace Saint with St.
  life_exp_rshp$country <- gsub("Saint", "St.", life_exp_rshp$country)

  # Hard-code country names to match drinks dataset, unfortunately this is unavoidable in this context
  life_exp_rshp$country[life_exp_rshp$country == "Bosnia and Herzegovina"] <- "Bosnia-Herzegovina"
  life_exp_rshp$country[life_exp_rshp$country == "Brunei Darussalam"] <- "Brunei"
  life_exp_rshp$country[life_exp_rshp$country == "Côte d'Ivoire"] <- "Cote d'Ivoire"
  life_exp_rshp$country[life_exp_rshp$country == "Democratic People's Republic of Korea"] <- "North Korea"
  life_exp_rshp$country[life_exp_rshp$country == "Democratic Republic of the Congo"] <- "DR Congo"
  life_exp_rshp$country[life_exp_rshp$country == "Lao People's Democratic Republic"] <- "Laos"
  life_exp_rshp$country[life_exp_rshp$country == "Republic of Korea"] <- "South Korea"
  life_exp_rshp$country[life_exp_rshp$country == "Republic of Moldova"] <- "Moldova"
  life_exp_rshp$country[life_exp_rshp$country == "Syrian Arab Republic"] <- "Syria"
  life_exp_rshp$country[life_exp_rshp$country == "The former Yugoslav republic of Macedonia"] <- "Macedonia"
  life_exp_rshp$country[life_exp_rshp$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
  life_exp_rshp$country[life_exp_rshp$country == "United Republic of Tanzania"] <- "Tanzania"
  life_exp_rshp$country[life_exp_rshp$country == "United States of America"] <- "USA"
  life_exp_rshp$country[life_exp_rshp$country == "Viet Nam"] <- "Vietnam"
  
  # Return data
  return(life_exp_rshp)
}

# Create load_factor_data function to load data with factor-type categorical variable
load_factor_data <- function(){
  
  # Call clean_drinks() function
  drinks <- clean_drinks()
  
  # Call clean_life_exp() function
  life_exp_rshp <- clean_life_exp()
  
  # Perform left join to merge datasets
  model_data_factor <- left_join(life_exp_rshp, drinks, by = "country")
  
  # Remove row without alcohol data (South Sudan)
  model_data_factor <- model_data_factor[!is.na(model_data_factor$total_litres_of_pure_alcohol),]
  
  # Return data
  return(model_data_factor)
}

# Create load_dummy_data function to load data with dummy categorical variables
load_dummy_data <- function(){
  
  # Load factor dataset to avoid repeating code
  model_data_factor <- load_factor_data()
  
  # Create dummy variables
  dummys <- model.matrix(~model_data_factor$income)

  # Remove income variable and add dummy variables instead
  model_data_dummy <- data.frame(model_data_factor[, !(names(model_data_factor) %in% "income")], dummys[,-1])
  
  # Return data
  return(model_data_dummy)
}










