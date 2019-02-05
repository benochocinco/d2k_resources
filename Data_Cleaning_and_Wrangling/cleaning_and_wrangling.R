# Load packages with load_dependencies.R script
source("load_dependencies.R")

# Create clean_consumption() function
clean_consumption <- function(consumption_file){
  
  # Read in alcohol consumption data we found here: https://data.worldbank.org/indicator/SH.ALC.PCAP.LI
  consumption <- read.csv(consumption_file, header = TRUE, stringsAsFactors = FALSE, skip = 3)
  
  # Remove empty columns
  consumption <- consumption[,colSums(is.na(consumption))<nrow(consumption)]
  
  # Clean up column names, remove leading X in year columns
  colnames(consumption) <- gsub("^(X{1})(\\d{4})$", "\\2",  colnames(consumption))
  
  # Grab most recent year column
  most_recent <- names(consumption)[ncol(consumption)]
  
  # Rename country column and keep only country column and most recent year column
  consumption <- consumption %>%
    rename(country = Country.Name, most_recent = most_recent) %>%
    select(country, most_recent)
  
  # Clean up country names to optimize matching -----------------------------------------------
  
  # Remove parentheses, contents within, and space before
  consumption$country <- gsub("\\s*\\([^\\)]+\\)", "", consumption$country)
  
  # Remove anything after a comma
  consumption$country <- gsub("(.*),.*", "\\1", consumption$country)
  
  # Refactor country names for join with life expectancy data
  consumption$country <- gsub("&", "and", consumption$country)
  
  # Return cleaned data
  return(consumption)
}


# Create clean_drinks() function
clean_drinks <- function(drinks_file, consumption_file){
  
  # Read in drinks data set
  drinks <- read.csv(drinks_file, header = TRUE, stringsAsFactors = FALSE)
  
  # Replace ? values with NA
  drinks[drinks == "?"] <- NA
  
  # Convert columns from character to numeric
  num_cols <- c("beer_servings", "spirit_servings", "wine_servings", "total_litres_of_pure_alcohol")
  drinks[num_cols] <- sapply(drinks[num_cols], as.numeric)
  
  # Refactor country names for join with life expectancy data
  drinks$country <- gsub("&", "and", drinks$country)
  
  # Create standard drink's alcohol content variable
  std_drink <- 0.017744
  
  # Calculate liters of alcohol per country
  # https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption/what-standard-drink
  drinks$total_litres_of_pure_alcohol <- (drinks$beer_servings + drinks$spirit_servings + drinks$wine_servings)*std_drink
  
  # Get supplementary data on alcohol consumption ----------------------------------------------
  
  # Call clean_consumption() function
  consumption <- clean_consumption(consumption_file)

  # Replace missing data from supplementary consumption data
  drinks <- drinks %>%
    left_join(consumption, by = "country") %>%
    mutate(total_litres_of_pure_alcohol = coalesce(total_litres_of_pure_alcohol, most_recent)) %>%
    select(-most_recent)
    
  # Engineer features --------------------------------------------------------------------------

  # Calculate healthy consumption threshold for annual consumption
  # https://www.niaaa.nih.gov/alcohol-health/overview-alcohol-consumption/moderate-binge-drinking

  threshold <- std_drink*7*52
  
  drinks <- drinks %>% 
    mutate(unhealthy_consumption = case_when(total_litres_of_pure_alcohol >= threshold ~ 1,
                                             total_litres_of_pure_alcohol < threshold ~ 0))
  
  # Return data
  return(drinks)
}


# Create clean_life_exp() function
clean_life_exp <- function(life_exp_file){
  
  # Read in life expectancy data set
  life_exp <- read.csv(life_exp_file, header = TRUE, stringsAsFactors = FALSE)
  
  # Clean and wrangle data in one dplyr pipe! -------------------------------------------------------
  life_exp <- life_exp %>%
    # Keep only most recent year's data
    filter(YearDisplay == max(life_exp$YearCode)) %>%
    # Keep only the rows with life expectancy data for "Both sexes"
    filter(SexDisplay == "Both sexes") %>%
    # Select relevant columns to keep 
    select(GhoDisplay, YearDisplay, WorldBankIncomeGroupDisplay, CountryDisplay, SexDisplay, Numeric) %>%
    # Fill in missing values with NA
    mutate(WorldBankIncomeGroupDisplay = replace(WorldBankIncomeGroupDisplay, WorldBankIncomeGroupDisplay == "", NA)) %>%
    # Arrange alphabetically so NA values are below for each category to utilize fill function 
    arrange(CountryDisplay, GhoDisplay) %>%
    fill(WorldBankIncomeGroupDisplay, .direction = "down") %>%
    # Keep life expectancy measure of interest
    filter(GhoDisplay == "Life expectancy at birth (years)") %>%
    # Select relevant columns and rename them
    select(CountryDisplay, WorldBankIncomeGroupDisplay, Numeric) %>%
    rename(country = CountryDisplay, income = WorldBankIncomeGroupDisplay, life_expectancy = Numeric)

  # Replace underscores with dashes
  life_exp$country <- gsub("_", "-", life_exp$country)
  
  # Remove parentheses, contents within, and space before
  life_exp$country <- gsub("\\s*\\([^\\)]+\\)", "", life_exp$country)
  
  # Replace Saint with St.
  life_exp_factor$country <- gsub("Saint", "St.", life_exp_factor$country)

  # Hard-code country names changes to match drinks dataset, unfortunately this is unavoidable in this context
  life_exp$country[life_exp$country == "Bosnia and Herzegovina"] <- "Bosnia-Herzegovina"
  life_exp$country[life_exp$country == "Brunei Darussalam"] <- "Brunei"
  life_exp$country[life_exp$country == "Côte d'Ivoire"] <- "Cote d'Ivoire"
  life_exp$country[life_exp$country == "Democratic People's Republic of Korea"] <- "North Korea"
  life_exp$country[life_exp$country == "Democratic Republic of the Congo"] <- "DR Congo"
  life_exp$country[life_exp$country == "Lao People's Democratic Republic"] <- "Laos"
  life_exp$country[life_exp$country == "Republic of Korea"] <- "South Korea"
  life_exp$country[life_exp$country == "Republic of Moldova"] <- "Moldova"
  life_exp$country[life_exp$country == "Syrian Arab Republic"] <- "Syria"
  life_exp$country[life_exp$country == "The former Yugoslav republic of Macedonia"] <- "Macedonia"
  life_exp$country[life_exp$country == "United Kingdom of Great Britain and Northern Ireland"] <- "United Kingdom"
  life_exp$country[life_exp$country == "United Republic of Tanzania"] <- "Tanzania"
  life_exp$country[life_exp$country == "United States of America"] <- "USA"
  life_exp$country[life_exp$country == "Viet Nam"] <- "Vietnam"
  
  # Return data
  return(life_exp)
}

# Create load_factor_data function to load data with factor-type categorical variable
load_factor_data <- function(drinks_file, consumption_file, life_exp_file){
  
  # Call clean_drinks() function
  drinks <- clean_drinks(drinks_file, consumption_file)
  
  # Call clean_life_exp() function
  life_exp_rshp <- clean_life_exp(life_exp_file)
  
  # Perform left join to merge datasets
  model_data_factor <- left_join(life_exp_factor, drinks, by = "country")
  
  # Remove row without alcohol data (South Sudan)
  model_data_factor <- model_data_factor[!is.na(model_data_factor$total_litres_of_pure_alcohol),]
  
  # Return data
  return(model_data_factor)
}

# Create load_dummy_data function to load data with dummy categorical variables
load_dummy_data <- function(drinks_file, consumption_file, life_exp_file){
  
  # Load factor dataset to avoid repeating code
  model_data_factor <- load_factor_data(drinks_file, consumption_file, life_exp_file)
  
  # Create dummy variables
  dummys <- model.matrix(~model_data_factor$income)

  # Remove income variable and add dummy variables instead
  model_data_dummy <- data.frame(model_data_factor[, !(names(model_data_factor) %in% "income")], dummys[,-1])
  
  # Return data
  return(model_data_dummy)
}


