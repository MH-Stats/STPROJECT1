#install.packages(c("httr","jsonlite"))
#install.packages("tidycensus")

# Accessed via get_acs() function
#library(tidycensus)
library(httr)
library(jsonlite)
library(tibble)
library(dplyr)
library(ggplot2)



# Helper function to process API response
process_api_response <- function(api_response) {
  # Check if the response was successful
  if (api_response$status_code != 200) {
    stop("Error: API request failed with status code ", api_response$status_code)
  }
  
  # Convert raw content to character string
  content_string <- rawToChar(api_response$content)
  
  # Parse JSON data
  parsed_data <- fromJSON(content_string)
  
  # First row contains column names, the rest is the data
  column_names <- parsed_data[1, ]
  data_rows <- parsed_data[-1, ]
  
  # Convert to tibble and set column names
  tibble_data <- as_tibble(data_rows)
  colnames(tibble_data) <- column_names
  
  return(tibble_data)
}

# 2. Function to validate the year
validate_year <- function(year) {
  cat("API year is :", year)
  if (year < 2010 || year > 2022) {
    stop("Invalid year. Year must be between 2010 and 2022.")
  }
}

# 3. Function to validate and process numeric variables
process_numeric_vars <- function(numeric_vars = c("AGEP", "PWGTP")) {
  valid_numeric_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", "JWDP", "JWMNP", "PWGTP")
  
  # Check PWGTP is always included
  if (!"PWGTP" %in% numeric_vars) {
    numeric_vars <- c(numeric_vars, "PWGTP")
  }
  
  # Validate numeric variables
  if (!all(numeric_vars %in% valid_numeric_vars)) {
    stop("Invalid numeric variables. Valid options: AGEP, GASP, GRPIP, JWAP, JWDP, JWMNP, PWGTP.")
  }
  
  # Check at least one numeric variable other than PWGTP
  if (!any(numeric_vars %in% valid_numeric_vars[valid_numeric_vars != "PWGTP"])) {
    stop("At least one numeric variable other than PWGTP must be returned.")
  }
} 
  ##### TODO############
  
  

process_categorical_vars <- function(categorical_vars = c("SEX")) {
  valid_categorical_vars <- c("FER", "HHL", "HISPEED", "JWAP", "JWDP", "JWTRNS", "SCH", "SCHL", "SEX")
  
  # Validate categorical variables
  if (!all(categorical_vars %in% valid_categorical_vars)) {
    stop("Invalid categorical variables. Valid options: FER, HHL, HISPEED, JWAP, JWDP, JWTRNS, SCH, SCHL, SEX.")
  }
  
  # Convert to factors
  for (var in categorical_vars) {
    categorical_vars[[var]] <- as.factor(var)
  }
  
  return(categorical_vars)
}



##Check that the value specified by the user is one of the above values


# 5. Main function: Query the API
query_census_pums <- function(
    year = 2023,
    numeric_vars = c("GASP", "PWGTP"),
    categorical_vars = c("SEX"),
    geography_level = "All",
    geography_subset = NULL
) {
  # Validate the year
  validate_year(year)
  
  # Validate numeric variables
  numeric_vars <- process_numeric_vars(numeric_vars)
  
  # Validate categorical variables
  queryparams <- process_categorical_vars(categorical_vars)
  
  # Get API URL 
  base_url <- "https://api.census.gov/data"
  pathparam <- "acs/acs1/pums"  
  
  # Get the full URL
  url <- paste0(base_url, "/", year, "/", pathparam, "?get=", 
                paste(c(numeric_vars, categorical_vars), collapse = ","),"&SCHL=24")
  
  # Check geography subset and add it to API call
  if (!is.null(geography_subset)) {
    url <- paste0(url, "&for=", geography_level, ":", geography_subset)
  }
  print(url)
  
  # API call using httr::GET
  api_response <- GET(url)
  
  # Check if the request was successful
  if (http_error(api_response)) {
    stop("API request failed: ", status_code(api_response))
  }
  
  # Process the response into a tibble
  data <- process_api_response(api_response)
  
  # Return the data 
  return(data)
}
print(url)

# 6. Function for multiple years
query_multiple_years <- function(
    years,                       
    numeric_vars = c("AGEP", "PWGTP"),  
    categorical_vars = c("SEX"),        
    geography_level = "All",            
    geography_subset = NULL             
) {
  all_years_data <- list()
  
  # Loop through each year
  for (year in years) {
    # Check if the year is valid
    #validate_year(year)
    cat("\nyear:", year)
    # Validate the year.    
    if (year < 2010 || year > 2022) {
      print(paste("Skipping invalid year:", year))
      next  # Skip this iteration and continue with the next year
    }
    
    # Get data for the current year using the single year function
    yearly_data <- query_census_pums(
      year = year,
      numeric_vars = numeric_vars,
      categorical_vars = categorical_vars,
      geography_level = geography_level,
      geography_subset = geography_subset
    )
    
    # Add a year column 
    yearly_data$year <- year
    
    # Store the current year data in the list
    all_years_data[[as.character(year)]] <- yearly_data
  }
  
  # Combine all the  data into one dataset
  final_data <- bind_rows(all_years_data)
  
  # Return dataset
  return(final_data)
}




# Example for the single year 2022 with numeric and categorical variables
result <- query_census_pums(
  year = 2021,
  numeric_vars = c("AGEP", "PWGTP"),
  categorical_vars = c("SEX", "HISPEED"),
  geography_level = "state",
  geography_subset = "10"
)

# View the results
print ("end of function")
print(result)

# Example of multiple years of data
multi_year_result <- query_multiple_years(
  years = c( 2016, 2017, 2018, 2024),    
  numeric_vars = c("AGEP", "PWGTP"),  
  categorical_vars = c("SEX", "HISPEED"),  
  geography_level = "state",  
  geography_subset = "10"     
)



# View the result
print(multi_year_result)

#Run these in your console
# After processing your tibble
class(multi_year_result) <- c("census", class(multi_year_result))


# Define custom summary function for the 'census' class
summary.census <- function(census_data, 
                           numeric_vars = NULL, 
                           categorical_vars = NULL) {
  
  # Ensure PWGTP exists and is numeric
  if (!"PWGTP" %in% names(census_data)) {
    stop("Weight variable 'PWGTP' is missing from the dataset.")
  }
  
  # Separate numeric and categorical columns from the data
  if (is.null(numeric_vars)) {
    numeric_vars <- names(census_data)[sapply(census_data, is.numeric) & names(census_data) != "PWGTP"]
  }
  
  if (is.null(categorical_vars)) {
    categorical_vars <- names(census_data)[sapply(census_data, is.factor)]
  }
  
  # Weighted mean and standard deviation calculations
  summarize_numeric <- function(var_name, data) {
    numeric_vector <- as.numeric(data[[var_name]])
    weight_vector <- as.numeric(data[["PWGTP"]])
    
    # Check for missing values in the weight vector
    if (any(is.na(weight_vector))) {
      stop("Missing values found in the weight variable 'PWGTP'.")
    }
    
    # Calculate weighted mean
    weighted_mean <- sum(numeric_vector * weight_vector, na.rm = TRUE) / sum(weight_vector, na.rm = TRUE)
    
    # Calculate weighted standard deviation
    weighted_var <- sum(numeric_vector^2 * weight_vector, na.rm = TRUE) / sum(weight_vector, na.rm = TRUE)
    weighted_sd <- sqrt(weighted_var - weighted_mean^2)
    
    return(list(mean = weighted_mean, sd = weighted_sd))
  }
  
  # Initialize a list to store the summary results
  summary_list <- list()
  
  # Summarize numeric variables
  for (var in numeric_vars) {
    summary_list[[var]] <- summarize_numeric(var, census_data)
  }
  
  # Summarize categorical variables (counts)
  for (var in categorical_vars) {
    summary_list[[var]] <- table(census_data[[var]], useNA = "ifany")
  }
  
  return(summary_list)
}

# Example: Use the summary function on your 'result' tibble
summary_result <- summary.census(result, numeric_vars = c("AGEP"), categorical_vars = c("SEX"))
print(summary_result)


########## TODO


plot.census <- function(tibble_data, cat_var, num_var) {
  # Create the weighted boxplot
  p <- ggplot(tibble_data, aes(x = get(cat_var), y = get(num_var), weight = PWGTP)) +
    geom_boxplot()# +
   # labs(x = cat_var, y = num_var) +
    #theme_minimal()
  
  print(p)
}



# Plot example
plot.census(census_data, cat_var = "SEX", num_var = "AGEP")


