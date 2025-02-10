# Script by ChatGPT

library(httr)
library(jsonlite)
library(lubridate)

# Function to get public holidays from an API
get_portugal_holidays <- function(year) {
  url <- paste0("https://date.nager.at/api/v3/PublicHolidays/", year, "/PT")
  response <- GET(url)
  
  if (status_code(response) == 200) {
    holidays <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(as.Date(holidays$date))
  } else {
    stop("Failed to retrieve holidays. Please check your internet connection or API availability.")
  }
}

# Function to find the next Wednesday that is not a holiday
calendar_nextBusinessWednesday <- function(start_date = Sys.Date()) {
  year <- year(start_date)
  holidays <- get_portugal_holidays(year)
  
  # Find the next Wednesday
  next_wed <- start_date + (4 - wday(start_date) + 7) %% 7
  
  # If next Wednesday is a holiday, keep searching
  while (next_wed %in% holidays) {
    next_wed <- next_wed + 7  # Move to the next Wednesday
    
    # If we cross into a new year, update holidays
    if (year(next_wed) != year) {
      year <- year(next_wed)
      holidays <- get_portugal_holidays(year)
    }
  }
  
  return(next_wed)
}

