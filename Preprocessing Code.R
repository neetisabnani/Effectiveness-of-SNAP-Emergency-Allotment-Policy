
# Libraries ---------------------------------------------------------------

#Loading Libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(fixest)
library(ggfixest)
library(bacondecomp)
getwd()

# Loading Data Files ------------------------------------------------------

file2 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY23.xlsx"
file3 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY22.xlsx"
file4 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY21.xlsx"
file5 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY20.xlsx"
file6 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY19.xls"
file7 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY18.xls"
file8 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY17.xls"
file9 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY16.xls"
file10 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY15.xls"
file11 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/FY14.xls"
file12 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/Household Pulse Survey_2020-2023.xlsx"
file13 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/Household Pulse Survey_2024.xlsx"
file14 <- "/Users/neetisabnani/Desktop/R Class/SNAP Project/Final Project/Raw Data/HPS_2020-2023.csv"


# Load and Clean Control Data ---------------------------------------------

#Income data

# Set your working directory to the new path
setwd('/Users/Mishu/Desktop/Spring 2025/R/Final Project_May8 /SNAP Project/Final Report /Control Data Cleanup/Clean Data')

getwd()

# Set the paths to income files
file_2020 <- '/Users/Mishu/Desktop/Spring 2025/R/Final Project_May8 /SNAP Project/Final Report /Control Data Cleanup/Raw Data/Household Income 2020.xlsx'
file_2021 <- '/Users/Mishu/Desktop/Spring 2025/R/Final Project_May8 /SNAP Project/Final Report /Control Data Cleanup/Raw Data/Household Income 2021.xlsx'
file_2022 <- '/Users/Mishu/Desktop/Spring 2025/R/Final Project_May8 /SNAP Project/Final Report /Control Data Cleanup/Raw Data/Household Income 2022.xlsx'
file_2023 <- '/Users/Mishu/Desktop/Spring 2025/R/Final Project_May8 /SNAP Project/Final Report /Control Data Cleanup/Raw Data/Household Income 2023.xlsx'

# Verify that the files exist
file.exists(file_2020)
file.exists(file_2021)
file.exists(file_2022)
file.exists(file_2023)

headers <- read_excel(file_2020, sheet = "Data", n_max = 1, col_names = FALSE)

#Manually extract every second column (these are the state names)
state_names <- headers %>% select(seq(2, ncol(headers), by = 2))
state_names <- as.character(state_names[1, ])

#Read the data and skip the first two rows now
test_data <- read_excel(file_2020, sheet = "Data", skip = 2, col_names = FALSE)

# Extract only the income columns (every 2nd column, same as headers)
income_data <- test_data %>% select(seq(2, ncol(test_data), by = 2))

#Set the proper state names
colnames(income_data) <- state_names

# Display the first few rows to verify
head(income_data)

#Pivot into long format
income_2020 <- income_data %>%
  pivot_longer(cols = everything(),
               names_to = "State_Name",
               values_to = "Median_Household_Income")

#Add the Year column
income_2020 <- income_2020 %>% mutate(Year = 2020)

#Convert the income to numeric and remove commas
income_2020$Median_Household_Income <- as.numeric(gsub(",", "", income_2020$Median_Household_Income))

#Display the first few rows to confirm
head(income_2020)
str(income_2020)

clean_income_data <- function(file_path, year) {
  # Read the headers separately to extract state names
  headers <- read_excel(file_path, sheet = "Data", n_max = 1, col_names = FALSE)
  state_names <- headers %>% select(seq(2, ncol(headers), by = 2))
  state_names <- as.character(state_names[1, ])
  
  # Read the main data, skipping the first two rows
  test_data <- read_excel(file_path, sheet = "Data", skip = 2, col_names = FALSE)
  
  # Extract only the income columns (every 2nd column, same as headers)
  income_data <- test_data %>% select(seq(2, ncol(test_data), by = 2))
  
  # Set the proper state names
  colnames(income_data) <- state_names
  
  # Pivot into long format
  income_data <- income_data %>%
    pivot_longer(cols = everything(),
                 names_to = "State_Name",
                 values_to = "Median_Household_Income")
  
  # Add the Year column
  income_data <- income_data %>% mutate(Year = year)
  
  # Convert the income to numeric and remove commas
  income_data$Median_Household_Income <- as.numeric(gsub(",", "", income_data$Median_Household_Income))
  
  # Return the cleaned dataset
  return(income_data)
}

# Load all years
income_2021 <- clean_income_data(file_2021, 2021)
income_2022 <- clean_income_data(file_2022, 2022)
income_2023 <- clean_income_data(file_2023, 2023)

# Check structure and head for each year
str(income_2021)
str(income_2022)
str(income_2023)

head(income_2021)
head(income_2022)
head(income_2023)

# Combine all the years into one DataFrame
income_all_years <- bind_rows(income_2020, income_2021, income_2022, income_2023)

# Final check
head(income_all_years)
str(income_all_years)

# Define the file path
file_path_income <- '/Users/Mishu/Desktop/Spring 2025/R/Final Project_May8 /SNAP Project/Final Report /Control Data Cleanup/Clean Data/Income_Profiles_Cleaned.rds'

# Create the directory if it doesn't exist
dir.create(dirname(file_path_income), recursive = TRUE, showWarnings = FALSE)

# Save the dataset
saveRDS(income_all_years, file = file_path_income)

# Check if it saved correctly
file.exists(file_path_income)  # Should return TRUE


#Other variables collected by HPS survey for the years 2020 - 2023
control_data <- read.csv(file14) %>% 
  select(time, NAME, COL_END_DATE, COL_START_DATE, DISPLACED_RATE, ENERGYBILL_RATE, EXPENSE_RATE, FOODASSIS_RATE, FOODFORCHLD1_RATE, JOBLOSS_RATE, SURVEY_YEAR) %>% 
  slice(-1)

control_data[control_data == "null"] <- NA

#Change variables from character to numeric format
control_data <- control_data %>% 
  mutate(across(ends_with("_RATE"), ~ as.numeric(.))) %>% 
  mutate(time = as.numeric(time))

#State-Year aggregates
control_data_yearly <- control_data %>%
  group_by(time, NAME) %>%
  summarise(
    across(
      .cols = ends_with("_RATE"),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_avg"
    ),
    .groups = "drop"
  )


# Clean and Load Food Scarcity Data ---------------------------------------

#Cleaning up files for outcome variable 

# Load and clean
foodscarcity_2020_23 <- read_excel(file12) %>% 
  slice(-(1:3))

# Rename columns
names(foodscarcity_2020_23)[1:8] <- c("State", "Week", "Absolute Food Scarcity",
                                      "Absolute Margin of Error +/-", "Food Scarcity in Percent", 
                                      "Margin of Error Percent", "Measure Universe", 
                                      "Total Population Age 18+")

foodscarcity_2020_23 <- foodscarcity_2020_23 %>%
  filter(!State %in% c("United States", "-> Metro Areas", "-> States")) %>%
  filter(!is.na(Week)) %>%
  mutate(
    # Remove prefix numbers and normalize formatting
    Week = str_remove(Week, "^\\d+\\s*[–-]\\s*"),
    Week = str_replace_all(Week, "[–—]", "-"),         # normalize all dash types
    Week = str_replace_all(Week, "\\u00A0", " "),      # remove non-breaking space
    Week = str_squish(Week),                           # remove double spaces
    Week = str_replace_all(Week, " - ", "-"),          # normalize dash spacing
    
    # Extract year
    Year = str_extract(Week, "\\d{4}$"),
    
    # Detect same-month format: e.g., "October 18-30, 2023"
    SameMonth = str_detect(Week, "^[A-Za-z]+ \\d{1,2}-\\d{1,2}, \\d{4}$"),
    
    # Extract start and end date strings
    Start_Date_String = str_extract(Week, "^[A-Za-z]+ \\d{1,2}"),
    
    End_Date_String = if_else(
      SameMonth,
      str_extract(Week, "\\d{1,2}, \\d{4}$"),
      str_extract(Week, "[A-Za-z]+ \\d{1,2}, \\d{4}$")
    ),
    
    # Build complete End Date for same-month case
    End_Date_String = if_else(SameMonth,
                              paste(str_extract(Week, "^[A-Za-z]+"), End_Date_String),
                              End_Date_String),
    
    # Combine and parse
    Start_Date = mdy(paste(Start_Date_String, Year)),
    End_Date = mdy(End_Date_String),
    
    # Month extraction
    Start_Month = format(Start_Date, "%Y-%m"),
    End_Month = format(End_Date, "%Y-%m"),
    Span_Months = if_else(Start_Month != End_Month,
                          paste(Start_Month, "to", End_Month),
                          End_Month),
    Month = End_Month,
    month_year = ym(Month)
  )


foodscarcity_2024 <- read_excel(file13) %>% 
  slice(-(1:3))

# Rename columns
names(foodscarcity_2024)[1:8] <- c("State", "Week", "Absolute Food Scarcity",
                                   "Absolute Margin of Error +/-", "Food Scarcity in Percent", 
                                   "Margin of Error Percent", "Measure Universe", 
                                   "Total Population Age 18+")

foodscarcity_2024 <- foodscarcity_2024 %>%
  filter(!State %in% c("United States", "-> Metro Areas", "-> States")) %>%
  filter(!is.na(Week)) %>%
  mutate(
    # Remove prefix numbers and normalize formatting
    Week = str_remove(Week, "^\\d+\\s*[–-]\\s*"),
    Week = str_replace_all(Week, "[–—]", "-"),         # normalize all dash types
    Week = str_replace_all(Week, "\\u00A0", " "),      # remove non-breaking space
    Week = str_squish(Week),                           # remove double spaces
    Week = str_replace_all(Week, " - ", "-"),          # normalize dash spacing
    
    # Extract year
    Year = str_extract(Week, "\\d{4}$"),
    
    # Detect same-month format: e.g., "October 18-30, 2023"
    SameMonth = str_detect(Week, "^[A-Za-z]+ \\d{1,2}-\\d{1,2}, \\d{4}$"),
    
    # Extract start and end date strings
    Start_Date_String = str_extract(Week, "^[A-Za-z]+ \\d{1,2}"),
    
    End_Date_String = if_else(
      SameMonth,
      str_extract(Week, "\\d{1,2}, \\d{4}$"),
      str_extract(Week, "[A-Za-z]+ \\d{1,2}, \\d{4}$")
    ),
    
    # Build complete End Date for same-month case
    End_Date_String = if_else(SameMonth,
                              paste(str_extract(Week, "^[A-Za-z]+"), End_Date_String),
                              End_Date_String),
    
    # Combine and parse
    Start_Date = mdy(paste(Start_Date_String, Year)),
    End_Date = mdy(End_Date_String),
    
    # Month extraction
    Start_Month = format(Start_Date, "%Y-%m"),
    End_Month = format(End_Date, "%Y-%m"),
    Span_Months = if_else(Start_Month != End_Month,
                          paste(Start_Month, "to", End_Month),
                          End_Month),
    Month = End_Month,
    month_year = ym(Month)
  )

foodscarcity_all <- bind_rows(foodscarcity_2020_23, foodscarcity_2024)

foodscarcity_split <- foodscarcity_all %>%
  mutate(
    `Food Scarcity in Percent` = as.numeric(`Food Scarcity in Percent`),
    # Total days in the week
    Total_Days = as.numeric(End_Date - Start_Date + 1),
    
    # Calculate days in each month
    Days_In_Start_Month = if_else(SameMonth, Total_Days, as.numeric(ceiling_date(Start_Date, "month") - Start_Date)),
    Days_In_End_Month   = if_else(SameMonth, 0, Total_Days - Days_In_Start_Month),
    
    # Calculate weights
    Start_Weight = Days_In_Start_Month / Total_Days,
    End_Weight   = Days_In_End_Month / Total_Days,
    
    # Compute weighted food scarcity percent for each part
    FS_Start = `Food Scarcity in Percent` * Start_Weight,
    FS_End   = `Food Scarcity in Percent` * End_Weight
  )

foodscarcity_split <- foodscarcity_split %>%
  filter(!str_detect(State, regex("metro area", ignore_case = TRUE)))

monthly_fs_summary <- foodscarcity_split %>%
  # Stack start and end parts
  transmute(
    State,
    Month = Start_Month,
    Weighted_FS = FS_Start
  ) %>%
  bind_rows(
    foodscarcity_split %>%
      transmute(
        State,
        Month = End_Month,
        Weighted_FS = FS_End
      )
  ) %>%
  filter(!is.na(Month), !is.na(Weighted_FS)) %>%
  group_by(State, Month) %>%
  summarise(
    Avg_FoodScarcity_Percent = sum(Weighted_FS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(month_year = ym(Month),
         month = format(month_year, "%b %Y"),
         month = my(month))


# Clean and Load SNAP Data ------------------------------------------------

#Clean and Load SNAP Data for every year
sheets_fy23 <- excel_sheets(file2)
sheets_fy23 <- sheets_fy23[-length(sheets_fy23)]

fy23_combined <- bind_rows(
  lapply(sheets_fy23, function(sheet) {
    df <- read_excel(file2, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy23_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy23_combined <- fy23_combined %>%
  slice(-(1:6)) %>%  
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy22 <- excel_sheets(file3)
sheets_fy22 <- sheets_fy22[-length(sheets_fy22)]

fy22_combined <- bind_rows(
  lapply(sheets_fy22, function(sheet) {
    df <- read_excel(file3, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy22_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy22_combined <- fy22_combined %>%
  slice(-(1:6)) %>%  
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy21 <- excel_sheets(file4)
sheets_fy21 <- sheets_fy21[-length(sheets_fy21)]

fy21_combined <- bind_rows(
  lapply(sheets_fy21, function(sheet) {
    df <- read_excel(file4, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy21_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy21_combined <- fy21_combined %>%
  slice(-(1:6)) %>%  
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy20 <- excel_sheets(file5)
sheets_fy20 <- sheets_fy20[-length(sheets_fy20)]

fy20_combined <- bind_rows(
  lapply(sheets_fy22, function(sheet) {
    df <- read_excel(file5, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy20_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy20_combined <- fy20_combined %>%
  slice(-(1:6)) %>%  
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy19 <- excel_sheets(file6)
sheets_fy19 <- sheets_fy19[-length(sheets_fy19)]

fy19_combined <- bind_rows(
  lapply(sheets_fy19, function(sheet) {
    df <- read_excel(file6, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy19_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy19_combined <- fy19_combined %>%
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy18 <- excel_sheets(file7)
sheets_fy18 <- sheets_fy18[-length(sheets_fy18)]

fy18_combined <- bind_rows(
  lapply(sheets_fy18, function(sheet) {
    df <- read_excel(file7, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy18_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy18_combined <- fy18_combined %>%
  slice(-(1:6)) %>%  
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy17 <- excel_sheets(file8)
sheets_fy17 <- sheets_fy17[-length(sheets_fy17)]

fy17_combined <- bind_rows(
  lapply(sheets_fy17, function(sheet) {
    df <- read_excel(file8, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy17_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy17_combined <- fy17_combined %>%
  slice(-(1:6)) %>%  
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy16 <- excel_sheets(file9)
sheets_fy16 <- sheets_fy16[-length(sheets_fy16)]

fy16_combined <- bind_rows(
  lapply(sheets_fy16, function(sheet) {
    df <- read_excel(file9, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy16_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy16_combined <- fy16_combined %>%
  slice(-(1:6)) %>%  
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy15 <- excel_sheets(file10)
sheets_fy15 <- sheets_fy15[-length(sheets_fy15)]

fy15_combined <- bind_rows(
  lapply(sheets_fy15, function(sheet) {
    df <- read_excel(file10, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy15_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy15_combined <- fy15_combined %>%
  slice(-(1:6)) %>%  
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

sheets_fy14 <- excel_sheets(file11)
sheets_fy14 <- sheets_fy14[-length(sheets_fy14)]

fy14_combined <- bind_rows(
  lapply(sheets_fy22, function(sheet) {
    df <- read_excel(file11, sheet = sheet)
    df$Region <- sheet
    return(df)
  })
)

names(fy14_combined)[1:6] <- c("Month-Year", "No. of Households", "No. of Persons",
                               "Total Cost", "Cost Per Household", "Cost Per Person")

fy14_combined <- fy14_combined %>%
  filter(!`Month-Year` %in% c("Total", "Footnotes:", "ALL DATA SUBJECT TO REVISION", "Fiscal Year and Month")) %>%
  mutate(across(everything(), ~ trimws(as.character(.)))) %>%
  
  mutate(State = if_else(
    !is.na(`Month-Year`) & rowSums(!is.na(select(., -`Month-Year`, -Region))) == 0,
    `Month-Year`,
    NA_character_
  )) %>%
  fill(State) %>%
  
  filter(grepl("^[A-Za-z]{3,9} \\d{4}$", `Month-Year`)) %>%
  
  mutate(month_year = parse_date_time(`Month-Year`, orders = c("b Y", "B Y"))) %>%
  
  mutate(across(-c(`Month-Year`, month_year, Region, State), as.numeric))

snap_all_years <- bind_rows(
  fy14_combined,
  fy15_combined,
  fy16_combined,
  fy17_combined,
  fy18_combined,
  fy19_combined,
  fy20_combined,
  fy21_combined,
  fy22_combined,
  fy23_combined
)

# Combine SNAP, FS and Control Data ------------------------------------------------

#Join SNAP and Food Scarcity Data
snap_all_years <- snap_all_years %>% 
  filter(!State %in% c("NERO", "SWRO", "MARO", "SERO", "MWRO", "MPRO", "WRO"),
         !is.na('No. of Households'),
         !is.na('Cost Per Households')) %>% 
  mutate(calendar_year = year(month_year))

snap_all_years <- snap_all_years %>% 
  mutate(month_year = dmy(month_year),
         month = my(`Month-Year`)) %>% 
  select(-`Month-Year`)

joined_data <- snap_all_years %>%
  full_join(monthly_fs_summary, by = c("State", "month"))

joined_data <- joined_data %>%
  mutate(
    ea_end_date = case_when(
      State == "Idaho" ~ as.Date("2021-03-31"),
      State == "North Dakota" ~ as.Date("2021-05-31"),
      State == "Arkansas" ~ as.Date("2021-06-30"),
      State == "Montana" ~ as.Date("2021-07-31"),
      State == "Florida" ~ as.Date("2021-07-31"),
      State == "Nebraska" ~ as.Date("2021-07-31"),
      State == "South Dakota" ~ as.Date("2021-07-31"),
      State == "Missouri" ~ as.Date("2021-08-30"),
      State == "Tennessee" ~ as.Date("2021-12-31"),
      State == "Mississippi" ~ as.Date("2021-12-31"),
      State == "Iowa" ~ as.Date("2022-03-31"),
      State == "Arizona" ~ as.Date("2022-04-30"),
      State == "Kentucky" ~ as.Date("2022-04-30"),
      State == "Wyoming" ~ as.Date("2022-04-30"),
      State == "Georgia" ~ as.Date("2022-05-31"),
      State == "Indiana" ~ as.Date("2022-05-31"),
      State == "Alaska" ~ as.Date("2022-08-31"),
      State == "South Carolina" ~ as.Date("2023-01-31"),
      State == "District of Columbia" ~ as.Date("2023-02-28"),
      TRUE ~ NA_Date_
    )
  )

joined_data <- joined_data %>%
  select(-c(month_year.x, Month, month_year.y)) %>% 
  filter(!is.na(Avg_FoodScarcity_Percent))

joined_data <- joined_data %>%
  mutate(ea_end_date = if_else(is.na(ea_end_date), as.Date("2023-02-28"), ea_end_date),
         ea_treatment = if_else(
           ea_end_date == as.Date("2023-02-28"), 0, 1),
         ea_end_year = year(ea_end_date),
         calendar_year = year(month))

joined_data <- joined_data %>% 
  filter(!State %in% c("Puerto Rico", "Virgin Islands", "Guam", "American Samoa", "Commonwealth of Northern Mariana Islands")) %>% 
  filter(!month %in% c("2020-01-01", "2020-02-01", "2020-03-01"))

#Build Staggered DiD Regression
joined_data <- joined_data %>% 
  mutate(rel_year = calendar_year - ea_end_year,
         ea_ended = if_else(calendar_year >= ea_end_year, 1, 0)
  ) %>% 
  filter(!calendar_year %in% c(2013,2014,2015,2016,2017,2018,2019))

#Add Controls
joined_data <- joined_data %>% 
  left_join(control_data_yearly,by = c("calendar_year" = "time", "State" = "NAME"))

joined_data <- joined_data %>% 
  left_join(income,by = c("calendar_year" = "Year", "State" = "State_Name"))

saveRDS(list(control_data = control_data_yearly, income_data = income, fs = monthly_fs_summary, snap = snap_all_years, reg_data = joined_data), "cleaned_objects.rds")
