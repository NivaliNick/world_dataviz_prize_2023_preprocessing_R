# Load packages
required_packages <- c('readr','tidyr','reshape2','janitor','dplyr')

lapply(required_packages, require, character.only = TRUE)

### WORLD DATAVIZ PRIZE 2023 IIB DATA ###

# Load data, without assigning column names
world_dataviz_prize_2023_IIB_data_raw <- read_csv("Data/World Dataviz Prize 2023 IIB Data/World Dataviz Prize 2023 IIB Data Raw/World Dataviz Prize 2023 - Challenge Concepts & Datasets - Dashboard of the Present Future.csv", col_names = FALSE)

# Drop rows 2-4 (they contain links to source and notes per column, not directly relevant for Tableau Dashboard)
world_dataviz_prize_2023_IIB_data_cleaned <- world_dataviz_prize_2023_IIB_data_raw[-c(2:4), ]

# Remove columns that consist only of NA's
world_dataviz_prize_2023_IIB_data_cleaned <- world_dataviz_prize_2023_IIB_data_cleaned[ , colSums(is.na(world_dataviz_prize_2023_IIB_data_cleaned)) != nrow(world_dataviz_prize_2023_IIB_data_cleaned)]

# Remove rows that consist only of NA's
world_dataviz_prize_2023_IIB_data_cleaned <- world_dataviz_prize_2023_IIB_data_cleaned[rowSums(is.na(world_dataviz_prize_2023_IIB_data_cleaned)) != ncol(world_dataviz_prize_2023_IIB_data_cleaned) , ]

# Retrieve combination of country name and country code, they are added back at the end of the transformation
country_code <- world_dataviz_prize_2023_IIB_data_cleaned[3:nrow(world_dataviz_prize_2023_IIB_data_cleaned),c(1,2)]
colnames(country_code) <- c('Country name','ISO Country code')

# Transpose
world_dataviz_prize_2023_IIB_data_cleaned <- as.data.frame(t(world_dataviz_prize_2023_IIB_data_cleaned))

# Store indicator names (for indicator name cleaning iteration below)
indicators <- world_dataviz_prize_2023_IIB_data_cleaned[,1]

# Initialize a variable to store the previous indicator name
prev_indcname <- ""

# Iterate over the indicator names
for (i in 1:length(indicators)) {
  # Check if the indicator name starts with '...'
  if (substr(indicators[i], 1, 3) == "..." | is.na(indicators[i])) {
    # Replace the indicator name with the previous indicator name
    indicators[i] <- prev_indcname
  } else {
    # Otherwise, update the previous indicator name
    prev_indcname <- indicators[i]
    # remove the trailing "..." and number if any
    indicators[i] <- gsub("\\.{3}[0-9]*$", "", indicators[i])
  }
}

# Replace original indicator names with the new cleaned indicator names
world_dataviz_prize_2023_IIB_data_cleaned[,1] <- indicators

# Put countries on the columns (to prepare for reshape)
names(world_dataviz_prize_2023_IIB_data_cleaned) <- as.matrix(world_dataviz_prize_2023_IIB_data_cleaned[1,])
world_dataviz_prize_2023_IIB_data_cleaned <- world_dataviz_prize_2023_IIB_data_cleaned[-1,]

# Clean the country names (temporarily for the reshape)
world_dataviz_prize_2023_IIB_data_cleaned <- clean_names(world_dataviz_prize_2023_IIB_data_cleaned)

# Reshape (so that cleaned indicators are back on the columns, making it easier to use in Tableau)
world_dataviz_prize_2023_IIB_data_cleaned <- melt(world_dataviz_prize_2023_IIB_data_cleaned,id.vars = c("indicator","data_year"), variable.name = "country", value.name = "value")
world_dataviz_prize_2023_IIB_data_cleaned <- dcast(world_dataviz_prize_2023_IIB_data_cleaned, country + data_year ~ indicator, value.var = "value")

# Get rid of whitespaces and anything after (and including) the dot in the year column
world_dataviz_prize_2023_IIB_data_cleaned$data_year <- gsub("\\.[0-9]+|\\s", "", world_dataviz_prize_2023_IIB_data_cleaned$data_year)
# Get rid of any non numerical elements in the year column
world_dataviz_prize_2023_IIB_data_cleaned$data_year <- gsub("[^0-9]", "", world_dataviz_prize_2023_IIB_data_cleaned$data_year)
# Replace empty year values (previously containing 'latest year', 'varies by country') with 2023 as placeholder
world_dataviz_prize_2023_IIB_data_cleaned$data_year[world_dataviz_prize_2023_IIB_data_cleaned$data_year == ""] <- "2023"

# Remove all % signs
world_dataviz_prize_2023_IIB_data_cleaned[] <- lapply(world_dataviz_prize_2023_IIB_data_cleaned, gsub, pattern = "[ %,]+", replacement = "")
# Remove all cells only containing '-' (they mark empty values)
world_dataviz_prize_2023_IIB_data_cleaned[world_dataviz_prize_2023_IIB_data_cleaned == '-'] <- NA

# Get combinations of country code and cleaned country name (necessary to bring back original country names)
clean_country_code <- na.omit(world_dataviz_prize_2023_IIB_data_cleaned[c('ISO Country code', 'country')])

# Convert columns to numeric
cols_to_exclude <- c('ISO Country code', 'data_year', 'country')
world_dataviz_prize_2023_IIB_data_cleaned[, !(names(world_dataviz_prize_2023_IIB_data_cleaned) %in% cols_to_exclude)] <- lapply(world_dataviz_prize_2023_IIB_data_cleaned[, !(names(world_dataviz_prize_2023_IIB_data_cleaned) %in% cols_to_exclude)], as.numeric)

# Clean combinations of indicator and year per country
world_dataviz_prize_2023_IIB_data_cleaned <- world_dataviz_prize_2023_IIB_data_cleaned %>% select(-`ISO Country code`) %>% group_by(country, data_year) %>% summarise_all(funs(min(., na.rm = TRUE)))
world_dataviz_prize_2023_IIB_data_cleaned[world_dataviz_prize_2023_IIB_data_cleaned == Inf] <- NA

# Bring back original country names
world_dataviz_prize_2023_IIB_data_cleaned <- left_join(world_dataviz_prize_2023_IIB_data_cleaned,clean_country_code, by = 'country')
world_dataviz_prize_2023_IIB_data_cleaned <- left_join(world_dataviz_prize_2023_IIB_data_cleaned,country_code, by = 'ISO Country code')
world_dataviz_prize_2023_IIB_data_cleaned <- world_dataviz_prize_2023_IIB_data_cleaned[,-1]

# Drop temporary data frames
rm(clean_country_code)
rm(country_code)

# Convert year column to date
world_dataviz_prize_2023_IIB_data_cleaned$data_year <- as.Date(paste0(world_dataviz_prize_2023_IIB_data_cleaned$data_year, "-01-01"), format = "%Y-%m-%d")

# Write transformed version to csv
write.csv(world_dataviz_prize_2023_IIB_data_cleaned, 'Data/World Dataviz Prize 2023 IIB Data/World Dataviz Prize 2023 IIB Data Cleaned/world_dataviz_prize_2023_IIB_data_cleaned.csv', row.names = FALSE)

### WORLD BANK DATA ###

# Initiate empty list which will be used to store the individual data frames (per World Bank indicator)
world_bank_data_cleaned_list <- list()

# Get the names of all World Bank csv files in the folder
files <- list.files(path = "Data/World Bank Data/World Bank Data Raw/", pattern = "*.csv")

# Loop through the files to load, clean and reshape them
for(i in files){
  
  # Remove .csv part of the file name, the indicator name will be used as value name when reshaping the data
  indicator_name <- sub(".csv$", "", i)
  
  # Load data, without assigning column names
  temp_df <- read_delim(paste0('Data/World Bank Data/World Bank Data Raw/',i), delim = ',', col_names = FALSE)
  
  # Drop empty rows
  temp_df <- temp_df[-c(1:4), ]
  
  # Use first row as column names
  names(temp_df) <- as.matrix(temp_df[1,])
  temp_df <- temp_df[-1,]
  
  # Drop unnecessary columns
  temp_df <- temp_df[, -c(3,4)]
  
  # Clean column names
  temp_df <- temp_df %>% rename(country_name = 'Country Name', country_code = 'Country Code')
  
  # Reshape data from wide to long, use the indicator name as value name
  temp_df <- melt(temp_df, id.vars = c('country_name','country_code'), variable.name = 'year', value.name = indicator_name)
  
  # Sort data frame based on country and year
  temp_df <- temp_df %>% arrange(country_name, year)
  
  # Store the temporary data frame in the list
  world_bank_data_cleaned_list[[i]] <- temp_df
  
}

# Drop temporary data frame
rm(temp_df)

# Merge all the individual indicator data frames per into one single combined data frame in which each indicator is a variable 
world_bank_data_cleaned <- Reduce(function(x, y) merge(x, y, by=c("country_name","country_code","year"), all = TRUE), world_bank_data_cleaned_list)

# Drop list
rm(world_bank_data_cleaned_list)

# Convert year column to date
world_bank_data_cleaned$year <- as.Date(paste0(world_bank_data_cleaned$year, "-01-01"), format = "%Y-%m-%d")

# Filter to contain only data from 2000 and later
world_bank_data_cleaned <- world_bank_data_cleaned %>% filter(year >= '2000-01-01')

# Write transformed version to csv
write.csv(world_bank_data_cleaned, 'Data/World Bank Data/World Bank Data Cleaned/world_bank_data_cleaned.csv', row.names = FALSE)

### COMBINE ORIGINAL WORLD DATAVIZE PRIZE 2023 IIB DATA WITH WORLD BANK DATA ###

# Create a subset of the World Dataviz Prize 2023 IIB data with columns that are not present in the World Bank data
world_dataviz_prize_2023_IIB_data_cleaned_sub <- world_dataviz_prize_2023_IIB_data_cleaned[,c('Country name', 'ISO Country code','data_year',
                                             '% of population in extreme poverty','control of corruption','financial freedom score',
                                             'GINI index','government effectiveness','government expenditure (% of GDP)',
                                             'government integrity score','happy planet index','human development index',
                                             'judicial effectiveness score','Military Spending as % of GDP','overall economic freedom score',
                                             'political rights score','political stability & absence of violence','regulatory quality',
                                             'rule of law','share of electricity from renewables generation','surface area (Km2)',
                                             'sustainable economic development assessment (SEDA)')]

# Create a subset of the World Bank data containing only countries that are present in the World Dataviz Prize 2023 IIB data
world_bank_data_cleaned_sub <- world_bank_data_cleaned %>% filter(country_name %in% world_dataviz_prize_2023_IIB_data_cleaned_sub$`Country name`)

# Merge World Dataviz Prize 2023 IIB data and World Bank data
world_dataviz_prize_2023_IIB_world_bank_data <- merge(world_dataviz_prize_2023_IIB_data_cleaned_sub, world_bank_data_cleaned_sub, by.x = c("Country name","ISO Country code","data_year"), by.y = c("country_name","country_code","year"), all = TRUE)

# Drop temporary data frames
rm(world_dataviz_prize_2023_IIB_data_cleaned_sub)
rm(world_bank_data_cleaned_sub)

iso_code_region <- read_delim('Data/iso_code_region.csv', delim = ',', col_names = TRUE)

world_dataviz_prize_2023_IIB_world_bank_data <- left_join(world_dataviz_prize_2023_IIB_world_bank_data, unique(iso_code_region[,c('alpha-3','region','sub-region')]), by=c('ISO Country code'='alpha-3'))

# Drop temporary data frames
rm(iso_code_region)

# Write combined data to csv
write.csv(world_dataviz_prize_2023_IIB_world_bank_data, 'Data/world_dataviz_prize_2023_IIB_world_bank_data.csv', row.names = FALSE)
