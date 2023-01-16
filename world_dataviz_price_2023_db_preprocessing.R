# Load packages
required_packages <- c('readr','tidyr','reshape2','janitor','dplyr')

lapply(required_packages, require, character.only = TRUE)

# Load data, without assigning column names
world_dataviz_price_2023_dbprefu <- read_csv("World Dataviz Prize 2023 - Challenge Concepts & Datasets - Dashboard of the Present Future.csv", col_names = FALSE)

# Drop rows 2-4 (they contain links to source and notes per column, not directly relevant for Tableau Dashboard)
world_dataviz_price_2023_dbprefu <- world_dataviz_price_2023_dbprefu[-c(2:4), ]

# Remove columns that consist only of NA's
world_dataviz_price_2023_dbprefu <- world_dataviz_price_2023_dbprefu[ , colSums(is.na(world_dataviz_price_2023_dbprefu)) != nrow(world_dataviz_price_2023_dbprefu)]

# Remove rows that consist only of NA's
world_dataviz_price_2023_dbprefu <- world_dataviz_price_2023_dbprefu[rowSums(is.na(world_dataviz_price_2023_dbprefu)) != ncol(world_dataviz_price_2023_dbprefu) , ]

# Retrieve combination of country name and country code, they are added back at the end of the transformation
country_code <- world_dataviz_price_2023_dbprefu[3:nrow(world_dataviz_price_2023_dbprefu),c(1,2)]
colnames(country_code) <- c('Country name','ISO Country code')

# Transpose
world_dataviz_price_2023_dbprefu <- as.data.frame(t(world_dataviz_price_2023_dbprefu))

# Store indicator names (for indicator name cleaning iteration below)
indicators <- world_dataviz_price_2023_dbprefu[,1]

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
world_dataviz_price_2023_dbprefu[,1] <- indicators

# Put countries on the columns (to prepare for reshape)
names(world_dataviz_price_2023_dbprefu) <- as.matrix(world_dataviz_price_2023_dbprefu[1,])
world_dataviz_price_2023_dbprefu <- world_dataviz_price_2023_dbprefu[-1,]

# Clean the country names (temporarily for the reshape)
world_dataviz_price_2023_dbprefu <- clean_names(world_dataviz_price_2023_dbprefu)

# Reshape (so that cleaned indicators are back on the columns, making it easier to use in Tableau)
world_dataviz_price_2023_dbprefu <- melt(world_dataviz_price_2023_dbprefu,id.vars = c("indicator","data_year"), variable.name = "country", value.name = "value")
world_dataviz_price_2023_dbprefu <- dcast(world_dataviz_price_2023_dbprefu, country + data_year ~ indicator, value.var = "value")

# Get rid of whitespaces and anything after (and including) the dot in the year column
world_dataviz_price_2023_dbprefu$data_year <- gsub("\\.[0-9]+|\\s", "", world_dataviz_price_2023_dbprefu$data_year)
# Get rid of any non numerical elements in the year column
world_dataviz_price_2023_dbprefu$data_year <- gsub("[^0-9]", "", world_dataviz_price_2023_dbprefu$data_year)
# Replace empty year values (previously containing 'latest year', 'varies by country') with 2023 as placeholder
world_dataviz_price_2023_dbprefu$data_year[world_dataviz_price_2023_dbprefu$data_year == ""] <- "2023"

# Remove all % signs
world_dataviz_price_2023_dbprefu[] <- lapply(world_dataviz_price_2023_dbprefu, gsub, pattern = "[ %,]+", replacement = "")
# Remove all cells only containing '-' (they mark empty values)
world_dataviz_price_2023_dbprefu[world_dataviz_price_2023_dbprefu == '-'] <- NA

# Get combinations of country code and cleaned country name (necessary to bring back original country names)
clean_country_code <- na.omit(world_dataviz_price_2023_dbprefu[c('ISO Country code', 'country')])

# Convert columns to numeric
cols_to_exclude <- c('ISO Country code', 'data_year', 'country')
world_dataviz_price_2023_dbprefu[, !(names(world_dataviz_price_2023_dbprefu) %in% cols_to_exclude)] <- lapply(world_dataviz_price_2023_dbprefu[, !(names(world_dataviz_price_2023_dbprefu) %in% cols_to_exclude)], as.numeric)

# Clean combinations of indicator and year per country
world_dataviz_price_2023_dbprefu <- world_dataviz_price_2023_dbprefu %>% select(-`ISO Country code`) %>% group_by(country, data_year) %>% summarise_all(funs(min(., na.rm = TRUE)))
world_dataviz_price_2023_dbprefu[world_dataviz_price_2023_dbprefu == Inf] <- NA

# Bring back original country names
world_dataviz_price_2023_dbprefu <- left_join(world_dataviz_price_2023_dbprefu,clean_country_code, by = 'country')
world_dataviz_price_2023_dbprefu <- left_join(world_dataviz_price_2023_dbprefu,country_code, by = 'ISO Country code')
world_dataviz_price_2023_dbprefu <- world_dataviz_price_2023_dbprefu[,-1]

# Convert year column to date
world_dataviz_price_2023_dbprefu$data_year <- as.Date(paste0(world_dataviz_price_2023_dbprefu$data_year, "-01-01"), format = "%Y-%m-%d")

# Write transformed version to csv to be imported in Tableau
write.csv(world_dataviz_price_2023_dbprefu, 'world_dataviz_price_2023_dbprefu.csv', row.names = FALSE)
