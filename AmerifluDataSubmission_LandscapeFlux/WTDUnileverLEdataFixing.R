# Install and load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(lubridate)

# names(way3_wtd_uni_t)
# way3_data[[1-7]] 2018, 2019, 2020, 2021, 2022, 2023, 2024 
# way4_data[[1-7]] 2018, 2019, 2020, 2021, 2022, 2023, 2024
# way3_data[[4]]$TIMESTAMP
# way3_wtd_uni_t$Way3_UnileverTowerStation_2021$TIMESTAMP
## 2020,2022, 2023 are in cm and 2021(way3) are in meters
## Convert all of them to meters
### Add Precipitation data from Way 4 to Way 3

# List of Excel files
excel_files <- c(
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/MasterFile_Way3_SoilProf_VWC.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/MasterFile_Way4SoilProfile.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2021_MasterFile_Way3_SoilProf_All.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2021_MasterFile_Way4SoilProfile.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2022_Way4_SoilProfile_MasterFile.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2022Way3SoilProfileMasterFile.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2023Way3SoilProfileMasterFile copy.xlsx",
  "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/WTD/2023Way4_SoilProfile_MasterFile copy.xlsx"
)
# Create an empty list to store the data frames
data_list <- list()
# Loop through each file in the list
for (file in excel_files) {
  # Extract the base name of the file (without path and extension)
  file_name <- tools::file_path_sans_ext(basename(file))
  # Extract Way3/Way4 and Year from the file name
  if (grepl("Way3", file_name, ignore.case = TRUE)) {
    way <- "Way3"
  } else if (grepl("Way4", file_name, ignore.case = TRUE)) {
    way <- "Way4"
  } else {
    way <- "Unknown"
  }
  # Extract the year (assuming the year is always the first 4 digits)
  year <- substr(file_name, 1, 4)
  # Create the new name in the desired format
  df_name <- paste0(way, "_UnileverTowerStation_", year)
  # Read the Excel file
  data <- read_excel(file, skip =1)
  # Assign the data to a variable with the new name
  assign(df_name, data)
  # Store the data frame in the list
  data_list[[df_name]] <- data
  # Print the first few rows and structure of the data frame
  cat("File:", file_name, "\n")
  cat("Data Frame Name:", df_name, "\n")
  print(head(data))
  print(str(data))
  cat("\n")  # Add a newline for readability
}

# Update the names in data_list to reflect the changes
names(data_list) <- gsub("Mast", "2020", names(data_list))

# Loop through each data frame in data_list
for (df_name in names(data_list)) {
  data_list[[df_name]] <- data_list[[df_name]] %>%
    mutate(
      TIMESTAMP = as.POSIXct(
        as.numeric(TIMESTAMP) * 86400,  # Convert Excel serial date to seconds
        origin = "1899-12-30",          # Excel's origin date
        tz = "UTC"                       # Set time zone (adjust as needed)
      )
    )
  
  # Print confirmation message
  cat("Converted TIMESTAMP for:", df_name, "\n")
}

### 2021 wt_corr to wt_corr_AVG_cm_fixedBias and change Fixed to fixed
data_list$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias <- data_list$Way3_UnileverTowerStation_2021$wt_corr
data_list$Way3_UnileverTowerStation_2022$wt_corr_AVG_cm_fixedBias <- data_list$Way3_UnileverTowerStation_2022$wt_corr_AVG_cm_FixedBias
data_list$Way4_UnileverTowerStation_2020$wt_corr_AVG_cm_fixedBias <- data_list$Way4_UnileverTowerStation_2020$wt_corr_AVG_cm_FixedBias

missing_columns <- sapply(data_list, function(df) {
  !"wt_corr_AVG_cm_fixedBias" %in% colnames(df)
})
column_variants <- sapply(data_list, function(df) {
  list(
    has_fixedBias = "wt_corr_AVG_cm_fixedBias" %in% colnames(df),
    has_FixedBias = "wt_corr_AVG_cm_FixedBias" %in% colnames(df)
  )
})

# Convert list to a dataframe for better readability
column_variants_df <- as.data.frame(do.call(rbind, column_variants))

# Print which datasets have each column va
filtered_data_list <- lapply(data_list, function(df) {
  df %>% select(TIMESTAMP, wt_corr_AVG_cm_fixedBias)
})

# Separate into Way3 and Way4
way3_wtd_uni_t <- filtered_data_list[grep("Way3", names(filtered_data_list))]
way4_wtd_uni_t <- filtered_data_list[grep("Way4", names(filtered_data_list))]
hist(way3_wtd_uni_t$Way3_UnileverTowerStation_2020$wt_corr_AVG_cm_fixedBias)

# Replace non-numeric values with NA (keeping the structure intact)
way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias[way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias %in% c("m", "Avg")] <- NA
# Convert to numeric
way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias <- as.numeric(
  way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias
)
# Round to 2 significant digits (NA values remain unchanged)
way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias <- signif(
  way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias, 2
)
way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias<-
  way3_wtd_uni_t$Way3_UnileverTowerStation_2021$wt_corr_AVG_cm_fixedBias*100

# Function to convert wt_corr_AVG_cm_fixedBias from cm to meters
convert_cm_to_m <- function(df) {
  if ("wt_corr_AVG_cm_fixedBias" %in% names(df)) {
    df$wt_corr_AVG_cm_fixedBias <- df$wt_corr_AVG_cm_fixedBias / 100
  }
  return(df)
}

# Apply to all data frames in way3_wtd_uni_t
way3_wtd_uni_t <- lapply(way3_wtd_uni_t, convert_cm_to_m)
# Apply to all data frames in way4_wtd_uni_t
way4_wtd_uni_t <- lapply(way4_wtd_uni_t, convert_cm_to_m)
names(way3_wtd_uni_t)
names(way4_wtd_uni_t)




# Define the output directory
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/WTD"
# Function to save data frames as CSV
save_data_list <- function(data_list, output_dir) {
  for (df_name in names(data_list)) {
    # Extract the data frame
    data <- data_list[[df_name]]
    
    # Create the new file name
    if (grepl("Mast", df_name)) {
      new_file_name <- gsub("Mast", "2020", df_name)  # Replace "Mast" with "2020"
    } else {
      new_file_name <- df_name
    }
    # Add the .csv extension
    new_file_name <- paste0(new_file_name, ".csv")
    # Create the full file path
    file_path <- file.path(output_dir, new_file_name)
    # Save the data frame as a CSV file
    write.csv(data, file_path, row.names = FALSE)
    # Print a confirmation message
    cat("Saved:", file_path, "\n")
  }
}

# Save data frames from both lists
save_data_list(way3_wtd_uni_t, output_dir)
save_data_list(way4_wtd_uni_t, output_dir)


# Load necessary library
library(base)

# Define the directories
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/Masterfiles/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/Masterfiles/Way4"

# Define the files to read
way3_files_to_read <- c("Way3 2018.csv", "Way3 2019.csv", "Way3 2020.csv", "Way3 2021.csv", "Way3 2022.csv", "Way3 2023.csv", "Way3 2024.csv")
way4_files_to_read <- c("Way4 2018.csv", "Way4 2019.csv", "Way4 2020.csv", "Way4 2021.csv", "Way4 2022.csv", "Way4 2023.csv", "Way4 2024.csv")

# Read and process Way3 and Way4 data
way3_data <- lapply(way3_files_to_read, function(filename) {
  file_path <- file.path(way3_directory, filename)
  data <- read.csv(file_path)
  #apply_column_mapping(data) # Apply column name mapping
})

way4_data <- lapply(way4_files_to_read, function(filename) {
  file_path <- file.path(way4_directory, filename)
  data <- read.csv(file_path)
  #apply_column_mapping(data) # Apply column name mapping
})

#####Rename Way 4 water table depth column WTD_Avg as Lvl_m_Avg
# Rename WTD_Avg to Lvl_m_Avg in Way4 data
# Loop through each dataframe in way4_data and rename WTD_Avg to Lvl_m_Avg
way4_data <- lapply(way4_data, function(df) {
  # Drop any columns named "Lvl_m_Avg"
  df <- df[, !colnames(df) %in% "Lvl_m_Avg", drop = FALSE]
  
  # Rename "WTD_Avg" to "Lvl_m_Avg"
  colnames(df) <- gsub("WTD_Avg", "Lvl_m_Avg", colnames(df))
  
  return(df)
})

###UNITS ROWS######
# Step 1: Extract unit row and store separately
unit_row <- way3_data[[6]][1, ] # Assuming the unit row is in the 6th list element

# Filter and rename
filtered_columns <- c("TIMESTAMP", "TIMESTAMP_START", "TIMESTAMP_END", "x_70_", "x_90_", "x_peak", 
                      "ch4_mole_fraction", "ch4_mixing_ratio", "co2_mole_fraction", "co2_mixing_ratio", 
                      "co2_flux", "ch4_flux", "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_flux", 
                      "H", "LE", "H_strg", "LE_strg", "air_pressure", "RH", "sonic_temperature", 
                      "qc_co2_flux", "qc_ch4_flux", "qc_H", "qc_LE", "qc_Tau", "co2_var", "co2_strg", 
                      "ch4_strg", "u_var", "v_var", "w_var", "wind_dir", "wind_speed", "max_wind_speed", 
                      "X_z_d__L", "air_temperature", "VPD", "LW_IN_Avg", "LW_OUT_Avg", "PAR_IN_Avg", 
                      "PAR_OUT_Avg", "SW_IN_Avg", "SW_OUT_Avg", "SWC_2_1_1_Avg", "L", "Tau", "TS_mean.2.", "u_")

# Filter `unit_row` to include only the columns in `filtered_columns`
filtered_unit_row <- unit_row[names(unit_row) %in% filtered_columns]

# View the filtered unit row
print(filtered_unit_row)


#### Get rid of the unit rows for 2022 to 2024####
way3_data[[5]] <- way3_data[[5]][-1, ]
way3_data[[6]] <- way3_data[[6]][-1, ]
way3_data[[7]] <- way3_data[[7]][-1, ]

way4_data[[5]] <- way4_data[[5]][-1, ]
way4_data[[6]] <- way4_data[[6]][-1, ]
way4_data[[7]] <- way4_data[[7]][-1, ]

# Print the first row of the TIMESTAMP column
cat("First row of TIMESTAMP column (way3_data[[5]]):", way3_data[[5]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way3_data[[6]]):", way3_data[[6]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way3_data[[7]]):", way3_data[[7]]$TIMESTAMP[1], "\n")

cat("First row of TIMESTAMP column (way4_data[[5]]):", way4_data[[5]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[6]]):", way4_data[[6]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[7]]):", way4_data[[7]]$TIMESTAMP[1], "\n")

# # Convert TIMESTAMP to a consistent format
# way4_data[[5]]$TIMESTAMP <- as.character(mdy_hm(way4_data[[5]]$TIMESTAMP))

# Print the first row to verify the change
cat("First row of TIMESTAMP column (way4_data[[5]]):", way4_data[[5]]$TIMESTAMP[1], "\n")

# Function to check and convert TIMESTAMP
convert_timestamp <- function(timestamp) {
  # If the TIMESTAMP is already in the correct format, return it as is
  if (grepl("^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$", timestamp)) {
    return(timestamp)
  } else {
    # Otherwise, attempt to convert using mdy_hm
    return(as.character(mdy_hm(timestamp)))
  }
}

# Apply the conversion to each TIMESTAMP column
way3_data[[5]]$TIMESTAMP <- sapply(way3_data[[5]]$TIMESTAMP, convert_timestamp)
way3_data[[6]]$TIMESTAMP <- sapply(way3_data[[6]]$TIMESTAMP, convert_timestamp)
way4_data[[5]]$TIMESTAMP <- sapply(way4_data[[5]]$TIMESTAMP, convert_timestamp)
way4_data[[6]]$TIMESTAMP <- sapply(way4_data[[6]]$TIMESTAMP, convert_timestamp)

# Print the first rows to verify the changes
cat("First row of TIMESTAMP column (way3_data[[5]]):", way3_data[[5]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way3_data[[6]]):", way3_data[[6]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[5]]):", way4_data[[5]]$TIMESTAMP[1], "\n")
cat("First row of TIMESTAMP column (way4_data[[6]]):", way4_data[[6]]$TIMESTAMP[1], "\n")


#############################################################################
##########################MERGING WtD With WAY 3 and 4 #####################
############################################################################
# Function to merge corresponding way3_wtd_uni_t and way3_data dataframes
merge_dataframes <- function(wtd_df, data_df) {
  # Convert TIMESTAMP to POSIXct (handling time zone differences)
  wtd_df <- wtd_df %>% 
    mutate(TIMESTAMP = ymd_hms(TIMESTAMP, tz = "UTC"))
  data_df <- data_df %>% 
    mutate(TIMESTAMP = ymd_hms(TIMESTAMP))
  # Perform full join to keep all records
  merged_df <- full_join(data_df, wtd_df, by = "TIMESTAMP")
  return(merged_df)
}

# Loop through the matching years
merged_list <- list()
years <- c(2020, 2021, 2022, 2023)

for (i in seq_along(years)) {
  year <- years[i]
  wtd_name <- paste0("Way3_UnileverTowerStation_", year)
  if (wtd_name %in% names(way3_wtd_uni_t)) {
    way3_data[[i+2]] <- merge_dataframes(way3_wtd_uni_t[[wtd_name]], way3_data[[i + 2]])  # Adjust index
  }
}


for (i in seq_along(years)) {
  year <- years[i]
  wtd_name <- paste0("Way3_UnileverTowerStation_", year)
  if (wtd_name %in% names(way4_wtd_uni_t)) {
    way4_data[[i+2]] <- merge_dataframes(way4_wtd_uni_t[[wtd_name]], way4_data[[i + 2]])  # Adjust index
  }
}


###Add wt_corr_AVG_cm_fixedBias to the dataframes of way 3
# Loop through each dataframe in the list
for (i in seq_along(way3_data)) {
  # Check if the column exists in the current dataframe
  if (!"wt_corr_AVG_cm_fixedBias" %in% colnames(way3_data[[i]])) {
    # Add the column with NA values
    way3_data[[i]]$wt_corr_AVG_cm_fixedBias <- NA
    print(paste("Added 'wt_corr_AVG_cm_fixedBias' column with NA values to dataframe", i))
  } else {
    print(paste("Column 'wt_corr_AVG_cm_fixedBias' is already present in dataframe", i))
  }
}

###Add wt_corr_AVG_cm_fixedBias to the dataframes of way 4
# Loop through each dataframe in the list
for (i in seq_along(way4_data)) {
  # Check if the column exists in the current dataframe
  if (!"wt_corr_AVG_cm_fixedBias" %in% colnames(way4_data[[i]])) {
    # Add the column with NA values
    way4_data[[i]]$wt_corr_AVG_cm_fixedBias <- NA
    print(paste("Added 'wt_corr_AVG_cm_fixedBias' column with NA values to dataframe", i))
  } else {
    print(paste("Column 'wt_corr_AVG_cm_fixedBias' is already present in dataframe", i))
  }
}

#################################################################
###############Adding WTD, Canopy Height, and LAI units #########
###################################################################
# Add the new column "wt_corr_AVG_cm_fixedBias" with unit "[m]" to filtered_unit_row
filtered_unit_row$wt_corr_AVG_cm_fixedBias <- "[m]"
filtered_unit_row$canopy_height <- "[m]"
filtered_unit_row$LAI_corrected<- "[m^2/m^2]"
filtered_unit_row$LAI_corrected_gapfilled<- "[m^2/m^2]"
filtered_unit_row$canopy_height_gapfilled <- "[m]"


# Verify the updated filtered_unit_row
print(filtered_unit_row)


#############################################################################
##########################MERGING CanopyHeight With WAY 3 and 4 #####################
############################################################################
###Data comes from the script LAICanopyHeight.R script
# Function to merge two data frames by TIMESTAMP
merge_pairwise <- function(df1, df2) {
  df1 <- df1 %>% mutate(TIMESTAMP = ymd_hms(TIMESTAMP, tz = "UTC"))
  df2 <- df2 %>% mutate(TIMESTAMP = ymd_hms(TIMESTAMP, tz = "UTC"))
  
  merged_df <- full_join(df1, df2, by = "TIMESTAMP")
  return(merged_df)
}


# Pairing them manually (adjust the year accordingly)
way3_data[[1]] <- merge_pairwise(way3_data[[1]], Way3CHLAIlist$Way32018CHLAI_processed)
way3_data[[2]] <- merge_pairwise(way3_data[[2]], Way3CHLAIlist$Way32019CHLAI_processed)
way3_data[[3]] <- merge_pairwise(way3_data[[3]], Way3CHLAIlist$Way32020CHLAI_processed)
way3_data[[4]] <- merge_pairwise(way3_data[[4]], Way3CHLAIlist$Way32021CHLAI_processed)
way3_data[[5]] <- merge_pairwise(way3_data[[5]], Way3CHLAIlist$Way32022CHLAI_processed)
way3_data[[6]] <- merge_pairwise(way3_data[[6]], Way3CHLAIlist$Way32023CHLAI_processed)
way3_data[[7]] <- merge_pairwise(way3_data[[7]], Way3CHLAIlist$Way32024CHLAI_processed)



way4_data[[1]] <- merge_pairwise(way4_data[[1]], Way4CHLAIlist$Way42018CHLAI_processed)
way4_data[[2]] <- merge_pairwise(way4_data[[2]], Way4CHLAIlist$Way42019CHLAI_processed)
way4_data[[3]] <- merge_pairwise(way4_data[[3]], Way4CHLAIlist$Way42020CHLAI_processed)
way4_data[[4]] <- merge_pairwise(way4_data[[4]], Way4CHLAIlist$Way42021CHLAI_processed)
way4_data[[5]] <- merge_pairwise(way4_data[[5]], Way4CHLAIlist$Way42022CHLAI_processed)
way4_data[[6]] <- merge_pairwise(way4_data[[6]], Way4CHLAIlist$Way42023CHLAI_processed)
way4_data[[7]] <- merge_pairwise(way4_data[[7]], Way4CHLAIlist$Way42024CHLAI_processed)


#######LE DATA###############
#######2021################

plot(way3_data[[4]]$TIMESTAMP, way3_data[[4]]$LE)
# Specify the file path
file_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalRawData/LE2021/eddypro_LE_Fix_2023_12_06_full_output_2023-12-06T144759_adv.csv"

# Read the CSV file
# Skip the first row (header) and use the second row as the new header
LEdata2021 <- read.csv(file_path, skip = 1, header = TRUE)
# Combine the 'date' and 'time' columns into a single 'TIMESTAMP' column
LEdata2021$TIMESTAMP <- as.POSIXct(paste(LEdata2021$date, LEdata2021$time), format = "%Y-%m-%d %H:%M")
# Remove the first row from LEdata2021_filtered
LEdata2021 <- LEdata2021 %>%
  slice(-1)  # Removes the first row
# Select only the required columns from LEdata2021
LEdata2021 <- LEdata2021 %>%
  select(TIMESTAMP, LE_scf, LE_strg, LE, qc_LE, un_LE)
#### Does LE of way3_data[[1]] have the non-growing season data, we need that
way3_data[[4]]$TIMESTAMP
# # Drop the specific columns from way3_data[[4]]
# way3_data[[4]] <- way3_data[[4]] %>%
#   select(-LE_scf, -LE_strg, -LE, -qc_LE, -un_LE)
# Perform left join to bring in the summer data (from LEdata2021) to way3_data[[4]]
way3_data[[4]] <- way3_data[[4]] %>%
  left_join(LEdata2021, by = "TIMESTAMP")
# Replace summer values with those from LEdata2021 (if they exist)
way3_data[[4]] <- way3_data[[4]] %>%
  mutate(
    LE_scf = ifelse(!is.na(LE_scf.y), LE_scf.y, LE_scf.x), # Replace summer LE_scf
    LE_strg = ifelse(!is.na(LE_strg.y), LE_strg.y, LE_strg.x), # Replace summer LE_strg
    LE = ifelse(!is.na(LE.y), LE.y, LE.x), # Replace summer LE
    qc_LE = ifelse(!is.na(qc_LE.y), qc_LE.y, qc_LE.x), # Replace summer qc_LE
    un_LE = ifelse(!is.na(un_LE.y), un_LE.y, un_LE.x) # Replace summer un_LE
  ) %>%
  select(-ends_with(".x"), -ends_with(".y")) # Remove redundant columns

plot(way3_data[[4]]$TIMESTAMP, way3_data[[4]]$LE)
plot(LEdata2021$TIMESTAMP, LEdata2021$LE)



####################################
############LE2020##################
####################################


#####Rename Way 4 water table depth column WTD_Avg as Lvl_m_Avg
# Rename WTD_Avg to Lvl_m_Avg in Way4 data
# Loop through each dataframe in way4_data and rename WTD_Avg to Lvl_m_Avg
way4_data <- lapply(way4_data, function(df) {
  # Drop any columns named "Lvl_m_Avg"
  df <- df[, !colnames(df) %in% "Lvl_m_Avg", drop = FALSE]
  # Rename "WTD_Avg" to "Lvl_m_Avg"
  colnames(df) <- gsub("WTD_Avg", "Lvl_m_Avg", colnames(df))
  return(df)
})

##### 2022, 2023, and 2024 has -9999 instead of NaN
# Replace -9999 with NaN in the 5th and 6th elements of the list
# Convert all columns to numeric before applying the ifelse function

way3_data[[5]] <- as.data.frame(lapply(way3_data[[5]], function(x) {
  ifelse(x %in% c("-9999", "-9999.0"), NaN, x)
}))

way3_data[[6]] <- as.data.frame(lapply(way3_data[[6]], function(x) {
  ifelse(x %in% c("-9999", "-9999.0"), NaN, x)
}))

way3_data[[7]] <- as.data.frame(lapply(way3_data[[7]], function(x) {
  ifelse(x %in% c("-9999", "-9999.0"), NaN, x)
}))

way4_data[[5]] <- as.data.frame(lapply(way4_data[[5]], function(x) {
  ifelse(x %in% c("-9999", "-9999.0"), NaN, x)
}))

way4_data[[6]] <- as.data.frame(lapply(way4_data[[6]], function(x) {
  ifelse(x %in% c("-9999", "-9999.0"), NaN, x)
}))

way4_data[[7]] <- as.data.frame(lapply(way4_data[[7]], function(x) {
  ifelse(x %in% c("-9999", "-9999.0"), NaN, x)
}))


#####Save the way3 data
# Directory path
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way3"
#Years corresponding to the dataframes
years <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024)
# Save each dataframe as a CSV file
for (i in seq_along(way3_data)) {
  file_name <- paste0("way3_", years[i], ".csv")
  file_path <- file.path(output_dir, file_name)
  write.csv(way3_data[[i]], file_path, row.names = FALSE)  # Save without row indices
  print(paste("Saved", file_path))
}
print("All files saved successfully.")

#### Save the way4 data
# Directory path
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/Way4"
#Years corresponding to the dataframes
years <- c(2018, 2019, 2020, 2021, 2022, 2023, 2024)
# Save each dataframe as a CSV file
for (i in seq_along(way4_data)) {
  file_name <- paste0("way4_", years[i], ".csv")
  file_path <- file.path(output_dir, file_name)
  write.csv(way4_data[[i]], file_path, row.names = FALSE)  # Save without row indices
  print(paste("Saved", file_path))
}
print("All files saved successfully.")

#####UnitFiles####
# Define the directory path
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/InputLocalProcessedData/MasterFiles/unitdf"
# Save the filtered_unit_row dataframe as Unit_df.csv
file_path <- file.path(output_dir, "Unit_df.csv")
write.csv(filtered_unit_row, file_path, row.names = FALSE)

ncol(filtered_unit_row)














# 
# ### Merge with Caanopy Height Data
# # Merge 2019 data (CHDLWay3[[1]] into way3_data[[2]])
# way3_data[[2]] <- merge(way3_data[[2]], CHDLWay3[[1]], by = "TIMESTAMP", all = TRUE)
# # Merge 2020 data (CHDLWay3[[2]] into way3_data[[3]])
# way3_data[[3]] <- merge(way3_data[[3]], CHDLWay3[[2]], by = "TIMESTAMP", all = TRUE)
# # Merge 2021 data (CHDLWay3[[3]] into way3_data[[4]])
# way3_data[[4]] <- merge(way3_data[[4]], CHDLWay3[[3]], by = "TIMESTAMP", all = TRUE)
# # Merge 2022 data (CHDLWay3[[4]] into way3_data[[5]])
# way3_data[[5]] <- merge(way3_data[[5]], CHDLWay3[[4]], by = "TIMESTAMP", all = TRUE)
# # Merge 2023 data (CHDLWay3[[5]] into way3_data[[6]])
# way3_data[[6]] <- merge(way3_data[[6]], CHDLWay3[[5]], by = "TIMESTAMP", all = TRUE)
# # Merge 2024 data (CHDLWay3[[6]] into way3_data[[7]])
# way3_data[[7]] <- merge(way3_data[[7]], CHDLWay3[[6]], by = "TIMESTAMP", all = TRUE)

