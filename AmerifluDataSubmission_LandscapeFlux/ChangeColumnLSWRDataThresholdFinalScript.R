##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
##### CHANGE THE COLUMN NAMES ##### 
##### ##### ##### ##### ##### ##### 
##### ##### ##### ##### ##### ##### 
library(tidyverse)
library(dplyr)
library(lubridate)

# Define the directories
way3_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way3"
way4_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/Way4"

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
# Remove the first row from the 5th and 6th data frames
# Step 2: Filter and rename columns
filtered_columns <- c("TIMESTAMP", "TIMESTAMP_START", "TIMESTAMP_END", "x_70_", "x_90_", "x_peak", 
                      "ch4_mole_fraction", "ch4_mixing_ratio", "co2_mole_fraction", "co2_mixing_ratio", 
                      "co2_flux", "ch4_flux", "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_flux", 
                      "H", "LE", "H_strg", "LE_strg", "air_pressure", "RH", "sonic_temperature", 
                      "qc_co2_flux", "qc_ch4_flux", "qc_H", "qc_LE", "qc_Tau", "co2_var", "co2_strg", 
                      "ch4_strg", "u_var", "v_var", "w_var", "wind_dir", "wind_speed", "max_wind_speed", 
                      "X_z_d__L", "air_temperature", "VPD", "LW_IN_Avg", "LW_OUT_Avg", "PAR_IN_Avg", 
                      "PAR_OUT_Avg", "SW_IN_Avg", "SW_OUT_Avg", "SWC_2_1_1_Avg", "L", "Tau", "TS_mean.2.", "u_")

# Filter and rename
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

######################################
##########AIRPRESSURE################
######################################
for (i in 1:7) {
  # Select only TIMESTAMP and air_pressure from way4_data
  way4_subset <- way4_data[[i]] %>%
    select(TIMESTAMP, air_pressure)
  
  # Perform a left join with way3_data based on TIMESTAMP
  way3_data[[i]] <- way3_data[[i]] %>%
    left_join(way4_subset, by = "TIMESTAMP")
}
way3_data[[1]]$air_pressure.y

# Function to process each dataset (for TIMESTAMP and derived columns)
process_data <- function(data) {
  # Create TIMESTAMP_START and TIMESTAMP_END columns
  data <- cbind(TIMESTAMP_START = NA, TIMESTAMP_END = NA, data)
  
  # Convert TIMESTAMP column to POSIXct format
  data$TIMESTAMP <- ymd_hms(data$TIMESTAMP)
  
  # Create TIMESTAMP_START in the desired format
  data$TIMESTAMP_START <- format(data$TIMESTAMP, "%Y%m%d%H%M")
  
  # Create TIMESTAMP_END by adding 30 minutes to TIMESTAMP and formatting it
  data$TIMESTAMP_END <- format(data$TIMESTAMP + minutes(30), "%Y%m%d%H%M")
  
  # Create additional columns: HOUR, MONTH, DAY_OF_YEAR
  data$HOUR <- hour(data$TIMESTAMP)
  data$MONTH <- month(data$TIMESTAMP)
  data$DOY <- yday(data$TIMESTAMP)
  
  return(data)
}

# Process Way3 and Way4 data
way3_processed_data <- lapply(way3_data, process_data)
way4_processed_data <- lapply(way4_data, process_data)
unit_row_processed<-process_data(unit_row)


# Mapping of equivalent column names
column_mapping <- list(
  "x_70_" = c("x_70."),
  "x_90_" = c("x_90."),
  "co2_flux" = c("NEE"),
  "RH" = c("rH"),
  "wind_dir" = c("WD_EC"),
  "X_z_d__L" = c("X.z.d..L"),
  "air_temperature" = c("Tair"),
  "u_" = c("ustar"),
  "SW_IN_Avg" = c( "Rg")  # Multiple possible names
)

# Function to apply column name mapping
apply_column_mapping <- function(data) {
  for (old_name in names(column_mapping)) {
    new_name <- column_mapping[[old_name]]
    if (new_name %in% names(data) && !old_name %in% names(data)) {
      names(data)[names(data) == new_name] <- old_name
    }
  }
  return(data)
}

 # Apply column mapping Way3 and Way4 data
way3_CM_data <- lapply(way3_processed_data, apply_column_mapping)
way4_CM_data <- lapply(way4_processed_data, apply_column_mapping)
unit_row_CM<-apply_column_mapping(unit_row_processed)

# Count the number of columns with the name "Lvl_m_Avg"
num_duplicates <- sum(colnames(way4_CM_data[[7]]) == "Lvl_m_Avg")


# Verify the change for the first few dataframes
head(way4_data[[1]])
head(way4_data[[7]])


# Function to filter and rename columns
filter_and_rename <- function(data) {
  data_filtered <- data %>%
    select(
      TIMESTAMP, TIMESTAMP_START, TIMESTAMP_END, x_70_, `x_90_`, x_peak, ch4_mole_fraction, 
      ch4_mixing_ratio, co2_mole_fraction, co2_mixing_ratio, co2_flux, ch4_flux, 
      h2o_mole_fraction, h2o_mixing_ratio, h2o_flux, H, LE, H_strg, LE_strg, 
      air_pressure, RH, sonic_temperature, qc_co2_flux, qc_ch4_flux, qc_H, qc_LE, 
      qc_Tau, co2_var, co2_strg, ch4_strg, u_var, v_var, w_var, wind_dir, wind_speed, 
      max_wind_speed, X_z_d__L, air_temperature, VPD, LW_IN_Avg, LW_OUT_Avg, 
      PAR_IN_Avg, PAR_OUT_Avg, SW_IN_Avg, SW_OUT_Avg, SWC_2_1_1_Avg, L, Tau, TS_mean.2., Lvl_m_Avg, u_
    )
  
  # Rename filtered columns
  data_renamed <- data_filtered %>%
    rename(
      TIMESTAMP = TIMESTAMP, TIMESTAMP_START = TIMESTAMP_START, TIMESTAMP_END = TIMESTAMP_END, 
      FETCH_70 = `x_70_`, FETCH_90 = `x_90_`, FETCH_MAX = x_peak, CH4 = ch4_mole_fraction, 
      CH4_MIXING_RATIO = ch4_mixing_ratio, CO2 = co2_mole_fraction, CO2_MIXING_RATIO = co2_mixing_ratio, 
      FC = co2_flux, FCH4 = ch4_flux, H2O = h2o_mole_fraction, H2O_MIXING_RATIO = h2o_mixing_ratio, 
      FH2O = h2o_flux, H = H, LE = LE, SH = H_strg, SLE = LE_strg, PA = air_pressure, RH = RH, 
      T_SONIC = sonic_temperature, FC_SSITC_TEST = qc_co2_flux, FCH4_SSITC_TEST = qc_ch4_flux, 
      H_SSITC_TEST = qc_H, LE_SSITC_TEST = qc_LE, TAU_SSITC_TEST = qc_Tau, CO2_SIGMA = co2_var, 
      SC = co2_strg, SCH4 = ch4_strg, U_SIGMA = u_var, V_SIGMA = v_var, W_SIGMA = w_var, 
      WD = wind_dir, WS = wind_speed, WS_MAX = max_wind_speed, ZL = X_z_d__L, TA = air_temperature, 
      VPD = VPD, LW_IN = LW_IN_Avg, LW_OUT = LW_OUT_Avg, PPFD_IN = PAR_IN_Avg, PPFD_OUT = PAR_OUT_Avg, 
      SW_IN = SW_IN_Avg, SW_OUT = SW_OUT_Avg, MO_LENGTH = L, TAU = Tau,  SWC_1_1_1=SWC_2_1_1_Avg, WTD=Lvl_m_Avg,USTAR = u_,
      TS_1_1_1=TS_mean.2.
    )
  
  return(data_renamed)
}


# Define the required column names
required_columns <- c(
  "TIMESTAMP", "TIMESTAMP_START", "TIMESTAMP_END", "x_70_", "x_90_", "x_peak", 
  "ch4_mole_fraction", "ch4_mixing_ratio", "co2_mole_fraction", "co2_mixing_ratio", 
  "co2_flux", "ch4_flux", "h2o_mole_fraction", "h2o_mixing_ratio", "h2o_flux", 
  "H", "LE", "H_strg", "LE_strg", "air_pressure", "RH", "sonic_temperature", 
  "qc_co2_flux", "qc_ch4_flux", "qc_H", "qc_LE", "qc_Tau", "co2_var", "co2_strg", 
  "ch4_strg", "u_var", "v_var", "w_var", "wind_dir", "wind_speed", "max_wind_speed", 
  "X_z_d__L", "air_temperature", "VPD", "LW_IN_Avg", "LW_OUT_Avg", "PAR_IN_Avg", 
  "PAR_OUT_Avg", "SW_IN_Avg", "SW_OUT_Avg", "SWC_2_1_1_Avg", "L", "Tau", "TS_mean.1.", "Lvl_m_Avg", "u_"
)

# Get the column names of way4_CM_data[[5]]
existing_columns <- names(way4_CM_data[[5]])

# Identify missing columns
missing_columns <- setdiff(required_columns, existing_columns)

# Print missing columns
cat("Missing columns in way4_CM_data[[5]]:\n")
print(missing_columns)


# Apply filtering and renaming
way3_filtered_renamed <- lapply(way3_CM_data, filter_and_rename)
way4_filtered_renamed <- lapply(way4_CM_data, filter_and_rename)
filtered_unit_row<-filter_and_rename(unit_row_CM)

###potential LSWR
directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PotentialSWR"
filename <- "US-HRA_HH_2017.csv"
file_path <- file.path(directory, filename)
data <- read.csv(file_path) # Read the CSV file

# Convert TIMESTAMP_START and TIMESTAMP_END to datetime format
data$TIMESTAMP_START <- ymd_hm(data$TIMESTAMP_START, tz = "UTC")
data$TIMESTAMP_END <- ymd_hm(data$TIMESTAMP_END, tz = "UTC")
data$HOUR <- hour(data$TIMESTAMP_START)# Extract the hour from TIMESTAMP_START
data <- data %>%
  mutate(
    TIMESTAMP_START = ymd_hms(TIMESTAMP_START),
    DOY = yday(TIMESTAMP_START),
    HOUR = hour(TIMESTAMP_START) + ifelse(minute(TIMESTAMP_START) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )
### Drop DOY so that it does not cause DOY.x and DOY.y
data <- data %>%
  select(-any_of(c("DOY", "HOUR", "TIMESTAMP_START", "TIMESTAMP_END")))

#### process and plot all the data
process_and_plot <- function(file_name, way_data, other_data, output_dir) {
  # Access the data for the given file
  file_data <- way_data[[file_name]]
  
  # Process the way data
  file_data <- file_data %>%
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    ) %>%
    filter(DOY >= 1 & DOY <= 365)
  
  # Adjust the timestamp for daylight saving time
  file_data <- file_data %>%
    mutate(
      TIMESTAMP = case_when(
        DOY >= 60 & DOY <= 315 ~ TIMESTAMP - hours(1),
        TRUE ~ TIMESTAMP
      ),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_"),
      PPFD_IN_Adjusted = PPFD_IN / 2.02  # 1010 umol m-1 s-1 = 500 Wm-2, 2.02 conversion factor
    )
  
  # Merge with the other dataset using DOY_HOUR
  merged_data <- file_data %>%
    inner_join(other_data, by = "DOY_HOUR") %>%
    mutate(
      PERIOD = ceiling(DOY / 15)  # Create non-overlapping 15-day periods
    )
  
  # Plot SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg for each 15-day period
  plot <- ggplot(merged_data, aes(x = HOUR)) +
    geom_line(aes(y = SW_IN, color = "SW_IN"), size = 1) +
    geom_line(aes(y = SW_IN_POT, color = "SW_IN_POT"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PPFD_IN_Adjusted, color = "PPFD_IN"), size = 1, linetype = "dotdash") +
    facet_wrap(~ PERIOD, scales = "free_y") +
    labs(
      title = paste("Radiation by Hour and Period for", file_name),
      x = "Hour of the Day",
      y = expression("Radiation (Wm"^-2*")"),
      color = "Legend"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(
    filename = file.path(output_dir, paste0(gsub(".csv", "", file_name), "_RadiationPlot.jpeg")),
    plot = plot,
    width = 10, height = 6
  )
  
  # Return the processed data
  return(merged_data)
}
# Define a function to process and plot data for a single file
process_and_plot <- function(file_name, way_data, other_data, output_dir) {
  # Access the data for the given file
  file_data <- way_data[[file_name]]
  
  # Process the `way` data
  file_data <- file_data %>%
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_"),
      PPFD_IN_Adjusted = PPFD_IN / 2.02  # 1010 umol m-1 s-1 = 500 Wm-2, 2.02 conversion factor
    ) %>%
    filter(DOY >= 1 & DOY <= 365)
  
  # Adjust the timestamp for daylight saving time
  file_data <- file_data %>%
    mutate(
      TIMESTAMP = case_when(
        DOY >= 60 & DOY <= 315 ~ TIMESTAMP - hours(1),
        TRUE ~ TIMESTAMP
      ),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_"),
      
    )
  
  # Merge with the other dataset using DOY_HOUR
  merged_data <- file_data %>%
    inner_join(other_data, by = "DOY_HOUR") %>%
    mutate(
      PERIOD = ceiling(DOY / 15)  # Create non-overlapping 15-day periods
    )
  # Define a custom function to handle max with NaN values
  max_with_nan <- function(x) {
    if (all(is.na(x))) {
      return(NA_real_)
    } else {
      return(max(x, na.rm = TRUE))
    }
  }
  # Calculate the maximum diurnal composite for each hour within each 15-day period
  max_diurnal_composite <- merged_data %>%
    group_by(PERIOD, HOUR) %>%
    summarize(
      SW_IN_Avg_max = max_with_nan(SW_IN),
      SW_IN_POT_max = max_with_nan(SW_IN_POT),
      PPFD_IN_Adjusted_max = max_with_nan(PPFD_IN_Adjusted)
    ) %>%
    ungroup()
  
  # Plot SW_IN_Avg, SW_IN_POT, and PAR_IN_Avg for each 15-day period
  plot <- ggplot(max_diurnal_composite, aes(x = HOUR)) +
    geom_line(aes(y = SW_IN_Avg_max, color = "SW_IN"), size = 1) +
    geom_line(aes(y = SW_IN_POT_max, color = "SW_IN_POT"), size = 1, linetype = "dashed") +
    geom_line(aes(y = PPFD_IN_Adjusted_max, color = "PPFD_IN"), size = 1, linetype = "dotdash") +
    facet_wrap(~ PERIOD, scales = "free_y") +
    labs(
      title = paste("Radiation by Hour and Period for", file_name),
      x = "Hour of the Day",
      y = expression("Radiation (Wm"^-2*")"),
      color = "Legend"
    ) +
    theme_minimal()
  
  # Save the plot
  ggsave(
    filename = file.path(output_dir, paste0(gsub(".csv", "", file_name), "_RadiationPlot.jpeg")),
    plot = plot,
    width = 10, height = 6
  )
  
  # Return the processed data
  return(merged_data)
}




# Directories to save plots
way3_output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/ShortwaveRadiation/AllPlot/Way3"
way4_output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/ShortwaveRadiation/AllPlot/Way4"

# Create directories if they don't exist
dir.create(way3_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(way4_output_dir, recursive = TRUE, showWarnings = FALSE)

names(way3_filtered_renamed) <- way3_files_to_read# Assign names to the processed Way3 data
names(way4_filtered_renamed) <- way4_files_to_read# Assign names to the processed Way4 data

way3_filtered_renamed[[5]]$PPFD_IN<-as.numeric(way3_filtered_renamed[[5]]$PPFD_IN)
way3_filtered_renamed[[5]]$SW_IN<-as.numeric(way3_filtered_renamed[[5]]$SW_IN)

way3_filtered_renamed[[6]]$PPFD_IN<-as.numeric(way3_filtered_renamed[[6]]$PPFD_IN)
way3_filtered_renamed[[6]]$SW_IN<-as.numeric(way3_filtered_renamed[[6]]$SW_IN)

way4_filtered_renamed[[5]]$PPFD_IN<-as.numeric(way4_filtered_renamed[[5]]$PPFD_IN)
way4_filtered_renamed[[5]]$SW_IN<-as.numeric(way4_filtered_renamed[[5]]$SW_IN)
way4_filtered_renamed[[5]]$PPFD_IN[way4_filtered_renamed[[5]]$PPFD_IN > 2000] <- NaN

way4_filtered_renamed[[6]]$PPFD_IN<-as.numeric(way4_filtered_renamed[[6]]$PPFD_IN)
way4_filtered_renamed[[6]]$SW_IN<-as.numeric(way4_filtered_renamed[[6]]$SW_IN)

way3_filtered_renamed[[7]]$PPFD_IN<-as.numeric(way3_filtered_renamed[[7]]$PPFD_IN)
way3_filtered_renamed[[7]]$SW_IN<-as.numeric(way3_filtered_renamed[[7]]$SW_IN)

way4_filtered_renamed[[7]]$PPFD_IN<-as.numeric(way4_filtered_renamed[[7]]$PPFD_IN)
way4_filtered_renamed[[7]]$SW_IN<-as.numeric(way4_filtered_renamed[[7]]$SW_IN)



########################################
#### Readjustment of the alignment####
########################################
## Way 3 misalignment Fix
#1-4 = 1 hour less; 5-7 = 30 minutes less
# Fix the TIMESTAMP in all_processed_data_way3[[5]]
#Way 3 PPFD_IN is misaligned after the alignment in 2023 (1,2,3,4,5,6,7)
way3_filtered_renamed[[5]] <- way3_filtered_renamed[[5]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 60 ~ TIMESTAMP - minutes(150),  # Subtract 1 hour for periods 1-4
      DOY >= 61 & DOY <= 105 ~ TIMESTAMP - hours(1),  # Subtract 30 minutes for periods 5-7
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

#Way 4 PPFD_IN is misaligned after the alignment in 2018 (11), 2019 (22, 23, 24, 25), 2020 (1), 2024 (1,2)#
## way4 PPFD_IN is misaligned after the alignment in 2018 (11), 2019 (22, 23, 24, 25), 2020 (1), 2024 (1,2)
#2018
way4_filtered_renamed[[1]] <- way4_filtered_renamed[[1]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 165 & DOY <= 195 ~ TIMESTAMP - minutes(30),  # Subtract 1 hour for periods 1-4
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )
#2019
way4_filtered_renamed[[2]] <- way4_filtered_renamed[[2]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 30 ~ TIMESTAMP + hours(1), 
      DOY >= 315 & DOY <= 365 ~ TIMESTAMP - hours(2),  # Subtract 1 hour for periods 1-4
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

#2020
way4_filtered_renamed[[3]] <- way4_filtered_renamed[[3]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 15 ~ TIMESTAMP - minutes(30),  # Subtract 1 hour for periods 1-4
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

#2024
way4_filtered_renamed[[6]] <- way4_filtered_renamed[[6]] %>%
  mutate(
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    # Adjust TIMESTAMP based on the PERIOD
    TIMESTAMP = case_when(
      DOY >= 1 & DOY <= 30 ~ TIMESTAMP - minutes(30),  # Subtract 1 hour for periods 1-4
      DOY >= 60 & DOY <= 75 ~ TIMESTAMP+minutes(30),
      TRUE ~ TIMESTAMP  # Keep unchanged for other periods
    ),
    # Recalculate DOY and HOUR based on the updated TIMESTAMP
    DOY = yday(TIMESTAMP),
    HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
    DOY_HOUR = paste(DOY, HOUR, sep = "_")
  )

# Process all files in Way3
all_processed_data_way3 <- list()
for (way_name in names(way3_filtered_renamed)) {
  all_processed_data_way3[[way_name]] <- tryCatch(
    {
      process_and_plot(
        file_name = way_name, 
        way_data = way3_filtered_renamed, 
        other_data = data, 
        output_dir = way3_output_dir
      )
    },
    error = function(e) {
      message(sprintf("Error processing file: %s", way_name))
      message(e)
      NULL  # Return NULL for failed processing
    }
  )
}

# Process all files in Way4
all_processed_data_way4 <- list()
for (way_name in names(way4_filtered_renamed)) {
  all_processed_data_way4[[way_name]] <- tryCatch(
    {
      process_and_plot(
        file_name = way_name, 
        way_data = way4_filtered_renamed, 
        other_data = data, 
        output_dir = way4_output_dir
      )
    },
    error = function(e) {
      message(sprintf("Error processing file: %s", way_name))
      message(e)
      NULL  # Return NULL for failed processing
    }
  )
}


# Function to convert all columns (except specified ones) to numeric
convert_columns_to_numeric <- function(data) {
  # Specify columns to exclude
  exclude_columns <- c("TIMESTAMP_START", "TIMESTAMP_END", "TIMESTAMP")
  
  # Convert columns to numeric if they are not in exclude_columns
  data <- data %>%
    mutate(across(.cols = -all_of(exclude_columns), .fns = ~ as.numeric(.)))
  
  return(data)
}

# Apply the function to the specified dataframes
all_processed_data_way3[[5]] <- convert_columns_to_numeric(all_processed_data_way3[[5]])
all_processed_data_way3[[6]] <- convert_columns_to_numeric(all_processed_data_way3[[6]])
all_processed_data_way3[[7]] <- convert_columns_to_numeric(all_processed_data_way3[[7]])

all_processed_data_way4[[5]] <- convert_columns_to_numeric(all_processed_data_way4[[5]])
all_processed_data_way4[[6]] <- convert_columns_to_numeric(all_processed_data_way4[[6]])
all_processed_data_way4[[7]] <- convert_columns_to_numeric(all_processed_data_way4[[7]])



#### Remove the PPFD values greater than SW_IN_POT
## Way 4 The PPFD_IN of way 4 is having anomalous values in 2018 (12), 2019 (3,7,8,9,10, 14, 15, 16, 17, 18, 2020 (2, 3, 4,5, 6,7,8, 16,17, 18, 19, 20, 21), 2021 (2, 8, 9), 2023 (1, 11, 12, 13, 14, 15, 16, 17, 18, 22, 23, 24), 2024(1,2,3,4)
########################################
########PLOTTING ALL THE COLUMNS########
########################################

###WAY 3 2018######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("CO2", "CO2_MIXING_RATIO", "FC", "FETCH_MAX", "SC", "SLE", "TAU", "ZL", "CO2_SIGMA", "MO_LENGTH")
# Replace outliers with NaN in the same columns
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way3[[1]])) {
    lower_bound <- quantile(all_processed_data_way3[[1]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way3[[1]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way3[[1]][[col]][all_processed_data_way3[[1]][[col]] < lower_bound |
                                          all_processed_data_way3[[1]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 3 2019######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("CH4", "CH4_MIXING_RATIO", "CO2_SIGMA", "TA", "TAU", "U_SIGMA","CO2_MIXING_RATIO","FC","FCH4","H","H2O","H2O_MIXING_RATIO","LE","MO_LENGTH", "PA")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way3[[2]])) {
    lower_bound <- quantile(all_processed_data_way3[[2]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way3[[2]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way3[[2]][[col]][all_processed_data_way3[[2]][[col]] < lower_bound |
                                          all_processed_data_way3[[2]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 3 2020######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("FC", "FCH4", "LE", "SWC", "V_SIGMA", "U_SIGMA", "ZL", "MO_LENGTH")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way3[[3]])) {
    lower_bound <- quantile(all_processed_data_way3[[3]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way3[[3]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way3[[3]][[col]][all_processed_data_way3[[3]][[col]] < lower_bound |
                                          all_processed_data_way3[[3]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 3 2021######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("CO2_SIGMA", "FC", "FCH4", "FH2O", "H", "LE", "MO_LENGTH", "SLE", "TAU", "U_SIGMA", "V_SIGMA", "ZL")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way3[[4]])) {
    lower_bound <- quantile(all_processed_data_way3[[4]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way3[[4]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way3[[4]][[col]][all_processed_data_way3[[4]][[col]] < lower_bound |
                                          all_processed_data_way3[[4]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 3 2023######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("CH4", "CH4_MIXING_RATIO", "FC", "FCH4", "FH2O", "H", "LE", "LW_OUT", "MO_LENGTH", "SCH4", "SLE", "TAU", "U_SIGMA", "V_SIGMA", "ZL")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way3[[5]])) {
    lower_bound <- quantile(all_processed_data_way3[[5]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way3[[5]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way3[[5]][[col]][all_processed_data_way3[[5]][[col]] < lower_bound |
                                          all_processed_data_way3[[5]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}



###WAY 3 2024######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("CH4", "CH4_MIXING_RATIO", "CO2_SIGMA", "FC", "FCH4", "FH2O", "H", "LE", "MO_LENGTH", "SCH4", "SLE", "ZL")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way3[[6]])) {
    lower_bound <- quantile(all_processed_data_way3[[6]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way3[[6]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way3[[6]][[col]][all_processed_data_way3[[6]][[col]] < lower_bound |
                                          all_processed_data_way3[[6]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}




###WAY 4 2018######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("CO2_SIGMA", "FC", "FCH4", "FH2O", "H", "LE", "MO_LENGTH","SC", "SLE", "TAU", "V_SIGMA", "U_SIGMA", "W_SIGMA", "ZL")

# Replace outliers with NaN in the same columns
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way4[[1]])) {
    lower_bound <- quantile(all_processed_data_way4[[1]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way4[[1]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way4[[1]][[col]][all_processed_data_way4[[1]][[col]] < lower_bound |
                                          all_processed_data_way4[[1]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 4 2019######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("FC", "FCH4","FETCH_MAX", "FH2O", "MO_LENGTH", "SLE", "ZL")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way4[[2]])) {
    lower_bound <- quantile(all_processed_data_way4[[2]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way4[[2]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way4[[2]][[col]][all_processed_data_way4[[2]][[col]] < lower_bound |
                                          all_processed_data_way4[[2]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 4 2020######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("FC", "MO_LENGTH", "ZL")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way4[[3]])) {
    lower_bound <- quantile(all_processed_data_way4[[3]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way4[[3]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way4[[3]][[col]][all_processed_data_way4[[3]][[col]] < lower_bound |
                                          all_processed_data_way4[[3]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 4 2021######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("CO2_MIXING_RATIO", "CO2", "CO2_SIGMA", "FC", "FH2O", "H","LE", "MO_LENGTH", "SC","SH", "ZL")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way4[[4]])) {
    lower_bound <- quantile(all_processed_data_way4[[4]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way4[[4]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way4[[4]][[col]][all_processed_data_way4[[4]][[col]] < lower_bound |
                                          all_processed_data_way4[[4]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 4 2023######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("CH4_MIXING_RATIO", "CH4", "FCH4", "H", "LE", "MO_LENGTH","SCH4", "SLE", "TA", "W_SIGMA", "ZL")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way4[[5]])) {
    lower_bound <- quantile(all_processed_data_way4[[5]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way4[[5]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way4[[5]][[col]][all_processed_data_way4[[5]][[col]] < lower_bound |
                                          all_processed_data_way4[[5]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

###WAY 4 2024######
# Define the columns for which percentiles need to be calculated
columns_to_process <- c("H", "LE", "LW_IN", "MO_LENGTH",  "PPFD_IN", "SLE", "TA")
for (col in columns_to_process) {
  if (col %in% names(all_processed_data_way4[[6]])) {
    lower_bound <- quantile(all_processed_data_way4[[6]][[col]], 0.025, na.rm = TRUE)
    upper_bound <- quantile(all_processed_data_way4[[6]][[col]], 0.975, na.rm = TRUE)
    all_processed_data_way4[[6]][[col]][all_processed_data_way4[[6]][[col]] < lower_bound |
                                          all_processed_data_way4[[6]][[col]] > upper_bound] <- NaN
  } else {
    warning(paste("Column", col, "not found in the dataframe."))
  }
}

################################
########QUALITY CONTROL########
################################
plot_way_data <- function(way_data, base_dir, way_name) {
  # Convert -9999 to NA
  way_data <- lapply(way_data, function(df) {
    df[df == -9999] <- NA
    return(df)
  })
  
  # List of years for processing
  years <- c("2018", "2019", "2020", "2021", "2023", "2024")
  
  # Loop through each year
  for (year in years) {
    # Access the dataframe for the corresponding year
    df <- way_data[[paste0(way_name, " ", year, ".csv")]]
    # Create the output directory for the year if it doesn't exist
    output_dir <- paste0(base_dir, year)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Get the column names except TIMESTAMP
    column_names <- names(df)[names(df) != "TIMESTAMP"]
    
    # Loop through each column and plot against TIMESTAMP
    for (col in column_names) {
      # Create the plot with points (instead of lines), ensuring TIMESTAMP is properly referenced
      p <- ggplot(df, aes_string(x = "TIMESTAMP", y = col)) +
        geom_point(na.rm = FALSE) + # Keep NA values (they will show as gaps in the plot)
        labs(title = paste("Plot of", col, "in", year), x = "Timestamp", y = col) +
        theme_minimal()
      
      # Save the plot to the corresponding directory
      ggsave(filename = paste0(output_dir, "/", col, "_", year, ".jpeg"), plot = p, width = 8, height = 4)
    }
  }
}

plot_way_data_with_units <- function(way_data, base_dir, way_name, unit_row) {
  # Convert -9999 to NA
  way_data <- lapply(way_data, function(df) {
    df[df == -9999] <- NA
    return(df)
  })
  
  # List of years for processing
  years <- c("2018", "2019", "2020", "2021", "2022", "2023", "2024")
  
  # Loop through each year
  for (year in years) {
    # Access the dataframe for the corresponding year
    df <- way_data[[paste0(way_name, " ", year, ".csv")]]
    # Create the output directory for the year if it doesn't exist
    output_dir <- paste0(base_dir, year)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Get the column names except TIMESTAMP
    column_names <- names(df)[names(df) != "TIMESTAMP"]
    
    # Loop through each column and plot against TIMESTAMP
    for (col in column_names) {
      # Find the unit for the current column
      unit <- filtered_unit_row[[col]]
      unit_label <- ifelse(is.na(unit), "", paste0(" (", unit, ")"))
      
      # Create the plot with points (instead of lines), ensuring TIMESTAMP is properly referenced
      p <- ggplot(df, aes_string(x = "TIMESTAMP", y = col)) +
        geom_point(na.rm = FALSE) + # Keep NA values (they will show as gaps in the plot)
        labs(
          title = paste("Plot of", col, "in", year, "from", way_name), 
          x = "Timestamp",
          y = paste0(col, unit_label)
        ) +
        theme_minimal()
      
      # Save the plot to the corresponding directory
      ggsave(filename = paste0(output_dir, "/", col, "_", year, ".jpeg"), plot = p, width = 8, height = 4)
    }
  }
}



# Paths where you want to save the plots for Way3 and Way4
base_dir_way3 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way3Columns/"
base_dir_way4 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way4Columns/"

# Apply the function for Way3 and Way4
plot_way_data_with_units(all_processed_data_way3, base_dir_way3, "Way3")
plot_way_data_with_units(all_processed_data_way4, base_dir_way4, "Way4")



####TIMESTAMP START AND END FUNCTION
process_data <- function(data) {
  # Ensure TIMESTAMP is in POSIXct format
  data$TIMESTAMP <- ymd_hms(data$TIMESTAMP)
  
  # Create or replace TIMESTAMP_START and TIMESTAMP_END
  data$TIMESTAMP_START <- format(data$TIMESTAMP, "%Y%m%d%H%M")
  data$TIMESTAMP_END <- format(data$TIMESTAMP + minutes(30), "%Y%m%d%H%M")
  
  # Create or replace additional columns: HOUR, MONTH, DAY_OF_YEAR
  data$HOUR <- hour(data$TIMESTAMP)
  data$MONTH <- month(data$TIMESTAMP)
  data$DOY <- yday(data$TIMESTAMP)
  
  return(data)
}


###TIMESTAMP START AND END AFTER ADJUSTING TIMESTAMP###
all_processed_data_way3 <- lapply(all_processed_data_way3, process_data)
all_processed_data_way4 <- lapply(all_processed_data_way4, process_data)

# Columns to drop
columns_to_drop <- c("DOY", "HOUR", "DOY_HOUR", "SW_IN_POT", "PERIOD", "TIMESTAMP", "PPFD_IN_Adjusted", "MONTH")

# Drop the columns from each data frame in the list
all_processed_data_way3 <- lapply(all_processed_data_way3, function(df) {
  df <- df[, !(colnames(df) %in% columns_to_drop)] # Keep only columns not in the drop list
  return(df)
})


# Assuming `all_processed_data_way3` is a named list containing the data frames
all_processed_data_way4 <- lapply(all_processed_data_way4, function(df) {
  df <- df[, !(colnames(df) %in% columns_to_drop)] # Keep only columns not in the drop list
  return(df)
})

# Define save directories
way3_save_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/AFguidedSubmitted/Way3"
way4_save_directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/AFguidedSubmitted/Way4"

# Function to save filtered data
save_filtered_data <- function(data, filename, directory) {
  data$TIMESTAMP_START <- as.character(data$TIMESTAMP_START)# Timestamp Start
  data$TIMESTAMP_END <- as.character(data$TIMESTAMP_END) ## Timestamp END
  data[is.na(data)] <- -9999 ## null values as -999
  data[is.nan.data.frame(data)] <- -9999
  
  file_path <- file.path(directory, filename)
  write.csv(data, file = file_path, row.names = FALSE)
  
  cat(filename, "saved successfully to", directory, "\n")
}

# Assign names to the processed Way3 data
names(all_processed_data_way3) <- way3_files_to_read

# Assign names to the processed Way4 data
names(all_processed_data_way4) <- way4_files_to_read


# Save Way3 data
for (i in seq_along(all_processed_data_way3)) {
  filename <- names(all_processed_data_way3)[i]
  save_filtered_data(all_processed_data_way3[[i]], filename, way3_save_directory)
}

# Save Way4 data
for (i in seq_along(all_processed_data_way4)) {
  filename <- names(all_processed_data_way4)[i]
  save_filtered_data(all_processed_data_way4[[i]], filename, way4_save_directory)
}


