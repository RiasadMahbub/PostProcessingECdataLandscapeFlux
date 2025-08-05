
library(ggplot2)

# Set the directory path
directory_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/AFguidedSubmitted/Way3"

# List all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
data_frames <- lapply(csv_files, read.csv)

# Optionally, name the list elements using the file names (without the path)
names(data_frames) <- basename(csv_files)

# Print the names of the files and preview the first few rows of each data frame
for (name in names(data_frames)) {
  cat("Preview of", name, ":\n")
  print(head(data_frames[[name]]))
  cat("\n")
}

# Convert the TIMESTAMP_START column to POSIXct for all data frames
data_frames <- lapply(data_frames, function(df) {
  if ("TIMESTAMP_START" %in% colnames(df)) { # Check if the column exists
    # Ensure TIMESTAMP_START is character
    df$TIMESTAMP_START <- as.character(df$TIMESTAMP_START)
    # Convert to POSIXct
    df$TIMESTAMP_START <- as.POSIXct(df$TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC")
  }
  return(df)
})

# Confirm the conversion for all data frames
for (i in 1:length(data_frames)) {
  cat("Preview of TIMESTAMP_START in data frame", names(data_frames)[i], ":\n")
  print(head(data_frames[[i]]$TIMESTAMP_START))
  cat("\n")
}

library(ggplot2)

# Function to plot and save data for Way3
plot_way3_data <- function(way3_data, base_dir) {
  # Convert -9999 to NA
  way3_data <- lapply(way3_data, function(df) {
    df[df == -9999] <- NA
    return(df)
  })
  
  # Extract years directly from file names
  years <- unique(gsub("Way3 (\\d{4})\\.csv", "\\1", names(way3_data)))
  
  # Loop through each year
  for (year in years) {
    # Access the dataframe for the corresponding year
    df <- way3_data[[paste0("Way3 ", year, ".csv")]]
    
    # Create the output directory for the year if it doesn't exist
    output_dir <- file.path(base_dir, year)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Get the column names except TIMESTAMP_START
    column_names <- names(df)[names(df) != "TIMESTAMP_START"]
    
    # Loop through each column and plot against TIMESTAMP_START
    for (col in column_names) {
      # Create the plot with points (instead of lines)
      p <- ggplot(df, aes_string(x = "TIMESTAMP_START", y = col)) +
        geom_point(na.rm = TRUE) + # Exclude NA values in the plot
        labs(
          title = paste("Plot of", col, "in", year),
          x = "Timestamp",
          y = col
        ) +
        theme_minimal()
      
      # Save the plot to the corresponding directory
      ggsave(
        filename = file.path(output_dir, paste0(col, "_", year, ".jpeg")),
        plot = p,
        width = 8,
        height = 4
      )
    }
  }
}

# Base directory for saving Way3 plots
base_dir_way3 <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way3CheckafterAnalysis/"

# Apply the function for Way3
plot_way3_data(data_frames, base_dir_way3)

data_frames[[1]]$S

library(dplyr)
library(ggplot2)
library(lubridate)

process_and_plot <- function(file_name, way_data, other_data, output_dir) {
  # Access the data for the given file
  file_data <- way_data[[file_name]]
  
  # Replace missing value placeholders (-9999, "-9999", etc.) with NA
  file_data <- file_data %>%
    mutate(across(
      everything(),
      ~ case_when(
        . %in% c(-9999.0, -9999, "-9999.0", "-9999") ~ NA_real_,
        TRUE ~ as.numeric(.)
      )
    ))
  
  # Process the `way` data
  file_data <- file_data %>%
    mutate(
      TIMESTAMP = ymd_hms(TIMESTAMP_START),
      DOY = yday(TIMESTAMP),
      HOUR = hour(TIMESTAMP) + ifelse(minute(TIMESTAMP) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_"),
      PPFD_IN_Adjusted = PPFD_IN / 2.02  # Conversion factor
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
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    )
  
  # Process the potential SWR data (US-HRA_HH_2017.csv)
  directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PotentialSWR"
  filename <- "US-HRA_HH_2017.csv"
  file_path <- file.path(directory, filename)
  swr_data <- read.csv(file_path) # Read the CSV file
  
  # Convert TIMESTAMP_START and TIMESTAMP_END to datetime format
  swr_data$TIMESTAMP_START <- ymd_hm(swr_data$TIMESTAMP_START, tz = "UTC")
  swr_data$TIMESTAMP_END <- ymd_hm(swr_data$TIMESTAMP_END, tz = "UTC")
  swr_data$HOUR <- hour(swr_data$TIMESTAMP_START) # Extract the hour from TIMESTAMP_START
  
  # Process SWR data
  swr_data <- swr_data %>%
    mutate(
      TIMESTAMP_START = ymd_hms(TIMESTAMP_START),
      DOY = yday(TIMESTAMP_START),
      HOUR = hour(TIMESTAMP_START) + ifelse(minute(TIMESTAMP_START) == 30, 0.5, 0),
      DOY_HOUR = paste(DOY, HOUR, sep = "_")
    ) %>%
    select(-any_of(c("DOY", "HOUR", "TIMESTAMP_START", "TIMESTAMP_END")))  # Drop unnecessary columns
  
  # Merge the `way` data with `swr_data` using DOY_HOUR
  merged_data <- file_data %>%
    inner_join(other_data, by = "DOY_HOUR") %>%
    inner_join(swr_data, by = "DOY_HOUR") %>%
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
  
  # Plot SW_IN_Avg, SW_IN_POT, and PPFD_IN_Adjusted for each 15-day period
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



# Set the output directory for the plots
output_dir <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Figure/Way3ProcessedPlots"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Placeholder for other data (adjust accordingly)
# Replace `other_data` with your actual dataset for merging
other_data <- data.frame(
  DOY_HOUR = paste0(rep(1:365, each = 24), "_", rep(0:23, times = 365)),
  SW_IN_POT = runif(365 * 24, 0, 1000)
)

# Apply the function to all Way3 files
processed_data <- lapply(names(data_frames), function(file) {
  process_and_plot(file, data_frames, other_data, output_dir)
})

# Combine processed data for all files into one list or data frame as needed
names(processed_data) <- names(data_frames)

table(merged_data$PERIOD, useNA = "ifany")
nrow(merged_data)
merged_data <- merged_data %>% filter(!is.na(PERIOD))
merged_data %>%
  mutate(PERIOD = ceiling(DOY / 15)) %>%
  select(DOY, PERIOD) %>%
  distinct() %>%
  head()






library(ggplot2)

# Set the directory path
directory_path <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/AFguidedSubmitted/Way3"

# List all CSV files in the directory
csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)

# Read all CSV files into a list of data frames
data_frames <- lapply(csv_files, read.csv)

# Optionally, name the list elements using the file names (without the path)
names(data_frames) <- basename(csv_files)

# Print the names of the files and preview the first few rows of each data frame
for (name in names(data_frames)) {
  cat("Preview of", name, ":\n")
  print(head(data_frames[[name]]))
  cat("\n")
}

# Convert the TIMESTAMP_START column to POSIXct for all data frames
data_frames <- lapply(data_frames, function(df) {
  if ("TIMESTAMP_START" %in% colnames(df)) { # Check if the column exists
    # Ensure TIMESTAMP_START is character
    df$TIMESTAMP_START <- as.character(df$TIMESTAMP_START)
    # Convert to POSIXct
    df$TIMESTAMP_START <- as.POSIXct(df$TIMESTAMP_START, format = "%Y%m%d%H%M", tz = "UTC")
  }
  return(df)
})

##################################
##################################
### single file
# Access the data for the given file
##################################
##################################
file_data <- data_frames[[1]]

# Replace missing value placeholders (-9999, "-9999", etc.) with NA
file_data <- file_data %>%
  mutate(across(
    everything(),
    ~ case_when(
      . %in% c(-9999.0, -9999, "-9999.0", "-9999") ~ NA_real_,
      TRUE ~ as.numeric(.)
    )
  ))

# Process the `way` data (step-by-step):
# Convert TIMESTAMP_START to datetime format (already in UTC)
file_data$TIMESTAMP <- ymd_hms(file_data$TIMESTAMP_START, tz = "UTC")
# Extract Day of Year (DOY) from TIMESTAMP
file_data$DOY <- yday(file_data$TIMESTAMP)
# Extract Hour and Adjust for 30-minute intervals (if minute is 30, add 0.5)
file_data$HOUR <- hour(file_data$TIMESTAMP) + ifelse(minute(file_data$TIMESTAMP) == 30, 0.5, 0)
# Combine DOY and HOUR into a single column DOY_HOUR
file_data$DOY_HOUR <- paste(file_data$DOY, file_data$HOUR, sep = "_")

file_data <- file_data %>%
  mutate(
    PPFD_IN_Adjusted = PPFD_IN / 2.02  # Use the correct column name
  )

# Adjust the timestamp for daylight saving time
file_data$TIMESTAMP <- case_when(
  file_data$DOY >= 60 & file_data$DOY <= 315 ~ file_data$TIMESTAMP - hours(1),
  TRUE ~ file_data$TIMESTAMP
)

# Recalculate DOY, HOUR, and DOY_HOUR after adjusting timestamp
file_data$DOY <- yday(file_data$TIMESTAMP)
file_data$HOUR <- hour(file_data$TIMESTAMP) + ifelse(minute(file_data$TIMESTAMP) == 30, 0.5, 0)
file_data$DOY_HOUR <- paste(file_data$DOY, file_data$HOUR, sep = "_")

# Process the potential SWR data (US-HRA_HH_2017.csv)
directory <- "C:/Users/rbmahbub/Documents/RProjects/AmerifluxDataSubmission_LandscapeFlux/Data/PotentialSWR"
filename <- "US-HRA_HH_2017.csv"
file_path <- file.path(directory, filename)
swr_data <- read.csv(file_path)  # Read the CSV file

# Convert TIMESTAMP_START and TIMESTAMP_END to datetime format
swr_data$TIMESTAMP_START <- ymd_hm(swr_data$TIMESTAMP_START, tz = "UTC")
swr_data$TIMESTAMP_END <- ymd_hm(swr_data$TIMESTAMP_END, tz = "UTC")
swr_data$HOUR <- hour(swr_data$TIMESTAMP_START)  # Extract the hour from TIMESTAMP_START

# Process SWR data step-by-step
swr_data$TIMESTAMP_START <- ymd_hms(swr_data$TIMESTAMP_START)
swr_data$DOY <- yday(swr_data$TIMESTAMP_START)
swr_data$HOUR <- hour(swr_data$TIMESTAMP_START) + ifelse(minute(swr_data$TIMESTAMP_START) == 30, 0.5, 0)
swr_data$DOY_HOUR <- paste(swr_data$DOY, swr_data$HOUR, sep = "_")


# Merge the `way` data with `swr_data` using DOY_HOUR
merged_data <- file_data %>%
  inner_join(swr_data, by = "DOY_HOUR") %>%
  mutate(
    PERIOD = ceiling(DOY/ 15)  # Create non-overlapping 15-day periods
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

# Plot SW_IN_Avg, SW_IN_POT, and PPFD_IN_Adjusted for each 15-day period
ggplot(max_diurnal_composite, aes(x = HOUR)) +
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
plot
# Save the plot
ggsave(
  filename = file.path(output_dir, paste0(gsub(".csv", "", file_name), "_RadiationPlot.jpeg")),
  plot = plot,
  width = 10, height = 6
)

# Return the processed data
return(merged_data)




#--------------------------------------------------------
#--------------------------------------------------------
# Scatterplots for Way3
for (i in 1:7) {
  df <- all_processed_data_way3[[i]]
  
  plot(df$WS, df$USTAR,
       main = paste("Way3 - Year", i, "WS vs USTAR"),
       xlab = "Wind Speed (WS)",
       ylab = "Friction Velocity (USTAR)",
       col = "darkgreen", pch = 16)
}

# Scatterplots for Way4
for (i in 1:7) {
  df <- all_processed_data_way4[[i]]
  
  plot(df$WS, df$USTAR,
       main = paste("Way4 - Year", i, "WS vs USTAR"),
       xlab = "Wind Speed (WS)",
       ylab = "Friction Velocity (USTAR)",
       col = "darkorange", pch = 16)
}



#--------------------------------------------------------
#--------------------------------------------------------
# Scatterplots for Way3
for (i in 1:7) {
  df <- way3_data[[i]]
  
  plot(df$wind_speed, df$u_,
       main = paste("Way3 - Year", i, "WS vs USTAR"),
       xlab = "Wind Speed (WS)",
       ylab = "Friction Velocity (USTAR)",
       col = "darkgreen", pch = 16)
}

# Scatterplots for Way4
for (i in 1:7) {
  df <- way4_data[[i]]
  
  plot(df$wind_speed, df$u_,
       main = paste("Way4 - Year", i, "WS vs USTAR"),
       xlab = "Wind Speed (WS)",
       ylab = "Friction Velocity (USTAR)",
       col = "darkorange", pch = 16)
}


