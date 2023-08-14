import pandas as pd
import numpy as np
import os
from datetime import datetime
from scipy import stats
import matplotlib.pyplot as plt
from numpy import nan
from matplotlib import pyplot as plt
from matplotlib.dates import DateFormatter
import matplotlib.dates as mdates
import matplotlib.cbook as cbook
import matplotlib as matplotlibZ
import matplotlib.lines as mlines
from scipy import stats
from sympy import S, symbols, printing
import matplotlib
import os
import sklearn.metrics as metrics

# Path to the directory containing the CSV files
directory = r'/storage/rbmahbub/Way4_Data/Way4_HorstCorr'
export_dir = r"/storage/rbmahbub/Exported_Image"

# Initialize an empty list to store individual DataFrames
dfs = []

# Iterate over each file in the directory
for filename in os.listdir(directory):
    if filename.endswith('.csv'):  # Consider only CSV files
        file_path = os.path.join(directory, filename)
        df = pd.read_csv(file_path, header = None)
        dfs.append(df)

# Concatenate all DataFrames into a single DataFrame
Way4 = pd.concat(dfs, ignore_index=True)

## Merge Datetime
Way4["Datetime-merged"] = Way4[6] + " " + Way4[7]
Way4["Datetime-merged"] = pd.to_datetime(Way4["Datetime-merged"], format="%Y-%m-%d %H:%M:%S:%f")
## Count the number of observations greather than 2000
count = (Way4.iloc[:, 11] > 2000).sum()
# Print the count to a text file
count_file = "observation_countway4.txt"
with open(count_file, "w") as file:
    file.write("Number of observations in column 11 greater than 2000: {}\n".format(count))
# Filter observations between 8 AM and 20 PM
Way4_filtered = Way4[(Way4["Datetime-merged"].dt.hour >= 8) & (Way4["Datetime-merged"].dt.hour < 20)]
filtered_data = Way4_filtered[(Way4_filtered[11] > 0) & (Way4_filtered[11] < 2000)]

# Plot the histogram
plt.figure(figsize=(10, 6))  # Set the desired figure size (width, height) in inches
plt.hist(filtered_data[11], bins=100)  # Adjust the number of bins as needed
plt.xlabel("H20 Molar density (mmol/m$^3$)")
plt.ylabel("Frequency")
plt.title("Way 4 H2o molar density histogram of 20 hz data 2021")
# Disable exponential notation on the x-axis
plt.ticklabel_format(style='plain', axis='x', useOffset=False)
plt.ticklabel_format(style='plain', axis='y', useOffset=False)
# Save the figure
plt.savefig(export_dir + '/H2Omolardensity2Way4.jpeg', dpi=500)


# Plot the scatterplot
plt.figure(figsize=(10, 6))  # Set the desired figure size (width, height) in inches
plt.scatter(filtered_data["Datetime-merged"], filtered_data[11])
plt.xlabel("Datetime")
plt.ylabel("H2O molar density")
plt.ylim(-5, 2500)
# Add a legend
plt.legend(['Moving Average'], loc='upper right')
plt.title("Way 4 H2o molar density time series of 20 hz data 2021")
plt.savefig(export_dir + '/H2OmolardensityscatterplotWay4.jpeg', dpi=500)


## export the daily counts as csv file
# Assuming your datetime column is named 'Datetime-merged'
filtered_data['Datetime-merged'] = pd.to_datetime(filtered_data['Datetime-merged'], format='%Y-%m-%d %H:%M:%S.%f')
# Extract the date portion from the datetime column
filtered_data['Date'] = filtered_data['Datetime-merged'].dt.date
# Count the number of data points for each day
daily_counts = filtered_data.groupby('Date').size()
# Print the counts for each day
print(daily_counts)
# Save the daily counts as a CSV file
daily_counts.to_csv(export_dir + '/daily_countsWay4.csv', header=True)


# Assuming column 11 is named 'Column11'
# Convert the 'Datetime-merged' column to datetime format if it's not already in datetime format
filtered_data['Datetime-merged'] = pd.to_datetime(filtered_data['Datetime-merged'], format='%Y-%m-%d %H:%M:%S.%f')
# Extract the date portion from the datetime column
filtered_data['Date'] = filtered_data['Datetime-merged'].dt.date
# Calculate the daily average of column 11
daily_average = filtered_data.groupby('Date')[11].mean()
# Print the daily average values
daily_average.to_csv(export_dir + '/daily_averageWay4.csv', header=True)



# Daily average'
plt.figure(figsize=(10, 6))
# Convert the 'Datetime-merged' column to datetime format if it's not already in datetime format
filtered_data['Datetime-merged'] = pd.to_datetime(filtered_data['Datetime-merged'], format='%Y-%m-%d %H:%M:%S.%f')
# Extract the date portion from the datetime column
filtered_data['Date'] = filtered_data['Datetime-merged'].dt.date
# Calculate the daily average of column 11
daily_average = filtered_data.groupby('Date')[11].mean()
# Plot the daily average values
plt.plot(daily_average)
plt.xlabel("Date")
# Add a legend
plt.legend(['Moving Average'], loc='upper right')
plt.ylabel("H20 Molar density (mmol/m$^3$)")
plt.title("Time series plot Daily Average of H20 Molar density (mmol/m$^3$)  of Way 4  in 2021")
#plt.show()
plt.savefig(export_dir + '/Dailyaverage.jpeg', dpi=200) # Save the figure in the specified directory


# 8 day moving window average'
# Convert the 'Datetime-merged' column to datetime format if it's not already in datetime format
filtered_data['Datetime-merged'] = pd.to_datetime(filtered_data['Datetime-merged'], format='%Y-%m-%d %H:%M:%S.%f')
# Extract the date portion from the datetime column
filtered_data['Date'] = filtered_data['Datetime-merged'].dt.date
# Calculate the daily average of column 11
daily_average = filtered_data.groupby('Date')[11].mean()
# Calculate the moving average with a window size of 8 days (including first 4 and last 4 observations)
moving_average = daily_average.rolling(window=8, min_periods=1, center=True).mean()
# Plot the moving average values
plt.plot(daily_average.index, moving_average)
plt.xlabel("Date")
plt.ylabel("H20 Molar density (mmol/m$^3$)")
# Add a legend
plt.legend(['Moving Average'], loc='upper right')
plt.title(" Time series plot Daily 8 day moving Average of H20 Molar density (mmol/m$^3$)  of Way 4  in 2021")
plt.savefig(export_dir + '/8dayDailyaverageWay4.jpeg', dpi=200) # Save the figure in the specified directory


# 8daycsv export
# Convert the 'Datetime-merged' column to datetime format if it's not already in datetime format
filtered_data['Datetime-merged'] = pd.to_datetime(filtered_data['Datetime-merged'], format='%Y-%m-%d %H:%M:%S.%f')
# Extract the date portion from the datetime column
filtered_data['Date'] = filtered_data['Datetime-merged'].dt.date
# Calculate the daily average of column 11
daily_average = filtered_data.groupby('Date')[11].mean()
# Print the daily average values
moving_average.to_csv(export_dir + '/8daily_averageWay4.csv', header=True)




############
## 5 percentile plotting###3
plt.figure(figsize=(10, 6))
# Calculate the daily 25th percentile of column 11
daily_5th_percentile = filtered_data.groupby('Date')[11].quantile(0.05)
# Plot the daily 25th percentile values
plt.plot(daily_5th_percentile)
plt.xlabel("Date")
plt.ylabel("Daily 5th Percentile of H20 Molar density Way 4(mmol/m$^3$)")
# Add a legend
plt.legend(['Moving Average'], loc='upper right')
plt.title("Daily 5th Percentile of H20 Molar density Way 4(mmol/m$^3$) over Time")
# Save the figure as a JPEG file
plt.savefig(export_dir + '/Daily_5th_percentileWay4.jpeg', dpi=100)
daily_5th_percentile.to_csv(export_dir + '/daily_5th_percentileWay4.csv', header=True)

## 25 percentile plotting###3
plt.figure(figsize=(10, 6))
# Calculate the daily 25th percentile of column 11
daily_25th_percentile = filtered_data.groupby('Date')[11].quantile(0.25)
# Plot the daily 25th percentile values
plt.plot(daily_25th_percentile)
plt.xlabel("Date")
plt.ylabel("Daily 25th Percentile of H20 Molar density Way 4 (mmol/m$^3$)")
# Add a legend
plt.legend(['Moving Average'], loc='upper right')
plt.title("Daily 25th Percentile of H20 Molar density Way 4 (mmol/m$^3$) over Time")
# Save the figure as a JPEG file
plt.savefig(export_dir + '/Daily_25th_PercentileWay4.jpeg', dpi=100)
daily_25th_percentile.to_csv(export_dir + '/daily_25th_percentileWay4.csv', header=True)


# Calculate the daily 75th percentile of column 11
daily_75th_percentile = filtered_data.groupby('Date')[11].quantile(0.75)
# Plot the daily 75th percentile values
plt.plot(daily_75th_percentile)
plt.xlabel("Date")
plt.ylabel("Daily 75th Percentile of H20 Molar density Way 4(mmol/m$^3$)")
plt.title("Daily 75th Percentile of H20 Molar density Way 4(mmol/m$^3$) over Time")
# Add a legend
plt.legend(['Moving Average'], loc='upper right')
# Save the figure as a JPEG file
plt.savefig(export_dir + '/Daily_75th_PercentileWay4.jpeg', dpi=100)
daily_75th_percentile.to_csv(export_dir + '/daily_75th_percentileWay4.csv', header=True)


# Calculate the daily 95th percentile of column 11
daily_95th_percentile = filtered_data.groupby('Date')[11].quantile(0.95)
# Plot the daily 95th percentile values
plt.plot(daily_95th_percentile)
plt.xlabel("Date")
plt.ylabel("Daily 95th Percentile of H20 Molar density Way 4(mmol/m$^3$)")
plt.title("Daily 95th Percentile of H20 Molar density Way 4(mmol/m$^3$) over Time")
# Add a legend
plt.legend(['Moving Average'], loc='upper right')
# Save the figure as a JPEG file
plt.savefig(export_dir + '/Daily_95th_PercentileWay4.jpeg', dpi=100)
daily_95th_percentile.to_csv(export_dir + '/daily_95th_percentileWay4.csv', header=True)



# Calculate the daily median of column 11
daily_median = filtered_data.groupby('Date')[11].median()
# Plot the daily median values
plt.plot(daily_median)
plt.xlabel("Date")
plt.ylabel("Daily Median of H20 Molar density Way 4 (mmol/m$^3$)")
plt.title("Daily Median of H20 Molar density Way 4(mmol/m$^3$) over Time")
# Add a legend
plt.legend(['Moving Average'], loc='upper right')
# Save the figure as a JPEG file
plt.savefig(export_dir + '/daily_median.jpeg', dpi=100)
daily_median.to_csv(export_dir + '/daily_medianWay4.csv', header=True)
