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
from scipy.optimize import fsolve


# Path to the directory containing the CSV files
directory = r'/storage/rbmahbub/Way3_Data/Way3_HorstCorr'
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
Way3 = pd.concat(dfs, ignore_index=True)

## Merge Datetime
Way3["Datetime-merged"] = Way3[6] + " " + Way3[7]
Way3["Datetime-merged"] = pd.to_datetime(Way3["Datetime-merged"], format="%Y-%m-%d %H:%M:%S:%f")

## Count the number of observations greather than 2000
count = (Way3.iloc[:, 11] > 2000).sum()
# Print the count to a text file
count_file = "observation_count.txt"
with open(count_file, "w") as file:
    file.write("Number of observations in column 11 greater than 2000: {}\n".format(count))


# Filter observations between 8 AM and 20 PM
Way3_filtered = Way3[(Way3["Datetime-merged"].dt.hour >= 8) & (Way3["Datetime-merged"].dt.hour < 20)]
way3_filtered = Way3_filtered[(Way3_filtered[11] > 0) & (Way3_filtered[11] < 2000)]
# Calculate the day of the year and create the 'DOY' column
way3_filtered['DOY'] = way3_filtered['Datetime-merged'].dt.dayofyear


import numpy as np
from scipy.optimize import fsolve
import pandas as pd

def equation(x, a, b, c, density, pressure):
    return (a*x + b*x**2 + c*x**3) * pressure - density

def fsolve(a, b, c, density, pressure):
    return np.roots([c, b, a, -density / pressure])

# Define the parameters
a = 5108.30
b = 3967550
c = -281375000
pressure = 100
spring2021span =1.09329
spring2021zero = 0.858539
factorycorrectedspan =  1.0063
factorycorrectedzero = 0.8870

# Define the function to calculate x for each density value
def calculate_x_spring2021_span_zero(density):
    x_solution = fsolve(a, b, c, density, pressure)
    return x_solution[2]  # Extract the first element from the solution array

# Apply the function to the density column of the dataframe
way3_filtered['x'] = way3_filtered[11].apply(calculate_x_spring2021_span_zero)
way3_filtered['x']

def calculate_transmittance_spring2021_span_zero(x):
    transmittance = 1 -((x*pressure)/(spring2021zero*spring2021span))
    return transmittance
# Apply the function to the x column of the dataframe
way3_filtered['transmittance'] = way3_filtered['x'].apply(calculate_transmittance_spring2021_span_zero)

# Define the function to calculate x from transmittance
def calculate_x(transmittance):
    x = (1 - transmittance) * (factorycorrectedzero * factorycorrectedspan) / pressure
    return x

# Define the function to calculate density from x
def calculate_density(x):
    density = (a * x + b * x**2 + c * x**3) * pressure
    return density

# Calculate x from transmittance
way3_filtered['x'] = way3_filtered['transmittance'].map(calculate_x)

# Calculate density from x
way3_filtered['density'] = way3_filtered['x'].map(calculate_density)
# Calculate density from x
way3_filtered['density'] = way3_filtered['x'].map(calculate_density)
# Plot scatter plot with colors based on DOY and increased size
plt.figure(figsize=(12, 8))
plt.scatter(way3_filtered[11], way3_filtered['density'], c=way3_filtered['DOY'], cmap='viridis', s=50)
plt.plot(way3_filtered[11], way3_filtered[11], color='red', linestyle='--')  # Adding the 1:1 line
plt.xlabel('Incorrect $H_2O$  Molar density based on spring 2021 span and zero' ,fontsize=14)
plt.ylabel('Correct $H_2O$  Molar density based on factory corrected 2021 span and zero',  fontsize=14)
plt.title('Scatter plot of Incorrect  and correct H2O Molar density')
plt.colorbar(label='Day of the Year')
plt.grid(True)
plt.savefig(export_dir + '/correctedh20andincorrectedh20density.jpeg', dpi=200) # Save the figure in the specified directory
