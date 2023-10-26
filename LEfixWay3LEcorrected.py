import pandas as pd
import numpy as np
import os
import datetime as dt
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
import sklearn.metrics as metrics
import math
import glob




# Define the parameters
a = 5108.30
b = 3967550
c = -281375000
pressure = 100
spring2021span =1.09329
spring2021zero = 0.858539
factorycorrectedspan =  1.0063
factorycorrectedzero = 0.8870

# Define your functions (same as before)
def equation(x, a, b, c, density, pressure):
    return (a * x + b * x**2 + c * x**3) * pressure - density

def fsolve(a, b, c, density, pressure):
    root_coeffs = [c, b, a, -density / pressure]
    root_coeffs = np.nan_to_num(root_coeffs)  # Handle NaN and inf values
    return np.roots(root_coeffs)

def calculate_x_spring2021_span_zero(density):
    x_solution = fsolve(a, b, c, density, pressure)
    x_solution = np.nan_to_num(x_solution)
    return x_solution[2]  # Extract the third element from the solution array

def calculate_transmittance_spring2021_span_zero(x):
    transmittance = 1 - ((x * pressure) / (spring2021zero * spring2021span))
    return transmittance

def calculate_x(transmittance):
    x = (1 - transmittance) * (factorycorrectedzero * factorycorrectedspan) / pressure
    return x

def calculate_density(x):
    density = (a * x + b * x**2 + c * x**3) * pressure
    return density

# Directory containing CSV files
input_directory = r'/storage/rbmahbub/Way3_Data/Way3_HorstCorr'
output_directory =  r"/storage/rbmahbub/Way3_Data/Way3_HorstCorr_LEcorrected"


# Get a list of all CSV files in the directory and its subdirectories
csv_files = glob.glob(os.path.join(input_directory, '**', '*.csv'), recursive=True)

# Loop through each CSV file
for csv_file in csv_files:
    # Load the CSV file
    way3_filtered = pd.read_csv(csv_file, header=None)  # Modify the read_csv parameters if needed

    # Apply the functions
    way3_filtered['x'] = way3_filtered[12].apply(calculate_x_spring2021_span_zero)
    way3_filtered['transmittance'] = way3_filtered['x'].apply(calculate_transmittance_spring2021_span_zero)
    way3_filtered['x'] = way3_filtered['transmittance'].map(calculate_x)
    way3_filtered['density'] = way3_filtered['x'].map(calculate_density)

    # Construct the output file path
    output_csv_file = os.path.join(output_directory, os.path.basename(csv_file))

    # Export the modified DataFrame to a new CSV file
    way3_filtered.to_csv(output_csv_file, header=False, index=False)
