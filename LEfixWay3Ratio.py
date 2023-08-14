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

input_directory = r'D:\EC_GHG\2020-2021\Summer\Way3 HorstCorr'

# Path to the directory containing the CSV files
directory = r'F:\EC_GHG\2020-2021\Summer\Way3_HorstCorrSample'
 
# Initialize an empty list to store individual DataFrames
dfs = []

# Iterate over each file in the directory
for filename in os.listdir(directory):
    if filename.endswith('.csv'):  # Consider only CSV files
        file_path = os.path.join(directory, filename)
        df = pd.read_csv(file_path, header = None)
        dfs.append(df)

# Concatenate all DataFrames into a single DataFrame
way3 = pd.concat(dfs, ignore_index=True)

way3["Datetime-merged"] = way3[6] + " " + way3[7]
way3["Datetime-merged"] = pd.to_datetime(way3["Datetime-merged"], format="%Y-%m-%d %H:%M:%S:%f")

