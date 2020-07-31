import os
from glob import glob
import pandas as pd
import numpy as np
from scipy import signal, stats
from math import sqrt

data_path = "/Volumes/LV_HD_2/Accelerometry/training_sessions_IMU/"
acc_files = [os.path.basename(x) for x in glob(data_path + "*.csv")]
acc_files = sorted(acc_files)

# Initialize an empty dictionary to store the variables of interest
output = {"ID": [],
          "date": [],
          "n_peaks": []}

for i in range(0, len(acc_files)):
    filename = acc_files[i]
    fullpath = data_path + filename

    print("Processing file", i + 1, "out of", len(acc_files), "-", filename)
    data = pd.read_csv(fullpath, skiprows=10)
    # Put each axis into a ndarray
    aX = data.iloc[:, 1].to_numpy()
    aY = data.iloc[:, 2].to_numpy()
    aZ = data.iloc[:, 3].to_numpy()

    # Create the lowpass filter
    samp_freq = 100  # sampling frequency (Hz)
    N = 4  # filter order
    cutoff = 20  # cut-off frequency (Hz)
    fnyq = samp_freq / 2  # Nyquist frequency
    Wn = cutoff / fnyq
    sos = signal.butter(N, Wn, btype="low", output="sos")

    aX = signal.sosfiltfilt(sos, aX)
    aY = signal.sosfiltfilt(sos, aY)
    aZ = signal.sosfiltfilt(sos, aZ)
    # Compute resultant acceleration
    aR = np.sqrt(aX ** 2 + aY ** 2 + aZ ** 2)

    # Find acceleration peaks above 4.9g
    min_height = 4.9
    min_dist = 0.4 * samp_freq

    peaks, _ = signal.find_peaks(aR, height=min_height, distance=min_dist)

    output["ID"].append(filename[:3])
    output["date"].append(filename[5:15])
    output["n_peaks"].append(len(peaks))

    df = pd.DataFrame(output)

# Compute mean and confidence interval of the number of peaks
std = df["n_peaks"].std()
n = len(df.index)

mean = df["n_peaks"].mean()
lower_ci = mean - stats.t.ppf(0.975, n - 1) * (std / sqrt(n))
upper_ci = mean + stats.t.ppf(0.975, n - 1) * (std / sqrt(n))
