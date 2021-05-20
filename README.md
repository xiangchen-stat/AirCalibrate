# AirCalibrate

Author: Xiang Chen

Advisor: Dr. Abhirup Datta

_Department of Biostatistics_

_Johns Hopkins University_

## Overview

This repository provides all the code and data to reproduce the ScM Thesis *Evaluating Machine Learning Methods for Calibrating Low-Cost PM<sub>2.5</sub> Sensor Data*. The full text will be available online soon from [JScholarship](https://jscholarship.library.jhu.edu/).

## Instructions

To run all of the analysis, you need to download all of the files in this repo and keep the folder structures to avoid potiential errors when saving results. It is recommended to open the `/AirCalibrate.Rproj` file to start a new R session and then open `/code/main.R` if you use Rstudio. You need to install `pacman` package before using `p_load()` function to install and load other required packages. When you begin the analysis, please make sure the current working directory of R is at home directory of this repository (`/AirCalibrate/`).

**Note 1: You may need 35G memory or more to run all the analysis at once. It may take around 50-125 hours to finish which depends on the system.**

**Note 2: The code is originally designed to be modulized to reduce the need for memory and time. You can utilize this design by uncommented all of the code lines including `save()` and `load()` functions in the `main.R` file (which is optional). After doing that, you only need to run all the code once and then you can run each section independently. The "section" here means sections in the code with level 1 heading, such as 1., 2., 3., etc.** 

The structure of this repository is as follows: 

* `code`: R code (`main.R`) to import, wrangle, visualize, analyze, and save data.
* `data`: including `raw`, `tidy`, `plot`, and `model` subfolders.
	* `raw`: including raw PM<sub>2.5</sub> and meterological data downloaded directly from [EPA AQS](https://aqs.epa.gov/aqsweb/airdata/download_files.html) and [PurpleAir](https://www2.purpleair.com/). The EPA data is under `EPA` subfolder which is named `hourly_88101_2019.zip` and `hourly_88101_2020.zip`. The PurpleAir data is under `CA` subfolder which is named `CA.zip`.
	* `tidy`: This folder includes `.RData` files generated during the analysis. These data files will be loaded in later parts of the code. It is also the basis of the modulized design.
	* `plot`: This folder includes `.RData` files related to making plots generated during the analysis. 
	* `model`: This folder includes `.RData` files related to modeling generated during the analysis. 
* `figures`: including `map`, `EDA`, and `mba` subfolders.
	* `map`: including map plots in png format.
	* `EDA`: including plots related to exploratory data analysis in png format.
	* `mba`: including PM<sub>2.5</sub> concentration maps generated by [mba package](https://cran.r-project.org/web/packages/MBA/index.html) in png format.
* `results`: including model evaluation results.

## Absrtact

The rapid development of low-cost air pollution sensors has provided great potential for their applications in scientific research, regulatory monitoring, personal health care, and business development. Compared with traditional regulatory (FRM or FEM) PM<sub>2.5</sub> monitors, the low-cost air pollution sensors are more affordable, portable, and easier to maintain thus allowing for ambient air quality monitoring at higher spatial resolution. However, since the accuracy and reliability of low-cost sensors are much lower than regulatory monitors and the performance of sensors differs across environments and low-cost technology types, calibration of low-cost data is of great importance to reduce systematic bias and error. 

This study evaluated the accuracy of PurpleAir low-cost air pollution sensors in California and found a consistent overestimation problem in PM<sub>2.5</sub> which is influenced by environmental factors such as humidity and temperature. Further investigations found the overestimation is also correlated to ambient PM<sub>2.5</sub> concentration. We then compared the performance of six different calibration methods including four different versions of linear regressions (LR), and two machine learning methods — random forest (RF), and gradient boosting machine (GBM). For this evaluation, we used collocated hourly PM<sub>2.5</sub> measurements from the PurpleAir low-cost sensors and EPA monitors for 6 months from October 2019 to March 2020 in California. The GBM outperforms all the remaining models with a root mean squared error (RMSE) of 4.26 µg/m<sup>3</sup> for temporal hold-out set and an RMSE of 5.11 µg/m<sup>3</sup> for the furthest spatial hold-out set. We also conducted a simulation analysis generating data from linear and non-linear calibration equations and GBM performs well under both scenarios. GBM reduced the overestimation significantly, and the calibrated PurpleAir data was in better agreement with EPA monitoring data both temporally and spatially. In conclusion, GBM is an effective and promising method in calibrating low-cost PM<sub>2.5</sub> sensor data. 

