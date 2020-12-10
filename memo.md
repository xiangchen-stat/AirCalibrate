## 11.13 Initial meeting

* Timeline: Dec 2nd - 6th for 2nd meeting
* Steps: 
	+ Literature review: Compare data source, time/location, methods, conclusions
	+ Write a report
	+ Download data: Year: 2019 and 2020. Location: all US states. Two sources: EPA and [purpleair(ppa)](https://www2.purpleair.com/). Content: pollution, covariates, and meteorological data
	+ Clean, combine data
* Goal: Calibarating low-cost sensor using relular monitor data across US + review of different methods (regression, random forest, neural networks, etc.), possible methodology improvement.
* Data: EPA, purpleair, meteorological data
* Methods: regression, random forest, neural networks, etc.
* Requirement: (a) Reproducible: data downloading details, analysis code, etc. (b) Github: share code/data (c) Quality control of ppa data (in bi2020)


## 12.10 2nd meeting
* Timeline: Dec 21st - 24th for 3rd meeting
* Literature review: 
	+ Use papaers after 2015
	+ Using gases papers for background, and PM2.5 papers for tables in discussion.
* Logistics:
	+ Apply for JHU Computing
	+ Share data by Onedrive
* Data:
	+ Area: CA and NY
	+ Time span: 2019.1.1-2020.12.31
	+ Download EPA and meteorological data. **Note:** Hourly average may represent different periods, 10:00 for 9-10 or 10-11.
	+ Process data: (1) ignore inside data (2) keep all columns in raw data (3) Pivot to wide format
* Goal: (1) Calibarating low-cost sensor using reguloratory monitors data (2) Predicting PM2.5 in other areas to create a concentration map.