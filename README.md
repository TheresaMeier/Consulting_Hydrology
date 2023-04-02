# Compound events in Bavaria: Multivariate analysis of climatological and hydrological drivers of low-flow events
In this repository, all codes used to analyze the Consulting Project of Nikita Paschan and Theresa Meier are summarized. In order to reproduce the analysis, the following steps are necessary:
## [Codes](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Codes)
#### Data Pre-Processing
In folder [**01_Data_Preparation**](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Codes/01_Data_Preparation) functions and main file for transforming the given data into a suitable format are stored. For reproduction, the raw data is required.
#### Descriptive Analysis
The [**02_Descriptive_Analysis**](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Codes/02_Descriptive%20Analysis) folder contains two R files that are used to generate all the analyses and plots for describing the data. 
#### Modelling Procedure
The codes to perform logistic regressions, model evaluation and visualization are all combined in the [**03_Modelling**](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Codes/03_Modelling) folder. The ROC-Analysis to obtain an appropriate threshold for prediction is stored in [**04_ROC_Analysis**](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Codes/04_ROC_Analysis).
#### Further Analyses
In [**05_Clustering**](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Codes/05_Clustering) the code to reproduce the K-means clustering is provided, while in [**06_Scenarios**](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Codes/06_Scenarios) the R files for performing and visualizing the climate scenario analysis are stored.
## Documents
In this folder, the slides for the oral presentation, the final report and the IWSM 4-Pager can be found.
## [Models](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Models)
All 1959 models stored as RDS files are provided for each member and each season separately.
## [Plots](https://github.com/TheresaMeier/Consulting_Hydrology/tree/master/Plots)
The plots of all analyses, i.e. descriptive analyses, effect plots, ROC analysis, climate scenarios and clustering, are contained in this folder.
