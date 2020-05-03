# PhillipsCurve
Empirical testing of relationship between inflation and the unemployment rate (1947-2017)

### Overview
This analysis was conducted and presented for a consulting case study. The case centered around two questions:
- Does William Phillips's theory of the relationship between inflation and unemployment (i.e. the Phillips Curve) hold for the United States?
- What alterations to the theory, if any, could improve its performance?

An initial selection of raw, unformatted data was provided; this data was supplemented by data collection from the St. Louis Fed's Federal Reserve Economic Data (FRED) database and the Bureau of Labor Statistics public data API.

The analysis produced interactive data visualizations along with regression tables to demonstrate the historical relationship between inflation and unemployment in the U.S. from 1947 to 2017, along with the changes in their relationship across the studied period. 

### Results
- The Phillips Curve has not held as a general relationship in the U.S. during the studied period of 1947-2017. A modest correlation held for a short period in the 1960s, but has not been reproduced.
- Tests of alternative unemployment calculations and figures did not improve the results.
- Comparison of different methods of calculating inflation did not improve the results.
- Based on the findings, *there is no empirical evidence to suggest that the U.S. government should use the Phillips Curve as a predictive indicator or policy tool.*

### Files

#### Data
- FRED_nrou.csv: Historic national rate of unemployment data from St. Louis Federal Reserve Economic Data (FRED)
- FRED_clf.csv: Historic civilian labor force data from St. Louis Federal Reserve Economic Data (FRED)
- BLS_U6.csv: U6 unemployment data from the Bureau of Labor Statistics
- Case_Study_Data.csv: Initial selection of consumer price index, unemployment level, and civilian labor force data provided for case study.

#### Code
The following code was delivered:
- PhillipsCurveAnalysis.R: Contains full analysis of the Phillips Curve. A single working file was requested that enabled rapid prototyping and figure development using alternative data sources. The book is additionally coded to allow users to select the granularity of data considered, as sources differ in time periods reported. The book is heavily commented to guide users. While an RShiny dashboard was not requested, the project would be easily adapted into a dashboard to allow for dynamic analysis figure generation.
- pc.analysis.R: Contains all custom functions used in conducting the Phillips Curve analysis.
The analysis used the following libraries:
- tidyverse: dplyr, tidyr, readr, tibble, and ggplot2
- ggpubr: for additional ggplot customization
- lubridate: for time series data management
- zoo: "Z's orderded observations" package for time series
- reshape2: for melt function
- stargazer: for multi-format regression table creation and exports
- plotly: for plot interactivity
- pc.analysis: custom functions for Phillips curve analysis

#### Figures

##### Data Visualization
- pc_scenarios: Modeled Phillips Curve: Three Scenarios (varying Beta), 1949-2017
- plot_infl_exp_nrou_60s: Modeled Phillips Curve: Three Scenarios, 1960-1969
- plot_infl_exp_nrou_80s: Modeled Phillips Curve: Three Scenarios, 1980-1989
- Inflation and Unemployment Rates Over Time, 1948-2017
- Phillips Curve 2000-2017
- Phillips Curve 2009-2017
- Phillips Curve (U6) 2009-2017
- Phillips Curve 1980-1989
- Phillips Curve 1960-1969

##### Regression Tables
- pc_linear_reg_table
- pc_nonlinear_reg_table

#### Presentation
- PhillipsCurveAnalysis.pptx
