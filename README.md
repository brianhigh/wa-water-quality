# wa-water-quality

## Introduction

This project explores the use of publicly available data to investigate 
drinking water system fluoride levels in Washington State. Methods for 
reproducible data cleanup and exploratory analysis using R, RMarkdown, knitr, 
are demonstrated, as well as some of the plotting capabilities of ggplot2. 

## Disclaimer

This repository was created for educational purposes only. The exploratory data 
analysis was performed to show some plotting techniques and in no way should be
considered statistically or scientifically valid in any way.

## Data Sources

Data files have been prepared using a companion [Markdown script](https://github.com/brianhigh/wa-water-quality/blob/master/data_cleanup_and_export.md) to generate text data files. These data and Markdown files are hosted in the [wa-water-quality](https://github.com/brianhigh/wa-water-quality) repository on [GitHub](https://github.com).

The water system data come from [WA DOH Water System Data](http://www.doh.wa.gov/DataandStatisticalReports/EnvironmentalHealth/DrinkingWaterSystemData/DataDownload) and [WA DOH Fluoride in Driking Water](http://www.doh.wa.gov/DataandStatisticalReports/EnvironmentalHealth/DrinkingWaterSystemData/FluorideinDrinkingWater). 

The lat/long coordinates for Washington cities were generated using the [ggmap](http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf) package for R. Spatial data are from: [GADM](http://gadm.org/) and the R `maps` package.

Medicaid expenses data come from the [CDC](http://www.cdc.gov/fluoridation/statistics/2012stats.htm) and [Washington Health Care Authority](http://www.hca.wa.gov/medicaid/dentalproviders/Pages/dental_data.aspx).
