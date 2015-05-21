# wa-water-quality

## Introduction

This project explores the use of publicly available data to investigate 
drinking water system fluoride levels in Washington State. Methods for 
reproducible data cleanup and exploratory analysis using R, RMarkdown, knitr, 
are demonstrated, as well as some of the plotting capabilities of ggplot2.

## Data Sources

Data files have been prepared using a companion 
[Markdown script](https://github.com/brianhigh/wa-water-quality/blob/master/data_cleanup_and_export.md) 
to generate text data files. These data and Markdown files are hosted in the 
[wa-water-quality](https://github.com/brianhigh/wa-water-quality) repository 
on [GitHub](https://github.com).

The water system data come from [WA DOH Water System Data](http://www.doh.wa.gov/DataandStatisticalReports/EnvironmentalHealth/DrinkingWaterSystemData/DataDownload) and [WA DOH Fluoride in Driking Water](http://www.doh.wa.gov/DataandStatisticalReports/EnvironmentalHealth/DrinkingWaterSystemData/FluorideinDrinkingWater). The lat/long coordinates were generated using the [ggmap](http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf) package for R.
