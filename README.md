# Acoustic tracking of European plaice within an offshore wind farm

Acoustic tracking study of plaice (*Pleuronectes platessa*) within the Belwind wind farm (BE). 

## Important

The calculation of the fish positions was performed using the yaps package, which was archived on CRAN due to its dependency on the deprecated package splusTimeSeries. 

## Introduction

This acoustic telemetry study on European plaice was conducted to investigate its presence within an offshore wind farm and its movements in relation to the hard substrate (turbines and scour protection layer). All information on material and methods, as well as an overview of the results, can be found in the paper published as Jolien Buyse, Jan Reubens, Kris Hostens, Steven Degraer, Jolien Goossens, Annelies De Backer, European plaice movements show evidence of high residency, site fidelity, and feeding around hard substrates within an offshore wind farm, ICES Journal of Marine Science, 2023;, fsad179, https://doi.org/10.1093/icesjms/fsad179.

## Data analysis

The scripts used to run the different analyses can be found in the folder `/src`.
The raw data underlying the analyses can be found in the folder `/data`.

### Overview

#### Scripts

1. `1_synctag_Data_preparation.R`: Prepare built-in sync tag metadata for development of synchronization model
2. `2_detection_data_preparation.R`: Prepare built-in sync detections for development of synchronization model
3. `3_synchronisation.R`: Develop the synchronization model and synchronize all the detection data 
4. `4_detection_data_analysis.R` 


