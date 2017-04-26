## Description: Sample ID key for samples harvested from Tyson rotplots in 2012 and 2014

## Origin: created by MRL based on "2012HarvestDecoder" and "2014HarvestDecoder" for use in R code

## Format: Tab-delim

## Columns...
species : 4 letter tree species code. First 2 letters are the genus; last 2 letters are the species. Some species 4-letter codes have a "2" behind them because these are the species that were added in the second decay cohort.
species_code : Upper case letter from A-X.
numDecayYrs_2012 : Number of years that sample had been decaying at the time of the 2012 harvest
numDecayYrs_2014 : Number of years that the sample had been decaying at the time of the 2014 harvest
plot: Rot plot identifier where number indicates the watershed (1-4) and the letter indicates the location in the watershed (L = low, H = high).
plot_code : Number from 1-8.
plotWatershed : Number from 1-4 that indicates which watershed the plot was located in
plotWatershedLocation : Upper case letter, L (low) or H (high), indicating where in the watershed the plot was located
logLocation : Location on the log where the microbial sample was taken.  Top means anywhere on the log that is not in contact with soil; bottom means any where on the log that is in contact with the soil.
logLocation_code : Lower case letter: t , b.
replicate: Two samples were taken from the same spot.  One of the replicate samples was used for identifying the microbial community, the other was used for enzyme analysis.
replicate_code_2012 : Upper case letter, Y or Z
replicate_code_2014 : Symbol, "+" or "-"