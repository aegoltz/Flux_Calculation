# Flux_Calculation

# This repository is associated with the submission by Goltz, A.E.; Till, C.B.; and Kent, A.J.R. entitled "From the bottom up: Calculating mantle-derived magma flux using subduction parameters and petrologic constraints at oceanic arcs"

To use, download all files. You will also need to download RStudio, which is free and open source (https://posit.co/download/rstudio-desktop/); typical install time is on the order of minutes. Put downloaded files in a single directory, and set this directory as your working directory in R. The scripts in this GitHub repository will enable you to repeat our calculations and recreate figures 3-7. 

"Formatted_Aleutian_Code.R" contains the code to calculate mantle-derived flux for the Aleutian arc 500,000 times. It also contains the code to plot figures 5, 6, and 7 (and a secondary calculation to re-calculate the mantle-derived magma flux at a smaller r_perp). It takes about 8 hours to run 500,000 iterations of the model; running this whole code will take about 16 hours. 

"Model_20_All_Varying.R" contains the code to calculate mantle-derived flux with all variables simultaneously varying in the ranges used for sensitivity tests.

To recreate our sensitivity tests and Figure 4, run all codes beginning with "Sensitivity_". This will take ~8 hrs/code if the number of iterations is maintained at 500,000; to run all the codes takes about 90 hours. Once all those are run, you will be able to calculate correlation coefficients using the code "Analyze_Sensitivity_Tests_Publication.R" This code includes simple, custom functions to remove NA rows and calculate pearson and spearman correlation coefficients. Using this code and with the other dataframes saved in your global environment, you will be able to recreate Figure 4.

Finally, to recreate Figure 3, use the code "Make_Figure_3.R". This code imports the Excel sheet "Supplementary Table 1.xlsx" using a simple command, but will only work if the working directory contains both the code and the Excel sheet.

All these codes are heavily annotated, but please feel free to reach out with any questions.

These codes were tested on a Macintosh with an M3 chip (OS Sonoma, version 14.6.1) using R version 4.4.3 ("Trophy Case"), but with proper installation of RStudio, this code should run on any device.
