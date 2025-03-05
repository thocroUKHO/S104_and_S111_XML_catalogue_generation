# S104_and_S111_XML_catalogue_generation

Example (not-production and not-supported) code using R to make a catalogue file for S-104 and S-111 (excludes digital signature at the moment). 

## Step 1 

Acquire S-104/S-111 files, put them in their own separate folder. Download R and R-Studio.

## Step 2

Install required packages, in R-Studio, open up and run required_packages.R (you can do this by opening the file and pressing source (which should be about middle of the screen near the top / the top right of the of the "code" tab.

## Step 3

Check that the function hdf5_limits_and_check() works. In R-Studio, source the file s100_testing_utility_functions.R, then type in

`hdf5_limits_and_check("104","name_of_your_104_file.h5")`

If this doesn't work, see if you can follow the error messages through as to why, or email thomas.cropper@ukho.gov.uk

## Step 4

In R, set the working directory to a folder that contains S-104 or S-111 files.

You can do thus by Session > Set Working Directory, or the command setwd("folder_path")

For example on my laptop it would be a folder in my downloads folder

`setwd("~/Downloads/S104_and_S111_XML_catalogue_generation-main/104")`

Then, source the file 20250211_write_out_XML_for_S104.R or the S-111 equivalent for the S-111 folder. You should end up with a file CATALOGUE.xml in the folder you run the command in

`source("~/Downloads/S104_and_S111_XML_catalogue_generation-main/s100_testing_utility_functions.R")`
