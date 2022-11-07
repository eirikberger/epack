######################################
# Make package
######################################

# Load the basics
library(devtools)
library(roxygen2)

# From the folder where the package will be places
create('epack')
use_mit_license()

######################################
# Cleanup
######################################
usethis::use_tidy_description()
