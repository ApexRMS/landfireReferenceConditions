#### a202 - TNC LANDFIRE Support
#### Script by Carina Firkowski 

#### 0. Constants across scripts 

################################################################################
# This code:                                                                   #
# - Loads the workspace, models and functions required by the other scripts    #
################################################################################

#### Workspace -----------------------------------------------------------------

# Packages
library(tidyverse)
library(magrittr)
library(qdapTools)
library(readtext)

#### Directories ---------------------------------------------------------------

# Folder with all documents with model descriptions
docDir <- "./Docs/"
# Folder where the outputs should be saved
resultsDir <- "./Results/"

#### Load data -----------------------------------------------------------------

# Get all models from file names before ".docx"
models <- list.files(docDir, pattern=".docx") %>%
  substr(., start=1, stop=nchar(.)-5)

#### Functions -----------------------------------------------------------------

# Function to extract the following attributes from text lines
# Class
get.class <- function(x){
  substr(x, start=7, stop=7)
}
# Cover Type
get.coverType <- function(x){
  start <- str_locate_all(x, "[A-Z]")[[1]][3,1]
  stop <- str_locate_all(x, " - ")[[1]][1,1]-1
  y <- substr(x, start=start, stop=stop)
}
# Structural Stage
get.structuralStage <- function(x){
  start <- str_locate_all(x, " - ")[[1]][1,1]+3
  stop <- nchar(x)
  y <- substr(x, start=start, stop=stop)
}
# Max Tree Size Class
get.maxTreeSizeClass <- function(x){
  if(!is.na(x) &                         # null data variations are given NA
     !grepl("no data", x, fixed=T) &
     !grepl("No Data", x, fixed=T) &
     !grepl("No data", x, fixed=T) &
     !grepl("None", x, fixed=T) & 
     nchar(x) > 23){
    start <- str_locate_all(x, "[A-Z]")[[1]][4,1] + 5
    stop <- nchar(x)
    y <- substr(x, start=start, stop=stop)
  } else {
    y <- NA
  }
}



