#### a202 - TNC LANDFIRE Support
#### Script by Chloé Debyser

#### 1. Generate Class Crosswalk 

###########################################################################################################################
# This code:                                                                                                              #
# - Reads Word documents containing class definitions for each model                                                      #
# - Creates a crosswalk for stage Classes A-E per Model | Cover Type | Structural Stage                                   #
###########################################################################################################################

#### Workspace ####
# Packages
library(tidyverse)
library(magrittr)
library(textreadr)

# Directories
docDir <- "C:/Users/leona/Documents/Apex Projects/A202 - TNC LANDFIRE Support/Data/Model Description Docs/"
resultsDir <- "C:/Users/leona/Documents/Apex Projects/A202 - TNC LANDFIRE Support/Results/"

#### Create Class Crosswalk ####
# Define functions extracting desired attributes from text lines
      # Attribute: Class
get.class <- function(x){
  substr(x, start=7, stop=7)
}

      # Attribute: Cover Type
get.coverType <- function(x){
  start <- str_locate_all(x, "[A-Z]")[[1]][3,1]
  stop <- str_locate_all(x, " - ")[[1]][1,1]-1
  y <- substr(x, start=start, stop=stop)
}

      # Attribute: Structural Stage
get.structuralStage <- function(x){
  start <- str_locate_all(x, " - ")[[1]][1,1]+3
  stop <- nchar(x)
  y <- substr(x, start=start, stop=stop)
}

# Get all model names
models <- list.files(docDir, pattern=".docx") %>%
  substr(., start=1, stop=nchar(.)-5)

# For each model, extract crosswalk from corresponding Word doc
for(i in 1:length(models)){
  # i = 1
  # Extract text lines from Word doc
  desc <- read_docx(paste0(docDir, models[i], ".docx")) %>%
    .[which((substr(., start=1, stop=6) == "Class ") & (grepl(" - ", ., fixed=T)) & (!substr(., start=8, stop=9) == " ")& (!is.na(as.numeric(substr(., start=9, stop=10)))))]
  
  # Compile model crosswalk
  modelCrosswalk <- data.frame(Model_Code = models[i],
                               Class = sapply(desc, get.class),
                               CoverType = sapply(desc, get.coverType),
                               StructuralStage = sapply(desc, get.structuralStage),
                               stringsAsFactors = F)
  
  # Add to master crosswalk
  if(i == 1){
    crosswalk <- modelCrosswalk
  }else{
    crosswalk %<>% bind_rows(., modelCrosswalk)
  }
}
rm(modelCrosswalk, desc, i)

# Cleanup non-standard document names
crosswalk$Model_Code <- gsub(" ", "", crosswalk$Model_Code)
crosswalk$Model_Code <- gsub(".doc", "", crosswalk$Model_Code)
crosswalk$Model_Code <- sapply(crosswalk$Model_Code, function(x) ifelse(substr(x, start=nchar(x), stop=nchar(x)) == "_", substr(x, start=1, stop=nchar(x)-1), x))
crosswalk$Model_Code <- gsub("_RS", "", crosswalk$Model_Code)

# Change cover type/structural stage labels to codes
      # Cover type
crosswalk %<>% mutate(CoverType = sub(" Development ", "", CoverType))

      # Structural stage
crosswalk$StructuralStage <- sapply(crosswalk$StructuralStage, function(x){ifelse(x %in% c("All Structures", "All"), "ALL", ifelse(x == "Closed", "CLS", "OPN"))})

# Save
write.csv(crosswalk, paste0(resultsDir, "ClassCrosswalk.csv"), row.names = F)
