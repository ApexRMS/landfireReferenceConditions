#### a202 - TNC LANDFIRE Support
#### Script by Chloé Debyser

#### 1. Generate Reference Condition Table 

###########################################################################################################################
# This code:                                                                                                              #
###########################################################################################################################

#### Workspace ####
# Packages
library(rsyncrosim)
library(tidyverse)
library(magrittr)
library(textreadr)

# Directories
docDir <- "E:/Data/Model Description Docs/BpSDocs_Parsed/"
resultsDir <- "E:/Results/"

# Library
library <- ssimLibrary("E:/Data/Reference Condition Model Library/LANDFIRE BpS Models 6 Oct 2019-V2-2-4/LANDFIRE BpS Models 6 Oct 2019.ssim")

# Scenario
scenario <- scenario(library, scenario = 8808)

# Datasheets
states <- datasheet(scenario, "OutputStratumState") %>% 
  arrange(StratumID, Iteration, Timestep)
transitions <- datasheet(scenario, "OutputStratumTransition") %>%
  arrange(StratumID, Iteration, Timestep)

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
  # Extract text lines from Word doc
  desc <- read_docx(paste0(docDir, models[i], ".docx")) %>%
    .[which((substr(., start=1, stop=6) == "Class ") & (grepl(" - ", ., fixed=T)) & (!substr(., start=8, stop=8) == " "))]
  
  # Compile model crosswalk
  modelCrosswalk <- data.frame(Class = sapply(desc, get.class),
                               Model = models[i],
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

# Change cover type/structural stage labels to codes
      # Cover type
crosswalk %<>% mutate(CoverType = sub(" Development ", "", CoverType))

      # Structural stage
crosswalk$StructuralStage <- sapply(crosswalk$StructuralStage, function(x){ifelse(x %in% c("All Structures", "All"), "ALL", ifelse(x == "Closed", "CLS", "OPN"))})

# Save
write.csv(crosswalk, paste0(resultsDir, "ClassCrosswalk.csv"), row.names = F)
