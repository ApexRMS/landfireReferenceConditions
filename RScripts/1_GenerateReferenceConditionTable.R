#### a202 - TNC LANDFIRE Support
#### Script by Chloé Debyser

#### 1. Generate Reference Condition Table 

###########################################################################################################################
# This code:                                                                                                              #
# - Creates a crosswalk for stage Classes A-E per Model | Cover Type | Structural Stage                                   #
# - 
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

# Input Parameters
scenarioId <- 8808 # Id number of the scenario of interest
timeStart <- 501 # First time step of interest for analyses
timeStop <- 1000 # Last time step of interest for analyses

# ST-Sim outputs
      # Library
library <- ssimLibrary("E:/Data/Reference Condition Model Library/LANDFIRE BpS Models 6 Oct 2019-V2-2-4/LANDFIRE BpS Models 6 Oct 2019.ssim")

      # Scenario
scenario <- scenario(library, scenario = scenarioId)

      # Datasheets
states <- datasheet(scenario, "OutputStratumState") %>% # Load
  filter((Timestep >= timeStart) & (Timestep <= timeStop)) %>% # Only retain timesteps of interest
  select_if(~!all(is.na(.))) %>% # Remove columns that contain NAs only
  rename(Model_Code = StratumID, CoverType = StateLabelXID, StructuralStage = StateLabelYID) %>% # Rename columns
  arrange(Model_Code, Iteration, Timestep) # Order records

transitions <- datasheet(scenario, "OutputStratumTransition") %>% # Load
  arrange(StratumID, Iteration, Timestep) # Order records

names <- datasheet(scenario, "Stratum") # Load

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

#### Initiate Reference Condition Table ####
# Start with Model_Code
table <- data.frame(Model_Code = unique(crosswalk$Model_Code), stringsAsFactors = F)

# Add BpS_Code
table$BpS_Code <- substr(table$Model_Code, start=1, stop=5)

# Add BpS_Name
table$BpS_Name <- sapply(table$Model_Code, function(x) names$Description[names$Name == x])

# Re-order columns & Check that all BpS have a name
table %<>% select(c(BpS_Code, BpS_Name, Model_Code)) # Re-order columns
if((sum(table$BpS_Name == "character(0)") > 0) | (sum(is.na(table$BpS_Name)) > 0)){stop("Some BpS lack names")} # Check that all BpS have a name

#### Indicator 1: % of the landscape in each class ####
# Add Class label to states dataframe
      # Create a Model_StateClassID identifier
crosswalk %<>% mutate(StateClassID = paste(CoverType, StructuralStage, sep=":"),
                      Model_StateClassID = paste(Model_Code, StateClassID, sep=":"))
states %<>% mutate(Model_StateClassId = paste(Model_Code, StateClassID, sep=":"))

      # Extract Class
states$Class <- sapply(states$Model_StateClassId, function(x) crosswalk$Class[crosswalk$Model_StateClassID == x])
states %<>% select(c(Model_Code, Iteration, Timestep, Class, Amount, AgeMin, AgeMax)) %>%
  mutate(Class = as.character(Class)) %>%
  arrange(Model_Code, Iteration, Timestep, Class)

# Area of landscape per model | iteration
ind1_iteration <- states %>%
  group_by(Model_Code, Iteration) %>%
  summarize(Amount = sum(Amount))

# Area and % of landscape per model | iteration | class
ind1_iteration_class <- states %>%
  group_by(Model_Code, Iteration, Class) %>%
  summarize(Amount = sum(Amount))
ind1_iteration_class$Amount_Iteration <- sapply(1:nrow(ind1_iteration_class), function(x) ind1_iteration$Amount[(ind1_iteration$Model_Code == ind1_iteration_class$Model_Code[x]) & (ind1_iteration$Iteration == ind1_iteration_class$Iteration[x])])
ind1_iteration_class %<>% mutate(Percentage = 100*Amount/Amount_Iteration)

# Average, for each model, of % across all iterations
ind1_class <- ind1_iteration_class %>%
  group_by(Model_Code, Class) %>%
  summarize(MeanPercentage = round(mean(Percentage), 2)) %>%
  ungroup() %>%
  spread(key = Class, value = MeanPercentage) %>%
  mutate(Model_Code = as.character(Model_Code))

# Join with Reference Condition Table
table %<>% full_join(., ind1_class, by = "Model_Code")

#### Indicator 2: Average fire frequencies by severity class (Replacement, Mixed, Low) ####


#### Indicator 3: Average aggregated fire frequency across all severity classes ####

#### Indicator 4: Percent of fires by severity class ####

#### Indicator 5: Fire Regime Group classification ####

