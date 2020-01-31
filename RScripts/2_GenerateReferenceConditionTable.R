#### a202 - TNC LANDFIRE Support
#### Script by Chloé Debyser

#### 2. Generate Reference Condition Table 

###########################################################################################################################
# This code:                                                                                                              #
# - 
###########################################################################################################################

#### Workspace ####
# Packages
library(rsyncrosim)
library(tidyverse)
library(magrittr)
library(openxlsx)

# Directories
resultsDir <- "E:/Results/"

# Input Parameters
scenarioId <- 8808 # Id number of the scenario of interest
timeStart <- 501 # First time step of interest for analyses
timeStop <- 1000 # Last time step of interest for analyses

# Tabular data
crosswalk <- read.csv(paste0(resultsDir, "ClassCrosswalk.csv"))
FRG_rules <- read.xlsx("E:/Data/Classification Rules - FRG/Computing AllFireFRI and % Fires.xlsx")

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

