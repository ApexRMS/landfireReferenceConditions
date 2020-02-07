#### a202 - TNC LANDFIRE Support
#### Script by Chloé Debyser

#### 2. Generate Reference Condition Table 

###########################################################################################################################
# This code:                                                                                                              #
# - Creates a Reference Condition Table with BpS_Code, BpS_Name, and Model_Code                                           #
# - Computes indicator 1: % of the landscape in each class                                                                #
# - Computes indicator 2/3: Fire Return Interval per severity class/for all fires                                         #
# - Computes indicator 4: Percent of fires by severity class                                                              #
# - Computes indicator 5: Fire Regime Groups (old and new)                                                                #
###########################################################################################################################

#### Workspace ####
# Packages
library(rsyncrosim)
library(tidyverse)
library(magrittr)
library(openxlsx)
library(zoo)

# Directories
resultsDir <- "E:/Results/"

# Input Parameters
scenarioId <- 8808 # Id number of the scenario of interest
timeStart <- 501 # First time step of interest for analyses
timeStop <- 1000 # Last time step of interest for analyses

# Tabular data
crosswalk <- read.csv(paste0(resultsDir, "ClassCrosswalk.csv"))
FRG_rules <- read.xlsx("E:/Data/Classification Rules - FRG/Computing AllFireFRI and % Fires.xlsx", startRow = 11) %>%
  na.locf()

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
  filter(Timestep == timeStop) %>% # Only retain timesteps of interest
  select(-c(Timestep)) %>% # Remove Timestep column (unique value)
  filter(TransitionGroupID %in% c("All Fire", "Replacement Fire", "Mixed Fire", "Surface Fire")) %>% # Only retain fire transitions
  select_if(~!all(is.na(.))) %>% # Remove columns that contain NAs only
  rename(Model_Code = StratumID, MeanTransitionAmount = Amount) %>% # Rename columns
  mutate(Model_Code = as.character(Model_Code), TransitionGroupID = as.character(TransitionGroupID)) %>% # Factor columns to character
  arrange(Model_Code, Iteration, TransitionGroupID) # Order records

names <- datasheet(scenario, "Stratum") # Load

#### Initiate Reference Condition Table ####
# Start with Model_Code
table <- data.frame(Model_Code = unique(crosswalk$Model_Code), stringsAsFactors = F)

# Add BpS_Code
table$BpS_Code <- substr(table$Model_Code, start=1, stop=5)

# Add BpS_Name
table$BpS_Name <- sapply(table$Model_Code, function(x) names$Description[names$Name == x])

# Format columns & Check that all BpS have a name
table %<>% select(c(BpS_Code, BpS_Name, Model_Code)) %>% # Re-order columns
  mutate(Model_Code = as.character(Model_Code))
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
  mutate(Model_Code = as.character(Model_Code)) %>%
  rename(ClassA_ReferencePercent = A, ClassB_ReferencePercent = B, ClassC_ReferencePercent = C, ClassD_ReferencePercent = D, ClassE_ReferencePercent = E)

# Join with Reference Condition Table
table %<>% full_join(., ind1_class, by = "Model_Code")
rm(ind1_class, ind1_iteration, ind1_iteration_class)

#### Indicator 2-3: Fire Return Interval (Replacement, Mixed, Low, and All Fires) ####
# Compute TimestepAmount = Area of landscape per model | iteration | timestep
      # Compute
ind2_iteration_timestep <- states %>%
  group_by(Model_Code, Iteration, Timestep) %>%
  summarize(Amount = sum(Amount))

      # Add TimestepAmount to df of model | iteration (since all timesteps per model | iteration have the same area)
ind2_iteration <- ind2_iteration_timestep %>%
  group_by(Model_Code, Iteration) %>%
  summarize(TimestepAmount = unique(Amount)) %>%
  ungroup() %>%
  mutate(Model_Code = as.character(Model_Code))

# Compute MeanProportion = Mean proportion of landscape affected by fire every year, per model | iteration | fire type
transitions %<>% left_join(., ind2_iteration, by=c("Model_Code", "Iteration")) %>% # Add TimestepAmount to transitions
  mutate(MeanProportion = MeanTransitionAmount/TimestepAmount) # Calculate mean proportion

# Compute Fire Return Interval (FRI)
ind2_transitionGroup <- ind4_transitionGroup <- transitions %>%
  group_by(Model_Code, TransitionGroupID) %>%
  summarize(Mean_MeanProportion = mean(MeanProportion)) %>% # Mean of MeanProportion across all iterations
  mutate(FRI = 1/Mean_MeanProportion) %>% # Fire Return Interval
  select(-Mean_MeanProportion) %>% # Remove Mean of MeanProportion
  spread(key = TransitionGroupID, value = FRI) %>% # Long to wide format
  rename(FRI_ReplacementFire = "Replacement Fire", FRI_MixedFire = "Mixed Fire", FRI_LowFire = "Surface Fire", FRI_AllFire = "All Fire") %>% # Rename columns
  select(c(Model_Code, FRI_ReplacementFire, FRI_MixedFire, FRI_LowFire, FRI_AllFire)) # Order columns

# Round FRI values
ind2_transitionGroup %<>% ungroup() %>%
  mutate_if(is.numeric, round, 2)

# Join with Reference Condition Table
table %<>% full_join(., ind2_transitionGroup, by = "Model_Code")
rm(ind2_iteration_timestep, ind2_iteration, ind2_transitionGroup)

#### Indicator 4: Percent of fires by severity class ####
# Compute % of fires by severity class
ind4_transitionGroup %<>% mutate(PercentOfFires_ReplacementFire = 100*FRI_AllFire/FRI_ReplacementFire,
                                 PercentOfFires_MixedFire = 100*FRI_AllFire/FRI_MixedFire,
                                 PercentOfFires_LowFire = 100*FRI_AllFire/FRI_LowFire) %>%
  select(c(Model_Code, PercentOfFires_ReplacementFire, PercentOfFires_MixedFire, PercentOfFires_LowFire))

# Round percentages
ind4_transitionGroup %<>% ungroup() %>%
  mutate_if(is.numeric, round, 2)

# Join with Reference Condition Table
table %<>% full_join(., ind4_transitionGroup, by = "Model_Code")
rm(ind4_transitionGroup)

#### Indicator 5: Fire Regime Group (FRG) classification ####
# Compute, for each FRG, min and max FRI_AllFire
      # Write function
allFireFRI.minMax <- function(x){
  x <- substr(x, start=1, stop=gregexpr("years", x, fixed=T)[[1]][1]-2)
  min <- ifelse(grepl("-", x , fixed=T),
                as.integer(substr(x, start=1, stop=gregexpr("-", x, fixed=T)[[1]][1]-1)),
                ifelse(grepl("to", x , fixed=T),
                       as.integer(substr(x, start=1, stop=gregexpr("to", x, fixed=T)[[1]][1]-1)),
                       501))
  max <- ifelse(grepl("-", x , fixed=T),
                as.integer(substr(x, start=gregexpr("-", x, fixed=T)[[1]][1]+1, stop=nchar(x))),
                ifelse(grepl("to", x , fixed=T),
                       as.integer(substr(x, start=gregexpr("to", x, fixed=T)[[1]][1]+2, stop=nchar(x))),
                       max(table$FRI_AllFire, na.rm=T))) + 1 # Add 1 because otherwise there are gaps between intervals
  
  y <- as.vector(c(min, max))
  return(y)
}

      # Apply
FRG_rules[, c("FRI_AllFire_min", "FRI_AllFire_max")] <- t(sapply(FRG_rules$All.Fire.Fire.Return.Interval, allFireFRI.minMax))
FRG_rules %<>% select(-All.Fire.Fire.Return.Interval)

# Compute, for each FRG, min and max PercentOfFire_ReplacementFire
      # Write function
PercentOfFire.ReplacementFire.minMax <- function(x){
  min <- ifelse(x == "Less than 66.7%",
                0,
                ifelse(x == "66.7% or greater",
                       66.7,
                       ifelse(x == "Less than 80%",
                              0,
                              ifelse(x == "80% or greater",
                                     80,
                                     0))))
  
  max <- ifelse(x == "Less than 66.7%",
                66.7,
                ifelse(x == "66.7% or greater",
                       101,
                       ifelse(x == "Less than 80%",
                              80,
                              ifelse(x == "80% or greater",
                                     101,
                                     101))))
  y <- as.vector(c(min, max))
  return(y)
}

      # Apply
FRG_rules[, c("PercentOfFire_ReplacementFire_min", "PercentOfFire_ReplacementFire_max")] <- t(sapply(FRG_rules$`%.Replacement.Fire`, PercentOfFire.ReplacementFire.minMax))
FRG_rules %<>% select(-`%.Replacement.Fire`)

# Compute, for each model, the correct FRG
      # Write function
get.FRG <- function(x){
  if(is.na(table$FRI_AllFire[x])){
    old <- new <- NA # NA if FRI_AllFire is NA
    
  }else{
    # If PercentOfFires_ReplacementFire is NA, use 0%
    if(is.na(table$PercentOfFires_ReplacementFire[x])){
      replFire <- 0
    }else{
      replFire <- table$PercentOfFires_ReplacementFire[x]
    }
    
    # Get record corresponding to correct FRG
    rules <- FRG_rules %>%
      filter(FRI_AllFire_min <= table$FRI_AllFire[x]) %>% # FRI_AllFire must be equal to or greater than min for FRG
      filter(FRI_AllFire_max > table$FRI_AllFire[x]) %>% # FRI_AllFire must be strictly lesser than max for FRG
      filter(PercentOfFire_ReplacementFire_min <= replFire) %>% # PercentOfFires_ReplacementFire must be equal to or greater than min for FRG
      filter(PercentOfFire_ReplacementFire_max > replFire) # PercentOfFires_ReplacementFire must be strictly lesser than max for FRG
  
    # Get FRG names
    old <- as.character(rules$Original.Fire.Regime.Group)
    new <- as.character(rules$New.Group.Designation)
  }
  final <- as.vector(c(old, new))
  return(final)
}

      # Apply
table[, c('FRG_Old', 'FRG_New')] <- t(sapply(1:nrow(table), get.FRG))

#### Export Reference Condition Table ####
write.csv(table, paste0(resultsDir, "ReferenceConditionTable_", scenarioId, ".csv"), row.names = F)
