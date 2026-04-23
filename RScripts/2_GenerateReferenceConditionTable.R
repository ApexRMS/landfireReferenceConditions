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
#                                                                                                                         #
# NOTE: this version of the code calculates all indicators at the iteration level before averaging across all iterations  #
# to obtain the model-level indicator. This differs from methods previously implemented by the TNC.                       #
# Corresponding areas in the code are flagged with the word "NOTE".                                                       #
#                                                                                                                         #
# Updated on 2026-04 (A372) for SyncroSim 3.1, stsim 4.5.3, rsyncrosim 2.1.13                                             #
###########################################################################################################################

#### Workspace ####
# Packages
library(rsyncrosim)
library(tidyverse)
library(magrittr)
library(openxlsx)
library(zoo)

# Custom functions
source("RScripts/0_Functions.R")

# Directories
resultsDir <- "./Results/"
dataDir <- "./Data/"
libraryDir <- "./Library/"
#libraryPath <- paste0(libraryDir, "LANDFIRE Alaska19Sept2024.ssim")
libraryPath <- paste0(libraryDir, "reference-condition-models.ssim")

# Input Parameters
#scenarioId <- 8966 # Id number of the scenario of interest
scenarioId <- 8808 # reference conditions All Models scenario
timeStart <- 501 # First time step of interest for analyses
timeStop <- 1000 # Last time step of interest for analyses

# Tabular data
crosswalk <- list.files(
  resultsDir,
  pattern = "ClassCrosswalk",
  full.names = T
) %>% # Dates are now appended to class crosswalk names
  tail(1) %>% # Only use the latest
  read_csv

# Load FRG Rules
FRG_rules <- read.csv(paste0(
  dataDir,
  "Computing AllFireFRI and Percent Fires.csv"
))

# ST-Sim outputs
# Library
library <- ssimLibrary(libraryPath)

# Scenario
scenario <- scenario(library, scenario = scenarioId)

# Datasheets
states <- datasheet(scenario, "stsim_OutputStratumState") %>% # Load
  filter((Timestep >= timeStart) & (Timestep <= timeStop)) %>% # Only retain timesteps of interest
  select_if(~ !all(is.na(.))) %>% # Remove columns that contain NAs only
  rename(
    Model_Code = StratumId,
    CoverType = StateLabelXId,
    StructuralStage = StateLabelYId
  ) %>% # Rename columns
  arrange(Model_Code, Iteration, Timestep) # Order records

transitions <- datasheet(scenario, "stsim_OutputStratumTransition") %>% # Load
  filter(Timestep == timeStop) %>% # Only retain timesteps of interest
  select(-c(Timestep)) %>% # Remove Timestep column (unique value)
  filter(
    TransitionGroupId %in%
      c("All Fire", "Replacement Fire", "Mixed Fire", "Surface Fire")
  ) %>% # Only retain fire transitions
  select_if(~ !all(is.na(.))) %>% # Remove columns that contain NAs only
  rename(Model_Code = StratumId, MeanTransitionAmount = Amount) %>% # Rename columns
  mutate(
    Model_Code = as.character(Model_Code),
    TransitionGroupId = as.character(TransitionGroupId)
  ) %>% # Factor columns to character
  arrange(Model_Code, Iteration, TransitionGroupId) # Order records

names <- datasheet(scenario, "stsim_Stratum") # Load


#### Initiate Reference Condition Table ####
# Start with Model_Code
table <- data.frame(
  Model_Code = unique(crosswalk$Model_Code),
  stringsAsFactors = F
)

# Add BpS_Code
table$BpS_Code <- substr(table$Model_Code, start = 1, stop = 5)

# Add BpS_Name
table$BpS_Name <- sapply(table$Model_Code, function(x) {
  names$Description[names$Name == x]
})

# Format columns & Check that all BpS have a name
table %<>%
  select(c(BpS_Code, BpS_Name, Model_Code)) %>% # Re-order columns
  mutate(Model_Code = as.character(Model_Code))
if (
  (sum(table$BpS_Name == "character(0)") > 0) | (sum(is.na(table$BpS_Name)) > 0)
) {
  stop("Some BpS lack names")
} # Check that all BpS have a name

#### Indicator 1: Percent of landscape in each class ####
# Add Class label to states dataframe
# Create a Model_StateClassId identifier
crosswalk %<>%
  mutate(
    StateClassId = paste(CoverType, StructuralStage, sep = ":"),
    Model_StateClassId = paste(Model_Code, StateClassId, sep = ":")
  )
states %<>%
  mutate(Model_StateClassId = paste(Model_Code, StateClassId, sep = ":"))
crosswalkSmall = select(crosswalk, Model_StateClassId, Class)
crosswalkSmall = rename(crosswalkSmall, Model_StateClassId = Model_StateClassId)

# Check that the crosswalk accounts for every model, cover type, and structural class combination in the scenario
missing_combinations <- setdiff(
  states$Model_StateClassId,
  crosswalkSmall$Model_StateClassId
)
if (length(missing_combinations) > 0) {
  stop(
    "One or more combinations of Model, Cover Type, and Structural Class in the scenario are not represented in the crosswalk."
  )
}

states = left_join(states, crosswalkSmall)

# Extract Class
# states$Class <- sapply(states$Model_StateClassId, function(x) crosswalk$Class[crosswalk$Model_StateClassId == x])
states %<>%
  select(c(Model_Code, Iteration, Timestep, Class, Amount, AgeMin, AgeMax)) %>%
  mutate(Class = as.character(Class)) %>%
  arrange(Model_Code, Iteration, Timestep, Class)

# Area of landscape per model | iteration
ind1_iteration <- states %>%
  group_by(Model_Code, Iteration) %>%
  summarize(Amount = sum(Amount))

## NOTE: The following two sections calculate indicator 1 at the iteration level, then average the result across all iterations
# % of landscape per model | iteration | class
ind1_iteration_class <- states %>%
  group_by(Model_Code, Iteration, Class) %>%
  summarize(Amount = sum(Amount)) %>%
  left_join(
    ind1_iteration %>%
      rename(Amount_Iteration = Amount),
    by = c("Model_Code", "Iteration")
  )
# ind1_iteration_class$Amount_Iteration <- sapply(
#   1:nrow(ind1_iteration_class),
#   function(x) {
#     ind1_iteration$Amount[
#       (ind1_iteration$Model_Code == ind1_iteration_class$Model_Code[x]) &
#         (ind1_iteration$Iteration == ind1_iteration_class$Iteration[x])
#     ]
#   }
# )
ind1_iteration_class %<>% mutate(Percentage = 100 * Amount / Amount_Iteration)

# % of landscape per model | class
ind1_class <- ind1_iteration_class %>%
  group_by(Model_Code, Class) %>%
  summarize(MeanPercentage = mean(Percentage)) %>% # Mean of % across all iterations
  ungroup() %>%
  spread(key = Class, value = MeanPercentage) %>% # Long to wide format
  mutate(Model_Code = as.character(Model_Code)) %>% # Format columns
  rename(
    ClassA_ReferencePercent = A,
    ClassB_ReferencePercent = B,
    ClassC_ReferencePercent = C,
    ClassD_ReferencePercent = D,
    ClassE_ReferencePercent = E
  ) # Rename columns

# Adjust percentages: round to nearest integer, force minimum to 1%, ensure sum = 100%
ind1_class %<>%
  mutate_if(is.numeric, round) %>% # Round to the nearest integer
  mutate_if(is.numeric, function(x) ifelse(x == 0, 1, x)) %>% # Force 0 values to 1%
  sum.100(., 2:6) # Ensure sum = 100%

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
transitions %<>%
  left_join(., ind2_iteration, by = c("Model_Code", "Iteration")) %>% # Add TimestepAmount to transitions
  #mutate(MeanProportion = MeanTransitionAmount / TimestepAmount) # Calculate mean proportion'
  mutate(
    MeanProportion = ifelse(
      TimestepAmount > 0,
      MeanTransitionAmount / TimestepAmount,
      NA_real_
    )
  ) # Calculate mean proportion while avoiding error if amount = 0

## NOTE: The following two sections calculate indicators 2/3 at the iteration level, then average the result across all iterations
# Compute Fire Return Interval (FRI) per model | iteration | fire type
#transitions %<>% mutate(FRI = 1 / MeanProportion)
transitions %<>%
  mutate(FRI = ifelse(MeanProportion > 0, 1 / MeanProportion, NA_real_)) # avoids issues with amount 0

# Compute Fire Return Interval (FRI) per model | fire type
ind2_transitionGroup <- transitions %>%
  group_by(Model_Code, TransitionGroupId) %>%
  summarize(FRI = mean(FRI)) %>% # Mean of FRI across all iterations
  spread(key = TransitionGroupId, value = FRI) %>% # Long to wide format
  rename(
    FRI_ReplacementFire = "Replacement Fire",
    FRI_MixedFire = "Mixed Fire",
    FRI_LowFire = "Surface Fire",
    FRI_AllFire = "All Fire"
  ) %>% # Rename columns
  select(c(
    Model_Code,
    FRI_ReplacementFire,
    FRI_MixedFire,
    FRI_LowFire,
    FRI_AllFire
  )) # Order columns

# Round FRI values
ind2_transitionGroup %<>% ungroup() %>% mutate_if(is.numeric, round)

# Join with Reference Condition Table
table %<>% full_join(., ind2_transitionGroup, by = "Model_Code")
rm(ind2_iteration_timestep, ind2_iteration, ind2_transitionGroup)

#### Indicator 4: Percent of fires by severity class ####
# Add MeanProportion_AllFire as separate column in transitions table
# transitions$MeanProportion_AllFire <- sapply(1:nrow(transitions), function(x) {
#   transitions$MeanProportion[which(
#     (transitions$Model_Code == transitions$Model_Code[x]) &
#       (transitions$Iteration == transitions$Iteration[x]) &
#       (transitions$TransitionGroupId == "All Fire")
#   )]
# })
transitions %<>%
  left_join(
    transitions %>%
      filter(TransitionGroupId == "All Fire") %>%
      select(Model_Code, Iteration, MeanProportion_AllFire = MeanProportion),
    by = c("Model_Code", "Iteration")
  )

## NOTE: The following two sections calculate indicator 4 at the iteration level, then average the result across all iterations
# Compute % of fires per model | iteration | fire type
transitions %<>%
  #mutate(PercentOfFires = 100 * (MeanProportion / MeanProportion_AllFire))
  mutate(
    PercentOfFires = ifelse(
      MeanProportion_AllFire > 0,
      100 * (MeanProportion / MeanProportion_AllFire),
      NA_real_
    )
  )

# Compute % of fires per model | fire type
ind4_transitionGroup <- transitions %>%
  group_by(Model_Code, TransitionGroupId) %>%
  summarize(PercentOfFires = mean(PercentOfFires)) %>% # Mean of PercentOfFires across all iterations
  spread(key = TransitionGroupId, value = PercentOfFires) %>% # Long to wide format
  rename(
    PercentOfFires_ReplacementFire = `Replacement Fire`,
    PercentOfFires_MixedFire = `Mixed Fire`,
    PercentOfFires_LowFire = `Surface Fire`
  ) %>% # Rename columns
  select(c(
    Model_Code,
    PercentOfFires_ReplacementFire,
    PercentOfFires_MixedFire,
    PercentOfFires_LowFire
  )) %>% # Order columns
  ungroup()

# Adjust percentages: round to nearest integer, force minimum to 1%, ensure sum = 100%
ind4_transitionGroup %<>%
  mutate_if(is.numeric, round) %>% # Round to the nearest integer
  mutate_if(is.numeric, function(x) ifelse(x == 0, 1, x)) %>% # Force 0 values to 1%
  sum.100(., 2:4) # Ensure sum = 100%

# Join with Reference Condition Table
table %<>% full_join(., ind4_transitionGroup, by = "Model_Code")
rm(ind4_transitionGroup)

#### Indicator 5: Fire Regime Group (FRG) classification ####
# Compute, for each FRG, min and max FRI_AllFire
FRG_rules[, c("FRI_AllFire_min", "FRI_AllFire_max")] <- t(sapply(
  FRG_rules$All.Fire.Fire.Return.Interval,
  allFireFRI.minMax
))
FRG_rules %<>% select(-All.Fire.Fire.Return.Interval)

# Compute, for each FRG, min and max PercentOfFire_ReplacementFire
FRG_rules[, c(
  "PercentOfFire_ReplacementFire_min",
  "PercentOfFire_ReplacementFire_max"
)] <- t(sapply(
  FRG_rules$Percent.Replacement.Fire,
  PercentOfFire.ReplacementFire.minMax
))
FRG_rules %<>% select(-Percent.Replacement.Fire)

# Compute, for each model, the correct FRG
table[, c('FRG_Old', 'FRG_New')] <- t(sapply(1:nrow(table), get.FRG))
table[is.na(table$FRI_AllFire), 'FRG_New'] <- "V-B" # Change BpS with no fire to V-B
table = select(
  table,
  BPS_CODE = BpS_Code, # renamed
  BPS_NAME = BpS_Name, # renamed
  BPS_MODEL = Model_Code, # renamed
  ClassA_ReferencePercent,
  ClassB_ReferencePercent,
  ClassC_ReferencePercent,
  ClassD_ReferencePercent,
  ClassE_ReferencePercent,
  FRI_ReplacementFire,
  FRI_MixedFire,
  FRI_LowFire,
  FRI_AllFire,
  PercentOfFires_ReplacementFire,
  PercentOfFires_MixedFire,
  PercentOfFires_LowFire,
  FRG_Old,
  FRG_New
)
table$BPS_NAME = as.character(table$BPS_NAME)

#### Export Reference Condition Table ####
write.csv(
  table,
  paste0(resultsDir, "ReferenceConditionTable_", scenarioId, ".csv"),
  row.names = F
)
