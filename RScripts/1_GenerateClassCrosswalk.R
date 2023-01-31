#### a202 - TNC LANDFIRE Support
#### Script by Chloé Debyser 
#### Updated by Caroline Tucker 09/2020 & Carina Firkowski 01/2023

#### 1. Generate Class Crosswalk 

################################################################################
# This code:                                                                   #
# - Reads Word documents containing class definitions for each model           #
# - Creates a crosswalk for stage Classes A-E per Model, Cover Type,           #
#   Structural Stage, Maximum Tree Size Class, and Description                 #
################################################################################

#### Workspace ####
# Packages
library(tidyverse)
library(magrittr)
library(qdapTools)
library(stringr)


#### Directories
# Specify docDir to the folder with all documents with model descriptions, ex.:
docDir <- "./Docs/"
# Specify resultsDir to the folder where the outputs should be saved, ex.:
resultsDir <- "./Results/"


#### Create Class Crosswalk 
## Define functions extracting desired attributes from text lines
      # Attribute: Class
get.class <- function(x){ # should be generalized
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

      # Attribute: Max Tree Size Class
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

## Get all model names
models <- list.files(docDir, pattern=".docx") %>%
  			substr(., start=1, stop=nchar(.)-5) # gets all text in name before .docx

#i <- which(models=="13110_50")

## For each model, extract crosswalk from corresponding Word doc
for(i in 1:length(models)) { 
  
  docu <- read_docx(paste0(docDir, models[i], ".docx"))
  
  # Collect data from lines beginning with "Class ", 
  # then omit NAs and missing data 
  classPos <- which((substr(docu, start=1, stop=6) == "Class ")
                    & (grepl(" - ", docu, fixed=T))
                    & (!substr(docu, start=8, stop=9) == " ")
                    & (!is.na(as.numeric(substr(docu, start=8, stop=8)))))
  classes <- docu[classPos]

  # Search for text descriptions within each Class' section.
  # Use 'Maximum Tree' header as description endpoint; 
  	
  # Identify start points for description of document as start point. 
  # Some documents have extra description header, remove  	
  descriptionPos <- which(substr(docu, start=1, stop=12) == "Description")
	if((length(descriptionPos) > length(classes))) {
		descriptionPos <- descriptionPos[-1]	
	} 
  # End positions is defined by "Maximum Tree Size Class"
	endPos <- maxTreePos <- which(substr(docu, 
	                                     start=1, 
	                                     stop=12) == "Maximum Tree")
	
	# Some documents have missing "Maximum Tree Size Class" inputs. 
	# Identify which entries exist and assign NA accordingly
	# If all expected entries are present, ... 
	if(length(maxTreePos) == length(descriptionPos)){
	  # ... assign document content
	  maxTree <- docu[maxTreePos] 
	} else {
	  # If no expected entries are present, ...
	  if(length(maxTreePos) == 0){
	    # ... assign NA to all
	    maxTree <- rep(NA, length(descriptionPos))
	  } else {
	    # If there are fewer then expected maxTreePos, 
	    # compare positions to assign data
	    maxTree <- vector()
	    expectedLength <- length(descriptionPos)
	    maxTreeEntry <- 1
	    for(comparisonEntry in 1:expectedLength){
	      # If NA or the last, ...
	      if(is.na(maxTreePos[maxTreeEntry])){
	        # ... assign NA
	        maxTree[maxTreeEntry] <- NA
	      } else {
	        # If not the last, ...
	        if(!is.na(classPos[comparisonEntry + 1])){
	          # ... "Maximum Tree Size Class" should be 
	          # preceded by "Description" 
	          # and followed by "Class"
	          if(maxTreePos[maxTreeEntry] > descriptionPos[comparisonEntry] &
	             maxTreePos[maxTreeEntry] < classPos[comparisonEntry + 1]){
	            # If so, assign document content to appropriate position, and ...
	            maxTree[comparisonEntry] <- docu[maxTreePos[maxTreeEntry]]
	            # ... update the maxTreePos position to evaluate
	            maxTreeEntry <- maxTreeEntry + 1
	          } else {
	            # If not, assign NA
	            maxTree[maxTreeEntry] <- NA
	          }
	        } else {
	          # If the last, "Maximum Tree Size Class" should be 
	          # preceded by "Description" 
	          if(maxTreePos[maxTreeEntry] > descriptionPos[comparisonEntry]){
	            # If so, assign document content to appropriate position, and ...
	            maxTree[comparisonEntry] <- docu[maxTreePos[maxTreeEntry]]
	            # ... update the maxTreePos position to evaluate
	            maxTreeEntry <- maxTreeEntry + 1
	          } else {
	            # If not, assign NA
	            maxTree[maxTreeEntry] <- NA
	          }
	        }
	      }
	    }
	  }
	}
	
	# For endPos of descriptionPos, if missing MaxTree endpoints, 
	# replace with position of 'Class', 'Model parameters' or 'References' headers
	if(length(endPos) < length(descriptionPos)){
	  maxPosClass <- c(classPos[-1],
	                   min(which(substr(docu, 
	                                    start=1, 
	                                    stop=18) == "Model Parameters"),
	                       which(substr(docu, 
	                                    start=1, 
	                                    stop=12) == "References")))
		endPos <- vector()			
		for(k in 1:length(descriptionPos)){
		  mp <- ifelse((k == length(descriptionPos)), 
		               maxPosClass[k], 
		               descriptionPos[k+1])
		  maxPosTree <- which(substr(docu[descriptionPos[k]:mp],
		                             start=1, stop=12) == "Maximum Tree") +
		    descriptionPos[k]
		  endPos[k] <- min(c(maxPosTree, maxPosClass[k]))
		}
	}
	
	# Selecting all description text across multiple lines 
	classText <- vector()			
  for(k in 1:length(descriptionPos)){  
    if(((descriptionPos[k] +1) == (endPos[k]-1))){
  	  textout <- docu[(descriptionPos[k] + 1)]
  	  } else {
  	    if((endPos[k]-1) > (descriptionPos[k] + 1)){
  	      textout <- paste(docu[((descriptionPos[k] + 1): (endPos[k] - 1))],
  	                       collapse= " ")
  	     }
      }
  	classText[k] <- textout
  }			
	
  # Compile model crosswalk
  modelCrosswalk <- data.frame(Model_Code = models[i],
                               Class = sapply(classes, get.class),
                               CoverType = sapply(classes, get.coverType),
                               StructuralStage = sapply(classes, 
                                                        get.structuralStage),
                               MaxTreeSizeClass = sapply(maxTree, 
                                                         get.maxTreeSizeClass),
                               Description = classText,
                               stringsAsFactors = F)
  
  # Add to master crosswalk
  if(i == 1){
    crosswalk <- modelCrosswalk
  }else{
    crosswalk %<>% bind_rows(., modelCrosswalk)
  }
  print(i)
}

rm(modelCrosswalk, classes, i)


## Cleanup non-standard document names
crosswalk$Model_Code <- gsub(" ", "", crosswalk$Model_Code)
crosswalk$Model_Code <- gsub(".doc", "", crosswalk$Model_Code)
crosswalk$Model_Code <- sapply(crosswalk$Model_Code, 
                               function(x) ifelse(substr(x, 
                                                         start=nchar(x), 
                                                         stop=nchar(x)) == "_", 
                                                  substr(x, 
                                                         start=1, 
                                                         stop=nchar(x)-1), 
                                                  x))
crosswalk$Model_Code <- gsub("_RS", "", crosswalk$Model_Code)

## Change cover type/structural stage labels to codes
  # Cover type
	crosswalk %<>% mutate(CoverType = sub(" Development ", "", CoverType))

  # Structural stage
	crosswalk$StructuralStage <- sapply(crosswalk$StructuralStage, 
	                                    function(x){ifelse(x %in% 
	                                                         c("All Structures", 
	                                                           "All", 
	                                                           " All"), 
	                                                       "ALL", 
	                                                       ifelse(x == "Closed", 
	                                                              "CLS", 
	                                                              "OPN"))})

	# Cover type
	crosswalk$CoverType <- gsub("Early 1", "Early1", crosswalk$CoverType)

	# Text description
	crosswalk$Description <- gsub("  ", " ", crosswalk$Description)

## Save
write.csv(crosswalk, 
          paste0(resultsDir, "ClassCrosswalk-", Sys.Date(), ".csv"), 
          row.names = F)
