#### a202 - TNC LANDFIRE Support
#### Script by Chloé Debyser 
#### Updated by Caroline Tucker 09/2020

#### 1. Generate Class Crosswalk 

###########################################################################################################################
# This code:                                                                                                              #
# - Reads Word documents containing class definitions for each model                                                      #
# - Creates a crosswalk for stage Classes A-E per Model | Cover Type | Structural Stage | Description                                  #
###########################################################################################################################

#### Workspace ####
# Packages
library(tidyverse)
library(magrittr)
library(textreadr)
library(stringr)


#### Directories
docDir <- "~/Dropbox/Documents/ApexRMS/Work/a202/Model Description Docs/"
#"C:/Users/leona/Documents/Apex Projects/A202 - TNC LANDFIRE Support/Data/Model Description Docs/"
resultsDir <- "~/Dropbox/Documents/ApexRMS/Work/a202/A202 - TNC LANDFIRE Support/Results/"


#### Create Class Crosswalk 
## Define functions extracting desired attributes from text lines
      # Attribute: Class
get.class <- function(x){ #Should be generalized
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


## Get all model names
models <- list.files(docDir, pattern=".docx") %>%
  			substr(., start=1, stop=nchar(.)-5) #extracts all text in name before .docx

#i <- which(models=="13110_50")

## For each model, extract crosswalk from corresponding Word doc
for(i in 1:length(models)) { 
  	
  	docu <- read_docx(paste0(docDir, models[i], ".docx"))
  	 
  	#Collect data from lines beginning with "Class ", then omit NAs and missing data 
  	classPos <- which((substr(docu, start=1, stop=6) == "Class ")
  			& (grepl(" - ", docu, fixed=T)) 
  			& (!substr(docu, start=8, stop=9) == " ") 
  			& (!is.na(as.numeric(substr(docu, start=9, stop=10)))))
  	
  	classes <-	docu[classPos]

  	#Search for text descriptions within each Class' section.
  	#Use 'Maximum Tree' header as description endpoint; 
  	#Account for missing endpoints by using next Class header as end point. 
  	
  	 # Identify start points for description of document as start point. 
  	 #Some documents have extra description header, remove  	
  	descriptionPos <- which(substr(docu, start=1, stop=12) == "Description")
		if((length(descriptionPos) > length(classes))) {
		descriptionPos <- descriptionPos[-1]	
				} 
	endPos <- which(substr(docu, start=1, stop=12) == "Maximum Tree")
	
	
	#If missing MaxTree endpoints, replace with position of 'Class' or 'Model parameters' or 'References' headers	
	if(length(endPos) < length(descriptionPos)){
		  	maxPosClass <- c(classPos[-1], min(which(substr(docu, start=1, stop=18) == "Model Parameters"), which(substr(docu, start=1, stop=12) == "References")))
		endPos <- vector()			
			for(k in 1:length(descriptionPos)){
				mp <- ifelse((k == length(descriptionPos)), maxPosClass[k], descriptionPos[k+1])
		  		maxPosTree <- which(substr(docu[descriptionPos[k]:mp], start=1, stop=12) == "Maximum Tree") + descriptionPos[k]
		  	endPos[k] <- min(c(maxPosTree, maxPosClass[k]))
	}			
}
  	
  	# Selecting all description text across multiple lines 
 	classText <- vector()			
  	for(j in 1:length(descriptionPos)){  
  		if(((descriptionPos[j] +1) == (endPos[j]-1))){
  			textout <- docu[(descriptionPos[j] + 1)] 
  			} else {
  		if((endPos[j]-1) > (descriptionPos[j] + 1)){
			textout <- paste(docu[((descriptionPos[j] + 1): (endPos[j] - 1))], collapse= " ")
  		}
  			}
  		classText[j] <- textout
  	}			
			
	  					 
  	# Compile model crosswalk
  	modelCrosswalk <- data.frame(Model_Code = models[i],
                               Class = sapply(classes, get.class),
                               CoverType = sapply(classes, get.coverType),
                               StructuralStage = sapply(classes, get.structuralStage),
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
crosswalk$Model_Code <- sapply(crosswalk$Model_Code, function(x) ifelse(substr(x, start=nchar(x), stop=nchar(x)) == "_", substr(x, start=1, stop=nchar(x)-1), x))
crosswalk$Model_Code <- gsub("_RS", "", crosswalk$Model_Code)

## Change cover type/structural stage labels to codes
  # Cover type
	crosswalk %<>% mutate(CoverType = sub(" Development ", "", CoverType))

  # Structural stage
	crosswalk$StructuralStage <- sapply(crosswalk$StructuralStage, function(x){ifelse(x %in% c("All Structures", "All", " All"), "ALL", ifelse(x == "Closed", "CLS", "OPN"))})

	# Text description
	crosswalk$Description <- gsub("  ", " ", crosswalk$Description)

## Save
write.csv(crosswalk, paste0(resultsDir, "ClassCrosswalk.csv"), row.names = F)
