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


# Directories
docDir <- "~/Dropbox/Documents/ApexRMS/Work/a202/A202 - TNC LANDFIRE Support/Model Description Docs/"
#"C:/Users/leona/Documents/Apex Projects/A202 - TNC LANDFIRE Support/Data/Model Description Docs/"
resultsDir <- "~/Dropbox/Documents/ApexRMS/Work/a202/A202 - TNC LANDFIRE Support/Results/"

#### Create Class Crosswalk ####
# Define functions extracting desired attributes from text lines
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


###
# Get all model names
models <- list.files(docDir, pattern=".docx") %>%
  			substr(., start=1, stop=nchar(.)-5) #extracts all text in name before .docx


###
# For each model, extract crosswalk from corresponding Word doc
for(i in 1:length(models)) { 
  	
  	#Collect data from lines beginning with "Class ", then omit NAs and missing data, 
  	docu <- read_docx(paste0(docDir, models[i], ".docx"))
  	
  	desc <-	docu %>% 
  			.[which((substr(docu, start=1, stop=6) == "Class ")
  			& (grepl(" - ", docu, fixed=T)) 
  			& (!substr(docu, start=8, stop=9) == " ") 
  			& (!is.na(as.numeric(substr(docu, start=9, stop=10)))))]

  	#Search for text descriptions in the correct sections of document, 
  	#catch multirow descriptions or missing descriptions 
  	#Use Maximum tree section head as endpoint; when missing uses next Class header as end point. 
  	  	
  	description <- which(substr(docu, start=1, stop=12) == "Description") + 1
		if((length(description) > length(desc))) {
		description <- description[-1]	
				}

  	maxTree <- 	which(substr(docu, start=1, stop=12) == "Maximum Tree") - 1
  	
  	if(is.null(length(maxTree)) | length(maxTree) == 0) { 
  		# if no max tree was included in the document, use the Class headers as ending point 
  		maxTree <- which((substr(docu, start=1, stop=6) == "Class ")
  			& (grepl(" - ", docu, fixed=T)) 
  			& (!substr(docu, start=8, stop=9) == " ") 
  			& (!is.na(as.numeric(substr(docu, start=9, stop=10))))) - 1
  		# shift vector 1 position to use values as section end points for previous section.
  		maxTree <- c(maxTree[-1], length(docu))
  	}
  	
  	  # Error checking for formatting variations
  	desctext <- vector()			
  	for(j in 1:length(description)){ desctext[j] <- 
  		if((description[j] == maxTree[j])){
  			textout <- docu[(description[j])] 
  			} else {
  		if(maxTree[j] > description[j]){
			textout <- paste(docu[(description[j]:maxTree[j])], collapse= " ")
  		}else{
  			textout <- "NA"
  		}}
  		textout
  	}			
			
	  			
  			 
  # Compile model crosswalk
  modelCrosswalk <- data.frame(Model_Code = models[i],
                               Class = sapply(desc, get.class),
                               CoverType = sapply(desc, get.coverType),
                               StructuralStage = sapply(desc, get.structuralStage),
                               Description = desctext,
                               stringsAsFactors = F)
  
  # Add to master crosswalk
  if(i == 1){
    crosswalk <- modelCrosswalk
  }else{
    crosswalk %<>% bind_rows(., modelCrosswalk)
  }
  print(i)
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
