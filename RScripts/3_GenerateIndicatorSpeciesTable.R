#### a202 - TNC LANDFIRE Support
#### Script by Carina Firkowski 

#### 3. Generate indicator species & canopy position table 

################################################################################
# This code:                                                                   #
# - Reads Word documents containing class definitions for each model           #
# - Creates a table for Model code, Class, Symbol, Scientific Name,            #
#   Common Names, and Canopy Position                                          #
################################################################################

# Load constants
source("RScripts/0_Constants.R")

# Create table -----------------------------------------------------------------

#i <- which(models=="11770_3")

# Loop across each model
for(i in 1:length(models)) { 
  
  # Read in document to identify classes
  docu <- qdapTools::read_docx(paste0(docDir, models[i], ".docx"))
  
  # Identify lines beginning with "Class " while omitting NAs & missing data 
  classPos <- which((substr(docu, start=1, stop=6) == "Class ")
                    & (grepl(" - ", docu, fixed=T))
                    & (!substr(docu, start=8, stop=9) == " ")
                    & (!is.na(as.numeric(substr(docu, start=8, stop=8)))))
  # Get "Class" data 
  classesText <- docu[classPos]
  classesNames <- as.vector(sapply(classesText, get.class))
  
  # Read in document to identify "Indicator species" tables
  docuSpp <- docxtractr::read_docx(paste0(docDir, models[i], ".docx"))
  
  # Get all tables
  allTables <- docx_extract_all_tbls(docuSpp)
  
  # Get "Indicator Species" tables
  tablesPos <- grep("Symbol", allTables)[-1]
  #sppTables <- allTables[tablesPos]
  sppTables <- lapply(tablesPos, function(x) docx_extract_tbl(docuSpp, 
                                                              tbl_number = x))
  
  # Identify number of rows per table
  dimTables <- sapply(sppTables, dim)[1,]
  # Write vector of class names
  classes <- as.character()
  for(j in 1:length(classesNames)){
    if(dimTables[j] == 0){ classNumber <- dimTables[j]+1
    } else { classNumber <- dimTables[j] }
    temp_classes <- rep(classesNames[j], each = classNumber)
    classes <- c(classes, temp_classes)
  }
  # Assign NA to table if missing all data
  for(k in 1:length(dimTables)){
    if(dimTables[k] == 0){
      temp_sppTables <- data.frame(Symbol = NA,
                                   Scientific.Name = NA,
                                   Common.Name = NA,
                                   Canopy.Position = NA)
      sppTables[[k]] <- bind_rows(sppTables[[k]], temp_sppTables)
    }
  }
  # Combine tables
  modelTables <- sppTables %>%
    bind_rows()
  
  # Compile crosswalk
  indicatorSpp <- data.frame(Model_Code = models[i],
                             Class = classes,
                             Symbol = modelTables$Symbol,
                             ScientificName = modelTables$Scientific.Name,
                             CommonName = modelTables$Common.Name,
                             CanopyPosition = modelTables$Canopy.Position,
                             stringsAsFactors = F)
  
  # Add to master crosswalk
  if(i == 1){
    crosswalk <- indicatorSpp
  }else{
    crosswalk %<>% bind_rows(., indicatorSpp)
  }
  print(i)
}

rm(indicatorSpp, classes, i)

# Save
write.csv(crosswalk, 
          paste0(resultsDir, "IndicatorSpecies-", Sys.Date(), ".csv"), 
          row.names = F)
