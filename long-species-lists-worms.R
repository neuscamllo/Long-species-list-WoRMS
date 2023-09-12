rm(list = ls())

# loading the require packages
library(jsonlite) # install.packages("jsonlite", repos="http://cran.r-project.org")
library(httr) # install.packages("httr")
library(stringr)
library(dplyr)

# Importing list of species from various sources
setwd("C:/R")
emodnet_raw <- read.csv2("faz_AMP_EMODnetrecords.csv")
sp_aut_emodnet <- cbind.data.frame(emodnet_raw$scientificnameaccepted, emodnet_raw$scientificnameauthorship)
sp_aut_emodnet_unique <- sp_aut_emodnet[!duplicated(sp_aut_emodnet),]
sp_aut_emodnet_unique[3] <- "EMODnet"
colnames(sp_aut_emodnet_unique) <- c("species","author","database")

obis_raw <- read.csv2("obis_square.csv")
sp_aut_obis <- cbind.data.frame(obis_raw$scientificName, obis_raw$scientificNameAuthorship)
sp_aut_obis_unique <- sp_aut_obis[!duplicated(sp_aut_obis),]
sp_aut_obis_unique[3] <- "OBIS"
colnames(sp_aut_obis_unique) <- c("species","author","database")

sp_aut_all <- rbind.data.frame(sp_aut_emodnet_unique, sp_aut_obis_unique)
tail(sp_aut_all)

# Cleaning species and author names
sp_aut_all$species <- str_remove(sp_aut_all$species, " sp.")
sp_aut_all$species[which(str_detect(sp_aut_all$species, " spp."))] # rank class, therefore we remove
sp_aut_all <- sp_aut_all[which(!str_detect(sp_aut_all$species, " spp.")),]
sp_aut_all$species <- str_replace(sp_aut_all$species, "\\(", " (")
sp_aut_all$author <- str_remove(sp_aut_all$author, "\\(")
sp_aut_all$author <- str_remove(sp_aut_all$author, "\\)")
sp_aut_all <- mutate(sp_aut_all, sp_aut = paste(species,author, sep = " "))

spplist <- sp_aut_all$sp_aut

database_spp_check <- function(spplist) {
  # Checking initial conditions
  if (is.character(spplist) == FALSE) {
    print("Species list must be a character vector")
  } else if (length(spplist) < 30){
    print("Species list must contain at least 30 species names")
  } else {
    # initializing output variables
    sp_info <- data.frame() 
    # selecting first group of 30 species and creating a url to run through worms
    for (i in seq(from = 1, to = length(spplist), by = 30)) { 
      skip_to_next <- FALSE
      namesToMatch <- as.character(unlist(spplist[i:(i+29)]))
      urlNamesPart <- ""
      for (index in 1:length(namesToMatch)) {
        urlNamesPart <- sprintf("%s&scientificnames[]=%s", urlNamesPart, namesToMatch[index]);
      }
      urlNamesPart <- URLencode(urlNamesPart)
      urlNamesPart <- substring(urlNamesPart, 2)
      url <- sprintf("http://www.marinespecies.org/rest/AphiaRecordsByMatchNames?%s", urlNamesPart);
      matches <- tryCatch(fromJSON(url), error = function(e) { skip_to_next <<- TRUE}) # detecting "error" results from worms search  
      if(skip_to_next) { next } # Skiping the erroneous entry without stopping the match search
      # extracting taxonomic information for each species
      for (matchesindex in 1:length(namesToMatch)) { 
        currentResultList = matches[[matchesindex]]
        numberOfResults <- tryCatch(length(currentResultList[[1]][[1]]), error = function(e) {numberOfResults <- 0}) # Number of results #&# Handle empty data due to no matches found at WoRMS website
        if (numberOfResults > 0) {
          for (listentry in 1:numberOfResults) {
            res_info <- data.frame(species = currentResultList[["scientificname"]][listentry], accepted_AphiaID = currentResultList[["valid_AphiaID"]][listentry],
                                   accepted_scientific_name = currentResultList[["valid_name"]][listentry],
                                   accepted_authorship = currentResultList[["valid_authority"]][listentry],
                                   phylum = currentResultList[["phylum"]][listentry], class = currentResultList[["class"]][listentry],
                                   order = currentResultList[["order"]][listentry], family = currentResultList[["family"]][listentry],
                                   genus = currentResultList[["genus"]][listentry], match_type = currentResultList[["match_type"]][listentry])
            sp_info <- rbind.data.frame(sp_info, res_info)
          }
        }
      }
    }
    sp_info_clean <- sp_info[which(sp_info$species != "Nia"),]
    return(sp_info_clean)
  }
    
}
  
res <- database_spp_check(spplist)
write.csv2(res, "sp_matched_worms.csv")


