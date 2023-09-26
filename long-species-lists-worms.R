#'database_spp_check': function to verify and extract taxonomic information and status for a large list of marine species from the World Register of Marine Species (WoRMS, marinespecies.org)
# 
# INPUT: 
#   - spplist: Character vector with list of taxa which can also include authorship name and year. The list needs to have a minimum of 30 taxa.
# 
# OUTPUT:
#   - a data frame with the taxa entered (species_source), accepted aphiaID, species names, classification and type of march at WoRMS.
# 
# Library requirements: jsonlite
###############################################################################################################################################

database_spp_check <- function(spplist) {
  # initializing output variables
  sp_info <- data.frame() 
  # Checking initial conditions
  if (is.character(spplist) == FALSE) {
    print("Species list must be a character vector")
  } else if (length(spplist) < 30){
    skip_to_next <- FALSE
    namesToMatch <- as.character(unlist(spplist))
    urlNamesPart <- ""
    for (index in 1:length(namesToMatch)) {
      urlNamesPart <- sprintf("%s&scientificnames[]=%s", urlNamesPart, namesToMatch[index]);
    }
    urlNamesPart <- URLencode(urlNamesPart)
    urlNamesPart <- substring(urlNamesPart, 2)
    url <- sprintf("http://www.marinespecies.org/rest/AphiaRecordsByMatchNames?%s", urlNamesPart);
    matches <- tryCatch(jsonlite::fromJSON(url), error = function(e) { skip_to_next <<- TRUE}) # detecting "error" results from worms search  
    if(skip_to_next) { next } # Skiping the erroneous entry without stopping the match search
    # extracting taxonomic information for each species
    for (matchesindex in 1:length(namesToMatch)) { 
      currentResultList = matches[[matchesindex]]
      numberOfResults <- tryCatch(length(currentResultList[[1]][[1]]), error = function(e) {numberOfResults <- 0}) # Number of results #&# Handle empty data due to no matches found at WoRMS website
      if (numberOfResults > 0) {
        for (listentry in 1:numberOfResults) {
          res_info <- data.frame(species_source = namesToMatch[matchesindex],
                                 species = currentResultList[["scientificname"]][listentry], accepted_AphiaID = currentResultList[["valid_AphiaID"]][listentry],
                                 accepted_scientific_name = currentResultList[["valid_name"]][listentry],
                                 accepted_authorship = currentResultList[["valid_authority"]][listentry],
                                 phylum = currentResultList[["phylum"]][listentry], class = currentResultList[["class"]][listentry],
                                 order = currentResultList[["order"]][listentry], family = currentResultList[["family"]][listentry],
                                 genus = currentResultList[["genus"]][listentry], match_type = currentResultList[["match_type"]][listentry])
          sp_info <- rbind.data.frame(sp_info, res_info)
        }
      }
    }
  } else {
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
      matches <- tryCatch(jsonlite::fromJSON(url), error = function(e) { skip_to_next <<- TRUE}) # detecting "error" results from worms search  
      if(skip_to_next) { next } # Skiping the erroneous entry without stopping the match search
      # extracting taxonomic information for each species
      for (matchesindex in 1:length(namesToMatch)) { 
        currentResultList = matches[[matchesindex]]
        numberOfResults <- tryCatch(length(currentResultList[[1]][[1]]), error = function(e) {numberOfResults <- 0}) # Number of results #&# Handle empty data due to no matches found at WoRMS website
        if (numberOfResults > 0) {
          for (listentry in 1:numberOfResults) {
            res_info <- data.frame(species_source = namesToMatch[matchesindex],
                                   species = currentResultList[["scientificname"]][listentry], accepted_AphiaID = currentResultList[["valid_AphiaID"]][listentry],
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
  }
  sp_info_clean <- sp_info[which(sp_info$species != "Nia"),]
  return(sp_info_clean)
}

res <- database_spp_check(spplist)



