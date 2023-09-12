# Long-species-list-WoRMS
 Verifying and extracting taxonomic information of long species lists

This function is designed to verify and extract taxonomic information from a list of 10s (> 30), 100s or 1000s of species from the WoRMS website (World Register of Marine Species) at once.

The input variable is a character vector of at least 30 taxonomic names, which can include authorship information (recommended to reduce match ambiguity) or not.

The function returns a data frame with the following information: scientific name, accepted AphiaID and scientific names, Phylum, Class, Order, Family, Genus and type of match according to WoRMS classification (e.g., exact, exact_subgenus, near_1, near_2, phonetic).

The function is designed to skip miss matches without stoping the matching process, which proves advantageous when dealing with large databases.

