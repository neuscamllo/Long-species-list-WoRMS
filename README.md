Citation: Neus Campanyà-Llovet. (2024). neuscamllo/Long-species-list-WoRMS: v1.0.0 (v1.0.0). Zenodo. https://doi.org/10.5281/zenodo.13991855
![image](https://github.com/user-attachments/assets/ed54b554-9b8b-41a3-b089-ecc8f8e8b197)

# Long-species-list-WoRMS
 Verifying and extracting taxonomic information of long species lists

This function is designed to verify and extract taxonomic information from a list of one, few, 10s, 100s or 1000s of species from the WoRMS website (World Register of Marine Species) at once.

The input variable is a character vector of any length, which can include authorship information (recommended to reduce match ambiguity) or not.

The function returns a data frame with the following information: scientific name, accepted AphiaID and scientific names, Phylum, Class, Order, Family, Genus and type of match according to WoRMS classification (e.g., exact, exact_subgenus, near_1, near_2, phonetic).

The function is designed to skip missmatches without stoping the full process, which proves advantageous when dealing with large databases.

