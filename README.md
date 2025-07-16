 # Scientific Collecting and Salvaged Specimens Play Complementary Roles in the Growth of Museum Collections 
 A case study for the state of California comparing taxonomic and geographic representation of preserved bird specimens deposited within the ornithology collection at the Museum of Vertebrate Zoology at University of California Berkley
 
 # Dataset description
 All records were pulled from https://arctos.database.museum/, with the following search criteria: PUT SEARCH CRITERIA HERE. The resulting dataset was entitiled: 
 'Arctos_birds_Calif_2000_2020'. The following datasets were subsequently generated:
 
 'Arctos_all': Resulting dataset after intial cleaning of 'Arctos_birds_Calif_2000_2020. Column headings are: 'guid', 'accn_number', 'spec_locality', 'scientific_name', 'genus', 'species', 'other', 'dec_lat', 'dec_long', 'coll_method', 'genus_species'. This dataset was used to generate 'taxa_dataset_for_analyses_and_plotting', 'species_per_order', and 'specimens_per_order'

'species_per_order' and 'specimens_per_order': Resulting data

'taxa_dataset_for_analyses_and_plotting' : Resulting dataset generated using 'Arctos_all', 'ebird_US-CA__1950_2025_1_12_barchart', 'species_per_order', and 'specimens_per_order' via script 'taxonomic_analyses_and_figures' for use in taxonomic analyses. Column headings are: 'order', 'species_count_salvage', 'species_count_active', 'species_count_prop_salvage', 'species_count_prop_active', 'log_species_prop_salvage', 'log_species_prop_active', 'specimens_count_salvage', 'specimens_count_active', 'specimens_count_prop_salvage', 'specimens_count_prop_active', 'log_specimens_prop_salvage', and 'log_specimens_prop_active'.
 
