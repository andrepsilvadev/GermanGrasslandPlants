This is a small database with data about growth, reproduction, dispersal, biomass and mortality of vascular plants that occur in Bavaria and Baden-Württemberg, Germany.

Operating instructions?

Copyright and licensing?

The folder includes:
-> BLIZ.Rproj: R project
-> codeBLIZ.Rmd: Code to be run
-> Data: contains all the data collected from various databases
	-Rote_List_Bayern: with the species list found in the red list of vascular plants in Bavaria

	-Species_List_Baden & Species_List_Bavaria: species list taken from GBIF database for Bavaria and Baden-Württemberg

	-Files with trait data: Losova_et_al_2023_Dispersal, PlantDryMass, PlantGrowthRate, PlantNE_Biomass, and seed_number

	-dispersal_classes: with information on maximum and mean dispersal distances from Losova_et_al_2023_Dispersal

-> Data_info&references - contains information about the databases and references
	-a table with all references of the data included in our database

	-2 files with information regarding the PlantNE database

	-2 files with information regarding the TRY database

-> Results - contains the resulting files from data collection

	-Occurrences_sp_bavaria: with the number of occurrences from GBIF database in Bavaria

	-traits_complete: with trait values, methods and units for seed number, dispersal distance, growth rate, wetmass, and mortality

	-traits_extra: with trait values, methods and units for seed number, dispersal distance, growth rate, and drymass

	-final_traits: is the combination of traits_complete and traits_extra, but with one row per species and only columns with values

	-final_traits_w_density: is the final_traits table with the planting density data which was manually collected

