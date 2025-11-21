-   [Scraping genome assembly information for fish species on
    GenBank](#scraping-genome-assembly-information-for-fish-species-on-genbank)
    -   [Loading the packages](#loading-the-packages)
    -   [Loading dataset](#loading-dataset)
    -   [Family order of
        Actinopterygii](#family-order-of-actinopterygii)
    -   [Preparing storages](#preparing-storages)
    -   [Defining scraping function](#defining-scraping-function)
    -   [Scraping on GenBank](#scraping-on-genbank)
-   [Sort along systematics](#sort-along-systematics)
    -   [Write a result file](#write-a-result-file)
    -   [Furture works](#furture-works)

# Scraping genome assembly information for fish species on GenBank

## Loading the packages

``` r
# Loading packages
library(tidyverse)
library(rentrez)
```

## Loading dataset

``` r
species_raw <- read_csv("FRA200List_Latin.csv")

#filtering out species with NA
species <- species_raw %>% drop_na()
n_species <- nrow(species) #number of fishes in the list

#Set a small dataset for test procedure
#n_species <- 20 # for test
#species <- species[1:no_species,]
```

``` r
# genome size estimate from animal genome size database
#http://www.genomesize.com/index.php
AGSDB_List <- read_csv("AGSDB_List.csv")
no_AGSDB_List <- nrow(AGSDB_List)
AGSDB_C_vec <- vector()
AGSDB_genus_vec <- vector()

for(i in 1:no_AGSDB_List){
  AGSDB_C_values <- strsplit(AGSDB_List$'C-value'[i], "-") %>% unlist
  AGSDB_C_vec[i] <- as.numeric(AGSDB_C_values[1])
  AGSDB_species_unlist <-strsplit(AGSDB_List$Species[i]," ") %>% unlist
  AGSDB_genus_vec[i] <- AGSDB_species_unlist[1]
}
AGSDB_List <- AGSDB_List %>% mutate(Genus=AGSDB_genus_vec, C_value=AGSDB_C_vec)

AGSDB_genus_ave_genome_size <- tapply(AGSDB_List$C_value,AGSDB_List$Genus, mean)
AGSDB_no_genus_avail <- tapply(AGSDB_List$C_value,AGSDB_List$Genus, length)
```

## Family order of Actinopterygii

``` r
FamilyOrderActinopterygii_info <- read_csv("20250911_JAFList.csv")
FamilyOrderActinopterygii <- FamilyOrderActinopterygii_info$Family %>% unique()
```

## Preparing storages

``` r
target_sp_genome_assembly_exist <- vector()
target_sp_genome_size_vec <- vector() # genome size of the target species
related_spp_genome_size_vec <- vector() # mean genome size of related species
related_spp_genome_assembly_exist <- vector()
no_related_spp_exist <- vector()

no_species_with_assembly_within_the_genus_vec <- vector()
ave_genome_size_of_the_genus_vec <- vector()


No_deposited_sp_level_genome_vec <- vector()
representative_assembly_ID_vec <- vector()
representative_assembly_status_vec <- vector()
representative_contigN50_vec <- vector()
representative_scaffoldN50_vec <- vector()
representative_genome_size_vec <- vector()

AGSDB_genus_genome_size_vec <- vector()
AGSDB_no_genus_avail_vec <- vector()
```

## Defining scraping function

``` r
#https://bioconnector.github.io/workshops/r-ncbi.html#introduction
#https://pediatricsurgery.hatenadiary.jp/entry/2018/01/10/205737
#entrez_db_searchable(db ="genome")


#-------------------------------------------------------------------------
#Scraping information of genome-assembly of a target species using rentrez
genome_info_sp_NCBI <- function(scientific_name){
  scientific_name_vec <- unlist(strsplit(scientific_name,"\\s+"))
  genus <- scientific_name_vec[1]
  species <- scientific_name_vec[2]
  species_lab <- paste0(genus," ",species)
  query_species_lab <- paste0(genus," ",species,"[Organism]")
  spp_genome_info <- entrez_search(db="assembly", term =query_species_lab)
  no_assembly <- length(spp_genome_info$ids)
  
  if(no_assembly > 1){
  
    spp_genome_info_list <- entrez_summary(db="assembly", id=spp_genome_info$ids)

      for(i in 1:no_assembly){
        represent_sp_genome <- NA
        
        if(spp_genome_info_list[[i]]$refseq_category=="representative genome"){
          represent_sp_genome <- spp_genome_info_list[[i]] #Primary genome-assembly
          represent_sp_genome_accession <- spp_genome_info_list[[i]]$assemblyaccession
    
          #extracting genome size info: the object "represent_sp_genome$meta" includes genome size etc.
          represent_meta <- unlist(strsplit(represent_sp_genome$meta,"> <"))
          represent_genome_size_raw <- represent_meta[which(str_detect(represent_meta, pattern="total_length"))]
          represent_genome_size <- as.numeric(str_extract_all(represent_genome_size_raw, "[0-9.]+"))
          represent_contigN50 <- represent_sp_genome$contign50
          represent_scaffoldN50 <- represent_sp_genome$scaffoldn50
        }
        
        if(!is.list(represent_sp_genome)){
          arbitary_represent_id <- max(as.numeric(spp_genome_info$ids))
          represent_sp_genome <- entrez_summary(db="assembly", id=arbitary_represent_id)
          represent_sp_genome_accession <- represent_sp_genome$assemblyaccession
          
          #extracting genome size info
          represent_meta <- unlist(strsplit(represent_sp_genome$meta,"> <"))
          represent_genome_size_raw <- represent_meta[which(str_detect(represent_meta, pattern="total_length"))]
          represent_genome_size <- as.numeric(str_extract_all(represent_genome_size_raw, "[0-9.]+"))
          represent_contigN50 <- represent_sp_genome$contign50
          represent_scaffoldN50 <- represent_sp_genome$scaffoldn50
        }
      }
    
  }else if(no_assembly == 1){
    represent_sp_genome <- entrez_summary(db="assembly", id=spp_genome_info$ids)
    represent_sp_genome_accession <- represent_sp_genome$assemblyaccession
    
    #extracting genome size info
    represent_meta <- unlist(strsplit(represent_sp_genome$meta,"> <"))
    represent_genome_size_raw <- represent_meta[which(str_detect(represent_meta, pattern="total_length"))]
    represent_genome_size <- as.numeric(str_extract_all(represent_genome_size_raw, "[0-9.]+"))
    represent_contigN50 <- represent_sp_genome$contign50
    represent_scaffoldN50 <- represent_sp_genome$scaffoldn50
    
  }else{#(no_assembly == 0)
    represent_sp_genome <- NA
    represent_sp_genome_accession <- NA
    represent_genome_size <- NA
    represent_contigN50 <- NA
    represent_scaffoldN50 <- NA

  }
  
  represent_genome_size_Mbp <- round(represent_genome_size/10^6, digits=0)
  output <- list(species_lab,
                 no_assembly,
                 represent_sp_genome,
                 represent_sp_genome_accession, 
                 represent_genome_size_Mbp,
                 represent_contigN50,
                 represent_scaffoldN50
                 )
  return(output)
  Sys.sleep(1) #
}

#-------------------------------------------------------------------------


#-------------------------------------------------------------------------
#Scraping information of genome-assembly of a target species using rentrez
genome_info_genus_NIH <- function(genus){
  
  query_genus_lab <- paste0(genus,"[Organism]")
  genome_info_genus <- entrez_search(db="assembly", term =query_genus_lab)
  genus_ids_vec <- genome_info_genus$ids
  no_assembly_genus <-length(genus_ids_vec)
  
  speciesname_vec <- vector()
  
  if(no_assembly_genus == 0){
    no_sp_within_genus <- 0 
    ave_genus_genome_size_Mbp <- NA
  
  }else{
    for(i in 1:no_assembly_genus){
      genus_ids_each <- genus_ids_vec[i]
      genome_info_genus_each <- entrez_summary(db="assembly", id=genus_ids_each) 
      speciesname_vec[i] <- genome_info_genus_each$speciesname
    }
    uniq_speciesname <- unique(speciesname_vec)
    no_sp_genus <- length(uniq_speciesname)
    
    genus_spp_genome_size_vec <- vector()    
    for(j in 1:no_sp_genus){
     
      each_speciesname <- uniq_speciesname[j]
      genus_species_name <- unlist(strsplit(each_speciesname," "))
      genus_name <- genus_species_name[1]
      species_name <- genus_species_name[2]
      scientific_name_input <- paste0(genus_name," ",species_name)
      
      representative_genus_spp_info <- genome_info_sp_NCBI(scientific_name_input)
      #representative_genus_spp_info <- genome_info_sp_NIH(genus_name, species_name)
      genus_spp_genome_size_vec[j] <- representative_genus_spp_info[[5]]
    } 
    no_sp_within_genus <- no_sp_genus
    ave_genus_genome_size_Mbp <- mean(genus_spp_genome_size_vec, na.rm=TRUE) %>% round(digits=1)
    
  } 

  
  genus_genome_output <- list(no_sp_within_genus,ave_genus_genome_size_Mbp)
  return(genus_genome_output)
  Sys.sleep(1) #
}
#-------------------------------------------------------------------------
```

## Scraping on GenBank

``` r
for(i in 1:n_species){
  scientific_name_input <- species[i,]$Scientific_name
  # split genus/species names
  genus_species_name <- strsplit(species[i,]$Scientific_name, " ") %>% unlist 
  genus_name <- genus_species_name[1]
  species_name <- genus_species_name[2]
  
  genus_info <- genome_info_genus_NIH(genus_name)
  no_species_with_assembly_within_the_genus_vec[i] <- genus_info[[1]]
  ave_genome_size_of_the_genus_vec[i] <- genus_info[[2]]
  
  #------------------------------------------
  #scraping representative genome using rentrez
  rentrez_scraping_out <- genome_info_sp_NCBI(scientific_name_input)

  if(rentrez_scraping_out[[2]]>=1){
    representative_assembly_ID_vec[i] <- rentrez_scraping_out[[4]]
    representative_assembly_status_vec[i] <- rentrez_scraping_out[[3]]$assemblystatus
    representative_contigN50_vec[i] <- rentrez_scraping_out[[6]]
    representative_scaffoldN50_vec[i] <- rentrez_scraping_out[[7]]
    representative_genome_size_vec[i] <- rentrez_scraping_out[[5]]

  }else{
    representative_assembly_ID_vec[i] <- NA
    representative_assembly_status_vec[i] <- NA
    representative_contigN50_vec[i] <- NA
    representative_scaffoldN50_vec[i] <- NA
    representative_genome_size_vec[i] <- NA
  }
  No_deposited_sp_level_genome_vec[i] <- rentrez_scraping_out[[2]]

  
  #------------------------------------------
  AGSDB_genus_ID <- which(names(AGSDB_genus_ave_genome_size)==genus_name)
  
  if(!(identical(AGSDB_genus_ID,integer(0)))){
    AGSDB_genus_genome_size_vec[i] <- round(as.numeric(AGSDB_genus_ave_genome_size[AGSDB_genus_ID])*978, digits=0)
    AGSDB_no_genus_avail_vec[i] <- AGSDB_no_genus_avail[AGSDB_genus_ID] %>% as.numeric()
  }else{
    AGSDB_genus_genome_size_vec[i] <- NA
    AGSDB_no_genus_avail_vec[i] <- NA
  }
 
  Sys.sleep(2)
}


species_genome <- species %>%
  mutate (Genome_size_of_the_species_Mbp = representative_genome_size_vec,
          No_assembly_of_the_species = No_deposited_sp_level_genome_vec,
          Representative_assembly = representative_assembly_ID_vec,
          Representative_assembly_status = representative_assembly_status_vec,
          Contig_N50 = representative_contigN50_vec,
          Scaffold_N50 = representative_scaffoldN50_vec,
          Average_genome_size_of_the_genus_Mbp = ave_genome_size_of_the_genus_vec, 
          No_species_with_assembly_within_the_genus = no_species_with_assembly_within_the_genus_vec,
          AGSDB_genus_genome_size = AGSDB_genus_genome_size_vec,
          AGSDB_No_genus_avail = AGSDB_no_genus_avail_vec)
```

# Sort along systematics

``` r
species_genome$Phylum <- factor(species_genome$Phylum,
                                 levels = c("Ochrophyta",
                                            "Arthropoda",
                                             "Mollusca",
                                            "Echinodermata",
                                            "Chordata")
                                )


class_tree <- c(
  "Phaeophyceae",
  "Malacostraca",
  "Bivalvia",
  "Gastropoda",
  "Cephalopoda",
  "Echinoidea",
  "Holothuroidea",
  "Chondrichthyes",
  "Actinopterygii",
  "Mammalia"
  )

order_tree <- c(
  "Laminariales",
  "Decapoda","Euphausiacea","Stomatopoda",
  "Arcoida","Veneroida",
  "Vetigastropoda","Sorbeoconcha",
  "Cephalopoda","Octopoda","Sepioidea","Teuthoidea",
  "Camarodonta","Aspidochirotida",
  "Squaliformes","Orectolobiformes","Lamniformes","Carcharhiniformes","Myliobatiformes",
  "Anguilliformes","Aulopiformes","Beloniformes","Beryciformes","Clupeiformes","Gadiformes",
  "Lampriformes","Lophiiformes","Ophidiiformes","Osmeriformes","Perciformes","Pleuronectiformes",
  "Salmoniformes","Scorpaeniformes","Syngnathiformes","Tetraodontiformes","Zeiformes",
  "Carnivora","Cetartiodactyla"
)



species_genome$Class <- factor(species_genome$Class,
                                levels = class_tree)

species_genome$Order <- factor(species_genome$Order,
                                levels = order_tree)


species_genome <- species_genome %>%
  arrange(Phylum, Class, Order, Family)
```

## Write a result file

``` r
write_csv(species_genome, "aquatic_organism_genome_size.csv")
```

## Furture works

``` r
# list up for no. contigs/scaffolds
```
