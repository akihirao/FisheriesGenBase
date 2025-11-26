-   [Linking Japanes/scientific names in aquatic
    species](#linking-japanesscientific-names-in-aquatic-species)
    -   [Loading the packages](#loading-the-packages)
    -   [Loading dataset](#loading-dataset)
    -   [Linking Japanese name and scientific
        name](#linking-japanese-name-and-scientific-name)
    -   [Write a taxonomy list file](#write-a-taxonomy-list-file)

# Linking Japanes/scientific names in aquatic species

## Loading the packages

``` r
# Loading packages
library(tidyverse)
library(rvest) #for scraping
library(rentrez)
```

## Loading dataset

``` r
rm(list=ls(all=TRUE))

# Current standard Japanese/scientific names of all fish species recorded from Japanese waters: https://www.museum.kagoshima-u.ac.jp/staff/motomura/jaf.html
JAFList <- read_csv("20250911_JAFList.csv") #update@2025No14 in this script

NonFish_List <- read_csv("NonFishList.csv")

# A list of aquatic species for fisheries resource assessment
FRA200List <- read_csv("FRA200List.csv")
FRA200List <- FRA200List %>% filter(SingleMulti == "Single")
FRA200List$Category <- factor(FRA200List$Category, levels=c("Fish","NonFish"))

# Taxonomic rank in fish species
TaxonRank_info <- read_csv("TaxonRank_info.csv")

# loading mismatch name list
mismatch_name_info <- read_csv("mismatch_name_list.csv")
```

## Linking Japanese name and scientific name

``` r
n_fish <- nrow(FRA200List)

scientific_name_vec <- vector()
query_scientific_name_vec <- vector()
genus_name_vec <- vector()
family_name_vec <- vector()
order_name_vec <- vector()
class_name_vec <- vector()
phylum_name_vec <- vector()



for(i in 1:n_fish){

    target_fish <- FRA200List[i,1][[1]]
    Category <- as.vector(FRA200List$Category)[i]
    
    if(Category == "Fish"){
      target_fish_ID <- which(JAFList$和名==target_fish)
      scientific_name_check <- JAFList$学名[target_fish_ID]
      Family_unlist <-JAFList$Family[target_fish_ID] %>% strsplit("\n") %>% unlist 
      Family_name <- as.character(Family_unlist[1])

      target_Family_ID <- which(TaxonRank_info$Family==Family_name)
      
      Phylum_name <- TaxonRank_info$Phylum[target_Family_ID]
      if(identical(Phylum_name,character(0))){
        Phylum_name <-NA
      }
      
      Class_name <- TaxonRank_info$Class[target_Family_ID]
      if(identical(Class_name,character(0))){
        Class_name <-NA
      }
      
      Order_name <- TaxonRank_info$Order[target_Family_ID]
      if(identical(Order_name,character(0))){
        Order_name <-NA
      }
      
      #if(str_detect(Family_name, pattern="Epinephelidae")){
    #  Family_name <- "Serranidae"  
      #}
      
      if(!(identical(scientific_name_check,character(0)))){#if non-fish species
      scientific_unlist <- scientific_name_check %>% strsplit("\n") %>% unlist 
      scientific_name <- scientific_unlist[1]
  
      # Check mismatch scientific name
      if(scientific_name %in% mismatch_name_info$Taxonomical_correct_name){
        target_query_vec_id <- which(mismatch_name_info$Taxonomical_correct_name==scientific_name)
        query_scientific_name <- mismatch_name_info$NCBI_query_name[target_query_vec_id]
      }else{
         query_scientific_name <- scientific_name
      }
      
      phylum_name_vec[i] <- Phylum_name
      class_name_vec[i] <- Class_name
      order_name_vec[i] <- Order_name
      family_name_vec[i] <- Family_name
      scientific_name_vec[i] <- scientific_name
      query_scientific_name_vec[i] <- query_scientific_name

    }else{
      phylum_name_vec[i] <- NA
        class_name_vec[i] <- NA
        order_name_vec[i] <- NA
        family_name_vec[i] <- NA
        scientific_name_vec[i] <- NA
        query_scientific_name_vec[i] <- query_scientific_name
      }
      
    }else if(Category == "NonFish"){
      target_NonFish_ID <- which(NonFish_List$和名==target_fish)
      scientific_name_check <- NonFish_List$Scientific_name[target_NonFish_ID]
      Family_unlist <-NonFish_List$Family[target_NonFish_ID] %>% strsplit("\n") %>% unlist 
      Family_name <- Family_unlist[1]
      Order_name <- NonFish_List$Order[target_NonFish_ID]
      Class_name <- NonFish_List$Class[target_NonFish_ID]
      Phylum_name <- NonFish_List$Phylum[target_NonFish_ID]
      
      if(!(identical(scientific_name_check,character(0)))){#if non-fish species
        
        scientific_unlist <- scientific_name_check %>% strsplit("\n") %>% unlist 
      scientific_name <- scientific_unlist[1]
      phylum_name_vec[i] <- NonFish_List$Phylum[target_NonFish_ID]
      class_name_vec[i] <- NonFish_List$Class[target_NonFish_ID]
      order_name_vec[i] <- NonFish_List$Order[target_NonFish_ID]
        family_name_vec[i] <- Family_name
        scientific_name_vec[i] <- scientific_name
        
        # Check mismatch scientific name
      if(scientific_name %in% mismatch_name_info$Taxonomical_correct_name){
        target_query_vec_id <- which(mismatch_name_info$Taxonomical_correct_name==scientific_name)
        query_scientific_name <- mismatch_name_info$NCBI_query_name[target_query_vec_id]
      }else{
        query_scientific_name <- scientific_name
      }
        query_scientific_name_vec[i] <- query_scientific_name

    }else{
      phylum_name_vec[i] <- NA
      class_name_vec[i] <- NA
      order_name_vec[i] <- NA
        family_name_vec[i] <- NA
        scientific_name_vec[i] <- NA
        query_scientific_name_vec[i] <- NA
    }
      
    }else{
      phylum_name_vec[i] <- NA
    class_name_vec[i] <- NA
    order_name_vec[i] <- NA
    family_name_vec[i] <- NA
      scientific_name_vec[i] <- NA
      query_scientific_name_vec[i] <- NA
    }  
}

# Checking number of phylum in the dataset
n_phylum <- length(unique(phylum_name_vec))
Phylum_order <- c("Ochrophyta",
                  "Arthropoda",
                  "Mollusca",
                  "Echinodermata",
                  "Chordata")
if(!(n_phylum==length(Phylum_order))){
  warning("Number of phylum-levels does not match the dataset")
}

# Checking number of class in the dataset
n_class <- length(unique(class_name_vec))
Class_order <- c(
  "Phaeophyceae",
  "Malacostraca",
  "Bivalvia",
  "Gastropoda",
  "Cephalopoda",
  "Echinoidea",
  "Holothuroidea",
  "Chondrichthyes",
  "Actinopterygii",
  "Mammalia")
if(!(n_class=length(Class_order))){
  warning("Number of class-levels does not match the dataset")
}

# Checking number of order in the dataset
n_order <- length(unique(order_name_vec))
Order_order <- c(
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
if(!(n_order=length(Order_order))){
  warning("Number of order-levels does not match the dataset")
}




# Summarizing a taxonomy list
FRA200List_Latin <- FRA200List %>% 
  mutate(Phylum = factor(phylum_name_vec, levels = Phylum_order),
         Class = factor(class_name_vec,levels = Class_order),
         Order = order_name_vec, 
         Family = family_name_vec,
         Scientific_name = scientific_name_vec,
         NCBI_query_scientific_name = query_scientific_name_vec
         ) %>%
  select(-Category,-SingleMulti,-TAC,-Target) %>% 
  arrange(Phylum, Class, Order, Family, Scientific_name) %>%
  mutate(Taxonomy_id=NA)

# add taxonomy id
for(i in 1:nrow(FRA200List_Latin)){
  target_species_name <- FRA200List_Latin[i,]$NCBI_query_scientific_name
  scientific_name_split_vec <- unlist(strsplit(target_species_name, " "))
    genus_name <- scientific_name_split_vec[1]
    specific_name <- scientific_name_split_vec[2]
  query_species_name  <- paste0(genus_name," ",specific_name)
  target_taxonomy_id_info <- rentrez::entrez_search(db="taxonomy",term=query_species_name)
  target_taxonomy_id <- target_taxonomy_id_info$ids %>%
    as.integer()
  if(length(target_taxonomy_id)==1){
    FRA200List_Latin[i,]$Taxonomy_id <- target_taxonomy_id
  }
  Sys.sleep(3)
}

FRA200List_Kokushi_Latin_info <- read_csv("FRA200List_Kokushi_Latin.csv") %>%
  mutate(Taxonomy_id=as.integer(Taxonomy_id))

FRA200List_Latin_query <- bind_rows(FRA200List_Latin,
                              FRA200List_Kokushi_Latin_info) %>%
  mutate(Phylum=factor(Phylum, levels=Phylum_order),
         Class =factor(Class , levels=Class_order),
         Order = factor(Order , levels=Order_order)) %>%
  arrange(Phylum, Class, Order, Family, Scientific_name) %>%
  dplyr::distinct(和名,.keep_all = TRUE)

FRA200List_Latin <- FRA200List_Latin_query %>%
  dplyr::select(-c(NCBI_query_scientific_name))
```

``` r
# Loading packages
duplicated_taxonomy_id <- FRA200List_Latin %>%
  group_by(Taxonomy_id) %>%
  filter(n()>1) %>%
  arrange(Taxonomy_id)

if (nrow(duplicated_taxonomy_id) > 1){
  warning(paste("Some of NCBI taxonomy IDs are duplicated!"))
}
```

## Write a taxonomy list file

``` r
write_csv(FRA200List_Latin_query, "FRA200List_Latin_query.csv")
write_csv(FRA200List_Latin, "FRA200List_Latin.csv")
```
