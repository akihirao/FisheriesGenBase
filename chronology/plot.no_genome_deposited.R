#plot.no_genome_deposited.R

library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggsci)


rm(list=ls(all=TRUE)) # initialize

csv_file_names <- list.files("./",pattern ="\\.csv$")
num_list <- length(csv_file_names)

csv_list <- list()
date_vec <- list()

for(i in 1:num_list){
  file_name_split <- str_split(csv_file_names[i], pattern = "_") %>%
    unlist()
  year_info <- file_name_split[5]
  month_info <- str_sub(file_name_split[6], start=1,end=2)
  #browser()
  day_info <-  str_sub(file_name_split[6], start=3,end=4)
  date_vec[[i]] <- as.Date(paste0(year_info,"-",month_info, "-", day_info))
  csv_list[[i]] <- read_csv(csv_file_names[i])
}

last_date <- do.call(c, date_vec[num_list])


date_series <- c(as.Date("2010-03-31"),as.Date("2020-04-21"),do.call(c, date_vec))

#Number of species/genus with public genome assembly 
num_sp_genome_series <- c()
num_sp_genus_genome_series <- c()
for(i in 1:num_list){
  target_csv <- csv_list[[i]] %>%
    filter(Phylum=="Chordata") %>%
    filter(Class %in% c("Chondrichthyes","Actinopterygii"))
  num_sp_genome_series[i] <- sum(!is.na(target_csv$Genome_size_of_the_species_Mbp)) 
  num_sp_genus_genome_series[i] <- sum(!is.na(target_csv$Average_genome_size_of_the_genus_Mbp)) 
}

# データベース作成前の情報を付与
#No_sp_genome_20100331 <- 1
#No_sp_genome_20200421 <- 18
num_sp_genome_series <- c(1,18,num_sp_genome_series)

#No_sp_genus_20100331 <- 3
#No_sp_genus_20200421 <- 43
num_sp_genus_genome_series <- c(3,43,num_sp_genus_genome_series)




genome_chronology <- tibble(date=date_series,
                            Species=num_sp_genome_series,
                            Genus=num_sp_genus_genome_series
                            )

                            
Taxonomic_class_lab <- c("Species","Genus")

genome_chronology <- genome_chronology %>% 
  tidyr::gather(Levels, Value, -date) %>% 
  mutate(Class = factor(Levels, levels=Taxonomic_class_lab))

current_day <-  Sys.Date()
title_lab <- paste0("Number of taxa with genome sequence deposited in GenBank: last update@",last_date)
plot_sp_level <- ggplot(data = genome_chronology, 
                        aes(x=date, y = Value, color=Class, group=Class)) + 
  geom_point(size=4.5) + 
  geom_line(linetype = "dashed", linewidth = 1) +
  labs(x = "Date", y="Number of taxa", title=title_lab)+
  theme(legend.text = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

png("Num_sp_genome_deposited.png", width = 600, height = 400)
plot_sp_level
dev.off()


# No. species/genus with genome assembly
num_target_species <- length(csv_list[[num_list]]$Scientific_name)
num_species_assembly <- sum(csv_list[[num_list]]$Genome_size_of_the_species_Mbp > 0,na.rm=TRUE)
num_genus_assembly <- sum(csv_list[[num_list]]$No_species_with_assembly_within_the_genus > 0)
cat("Number of species with genome assembly")
print(num_species_assembly)
cat("Percentage of number of species with genome assembly")
print(num_species_assembly/num_target_species)

cat("Number of genus with genome assembly")
print(num_genus_assembly)
cat("Percentage of number of genus with genome assembly")
print(num_genus_assembly/num_target_species)


last_list_with_assembly_fish <- csv_list[[num_list]] %>% 
  filter(Genome_size_of_the_species_Mbp > 0) %>%
  filter(Phylum=="Chordata") %>%
  filter(Class %in% c("Chondrichthyes","Actinopterygii"))
  
gg_assembly_len_dist <- ggplot(last_list_with_assembly_fish,
                               aes(x= Genome_size_of_the_species_Mbp)) +
  geom_histogram() +
  labs(x = "Genome size (Mbp)", y = "Count") +
  theme(text = element_text(size = 24))


png("genome_size_dist.png", width = 600, height = 400)
plot(gg_assembly_len_dist)
dev.off()


#genome_status = unlist(tapply(csv_list[[num_list]],csv_list[[num_list]]$Representative_assembly_status,count))

genome_status = unlist(tapply(last_list_with_assembly_fish,last_list_with_assembly_fish$Representative_assembly_status,count))

genome_status_tidy = tibble(Assembly_level= names(genome_status), count=genome_status) %>%
  mutate(neg_count = -count)
genome_status_tidy$Assembly_level = factor(genome_status_tidy$Assembly_level, 
                                           levels=c("Complete Genome",
                                                    "Chromosome",
                                                    "Scaffold",
                                                    "Contig"))

genome_status_pi_plot = ggplot(genome_status_tidy, 
                               aes(x = "", y = neg_count, fill = Assembly_level)) + 
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") + 
  theme_void() +
  theme(legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  guides(fill= guide_legend(reverse=FALSE)) +
  labs(fill="Assembly level") + 
  scale_fill_nejm()

plot(genome_status_pi_plot)
ggsave("genome_status_pi_plot.png",
        height=5, width=5)

