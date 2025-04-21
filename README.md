## Genome size information and demographic histories of aquatic organisms for fisheries resource management in Japan


* [Genome size of comercially important fisheries species in Japan](aquatic_organism_genome_size.csv)
* [Number of fisheries fish species in Japan with genome assembly available in GenBank](https://github.com/akihirao/FishGenBase/blob/main/chronology/Num_sp_genome_deposited.png)
* [Generation time for fish species](species_generation_time_age.csv)


### Work flow for scaping genome size information

* Step 1：Link the standard Japanese name to the scientific name in each of species [[Rcode]](Link_Jap2Latin_Fish.md)
* Step 2：Scrape genome assembly infomation depsited on GenBank [[Rcode]](Scraping_FishGenome.md)


### Generation time of fish species
* The generation time for each fish species was inferred based on the equation proposed by Pacoureau et al (2021) [[R code]](fish_species_generation_time.md)


### Historical effective population size
* [Summary table](harmonic_mean_Ne_PSMC.csv)
* The history of effective population size (Ne) for each species was inferred using [PSMC](https://github.com/lh3/psmc) (Li & Durbin 2011). The historical Ne was represented as the harmonic mean across temporal estimates (see detailed in Wilder et al 2023). Parameters for time-interval pattern in PSMC analysis were followed by Li et al (2021).


### References
* [List of Japan's all fish species (JAF list: Motomura 2024)](https://www.museum.kagoshima-u.ac.jp/staff/motomura/jaf.html)：日本産全魚種の標準和名—学名の目録
* [Eschmeyer's Catalog of Fishes](https://www.calacademy.org): 魚類の科—目レベルの分類階級の参照先
* [Biological Information System for Marine Life; BISMaL](https://www.godac.jamstec.go.jp/bismal): 魚類以外の分類群の科—目レベルの分類階級の参照先
* [Animal Genome Size Database](http://www.genomesize.com/index.php): 動物6222種（脊椎動物2793種; 無脊椎動物2429種）を対象としたハプロイドDNA含量に関するデータベース

* Li & Durbin (2011) Inference of human population history from individual whole-genome sequences. Nature 475, 493-496. [link](https://www.nature.com/articles/nature10231) 
* Li et al. (2011) Temporal dynamics of teleost populations during the Pleistocene: a report from publicly available genome data. BMC genomics 22, 490. [link](https://doi.org/10.1186/s12864-021-07816-7) 
* Motomura, H. (2020) List of Japan’s all fish species. Current standard Japanese and scientific names of all fish species recorded from Japanese waters. https://www.museum.kagoshima-u.ac.jp/staff/motomura/jaf.html
* Pacoureau et al. (2021) Half a century of global decline in oceanic sharks and rays. Nature 589, 567–571. [link](https://www.nature.com/articles/s41586-020-03173-9)
* Wilder et al. (2023) The contribution of historical processes to contemporary extinction risk in placental mammals. Sicence 380 eabn5856. [link](https://www.science.org/doi/10.1126/science.abn5856)
