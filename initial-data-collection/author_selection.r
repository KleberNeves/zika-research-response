# Loads required libraries and helper scripts

library(bibliometrix)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("helper.r")

# Loads Web of Science data
data_folder = "../data/"
specific_folder = "search-data"

M = get.biblio.data(specific_folder)
M = metaTagExtraction(M, Field = "AU_CO", sep = ";")


# Extracts the affiliation of each author as well as the order of authors in a structured way
MBR = M %>% filter(str_detect(AU_CO, "BRAZIL"))
AU_CO_LIST = extract.author.country.order(MBR)


# Identify the target authors: last authors with a Brazilian affiliation
TARGET_AU = unique(
  (AU_CO_LIST %>% 
     filter(NegPosition %in% c(-1)))$Author # Primeiros autores: Position %in% c(1)
)


# Computes the number of papers by each last author, to be able to apply a cutoff (2 papers)
AU_BR = AU_CO_LIST %>% filter(str_detect(Country, "BRAZIL") & Author %in% TARGET_AU) %>%
  group_by(Author) %>% summarise(Publications = n())

AU = AU_BR %>% filter(Publications >= 2) %>% arrange(-Publications)


# Diagnostic plot of the distribution of number of papers
ggplot(AU_BR, aes(x = Publications)) + geom_histogram(bins = max(AU_BR$Publications))


# Saves the data for later manual searches, adding an example paper for double checking
AU = merge(AU, AU_CO_LIST, by = "Author")
AU = AU[!duplicated(AU$Author),] %>% select(-Country, -Position, -NegPosition) %>% rename(`Example Paper` = Title)
write.table(AU, "Target Author List - Zika - Last Authors.csv", row.names = F)







