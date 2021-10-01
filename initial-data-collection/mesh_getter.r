# Loads required libraries and helper scripts
library(bibliometrix)
library(tidyverse)
library(iCiteR)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("helper.r")

# Loads Web of Science data
data_folder = "../../data/search-data/"

M = get.biblio.data(data_folder)
M = metaTagExtraction(M, Field = "AU_CO", sep = ";")
pmids = M$PM[!is.na(M$PM)]

# Queries iCite for ACH classification and translation potential
ICD = get_metrics(pmids)
ICD = ICD %>% select(pmid, animal, human, molecular_cellular, apt) %>%
  mutate(A = ifelse(animal > 0, "A", ""),
         C = ifelse(molecular_cellular > 0, "C", ""),
         H = ifelse(human > 0, "H", ""),
         ACH = paste0(A,C,H)) %>%
  select(pmid, ACH, apt) %>%
  rename(`TranslationPotential` = apt)

M = merge(M, ICD, by.x = "PM", by.y = "pmid", all.x = T)

# Queries PubMed for the MeSH terms
raw_mesh_terms = get_mesh_terms(M$PM)
PMD = organize_mesh_terms(raw_mesh_terms)
M = merge(M, PMD, by.x = "PM", by.y = "pmid", all.x = T)

# Saves the list of MeSH terms so that we can classify the most common ones
MESHLIST = data.frame(Term = unlist(str_split(M$MeshFullTerms,";"))) %>%
  group_by(Term) %>% summarise(N = n()) %>% arrange(-N)

MESHLIST$Cumulative = cumsum(MESHLIST$N) / sum(MESHLIST$N)

write.table(MESHLIST, "Full List of MeSH Terms.csv", sep = "\t", row.names = F)
write.table(MESHLIST %>% filter(N >= 10), "Selected List of MeSH Terms.csv", sep = "\t", row.names = F)

dataset_info = "Generated from mesh_getter.r in 01/10/2021 (dmy). Caching all the iCite and PubMed data so it's not necessary to query for it again later."

save(M, MESHLIST, dataset_info, file = "Combined WoS + iCite dataset.RData")
