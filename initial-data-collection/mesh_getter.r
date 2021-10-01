# Loads required libraries and helper scripts
library(bibliometrix)
library(tidyverse)
library(iCiteR)
library(RISmed)

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
PMD = get.full.mesh(pmids)
M = merge(M, PMD, by.x = "PM", by.y = "pmid", all.x = T)
M = M %>% select(-`.id`)

M$MeshFullTerms = as.character(M$MeshFullTerms)
M$MeshHeadings = as.character(M$MeshHeadings)

M$MeshFullTerms = str_replace_all(M$MeshFullTerms, "&amp;", "&")
M$MeshHeadings = str_replace_all(M$MeshHeadings, "&amp;", "&")

# Saves the list of MeSH terms so that we can classify the most common ones
MESHLIST = data.frame(Term = unlist(str_split(M$MeshFullTerms,";"))) %>%
  group_by(Term) %>% summarise(N = n()) %>% arrange(-N)

MESHLIST$Cumulative = cumsum(MESHLIST$N) / sum(MESHLIST$N)

write.table(MESHLIST, "Full List of MeSH Terms.csv", sep = "\t", row.names = F)
write.table(MESHLIST %>% filter(N >= 10), "Selected List of MeSH Terms.csv", sep = "\t", row.names = F)

dataset_info = "Generated from mesh_getter.r in 18/08/2020 (dmy). Caching all the iCite data so it's not necessary to query for it again later."

save(M, MESHLIST, dataset_info, file = "Combined WoS + iCite dataset.RData")
