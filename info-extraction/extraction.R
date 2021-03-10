##### Set parameters for the script #####

# Without extension, will save .RData and .csv
SAVE_FILENAME = "./extracted_author_data-general"

AUTHOR_DATA_PATH = "../../data/authors-general"

INITIAL_SEARCH_DATA = "../../data/Zika Papers - Combined WoS + iCite dataset.RData"

MESH_CLASSIFICATION = "../../data/Selected List of MeSH Terms - classification Kleber.xlsx"

AFFILIATION_CLASSIFICATION = "../../data/Institutions and States - manual curation.xlsx"

##### Runs the extraction #####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("extraction-functions.R")

# Load the data for all the Zika papers
load(INITIAL_SEARCH_DATA)
ZIKA_PAPERS = M
rm(M)
rm(MESHLIST)

# Load affiliation data
if (file.exists(AFFILIATION_CLASSIFICATION)) {
  AFFIL_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 1)
  GEO_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 2)
}

# Load mesh terms categorization
MESH_CATS = read_excel(MESH_CLASSIFICATION, na = "NA") %>%
  filter(!is.na(Category))
  
# Run the extraction function for each author
filenames = paste0(AUTHOR_DATA_PATH, "/", list.files(AUTHOR_DATA_PATH, "txt$"))
# i = which(filenames == paste0(AUTHOR_DATA_PATH, "/KO A.txt"))
# AUTHOR_DATA = map_dfr(filenames[i:length(filenames)], extract_author_info)
AUTHOR_DATA = map_dfr(filenames, extract_author_info)

# Save the whole dataset
date_stamp = strftime(today(), format = "%d-%m-%Y")
save(file = paste0(SAVE_FILENAME, " ", date_stamp, ".RData"), AUTHOR_DATA)
write.table(AUTHOR_DATA, file = paste0(SAVE_FILENAME, " ", date_stamp, ".csv"), sep = ";", dec = ",", row.names = F)
