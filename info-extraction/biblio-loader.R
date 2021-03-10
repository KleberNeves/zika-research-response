##### Set parameters for the script #####

# Without extension, will save .RData and .csv
SAVE_FILENAME = "./biblio_data-general"

AUTHOR_EXTRACTED_DATA = "../../data/extracted-author-data/extracted_author_data-general 10-03-2021.RData"

AUTHOR_EXTRACTED_DATA_PATH = "../../data/authors-general"

##### Runs the extraction #####

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("biblio-functions.R")

load(AUTHOR_EXTRACTED_DATA)

filenames = paste0(AUTHOR_DATA_PATH, "/", list.files(AUTHOR_DATA_PATH, "txt$"))
# i = which(filenames == paste0(AUTHOR_DATA_PATH, "/KO A.txt"))
# AUTHOR_DATA = map_dfr(filenames[i:length(filenames)], extract_author_info)
BIBLIO_DATA = map_dfr(filenames, load_biblio_author)


