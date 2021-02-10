

##### Set parameters for the script #####

# Without extension, will save .RData and .csv
SAVE_FILENAME = "./extracted_author_data-general"

AUTHOR_DATA_PATH = "../data/authors-general"

INITIAL_SEARCH_DATA = "../data/Zika Papers - Combined WoS + iCite dataset.RData"

##### Runs the extraction #####

library(tidyverse)
library(bibliometrix)
library(lubridate)
library(metricshelpr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("extraction-functions.R")

# Load the data for all the Zika papers.
load(INITIAL_SEARCH_DATA)
ZIKA_PAPERS = M
rm(M)
rm(MESHLIST)

# Run the extraction function for each author.
filenames = list.files(AUTHOR_DATA_PATH, "txt$")
AUTHOR_DATA = map(filenames, extract_author_info)

# Save the whole dataset.
date_stamp = strftime(today(), format = "%d-%m-%Y")
save(file = paste0(SAVE_FILENAME, data_stamp, ".RData"), AUTHOR_DATA)
write.table(AUTHOR_DATA, file = paste0(SAVE_FILENAME, ".RData"), sep = ";", dec = ",")
