##### Set parameters for the script #####

INITIAL_SEARCH_DATA = "../../data/Zika Papers - Combined WoS + iCite dataset 010921.RData"

MESH_CLASSIFICATION = "../../data/Selected List of MeSH Terms - manual classification.xlsx"

AFFILIATION_CLASSIFICATION = "../../data/institutions/Institutions and States - manual curation.xlsx"

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("helper.r")
source("extraction-functions.R")

# Load the data for all the Zika papers
load(INITIAL_SEARCH_DATA)
ZIKA_PAPERS = M
rm(M)
rm(MESHLIST)

FAPERJ_NETWORK_INFO = read_excel("../../data/institutions/FAPERJ_Networks.xlsx")

ZIKA_PAPERS = ZIKA_PAPERS |> mutate(HasSoftPivotAuthor = F, HasHardPivotAuthor = F)
TOP_ZIKA_PAPERS = ZIKA_PAPERS |> slice_max(order_by = TC, prop = 0.1)

# Load affiliation data
if (file.exists(AFFILIATION_CLASSIFICATION)) {
  HARM_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 1)
  AFFIL_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 2)
  REGION_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 3)
  HARM_DATA[is.na(HARM_DATA)] = "-"
  AFFIL_DATA[is.na(AFFIL_DATA)] = "-"
}

# Load mesh terms categorization
MESH_CATS = read_excel(MESH_CLASSIFICATION, na = "NA") %>%
  filter(!is.na(Category))

# Arguments passed without extension, will save .RData and .csv
run_data_extraction = function (SAVE_FILENAME, AUTHOR_DATA_PATH) {
  ZIKA_PAPERS <<- ZIKA_PAPERS |>
    mutate(
      HasSoftPivotAuthor = F, HasHardPivotAuthor = F,
      AuthorFields = "", AuthorFAPERJNetworks = ""
    )
  
  # Run the extraction function for each author
  filenames = list.files(AUTHOR_DATA_PATH, "txt$", full.names = T)
  # i = which(filenames == paste0(AUTHOR_DATA_PATH, "/GARCEZ PP.txt"))
  # AUTHOR_DATA = map_dfr(filenames[i:length(filenames)], extract_author_info)
  
  AUTHOR_DATA = map_dfr(sample(filenames, length(filenames), replace = F), extract_author_info)
# browser()
  ZIKA_PAPERS_PIVOTS <<- ZIKA_PAPERS |> mutate(
    PivotType = ifelse(HasSoftPivotAuthor == F & HasHardPivotAuthor == F, NA,
                       ifelse(HasSoftPivotAuthor == T & HasHardPivotAuthor == T, "Soft and Hard Pivots",
                              ifelse(HasSoftPivotAuthor, "Soft Pivot Only", "Hard Pivot Only")))
  )
  
  # Save the whole dataset
  date_stamp = strftime(today(), format = "%d-%m-%Y")
  save(file = paste0(SAVE_FILENAME, " ", date_stamp, ".RData"), AUTHOR_DATA, ZIKA_PAPERS_PIVOTS)
  write.table(AUTHOR_DATA, file = paste0(SAVE_FILENAME, " ", date_stamp, ".csv"), sep = ";", dec = ",", row.names = F)
}

# AUTHOR_DATA = run_data_extraction(SAVE_FILENAME = "./extracted_author_data-teste",
#                     AUTHOR_DATA_PATH = "../../data/authors-teste")

run_data_extraction(SAVE_FILENAME = "./extracted_author_data-faperj",
                    AUTHOR_DATA_PATH = "../../data/authors-faperj-call")

run_data_extraction(SAVE_FILENAME = "./extracted_author_data-general",
                    AUTHOR_DATA_PATH = "../../data/authors-general")

run_data_extraction(SAVE_FILENAME = "./extracted_author_data-cnpq",
                    AUTHOR_DATA_PATH = "../../data/authors-cnpq-call")

