setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

AUTHOR_DATA_FILENAMES = c(
  "../../data/extracted-author-data/extracted_author_data-general 10-03-2021.RData",
  "../../data/extracted-author-data/extracted_author_data-cnpq 10-03-2021.RData"
)

AFFILIATION_CLASSIFICATION = "../../data/Institutions and States - manual curation.xlsx"

##### Preparing institutions for name harmonization #####

# Load affiliation data
if (file.exists(AFFILIATION_CLASSIFICATION)) {
  HARMONIZED_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 1)
  AFFIL_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 2)
  REGION_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 3)
}

AUTHOR_DATA = map_dfr(AUTHOR_DATA_FILENAMES, function (fn) {
  load(fn)
  AUTHOR_DATA
})

institutions_to_harmonize = AUTHOR_DATA %>%
  filter(ShortName == "Affiliation", !is.na(Value)) %>%
  select(Value) %>%
  count(Value) %>%
  arrange(Value) %>%
  rename(Name = Value) %>%
  mutate(`Harmonized` = "") %>%
  select(Name, Harmonized, n)

# Remove the ones that are already present in the classification
already_done = HARMONIZED_DATA %>% filter(Harmonized != "") %>% pull(Name)
institutions_to_harmonize = institutions_to_harmonize %>% filter(!(Name %in% already_done))

# Save the remaining ones
write_excel_csv2(institutions_to_harmonize, "ToBeHarmonized.csv")


##### Preparing unique institutions for classification into states #####

if (file.exists(AFFILIATION_CLASSIFICATION)) {
  HARMONIZED_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 1)
  AFFIL_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 2)
  REGION_DATA = read_excel(AFFILIATION_CLASSIFICATION, sheet = 3)
}

HARMONIZED_DATA$Harmonized = ifelse(
  is.na(HARMONIZED_DATA$Harmonized),
  HARMONIZED_DATA$Name, HARMONIZED_DATA$Harmonized
)

unique_institutions = HARMONIZED_DATA %>%
  count(Harmonized) %>%
  mutate(State = "") %>%
  select(Harmonized, State, n)

write_excel_csv2(unique_institutions, "ToAddStates.csv")
