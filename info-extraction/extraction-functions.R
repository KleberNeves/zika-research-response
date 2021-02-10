extract_author_info = function (filename) {
  
  # Load bibliometric data
  BD = bibliometrix::convert2df(filenames, dbsource = "isi", format = "plaintext")
  
  # Break dataset in the relevant parts (pre-outbreak, zika-related papers)
  BD_PRE = BD %>% filter(PY < 2016)
  BD_POST = BD %>% filter(PY >= 2016)
  BD_ZIKA = BD %>% inner_join(ZIKA_PAPERS)
  
  # For each part of the dataset, extract/calculate all the info
  
  EXTRACTED_INFO = rbind(
    get_info_from_biblio_data(BD_PRE) %>%
      add_column(Class = "Pre-Outbreak", .before = 1),
    get_info_from_biblio_data(BD_POST) %>%
      add_column(Class = "Post-Outbreak", .before = 1),
    get_info_from_biblio_data(BD_ZIKA) %>%
      add_column(Class = "Zika", .before = 1)
  )
  
  EXTRACTED_INFO = EXTRACTED_INFO %>%
    add_column(Author = filename, .before = 1)
  
  EXTRACTED_INFO
}

get_info_from_biblio_data = function (BD) {
  rbind(
    # Identify the primary field of the author - most common journal area
    get_top_field(BD),
    # Calculate the percentage of papers in each MeSH category
    get_mesh_categories(BD),
    # Number and rate of citations
    get_citations(BD),
    # Average number of authors per paper
    get_author_number(BD),
    # Percentage of papers with international collaborations
    get_intl_collabs(BD),
    # Academic age of the author – years since their first paper was published
    get_academic_age(BD),
    # Percentage of papers that are zika-related
    get_perc_zika(BD)
  )
}

get_top_field = function (BD) {
  
}

get_mesh_categories = function (BD) {
  
}

get_citations = function (BD) {
  
}

get_author_number = function (BD) {
  
}

get_intl_collabs = function (BD) {
  
}

get_academic_age = function (BD) {
  
}

get_perc_zika = function (BD) {
  
}
