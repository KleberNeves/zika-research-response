extract_author_info = function (filename) {

  # Load bibliometric data
  BD = bibliometrix::convert2df(filename, dbsource = "isi", format = "plaintext")
  
  # Break dataset in the relevant parts (pre-outbreak, zika-related papers)
  BD_PRE = BD %>% filter(PY < 2016)
  BD_POST = BD %>% filter(PY >= 2016)
  BD_ZIKA = BD %>% filter(TI %in% ZIKA_PAPERS$TI)
  
  if (nrow(BD_ZIKA) == 0) { return (tibble()) }
  
  # For each part of the dataset, extract/calculate all the info
  
  EXTRACTED_INFO = rbind(
    get_info_from_biblio_data(BD_PRE) %>%
      add_column(Class = "Pre-Outbreak", .before = 1),
    get_info_from_biblio_data(BD_POST) %>%
      add_column(Class = "Post-Outbreak", .before = 1),
    get_info_from_biblio_data(BD_ZIKA) %>%
      add_column(Class = "Zika", .before = 1)
  )
  
  browser()
  
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

get_max_from_df = function (x) {
  tibble(x) %>% count(x) %>% slice_max(order_by = n, n = 1) %>% pull(1)
}

get_top_field = function (BD) {
  x = unlist(str_split(BD$SC, "; "))
  tibble(
    Name = "Most Common Journal Area",
    ShortName = "TopField",
    Value = get_max_from_df(x)
  )
}

get_mesh_categories = function (BD) {
  tibble(
    Name = c("MeSH Category Frequency - ",
             "MeSH Category Frequency - ",
             "MeSH Category Frequency - ",
             "MeSH Category Frequency - ",
             "MeSH Category Frequency - "),
    
    ShortName = c("MeSHCat",
                  "MeSHCat",
                  "MeSHCat",
                  "MeSHCat",
                  "MeSHCat"),
    
    Value = c(0,0,0,0,0)
  )
}

get_citations = function (BD) {
  tibble(
    Name = c("Total Citations", "Citations per Paper"),
    ShortName = c("Citations", "CitationRate"),
    Value = c(
      round(sum(as.numeric(BD$Z9), na.rm = T), 2),
      round(mean(as.numeric(BD$Z9), na.rm = T), 2)
    )
  )
}

get_author_number = function (BD) {
  tibble(
    Name = "Number of Authors per Paper",
    ShortName = "AvgAuthorNumber",
    Value = 0
  )
}

get_intl_collabs = function (BD) {
  tibble(
    Name = c("Total Papers", "Papers with International Affiliations", "Percentage of Papers with International Affiliations"),
    ShortName = c("Papers", "PapersINTL", "PercPapersINTL"),
    Value = c(0,0,0)
  )
}

get_academic_age = function (BD) {
  tibble(
    Name = "Academic Age - Years Since the First Paper Record",
    ShortName = "Academic Age",
    Value = 2021 - min(BD$PY, na.rm = T)
  )
}

get_perc_zika = function (BD) {
  zika_number = BD %>% filter(TI %in% ZIKA_PAPERS$TI) %>% nrow()
  tibble(
    Name = "Percentage of Zika-related papers",
    ShortName = "PercZika",
    Value = round(zika_number / nrow(BD), 2)
  )
}
