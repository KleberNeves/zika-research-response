extract_author_info = function (filename) {
  print(basename(filename))
  # if (basename(filename) == "ALMEIDA FJ.txt") browser()
  # Load bibliometric data
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  }
  print("Reading bibliometric records ...")
  # browser()
  BD = NULL
  tryCatch({
    BD = quiet(bibliometrix::convert2df(filename, dbsource = "isi", format = "plaintext"))
  }, error = function (e) {
    print(e)
  })
  
  if (is.null(BD)) {
    return (tibble(Author = basename(tools::file_path_sans_ext(filename)),
                   Class = "ERROR", Name = "Couldn't load in bibliometrix",
                   ShortName = NA, Value = NA))
  }

  # Break dataset in the relevant parts (pre-outbreak, zika-related papers)
  BD_PRE = BD %>% filter(PY < 2016)
  BD_POST = BD %>% filter(PY >= 2016)
  BD_ZIKA = BD %>% inner_join(ZIKA_PAPERS %>% select(TI, MeshFullTerms), by = "TI")
  
  if (nrow(BD_ZIKA) == 0 | nrow(BD_PRE) == 0) {
    msg = paste0("No records: ZIKA = ", nrow(BD_ZIKA),
                 ", PRE = ", nrow(BD_PRE),
                 ", POST = ", nrow(BD_POST))
    return (tibble(Author = basename(tools::file_path_sans_ext(filename)),
                   Class = "ERROR", Name = msg, ShortName = NA, Value = NA))
  }
  
  # For each part of the dataset, extract/calculate all the info
  print("Extracting and calculating info ...")
  EXTRACTED_INFO = rbind(
    get_info_from_biblio_data(BD_PRE) %>%
      add_column(Class = "Pre-Outbreak", .before = 1),
    get_info_from_biblio_data(BD_POST) %>%
      add_column(Class = "Post-Outbreak", .before = 1),
    get_info_from_biblio_data(BD_ZIKA) %>%
      add_column(Class = "Zika", .before = 1)
  )
  
  EXTRACTED_INFO = EXTRACTED_INFO %>%
    add_column(Author = basename(tools::file_path_sans_ext(filename)),
               .before = 1)
  
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

get_zika_cat = function (mesh_term_list) {
  # This function assumes that there is a MESH_CATS data frame
  map_chr(mesh_term_list, function (terms) {
    terms = unlist(str_split(terms, ";"))
    cats = MESH_CATS %>% filter(Term %in% terms) %>% pull(Category) %>% unique() %>% paste0(collapse = ";")
  })
}

get_mesh_categories = function (BD) {
  if (!("MeshFullTerms" %in% colnames(BD))) {
    return (tibble(Name = character(0), ShortName = character(0), Value = character(0)))
  }
  
  BD = BD %>%
    mutate(ZikaCats = get_zika_cat(MeshFullTerms))
  
  if (nrow(BD) == 0) {
    return (tibble(Name = character(0), ShortName = character(0), Value = character(0)))
  }
  
  lvls = unique(MESH_CATS$Category)
  lvls = lvls[!(lvls %in% c("Type of Study", "")) & !is.na(lvls)]
  
  zika_cats = unlist(str_split(BD$ZikaCats, ";"))
  BD = tibble(ZikaCats = factor(zika_cats, levels = lvls)) %>%
    filter(!is.na(ZikaCats)) %>%
    count(ZikaCats, .drop = F) %>%
    rename(Name = ZikaCats, Value = n) %>%
    mutate(ShortName = paste("MeSHCat -", Name),
           Name = paste("Frequency of MeSH Category -", Name))
    
  BD
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
  # Makes use of the separation of authors by semicolons in the AU field
  tibble(
    Name = "Number of Authors per Paper",
    ShortName = "AvgAuthorNumber",
    Value = round(mean(str_count(BD$AU, ";") + 1, na.rm = T), 2)
  )
}

get_intl_collabs = function (BD) {
  CO_LIST = extract_author_country_order(BD) %>%
    select(Title, Country) %>%
    filter(!is.na(Country))
  n_total = length(unique(CO_LIST$Title))
  
  # Remove occurrences of Brazil - whatever papers remain have INTL affiliations
  CO_LIST = CO_LIST %>% filter(!str_detect(Country, "BRAZIL"))
  n_intl = length(unique(CO_LIST$Title))
  
  tibble(
    Name = c("Total Papers", "Papers with International Affiliations", "Percentage of Papers with International Affiliations"),
    ShortName = c("Papers", "PapersINTL", "PercPapersINTL"),
    Value = c(n_total, n_intl, round(n_intl / n_total, 2))
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
