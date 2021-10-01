library(tidyverse)
library(bibliometrix)
library(lubridate)
library(readxl)

source("../initial-data-collection/helper.r")

extract_author_info = function (filename) {
  print(basename(filename))
  author_filename = basename(tools::file_path_sans_ext(filename))
  
  # Load bibliometric data
  quiet = function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  }
  
  print("Reading bibliometric records ...")
  BD = NULL
  tryCatch({
    BD = quiet(bibliometrix::convert2df(filename, dbsource = "isi", format = "plaintext"))
  }, error = function (e) {
    print(paste0("Error in bibliometrix::convert2df: ", e))
  })
  
  if (is.null(BD)) {
    return (tibble(Author = author_filename,
                   Class = "ERROR", Name = "Couldn't load in bibliometrix",
                   ShortName = NA, Value = NA))
  }
  
  # Get MeSH terms for all the papers
  print("Obtaining MeSH terms ...")
  
  pmids = BD$PM[!is.na(BD$PM)]
  if (length(pmids) > 0) {
    raw_mesh_terms = get_mesh_terms(pmids)
    PMD = organize_mesh_terms(raw_mesh_terms)
    BD = merge(BD, PMD, by.x = "PM", by.y = "pmid", all.x = T)
  } else {
    BD$MeshFullTerms = NA
    BD$MeshHeadings = NA
  }
  
  # Break dataset in the relevant parts (pre-outbreak, zika-related papers)
  BD_PRE = BD %>% filter(PY < 2016)
  BD_POST = BD %>% filter(PY >= 2016)
  BD_ZIKA = BD %>% select(-MeshFullTerms) %>%
    inner_join(ZIKA_PAPERS %>% select(TI, MeshFullTerms), by = "TI")
  
  if (nrow(BD_ZIKA) == 0 | nrow(BD_PRE) == 0) {
    msg = paste0("No records: ZIKA = ", nrow(BD_ZIKA),
                 ", PRE = ", nrow(BD_PRE),
                 ", POST = ", nrow(BD_POST))
    return (tibble(Author = author_filename,
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
      add_column(Class = "Zika", .before = 1),

    get_affiliation(BD, author_filename) %>%
      add_column(Class = "All", .before = 1),
    
    # Is there a break in the cognitive career network (side-effect: saves the plot)
    get_cognitive_career_pivot(BD) %>%
      add_column(Class = "All", .before = 1)
  )
  
  EXTRACTED_INFO = EXTRACTED_INFO %>%
    add_column(Author = author_filename,
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
    # Average number of authors per paper, separating by international affiliation
    get_author_number_intl(BD),
    # Percentage of papers with international collaborations
    get_intl_collabs(BD),
    # Academic age of the author – years since their first paper was published
    get_academic_age(BD),
    # Percentage of papers that are zika-related
    get_perc_zika(BD),
    # Percentage of papers that are virus-related
    get_perc_virus_papers(BD),
    # Percentage of papers that are in epidemiology
    get_perc_epidemio_papers(BD)
  )
}

get_max_from_df = function (x) {
  tibble(x) %>% count(x) %>% slice_max(order_by = n, n = 1) %>% pull(1)
}

get_top_field = function (BD) {
  print("Extracting: top field")
  x = unlist(str_split(BD$SC, "; "))
  tibble(
    Name = "Most Common Journal Area",
    ShortName = "TopField",
    Value = get_max_from_df(x)
  )
}

get_affiliation = function (BD, author_filename) {
  print("Extracting: affiliation")
  author_affiliations = tibble()
  tryCatch({
    author_affiliations = extract_author_affiliations(BD) %>%
      filter(str_detect(Country, "BRAZIL")) %>%
      select(Author, Institution)
  }, error = function (e) {
      print(paste0("Error in extract_author_affiliations: ", e))
  })
  
  if (nrow(author_affiliations) == 0) {
    return (tibble(
      Name = c("Author affiliations", "Authos afilliation states", "Author affiliation regions"),
      ShortName = c("Affiliation", "State", "Region"),
      Value = NA
    ))
  }
    
  author_matches = find_best_author_match(unique(author_affiliations$Author), author_filename) %>%
    filter(is_match) %>% pull(author_target)
  
  institutions = author_affiliations %>%
    filter(Author %in% author_matches) %>%
    group_by(Institution) %>%
    count() %>%
    slice_max(order_by = n, n = 1) %>%
    pull(Institution) %>%
    str_trim() %>%
    unique()
  
  if (length(institutions) == 0) {
    institutions = NA
    states = NA
    regions = NA
  } else {
    if (exists("AFFIL_DATA")) {
      institutions = HARM_DATA %>%
        filter(Name %in% institutions) %>% pull(Harmonized) %>% unique()
      states = AFFIL_DATA %>%
        filter(Harmonized %in% institutions) %>% pull(State) %>% unique()
      regions = REGION_DATA %>%
        filter(State %in% states) %>% pull(Region) %>% unique()
      
      institutions = paste(institutions, collapse = ";")
      states = paste(states, collapse = ";")
      regions = paste(regions, collapse = ";")
    }
  }
  
  tibble(
    Name = c("Author affiliations", "Authos afilliation states", "Author affiliation regions"),
    ShortName = c("Affiliation", "State", "Region"),
    Value = c(institutions, states, regions)
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
  print("Extracting: Zika categories")
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
  
  zika_cats = factor(unlist(str_split(BD$ZikaCats, ";")), levels = lvls)
  ZC = tibble(ZikaCats = factor(zika_cats, levels = lvls)) %>%
    filter(!is.na(ZikaCats)) %>%
    count(ZikaCats, .drop = F) %>%
    rename(Name = ZikaCats, Value = n) %>%
    mutate(Name = paste("Frequency of MeSH Category -", Name),
           ShortName = paste("MeSHCat -", Name)) #%>%
    # select(Name, ShortName, Value)
  
  CIT = map_dfr(levels(zika_cats), function (z) {
    citation_count = BD %>%
      filter(str_detect(ZikaCats, z)) %>% pull(TC) %>% sum(na.rm = T)
    
    tibble(Name = paste("Citations from papers in MeSH Category -", z),
           ShortName = paste("MeSHCatCites -", z),
           Value = citation_count)
  })
    
  rbind(ZC, CIT)
}

get_citations = function (BD) {
  print("Extracting: citations")
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
  print("Extracting: author number")
  # Makes use of the separation of authors by semicolons in the AU field
  tibble(
    Name = "Number of Authors per Paper",
    ShortName = "AvgAuthorNumber",
    Value = round(mean(str_count(BD$AU, ";") + 1, na.rm = T), 2)
  )
}

get_author_number_intl = function (BD) {
  print("Extracting: INTL author number")
  CO_LIST = extract_author_country_order(BD) %>%
    select(Title, Author, Country) %>%
    filter(!is.na(Country))
  
  INTL_AU = CO_LIST %>%
    group_by(Title, Author) %>%
    # If any affiliation is not BRAZIL, count as international author
    summarise(INTL = any(str_trim(Country, "both") != "BRAZIL", na.rm = T)) %>%
    ungroup() %>%
    count(Title, INTL) %>%
    pivot_wider(id_cols = Title,
                names_from = INTL, names_prefix = "INTL_",
                values_from = n, values_fill = 0)
  
  tibble(
    Name = c("Number of non-INTL Authors per Paper", "Number of INTL Authors per Paper"),
    ShortName = c("AvgAuthorNumberNonINTL", "AvgAuthorNumberINTL"),
    Value = c(mean(INTL_AU$INTL_FALSE, na.rm = T), mean(INTL_AU$INTL_TRUE, na.rm = T))
  )
}

get_intl_collabs = function (BD) {
  print("Extracting: % INTL collaboration")
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
  print("Extracting: academic age")
  tibble(
    Name = "Academic Age - Years Since the First Paper Record",
    ShortName = "Academic Age",
    Value = 2021 - min(BD$PY, na.rm = T)
  )
}

get_perc_zika = function (BD) {
  print("Extracting: % zika papers")
  n_matched_papers = BD %>%
    filter(TI %in% ZIKA_PAPERS$TI) %>% nrow()
  
  tibble(
    Name = "Percentage of Zika-related papers",
    ShortName = "PercZika",
    Value = round(n_matched_papers / nrow(BD), 2)
  )
}

get_perc_virus_papers = function (BD) {
  print("Extracting: % virus papers")
  if (!("MeshFullTerms" %in% colnames(BD))) {
    return (tibble(Name = character(0), ShortName = character(0), Value = character(0)))
  }
  
  n_matched_papers = BD %>%
    filter(str_detect(MeshFullTerms %>% str_to_lower(), "(viral|virus|infection)")) %>% nrow()
  
  tibble(
    Name = "Percentage of virus-related papers",
    ShortName = "PercVirus",
    Value = round(n_matched_papers / nrow(BD), 2)
  )
}

get_perc_epidemio_papers = function (BD) {
  print("Extracting: % epidemiological papers")
  if (!("MeshFullTerms" %in% colnames(BD))) {
    return (tibble(Name = character(0), ShortName = character(0), Value = character(0)))
  }
  
  n_matched_papers = BD %>%
    filter(
      str_detect(
        MeshFullTerms %>%
          str_to_lower(), "(outbreaks|epidemiolog|public health surveillance|reproduction number)")) %>%
    nrow()
  
  tibble(
    Name = "Percentage of epidemiology papers",
    ShortName = "PercEpidemio",
    Value = round(n_matched_papers / nrow(BD), 2)
  )
}

find_best_author_match = function (target, query) {
  queries = str_split(query, " ") %>% unlist()
  sizes = map_dbl(queries, str_length)
  queries = queries[sizes >= 3]
  if (max(sizes) < 3) queries = query %>% str_to_upper()  
  
  queries = queries %>% str_to_upper()
  target = target %>% str_to_upper()
  
  found = map_dfr(target, function (t) {
    map_dfr(queries, function (q) {
      tibble(author_target = t, author_query = q, is_match = str_detect(t, q))
    })
  })
  
  found
}

extract_author_affiliations = function (M) {
  author_country_data = plyr::ldply(1:nrow(M), function (i) {
    title = M[i, "TI"]
    affil = M[i, "C1"]
    author_field = M[i, "AU"]
    
    if (is.na(affil) | !stringr::str_detect(affil, "\\[")) {
      return (
        data.frame(Title = title,
                   Author = NA,
                   Position = NA,
                   Country = NA,
                   stringsAsFactors = F)
      )
    }
    
    # Extract author list and positions to be merged later
    author_list = unlist(stringr::str_split(author_field, ";"))
    author_list = data.frame(Author = author_list, Position = 1:length(author_list))
    author_list$NegPosition = author_list$Position - nrow(author_list) - 1
    
    # Extracts author list from affiliation text
    authors = unlist(stringr::str_extract_all(affil, "(\\[.+?\\])"))
    authors = stringr::str_replace_all(authors, "[.];", ".")
    nauthors_per_group = stringr::str_count(authors, ";") + 1
    authors = stringr::str_remove_all(authors, "\\[")
    authors = stringr::str_remove_all(authors, "\\]")
    authors = stringr::str_remove_all(authors, ",")
    authors = stringr::str_remove_all(authors, "[.]")
    authors = unlist(stringr::str_split(authors, "; "))
    
    # Extracts affiliations per group of authors
    affil = stringr::str_replace_all(affil, "; \\[", " [")
    affil = stringr::str_replace_all(affil, "(\\[.+?\\])", "]")
    affil = unlist(stringr::str_split(affil, "\\] "))
    affil = affil[2:length(affil)]
    affil = affil[affil != ""]
    
    affils = rep(affil, nauthors_per_group) %>%
      stringr::str_remove_all("(;$)|([.] $)")
    
    # Extract countries from affiliations
    countries = unlist(plyr::llply(affil, function (x) {
      x = unlist(stringr::str_split(x, "; "))
      x = stringr::str_trim(x)
      x = stringr::str_remove_all(x, "[.]")
      x = stringr::str_remove(x, ".+, ")
      x[stringr::str_which(x, " USA$")] = "USA"
      x = paste(x, collapse = "; ")
    }))
    countries = rep(countries, nauthors_per_group)
    
    # Building results data frame
    R = data.frame(Title = title,
                   Author = authors,
                   Affiliation = affils,
                   Country = countries,
                   stringsAsFactors = F)
    
    RR = merge(R, author_list, by = "Author")
    
    # If merge fails, see if author list is using initials and remerge
    if (nrow(RR) == 0) {
      author_names = unlist(lapply(authors, function (x) {
        x = unlist(stringr::str_split(x, " "))
        first_name = x[1]
        if (first_name %in% c("DE","DOS","DAS","DA")) {
          first_name = paste(x[1:2], collapse = " ")
          x = x[3:length(x)]
        } else {
          x = x[2:length(x)]
        }
        
        if (any(x %in% c("DE","DOS","DAS","DA"))) {
          dosdas = which(x %in% c("DE","DOS","DAS","DA")) + 1
          x = x[-dosdas]
        }
        x = stringr::str_extract(x, "[A-Z]")
        # } else { x = c() }
        fullname = paste(c(first_name, " ", x), collapse = "")
        fullname
      }))
      
      R = data.frame(Title = title,
                     Author = author_names,
                     Affiliation = affils,
                     Country = countries,
                     stringsAsFactors = F)
      RR = merge(R, author_list, by = "Author")
    }
    
    RR
  })
  
  author_country_data$Institution = stringr::str_extract(author_country_data$Affiliation, ".+?,") %>%
    stringr::str_remove(",")
  
  author_country_data
}

extract_author_country_order = function (M) {
  author_country_data = purrr::map_dfr(1:nrow(M), function (i) {
    title = M[i, "TI"]
    affil = M[i, "C1"]
    author_field = M[i, "AU"]
    
    if (is.na(affil) | !stringr::str_detect(affil, "\\[")) {
      return (
        data.frame(Title = title,
                   Author = NA,
                   Position = NA,
                   Country = NA,
                   stringsAsFactors = F)
      )
    }
    
    # Extract author list and positions to be merged later
    author_list = unlist(stringr::str_split(author_field, ";"))
    author_list = data.frame(Author = author_list, Position = 1:length(author_list))
    author_list$NegPosition = author_list$Position - nrow(author_list) - 1
    
    # Extracts author list from affiliation text
    authors = unlist(stringr::str_extract_all(affil, "(\\[.+?\\])"))
    authors = stringr::str_replace_all(authors, "[.];", ".")
    nauthors_per_group = stringr::str_count(authors, ";") + 1
    authors = stringr::str_remove_all(authors, "\\[")
    authors = stringr::str_remove_all(authors, "\\]")
    authors = stringr::str_remove_all(authors, ",")
    authors = stringr::str_remove_all(authors, "[.]")
    authors = unlist(stringr::str_split(authors, "; "))
    
    # Extracts affiliations per group of authors
    affil = stringr::str_replace_all(affil, "; \\[", " [")
    affil = stringr::str_replace_all(affil, "(\\[.+?\\])", "]")
    affil = unlist(stringr::str_split(affil, "\\] "))
    affil = affil[2:length(affil)]
    affil = affil[affil != ""]
    
    # Extract countries from affiliations
    countries = unlist(plyr::llply(affil, function (x) {
      x = unlist(stringr::str_split(x, "; "))
      x = stringr::str_trim(x)
      x = stringr::str_remove_all(x, "[.]")
      x = stringr::str_remove(x, ".+, ")
      x[stringr::str_which(x, " USA$")] = "USA"
      x = paste(x, collapse = "; ")
    }))
    countries = rep(countries, nauthors_per_group)
    
    # Building results data frame
    R = data.frame(Title = title,
                   Author = authors,
                   Country = countries,
                   stringsAsFactors = F)
    
    RR = merge(R, author_list, by = "Author")
    
    # If merge fails, see if author list is using initials and remerge
    if (nrow(RR) == 0) {
      author_names = unlist(lapply(authors, function (x) {
        x = unlist(stringr::str_split(x, " "))
        first_name = x[1]
        if (first_name %in% c("DE","DOS","DAS","DA")) {
          first_name = paste(x[1:2], collapse = " ")
          x = x[3:length(x)]
        } else {
          x = x[2:length(x)]
        }
        
        if (any(x %in% c("DE","DOS","DAS","DA"))) {
          dosdas = which(x %in% c("DE","DOS","DAS","DA")) + 1
          x = x[-dosdas]
        }
        x = stringr::str_extract(x, "[A-Z]")
        # } else { x = c() }
        fullname = paste(c(first_name, " ", x), collapse = "")
        fullname
      }))
      
      R = data.frame(Title = title,
                     Author = author_names,
                     Country = countries,
                     stringsAsFactors = F)
      RR = merge(R, author_list, by = "Author")
    }
    
    RR
  })
  
  author_country_data
}

get_cognitive_career_pivot = function (BD) {
  print("Extracting: cognitive career pivot")
  ZIKA_TI = BD %>% select(TI) %>%
    inner_join(ZIKA_PAPERS %>% select(TI), by = "TI") %>%
    pull(TI)
  
  BD = BD %>% mutate(
    COMPONENT = ifelse(
      PY >= 2016 | is.na(PY),
      ifelse(
        TI %in% ZIKA_TI,
        "ZIKA",
        "POST"
      ),
      "PRE"
    )
  )

  CM = BD %>%
    group_by(COMPONENT) %>%
    summarise(
      TI = COMPONENT[1],
      CR = paste(CR, collapse = ";")
    ) %>%
    as.data.frame()
  
  # If no pre- or zika papers are found, return NA
  if (sum(CM$COMPONENT == "ZIKA") == 0 | sum(CM$COMPONENT == "PRE") == 0) {
    return (tibble(
      Name = c("CognitiveCareerNetworkCoupling", "CognitiveCareerNetworkPivot"),
      ShortName = c("CareerNetCoupling", "CareerNetPivot"),
      Value = NA
    ))
  }
  
  # Makes the adjancency matrix and graph for the bibliographic coupling network
  got_error = F
  tryCatch({
    WCR = Matrix::t(cocMatrix(CM, Field = "CR", sep = ";"))
    ADJ = as.matrix(Matrix::crossprod(WCR, WCR))
    colnames(ADJ) = rownames(ADJ) = CM$COMPONENT
  }, error = function (e) {
    print(e)
    got_error <<- T
  })
  
  if (got_error) {
    return (tibble(
      Name = c("CognitiveCareerNetworkCoupling", "CognitiveCareerNetworkPivot"),
      ShortName = c("CareerNetCoupling", "CareerNetPivot"),
      Value = NA
    ))
  }
  
  # if there's non zika papers post-outbreak
  if (sum(CM$COMPONENT == "POST") == 0) {
    pre2zika = ADJ["PRE","ZIKA"] > 0
    if (pre2zika) {
      coupling_type = "Coupled to Zika"  
    } else {
      coupling_type = "Uncoupled to Zika" 
    }
  } else { # if all post-outbreak papers are zika papers
    pre2zika = ADJ["PRE","ZIKA"] > 0
    pre2post = ADJ["PRE","POST"] > 0
    if (pre2zika) {
      if (pre2post) {
        coupling_type = "Coupled to both"    
      } else {
        coupling_type = "Coupled to Zika only"    
      }
    } else {
      if (pre2post) {
        coupling_type = "Coupled to non-Zika only"    
      } else {
        coupling_type = "Uncoupled to either"    
      }
    }
  }
  
  if (coupling_type %in%
      c("Coupled to non-Zika only","Uncoupled to either","Uncoupled to Zika")) {
    pivot_type = "Hard pivot"
  } else {
    pivot_type = "Soft pivot"
  }
  
  return (tibble(
    Name = c("CognitiveCareerNetworkCoupling", "CognitiveCareerNetworkPivot"),
    ShortName = c("CareerNetCoupling", "CareerNetPivot"),
    Value = c(coupling_type, pivot_type)
  ))
}

# Same as the bibliometrix::isi2df function, except it doesn't mess with the C1 field
# Seemed to work before, I suppose a bibliometrix update broke it
my_isi2df = function (D) 
{
  D <- D[nchar(D) > 0]
  D <- D[!(substr(D, 1, 3) %in% c("FN ", "VR "))]
  for (i in 1:length(D)) {
    if (substr(D[i], 1, 3) == "   ") 
      substr(D[i], 1, 3) <- substr(D[i - 1], 1, 3)
  }
  Papers <- which(substr(D, 1, 3) == "PT ")
  nP = length(Papers)
  rowPapers <- diff(c(Papers, length(D) + 1))
  numPapers <- rep(1:nP, rowPapers)
  DATA <- data.frame(Tag = substr(D, 1, 3), content = substr(D, 
                                                             4, nchar(D)), Paper = numPapers, stringsAsFactors = FALSE)
  DATA$Tag <- gsub(" ", "", DATA$Tag)
  df <- DATA %>% group_by(.data$Paper, .data$Tag) %>% summarise(cont = paste(.data$content, 
                                                                             collapse = "---", sep = "")) %>% arrange(.data$Tag, 
                                                                                                                      .data$Paper) %>% pivot_wider(names_from = .data$Tag, 
                                                                                                                                                   values_from = .data$cont) %>% ungroup() %>% as.data.frame()
  df$PY <- as.numeric(df$PY)
  missingTags <- setdiff(c("AU", "DE", "C1", "RP", "CR", "PY", 
                           "SO", "TI", "TC"), names(df))
  if (length(missingTags) > 0) {
    cat("\nWarning:\nIn your file, some mandatory metadata are missing. Bibliometrix functions may not work properly!\n\nPlease, take a look at the vignettes:\n- 'Data Importing and Converting' (https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html)\n- 'A brief introduction to bibliometrix' (https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html)\n\n")
    cat("\nMissing fields: ", missingTags, "\n")
  }
  tagsComma <- c("AU", "AF", "CR")
  nolab <- setdiff(tagsComma, names(df))
  tagsComma <- tagsComma[(!(tagsComma %in% nolab))]
  df1 <- data.frame(lapply(df[tagsComma], function(x) {
    gsub("---", ";", x)
  }), stringsAsFactors = FALSE)
  otherTags <- setdiff(names(df), tagsComma)
  df2 <- data.frame(lapply(df[otherTags], function(x) {
    trimES(gsub("---", " ", x))
  }), stringsAsFactors = FALSE)
  df <- cbind(df1, df2)
  rm(df1, df2)
  df$DB <- "ISI"
  df$AU <- trimES(gsub(",", " ", df$AU))
  DI <- df$DI
  df <- data.frame(lapply(df, toupper), stringsAsFactors = FALSE)
  df$DI <- DI
  # df$C1 <- trim(gsub("\\[.*?\\]", "", df$C1))
  # df$C1 <- gsub("\\.", ".;", df$C1)
  df <- df[names(df) != "Paper"]
  return(df)
}

environment(my_isi2df) = asNamespace('bibliometrix')
assignInNamespace("isi2df", my_isi2df, ns = "bibliometrix")