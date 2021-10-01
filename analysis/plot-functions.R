plot_cite_rate_by_mesh_cat = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(
      ShortName %>% str_detect("Frequency of MeSH") |
        ShortName %>% str_detect("Cites"), Class == "Zika"
    ) %>%
    select(-Class) %>%
    mutate(
      Value = as.numeric(Value),
      Measure = ShortName %>%
        str_replace("MeSHCat - Frequency of MeSH Category", "Pubs") %>%
        str_replace("MeSHCatCites", "Cite") %>%
        str_sub(1,4),
      Category = ShortName %>%
        str_remove("MeSHCat - Frequency of MeSH Category - ") %>%
        str_remove("MeSHCatCites - ")
    ) %>%
    select(Author, Category, Measure, Value) %>%
    pivot_wider(id_cols = c(Author, Category), names_from = Measure, values_from = Value) %>%
    mutate(CitationRate = Cite / Pubs) %>%
    select(Author, Category, CitationRate)
  
  DF$Category = DF$Category %>% recode_mesh_cats()
  
  plot_mesh_cite_rate = function (DF, title = "") {
    DF = DF %>% group_by(Category) %>% summarise(Value = mean(CitationRate, na.rm = T))
    ggplot(DF) +
      aes(x = Category, y = Value, fill = Category) +
      geom_col() +
      labs(x = "", y = "Average Citations per Paper", title = title) +
      scale_y_continuous(breaks = pretty_breaks(n = 5), expand = c(0,0)) +
      coord_flip() +
      theme(axis.text.y = element_text(size = 10),
            legend.position = "none")
  }
  
  p = plot_mesh_cite_rate(DF, title = "All Fields")
  p
}

plot_pubs_by_mesh_cat_by_field = function(AUTHOR_DATA, fields_to_plot = NULL, min_authors_to_plot = 10) {
  AUTHORS_BY_FIELD = AUTHOR_DATA %>%
    filter(ShortName == "TopField" & Class == "Pre-Outbreak") %>%
    mutate(Field = str_to_title(Value)) %>%
    select(Author, Field)
  
  DF = AUTHOR_DATA %>% filter(ShortName %>% str_detect("Frequency of MeSH"), Class == "Zika") %>%
    mutate(Value = as.numeric(Value),
           ShortName = ShortName %>% str_remove("MeSHCat - Frequency of MeSH Category - ")) %>%
    left_join(AUTHORS_BY_FIELD, by = "Author")
  
  if (is.null(fields_to_plot)) {
    top_fields = AUTHORS_BY_FIELD %>%
      count(Field) %>% filter(n >= min_authors_to_plot) %>% pull(Field) %>% unique()
  } else {
    top_fields = str_to_title(fields_to_plot)
  }
  
  map(top_fields, function (selected_field) {
    d = DF %>% filter(Field == selected_field)
    plot_pubs_by_mesh_cat(d, title = recode_areas(selected_field), preprocessed = T)
  })
}

plot_pubs_by_mesh_cat_pubset = function(PAPER_SET, title) {
  DF = PAPER_SET %>%
    filter(!is.na(ZikaCats) & ZikaCats != "") %>%
    pull(ZikaCats) %>%
    str_split(";") %>%
    unlist() %>%
    recode_mesh_cats() %>%
    as_tibble() %>%
    count(value) %>%
    rename(ShortName = value, Value = n) %>%
    filter(ShortName != "Type of Study")
  
  plot_pubs_by_mesh_cat(DF, title = title, preprocessed = T)
}

recode_mesh_cats = function (meshes) {
  dplyr::recode(meshes,
    `Microcephaly, Fetal infections &\nPregnancy Complications` = "Microcephaly",
    `Microcephaly, Fetal infections & Pregnancy Complications` = "Microcephaly",
    `Vector & Transmission` = "Vector",
    `Epidemiology, Outbreaks &\nSurveillance` = "Epidemiology",
    `Epidemiology, Outbreaks & Surveillance` = "Epidemiology",
    `Pathogenesis & Virus` = "Virus",
    `Related Diseases` = "Related Diseases",
    `Treatments, Vaccines &\nDiagnostic Tests` = "Treatments",
    `Treatments, Vaccines & Diagnostic Tests` = "Treatments",
    `Clinical manifestations` = "Clinical"
  )
}

plot_pubs_by_mesh_cat = function(AUTHOR_DATA, title = "", preprocessed = F) {
  if (!preprocessed) {
    DF = AUTHOR_DATA %>% filter(ShortName %>% str_detect("Frequency of MeSH"), Class == "Zika") %>%
      mutate(Value = as.numeric(Value),
             ShortName = ShortName %>% str_remove("MeSHCat - Frequency of MeSH Category - "))
  } else {
    DF = AUTHOR_DATA
  }
  
  DF$ShortName = DF$ShortName %>% recode_mesh_cats()

  DF = DF %>% group_by(ShortName) %>% summarise(Value = sum(Value, na.rm = T))
  p = ggplot(DF) +
    aes(x = ShortName, y = Value, fill = ShortName) +
    geom_col() +
    labs(x = "", y = "# of Publications", title = title) +
    scale_y_continuous(breaks = pretty_breaks(n = 5), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 10),
          legend.position = "none")
  
  p
}

plot_cites_by_mesh_cat = function(AUTHOR_DATA, title = "", preprocessed = F) {
  if (!preprocessed) {
    DF = AUTHOR_DATA %>% filter(ShortName %>% str_detect("Cites"), Class == "Zika") %>%
      mutate(Value = as.numeric(Value),
             ShortName = ShortName %>% str_remove("MeSHCatCites - "))
  } else {
    DF = AUTHOR_DATA
  }
  
  DF$ShortName = DF$ShortName %>% recode_mesh_cats()
  
  DF = DF %>% group_by(ShortName) %>% summarise(Value = sum(Value, na.rm = T))
  p = ggplot(DF) +
    aes(x = ShortName, y = Value, fill = ShortName) +
    geom_col() +
    labs(x = "", y = "# of Citations", title = title) +
    scale_y_continuous(breaks = pretty_breaks(n = 5), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 10),
          legend.position = "none")
  
  p
}

plot_perc_zika_papers = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "PercZika", Class == "Post-Outbreak") %>%
    mutate(Value = 100 * as.numeric(Value))
  
  bin_number = 50
  
  p = ggplot(DF) +
    aes(x = Value) +
    geom_histogram(bins = bin_number) +
    geom_vline(xintercept = mean(DF$Value, na.rm = T), linetype = "dashed", color = "black") +
    labs(x = "Percentage of Zika-related\npapers, post-outbreak", y = "Frequency") +
    scale_x_continuous(breaks = pretty_breaks(n = 5),
                       expand = c(0,0), limits = c(0,100)) +
    scale_y_continuous(expand = c(0,0))
  
  p
}

plot_perc_virus_papers = function(AUTHOR_DATA, period) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "PercVirus", Class == period) %>%
    mutate(Value = 100 * as.numeric(Value))
  
  bin_number = 50
  
  p = ggplot(DF) +
    aes(x = Value) +
    geom_histogram(bins = bin_number) +
    geom_vline(xintercept = mean(DF$Value, na.rm = T), linetype = "dashed", color = "black") +
    labs(x = "Percentage of virus- or infection-related\npapers, pre-outbreak", y = "Frequency") +
    scale_x_continuous(breaks = pretty_breaks(n = 5),
                       expand = c(0,0), limits = c(0,100)) +
    scale_y_continuous(expand = c(0,0))
  
  p
}

plot_scatter_virus_zika_papers = function(AUTHOR_DATA) {
  # browser()
  DF = rbind(
    AUTHOR_DATA %>%
      filter(ShortName == "PercZika", Class == "Post-Outbreak") %>%
      mutate(Value = 100 * as.numeric(Value)),
    AUTHOR_DATA %>%
      filter(ShortName == "PercVirus", Class == "Pre-Outbreak") %>%
      mutate(Value = 100 * as.numeric(Value))
  ) %>% select(Author, ShortName, Value) %>%
    pivot_wider(id_cols = Author, names_from = ShortName, values_from = Value)
  
  corr = cor(DF$PercVirus, DF$PercZika, method = "spearman")
 
  p = ggplot(DF) +
    aes(x = PercVirus, y = PercZika) +
    geom_point(size = 2, alpha = 0.7, color = "black") +
    geom_smooth(method = "lm", se = F) +
    labs(x = "Percentage of virus- or infection-related\npapers, pre-outbreak",
         y = "Percentage of Zika-related\npapers, post-outbreak") +
    scale_x_continuous(breaks = pretty_breaks(n = 5)) +
    scale_y_continuous(breaks = pretty_breaks(n = 5))
  
  list(p = p, corr = corr)
}

plot_compare_intl_papers = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "PercPapersINTL", Class %in% c("Pre-Outbreak", "Zika")) %>%
    mutate(Value = as.numeric(Value))
  
  p1 = ggplot(DF) +
    aes(x = Class, y = Value, group = Author) +
    geom_point(size = 3, alpha = 0.5) +
    geom_line(alpha = 0.5) +
    labs(x = "Period", y = "Percentage of\nInternational Collaborations") +
    scale_y_continuous(breaks = pretty_breaks(n = 5))
  
  p2 = ggplot(DF) +
    aes(x = Class, y = Value) +
    geom_violin() +
    stat_summary(fun = median, fun.min = median, fun.max = median, geom = "errorbar", width = 0.25, size = 0.5, alpha = 0.5, linetype = "dashed") +
    labs(x = "Period", y = "Percentage of\nInternational Collaborations") +
    scale_y_continuous(breaks = pretty_breaks(n = 5))
  
  list(p1, p2)
}

plot_compare_authors_per_paper = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "AvgAuthorNumber", Class %in% c("Pre-Outbreak", "Zika")) %>%
    mutate(Value = as.numeric(Value))
  
  p1 = ggplot(DF) +
    aes(x = Class, y = Value, group = Author) +
    geom_point(size = 3, alpha = 0.5) +
    geom_line(alpha = 0.5) +
    labs(x = "Period", y = "Average # of\nAuthors per Paper") +
    scale_y_continuous(breaks = pretty_breaks(n = 5))
  
  p2 = ggplot(DF) +
    aes(x = Class, y = Value) +
    geom_violin() +
    stat_summary(fun = median, fun.min = median, fun.max = median, geom = "errorbar", width = 0.25, size = 0.5, alpha = 0.5, linetype = "dashed") +
    labs(x = "Period", y = "Average # of\nAuthors per Paper") +
    scale_y_continuous(breaks = pretty_breaks(n = 5))
  
  list(p1, p2)
}

plot_compare_citations_per_paper = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "CitationRate", Class %in% c("Pre-Outbreak", "Zika")) %>%
    mutate(Value = as.numeric(Value))
  
  p1 = ggplot(DF) +
    aes(x = Class, y = Value, group = Author) +
    geom_point(size = 3, alpha = 0.5) +
    geom_line(alpha = 0.5) +
    labs(x = "Period", y = "Average # of\nCitations per Paper") +
    scale_y_continuous(breaks = pretty_breaks(n = 6))
  
  p2 = ggplot(DF) +
    aes(x = Class, y = Value) +
    geom_violin() +
    stat_summary(fun = median, fun.min = median, fun.max = median, geom = "errorbar", width = 0.25, size = 0.5, alpha = 0.5, linetype = "dashed") +
    labs(x = "Period", y = "Average # of\nCitations per Paper") +
    scale_y_continuous(breaks = pretty_breaks(n = 6))
  
  list(p1,p2)
}

plot_total_citations = function (AUTHOR_DATA, period = "Pre-Outbreak", logscale = T) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "Citations", Class == "Pre-Outbreak") %>%
    mutate(Value = as.numeric(Value))
  
  if (logscale) {
    p = ggplot(DF) +
      aes(x = Value) +
      geom_histogram(bins = 50) +
      labs(x = "Total Citations \nBefore the Outbreak", y = "Frequency") +
      scale_y_continuous(breaks = c(10, 50, 200, 1000, 10000), trans = "log10", expand = c(0,0))
  } else {
    p = ggplot(DF) +
      aes(x = Value) +
      geom_histogram(bins = 50) +
      labs(x = "Total Citations \nBefore the Outbreak", y = "Frequency") +
      scale_y_continuous(breaks = pretty_breaks(n = 6), expand = c(0,0))
  }
  
  p
}


plot_academic_age_hist = function(AUTHOR_DATA, period) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "Academic Age", Class == period) %>%
    mutate(Value = 2021 - as.numeric(Value))
  
  p = ggplot(DF) +
    aes(x = Value) +
    geom_histogram(bins = 50) +
    geom_vline(xintercept = mean(DF$Value, na.rm = T), linetype = "dashed", color = "black") +
    # labs(title = "Academic age", x = "Year of First Publication", y = "Frequency") +
    labs(title = "", x = "Year of First Publication", y = "Frequency") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(n = 5), expand = c(0,0))
  
  p
}

recode_areas = function (areas) {
  dplyr::recode(areas,
                `Acoustics` = "Acoustics",
                `Agriculture` = "Agriculture",
                `Automation & Control Systems` = "Automation",
                `Biochemistry & Molecular Biology` = "Bioch. & Mol. Biol.",
                `Biophysics` = "Biophysics",
                `Biotechnology & Applied Microbiology` = "Biotechnology",
                `Cardiovascular System & Cardiology` = "Cardiology",
                `Cell Biology` = "Cell Biology",
                `Chemistry` = "Chemistry",
                `Computer Science` = "Computer Science",
                `Dentistry, Oral Surgery & Medicine` = "Dentistry",
                `Dermatology` = "Dermatology",
                `Education & Educational Research` = "Education",
                `Endocrinology & Metabolism` = "Endocrinology",
                `Engineering` = "Engineering",
                `Entomology` = "Entomology",
                `Evolutionary Biology` = "Evolution",
                `Food Science & Technology` = "Food Science",
                `Gastroenterology & Hepatology` = "Gastro. & Hepatology",
                `General & Internal Medicine` = "Internal Medicine",
                `Genetics & Heredity` = "Genetics",
                `Health Care Sciences & Services` = "Health Care Sciences",
                `Hematology` = "Hematology",
                `History & Philosophy Of Science` = "Philosophy of Science",
                `Immunology` = "Immunology",
                `Infectious Diseases` = "Infectious Diseases",
                `Information Science & Library Science` = "Library Science",
                `Life Sciences & Biomedicine - Other Topics` = "Life Sciences (Other)",
                `Marine & Freshwater Biology` = "Marine Biology",
                `Materials Science` = "Materials Science",
                `Medical Informatics` = "Medical Informatics",
                `Medical Laboratory Technology` = "Laboratory Technology",
                `Microbiology` = "Microbiology",
                `Microscopy` = "Microscopy",
                `Neurosciences & Neurology` = "Neurosciences",
                `Nursing` = "Nursing",
                `Obstetrics & Gynecology` = "Ob. & Gyn.",
                `Oncology` = "Oncology",
                `Ophthalmology` = "Ophthalmology",
                `Orthopedics` = "Orthopedics",
                `Parasitology` = "Parasitology",
                `Pathology` = "Pathology",
                `Pediatrics` = "Pediatrics",
                `Pharmacology & Pharmacy` = "Pharmacology",
                `Physics` = "Physics",
                `Psychiatry` = "Psychiatry",
                `Public, Environmental & Occupational Health` = "Public Health",
                `Radiology, Nuclear Medicine & Medical Imaging` = "Medical Imaging",
                `Reproductive Biology` = "Reproductive Biology",
                `Research & Experimental Medicine` = "Experimental Medicine",
                `Respiratory System` = "Respiratory System",
                `Science & Technology - Other Topics` = "S&T (Other)",
                `Sport Sciences` = "Sport Sciences",
                `Telecommunications` = "Telecommunications",
                `Tropical Medicine` = "Tropical Medicine",
                `Urology & Nephrology` = "Urology & Nephrology",
                `Veterinary Sciences` = "Veterinary Sciences",
                `Virology` = "Virology",
                `Zoology` = "Zoology")
}

plot_most_common_areas = function(AUTHOR_DATA, period, min_thres = 2) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "TopField", Class %in% period) %>%
    mutate(Value = recode_areas(str_to_title(Value))) %>%
    count(Value) %>%
    filter(n > min_thres)
  
  p = ggplot(DF) +
    aes(x = reorder(Value, n), y = n) +
    geom_col() +
    # labs(title = "Most common journal area", x = "", y = "Frequency") +
    labs(title = "", x = "", y = "Frequency") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 9))
  
  p
}

plot_most_common_institutions = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "Affiliation") %>%
    pull(Value) %>%
    str_split(";") %>%
    unlist() %>%
    tibble(Value = .) %>%
    filter(Value != "-") %>%
    filter(!(Value %in% c("CNPQ", "BRAZIL MINISTERIO SAUDE"))) %>%
    mutate(Value = ifelse(Value %>% str_detect("FIOCRUZ"), "FIOCRUZ", Value)) %>%
    count(Value) %>%
    slice_max(n = 10, order_by = n)
  
  p = ggplot(DF) +
    aes(x = reorder(Value, n), y = n) +
    geom_col() +
    # labs(title = "Most common institutions", x = "", y = "Frequency") +
    labs(title = "", x = "", y = "Frequency") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 9))
  
  p
}

plot_most_common_regions = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "Region") %>%
    filter(!is.na(Value)) %>%
    pull(Value) %>%
    str_split(";") %>%
    unlist() %>%
    tibble(Value = .) %>%
    filter(!(Value %in% c("-",""))) %>%
    count(Value) %>%
    slice_max(n = 10, order_by = n)
  
  p = ggplot(DF) +
    aes(x = reorder(Value, n), y = n) +
    geom_col() +
    labs(title = "", x = "", y = "Frequency") +
    # labs(title = "Most common regions", x = "", y = "Frequency") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 9))
  
  p
}

plot_most_common_states = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "State") %>%
    pull(Value) %>%
    str_split(";") %>%
    unlist() %>%
    tibble(Value = .) %>%
    filter(Value != "-") %>%
    count(Value) %>%
    slice_max(n = 10, order_by = n)
  
  p = ggplot(DF) +
    aes(x = reorder(Value, n), y = n) +
    geom_col() +
    # labs(title = "Most common states", x = "", y = "Frequency") +
    labs(title = "", x = "", y = "Frequency") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 9))
  
  p
}

plot_pubs_by_mesh_cat_by_region = function (AUTHOR_DATA, regions) {
  AUTHOR_REGIONS = AUTHOR_DATA %>%
    filter(ShortName == "Region") %>%
    select(Author, Value) %>%
    rename(Region = Value)
  
  AUTHOR_DATA = AUTHOR_DATA %>%
    left_join(AUTHOR_REGIONS, by = "Author")
  
  map(regions, function (a_region) {
    plot_pubs_by_mesh_cat(
      AUTHOR_DATA %>% filter(Region %>% str_detect(a_region)),
      title = a_region)
  })
}

plot_pubs_by_mesh_cat_FAPERJ = function (AUTHOR_DATA, FAPERJ_NETWORK_INFO) {
  FAPERJ_NETWORK_INFO %>% group_by(FAPERJ_Net) %>%
    group_map(~ {
      authors_in_the_network = .x$Author
      plot_pubs_by_mesh_cat(
        AUTHOR_DATA %>% filter(Author %in% authors_in_the_network),
        title = paste("FAPERJ Network", .y$FAPERJ_Net))
    })
}

plot_cites_by_mesh_cat_FAPERJ = function (AUTHOR_DATA, FAPERJ_NETWORK_INFO) {
  FAPERJ_NETWORK_INFO %>% group_by(FAPERJ_Net) %>%
    group_map(~ {
      authors_in_the_network = .x$Author
      plot_cites_by_mesh_cat(
        AUTHOR_DATA %>% filter(Author %in% authors_in_the_network),
        title = paste("FAPERJ Network", .y$FAPERJ_Net))
    })
}

plot_cite_rate_by_mesh_cat_FAPERJ = function (AUTHOR_DATA, FAPERJ_NETWORK_INFO) {
  FAPERJ_NETWORK_INFO %>% group_by(FAPERJ_Net) %>%
    group_map(~ {
      authors_in_the_network = .x$Author
      plot_cite_rate_by_mesh_cat(
        AUTHOR_DATA %>% filter(Author %in% authors_in_the_network)
      ) + labs(title = paste("FAPERJ Network", .y$FAPERJ_Net))
    })
}
