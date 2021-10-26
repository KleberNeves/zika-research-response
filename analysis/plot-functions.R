plot_cite_rate_by_mesh_cat = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA |>
    filter(
      ShortName |> str_detect("Frequency of MeSH") |
        ShortName |> str_detect("Cites"), Class == "Zika"
    ) |>
    select(-Class) |>
    mutate(
      Value = as.numeric(Value),
      Measure = ShortName |>
        str_replace("MeSHCat - Frequency of MeSH Category", "Pubs") |>
        str_replace("MeSHCatCites", "Cite") |>
        str_sub(1,4),
      Category = ShortName |>
        str_remove("MeSHCat - Frequency of MeSH Category - ") |>
        str_remove("MeSHCatCites - ")
    ) |>
    select(Author, Category, Measure, Value) |>
    pivot_wider(id_cols = c(Author, Category), names_from = Measure, values_from = Value) |>
    mutate(CitationRate = Cite / Pubs) |>
    select(Author, Category, CitationRate)
  
  DF$Category = DF$Category |> recode_mesh_cats()
  
  plot_mesh_cite_rate = function (DF, title = "") {
    DF = DF |> group_by(Category) |> summarise(Value = mean(CitationRate, na.rm = T))
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
  AUTHORS_BY_FIELD = AUTHOR_DATA |>
    filter(ShortName == "TopField" & Class == "Pre-Outbreak") |>
    mutate(Field = str_to_title(Value)) |>
    select(Author, Field)
  
  DF = AUTHOR_DATA |> filter(ShortName |> str_detect("Frequency of MeSH"), Class == "Zika") |>
    mutate(Value = as.numeric(Value),
           ShortName = ShortName |> str_remove("MeSHCat - Frequency of MeSH Category - ")) |>
    left_join(AUTHORS_BY_FIELD, by = "Author")
  
  if (is.null(fields_to_plot)) {
    top_fields = AUTHORS_BY_FIELD |>
      count(Field) |> filter(n >= min_authors_to_plot) |> pull(Field) |> unique()
  } else {
    top_fields = str_to_title(fields_to_plot)
  }
  
  map(top_fields, function (selected_field) {
    d = DF |> filter(Field == selected_field)
    plot_pubs_by_mesh_cat(d, title = recode_areas(selected_field), preprocessed = T)
  })
}

plot_pubs_by_mesh_cat_pubset = function(PAPER_SET, title) {
  DF = PAPER_SET |>
    filter(!is.na(ZikaCats) & ZikaCats != "") |>
    pull(ZikaCats) |>
    str_split(";") |>
    unlist() |>
    recode_mesh_cats() |>
    as_tibble() |>
    count(value) |>
    rename(ShortName = value, Value = n) |>
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
    DF = AUTHOR_DATA |> filter(ShortName |> str_detect("Frequency of MeSH"), Class == "Zika") |>
      mutate(Value = as.numeric(Value),
             ShortName = ShortName |> str_remove("MeSHCat - Frequency of MeSH Category - "))
  } else {
    DF = AUTHOR_DATA
  }
  
  DF$ShortName = DF$ShortName |> recode_mesh_cats()

  DF = DF |> group_by(ShortName) |> summarise(Value = sum(Value, na.rm = T))
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
    DF = AUTHOR_DATA |> filter(ShortName |> str_detect("Cites"), Class == "Zika") |>
      mutate(Value = as.numeric(Value),
             ShortName = ShortName |> str_remove("MeSHCatCites - "))
  } else {
    DF = AUTHOR_DATA
  }
  
  DF$ShortName = DF$ShortName |> recode_mesh_cats()
  
  DF = DF |> group_by(ShortName) |> summarise(Value = sum(Value, na.rm = T))
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

plot_thematic_deviation = function (AUTHOR_DATA, selected_authors) {
  DF_BASE = AUTHOR_DATA |>
    filter(!(Author %in% selected_authors)) |>
    filter(ShortName |> str_detect("Frequency of MeSH"), Class == "Zika") |>
    mutate(
      RefValue = as.numeric(Value),
      ShortName = ShortName |>
        str_remove("MeSHCat - Frequency of MeSH Category - ") |>
        recode_mesh_cats()
    ) |>
    group_by(ShortName) |>
    summarise(RefValue = sum(RefValue, na.rm = T)) |>
    ungroup() |>
    mutate(RefPerc = RefValue / sum(RefValue))
  
  DF_SELECTED = AUTHOR_DATA |>
    filter(Author %in% selected_authors) |>
    filter(ShortName |> str_detect("Frequency of MeSH"), Class == "Zika") |>
    mutate(
      Value = as.numeric(Value),
      ShortName = ShortName |>
        str_remove("MeSHCat - Frequency of MeSH Category - ") |>
        recode_mesh_cats()
    ) |>
    group_by(ShortName) |>
    summarise(Value = sum(Value, na.rm = T)) |>
    ungroup() |>
    mutate(Perc = Value / sum(Value))
  
  DF = merge(DF_BASE, DF_SELECTED, by = "ShortName") |>
    mutate(Diff = 100 * (Perc - RefPerc))
  
  p = ggplot(DF) +
    aes(x = ShortName, y = Diff, fill = ShortName) +
    geom_col() +
    labs(x = "", y = "% Excess Publications") +
    coord_flip() +
    theme(axis.text.y = element_text(size = 10),
          legend.position = "none")
  
  p
}

plot_compare_by_pivot = function (AUTHOR_DATA, outcome) {
  DF = rbind(
    AUTHOR_DATA |>
      filter(ShortName == "CareerNetPivot", Class == "All"),
    AUTHOR_DATA |>
      filter(ShortName == outcome, Class == "Zika")
  ) |>
    pivot_wider(id_cols = Author, names_from = ShortName, values_from = Value)
    
  DF$Outcome = as.double(DF[[outcome]])
  
  if (outcome == "Papers") {
    yname = "Average # of\nPapers"
  } else if (outcome == "Citations") {
    yname = "Average # of\nCitations"
  } else if (outcome == "CitationRate") {
    yname = "Average # of\nCitations per Paper"
  }
  
  p1 = ggplot(DF) +
    aes(x = CareerNetPivot, y = Outcome) +
    geom_point(size = 3, alpha = 0.5) +
    geom_line(alpha = 0.5) +
    labs(x = "", y = yname) +
    scale_y_continuous(breaks = pretty_breaks(n = 6))
  
  p2 = ggplot(DF) +
    aes(x = CareerNetPivot, y = Outcome) +
    geom_violin() +
    stat_summary(fun = median, fun.min = median, fun.max = median, geom = "errorbar", width = 0.25, size = 0.5, alpha = 0.5, linetype = "dashed") +
    labs(x = "", y = yname) +
    scale_y_continuous(breaks = pretty_breaks(n = 6))
  
  list(p1,p2)
}

plot_compare_by_pivot_all = function (AUTHOR_DATA) {
  n_papers_by_pivot = AUTHOR_DATA |>
    filter(ShortName %in% c("CareerNetPivot", "Papers", "Citations", "HighlyCited") &
             Class %in% c("All","Zika")) |>
    pivot_wider(id_cols = c(Author), names_from = ShortName, values_from = Value) |>
    mutate(Citations = as.numeric(Citations), Papers = as.numeric(Papers)#, HighlyCited = as.numeric(HighlyCited)
    ) |>
    group_by(CareerNetPivot) |>
    summarise(Citations = sum(Citations), Publications = sum(Papers)#, `Highly Cited Publications` = as.numeric(HighlyCited)
    ) |>
    ungroup() |>
    mutate(`Citations per Paper` = Citations / Publications,
           `Highly Cited Publications` = c(50, 55))
  
  DF = n_papers_by_pivot |>
    pivot_longer(cols = -CareerNetPivot)
  
  ggplot(DF) +
    aes(x = name, y = value, fill = CareerNetPivot) +
    geom_col(position = position_dodge()) +
    facet_wrap(~name, scales = "free") +
    theme(legend.position = "bottom",
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_blank())
}

plot_pivots_by_field = function (AUTHOR_DATA) {
  
  AUTHORS_BY_FIELD = AUTHOR_DATA |>
    filter(ShortName == "TopField" & Class == "Pre-Outbreak") |>
    mutate(Field = recode_areas(str_to_title(Value))) |>
    select(Author, Field)
  
  top_fields = AUTHORS_BY_FIELD |>
    count(Field) |> filter(n >= 10) |> pull(Field) |> unique()
  
  DF = AUTHOR_DATA |>
    filter(ShortName == "CareerNetPivot", Class == "All") |>
    left_join(AUTHORS_BY_FIELD, by = "Author") |>
    filter(Field %in% top_fields) |>
    group_by(Value, Field) |>
    summarise(n = n()) |>
    ungroup() |>
    group_by(Field) |>
    mutate(s = sum(n), perc = n / s) |>
    filter(Value == "Hard pivot")
    
  p = ggplot(DF) +
    aes(x = reorder(Field, perc), y = perc,
        label = paste0(n, "/", s)) +
    geom_col() +
    geom_text(hjust = 1, vjust = 0.5, size = 3, color = "white") +
    # labs(title = "Most common journal area", x = "", y = "Frequency") +
    labs(title = "", x = "", y = "Frequency") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 9))
  
  p
}

plot_compare_intl_papers = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA |>
    filter(ShortName == "PercPapersINTL", Class %in% c("Pre-Outbreak", "Zika")) |>
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
  DF = AUTHOR_DATA |>
    filter(ShortName == "AvgAuthorNumber", Class %in% c("Pre-Outbreak", "Zika")) |>
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
  DF = AUTHOR_DATA |>
    filter(ShortName == "CitationRate", Class %in% c("Pre-Outbreak", "Zika")) |>
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
  DF = AUTHOR_DATA |>
    filter(ShortName == "Citations", Class == "Pre-Outbreak") |>
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
  DF = AUTHOR_DATA |>
    filter(ShortName == "Academic Age", Class == period) |>
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
  DF = AUTHOR_DATA |>
    filter(ShortName == "TopField", Class %in% period) |>
    mutate(Value = recode_areas(str_to_title(Value))) |>
    count(Value) |>
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
  DF = AUTHOR_DATA |>
    filter(ShortName == "Affiliation") |>
    pull(Value) |>
    str_split(";") |>
    unlist() %>%
    tibble(Value = .) |>
    filter(Value != "-") |>
    filter(!(Value %in% c("CNPQ", "BRAZIL MINISTERIO SAUDE"))) |>
    mutate(Value = ifelse(Value |> str_detect("FIOCRUZ"), "FIOCRUZ", Value)) |>
    count(Value) |>
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
  DF = AUTHOR_DATA |>
    filter(ShortName == "Region") |>
    filter(!is.na(Value)) |>
    pull(Value) |>
    str_split(";") |>
    unlist() %>%
    tibble(Value = .) |>
    filter(!(Value %in% c("-",""))) |>
    count(Value) |>
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
  DF = AUTHOR_DATA |>
    filter(ShortName == "State") |>
    pull(Value) |>
    str_split(";") |>
    unlist() %>%
    tibble(Value = .) |>
    filter(Value != "-") |>
    count(Value) |>
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
  AUTHOR_REGIONS = AUTHOR_DATA |>
    filter(ShortName == "Region") |>
    select(Author, Value) |>
    rename(Region = Value)
  
  AUTHOR_DATA = AUTHOR_DATA |>
    left_join(AUTHOR_REGIONS, by = "Author")
  
  map(regions, function (a_region) {
    plot_pubs_by_mesh_cat(
      AUTHOR_DATA |> filter(Region |> str_detect(a_region)),
      title = a_region)
  })
}

plot_pubs_by_mesh_cat_FAPERJ = function (AUTHOR_DATA, FAPERJ_NETWORK_INFO) {
  FAPERJ_NETWORK_INFO |> group_by(FAPERJ_Net) |>
    group_map(~ {
      authors_in_the_network = .x$Author
      plot_pubs_by_mesh_cat(
        AUTHOR_DATA |> filter(Author %in% authors_in_the_network),
        title = paste("FAPERJ Network", .y$FAPERJ_Net))
    })
}

plot_cites_by_mesh_cat_FAPERJ = function (AUTHOR_DATA, FAPERJ_NETWORK_INFO) {
  FAPERJ_NETWORK_INFO |> group_by(FAPERJ_Net) |>
    group_map(~ {
      authors_in_the_network = .x$Author
      plot_cites_by_mesh_cat(
        AUTHOR_DATA |> filter(Author %in% authors_in_the_network),
        title = paste("FAPERJ Network", .y$FAPERJ_Net))
    })
}

plot_cite_rate_by_mesh_cat_FAPERJ = function (AUTHOR_DATA, FAPERJ_NETWORK_INFO) {
  FAPERJ_NETWORK_INFO |> group_by(FAPERJ_Net) |>
    group_map(~ {
      authors_in_the_network = .x$Author
      plot_cite_rate_by_mesh_cat(
        AUTHOR_DATA |> filter(Author %in% authors_in_the_network)
      ) + labs(title = paste("FAPERJ Network", .y$FAPERJ_Net))
    })
}


cognitiveCareerPlot = function(author_filename, base.size = 10, n = 30, periods = c(2015), period.names = c("Before Zika Outbreak", "After Zika Outbreak")) {
  
  M = bibliometrix::convert2df(author_filename, dbsource = "isi", format = "plaintext")
    
    # Filter the papers that will appear, if not showing all of them.
    if (nrow(M) > n) {
      # Possibilidade: incluir todas as revisões, porque elas são marcos
      
      # Picks the most cited for each year.
      topyear = (M |> group_by(PY) |> top_n(1,TC))$DI
      k = min(n, nrow(M)) - length(topyear)
      
      # Picks the most cited overall
      topall = (M |> filter(!(DI %in% topyear)) |> top_n(k,TC))$DI
      
      # Filters
      toinclude = unique(c(topyear, topall))
      M = M[M$DI %in% toinclude, ]
    }
  
  # Makes the adjancency matrix and graph for the bibliographic coupling network
  ADJ = as.matrix(biblioNetwork(M, analysis = "coupling", network = "references", sep = ";", shortlabel = F))
  NET = graph_from_adjacency_matrix(ADJ)
  
  # Creates the layout from graphs, to be able to specify positioning of nodes
  NET = simplify(NET, remove.multiple = T, remove.loops = T)
  layout_m = create_layout(NET, layout = "auto")
  
  # Extract numeric years
  layout_m$Year = as.numeric(str_extract(V(NET)$name, "[0-9]{4,}"))
  Years = layout_m$Year
  
  # Defines size of nodes
  # layout_m$node.size = 10 * (base.size + 2 * base.size * M$TC / max(M$TC))
  # TODO for some reason, setting node.size via aes(size = node.size) from layout_m does not work
  V(NET)$node.size = (base.size / 2 + 2 * base.size * M$TC / max(M$TC))
  
  # Name of nodes is the paper title, truncated at 50 characters
  layout_m$id = toupper(M$TI)
  layout_m$name.truncated = str_sub(M$TI, 1, 60)
  
  # Breaks the label in three lines for longer titles and sets node properties
  layout_m$name.label = ifelse(V(NET)$node.size < 0.8 * base.size,
                               "", paste0(
                                 str_sub(M$TI, 1, 20),"\n",
                                 str_sub(M$TI, 21, 40),"\n",
                                 str_sub(M$TI, 41, 60),"\n"
                               )
  )
  
  # Separates papers in clusters and add cluster data to layout, to use for y pos
  V(NET)$id = layout_m$id # will use to check for cluster belonging in the loop below
  dg = decompose.graph(NET, mode = "weak")
  layout_m$cluster = 0
  for (k in 1:length(dg)) {
    dec = dg[[k]]
    layout_m$cluster = ifelse(layout_m$id %in% V(dec)$id, k, layout_m$cluster)
  }
  
  # Join isolated nodes in a single cluster
  d = layout_m |> group_by(cluster) |> summarise(n = n())
  isolated = (d |> filter(n == 1))$cluster
  layout_m$cluster = ifelse(layout_m$cluster %in% isolated, 0, layout_m$cluster)
  # browser()
  
  # Sets position of nodes based on cluster and year
  layout_m$x = layout_m$Year
  posy = layout_m |> group_by(Year) |> mutate(py = 1:n()) |> mutate(maxpy = max(py))
  layout_m$py = (posy$py) / (posy$maxpy + 1)
  layout_m$py = layout_m$py + ifelse(layout_m$Year %% 2 == 0, 0.5, 0)
  pheight = max(posy$maxpy) * base.size / 2
  layout_m$y = layout_m$py * pheight + rnorm(nrow(layout_m), 0, layout_m$py * 1.5)
  layout_m$y = layout_m$y - min(layout_m$y)
  
  # Builds ggplot
  g = ggraph(layout_m)
  
  if (!is.null(periods)) {
    g = g + geom_vline(xintercept = periods, linetype = "dashed", color = "black")
  }
  # browser()
  g = g +
    geom_edge_link(width = 1, check_overlap = T, edge_alpha = 0.8, color = "grey") +
    geom_node_point(aes(color = as.factor(cluster)), size = V(NET)$node.size, alpha = 0.8) +
    geom_node_text(aes(label = name.label), size = 2.2, 
                   repel = F, color = "black", alpha = 1) + 
    scale_x_continuous(labels = as.character(seq(min(Years), max(Years))), breaks = seq(min(Years), max(Years)), expand = c(.1, .1)) +
    labs(title = "Cognitive Career Plot (Reference Coupling)", x = "", y = "")
  
  # Adicionar retângulos e nomes de períodos
  if (!is.null(periods)) {
    periods = c(min(Years), periods, c(max(Years)))
    tly = (max(layout_m$y) * 1.15)
    for (i in 1:(length(periods)-1)) {
      g = g +
        annotate("rect", xmin = periods[i], ymin = (tly),
                 xmax = periods[i+1], ymax = (tly*0.98), fill = "grey80", color = "black") +
        annotate("text", label = period.names[i], x = (periods[i] + periods[i+1]) / 2,
                 y = (tly * 0.95), size = 3)
    }
  }
  # browser()
  g + theme_minimal() + theme(
    legend.position = "none",
    
    panel.background = element_rect(fill = "grey97", color = "grey97"),
    panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    axis.line.y = element_blank(), axis.text.y = element_blank(),
    axis.ticks.y = element_blank(), axis.title.y = element_blank(),
    
    axis.title.x = element_blank(), axis.line.x = element_blank(),
    axis.text.x = element_text(face = "bold")
  )
}
