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

plot_pubs_by_mesh_cat_by_field = function(AUTHOR_DATA, min_authors_to_plot = 10) {
  AUTHORS_BY_FIELD = AUTHOR_DATA %>%
    filter(ShortName == "TopField" & Class == "Pre-Outbreak") %>%
    mutate(Field = str_to_title(Value)) %>%
    select(Author, Field)
  
  DF = AUTHOR_DATA %>% filter(ShortName %>% str_detect("Frequency of MeSH"), Class == "Zika") %>%
    mutate(Value = as.numeric(Value),
           ShortName = ShortName %>% str_remove("MeSHCat - Frequency of MeSH Category - ")) %>%
    left_join(AUTHORS_BY_FIELD, by = "Author")
  
  top_fields = AUTHORS_BY_FIELD %>%
    count(Field) %>% filter(n >= min_authors_to_plot) %>% pull(Field) %>% unique()
  
  map(top_fields, function (selected_field) {
    d = DF %>% filter(Field == selected_field)
    plot_pubs_by_mesh_cat(d, title = selected_field, preprocessed = T)
  })
}

plot_pubs_by_mesh_cat = function(AUTHOR_DATA, title = "", preprocessed = F) {
  if (!preprocessed) {
    DF = AUTHOR_DATA %>% filter(ShortName %>% str_detect("Frequency of MeSH"), Class == "Zika") %>%
      mutate(Value = as.numeric(Value),
             ShortName = ShortName %>% str_remove("MeSHCat - Frequency of MeSH Category - "))
  } else {
    DF = AUTHOR_DATA
  }
  
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
  
  bin_number = round(max(DF$Value) / 2)
  
  p = ggplot(DF) +
    aes(x = Value) +
    geom_histogram(bins = bin_number) +
    geom_vline(xintercept = mean(DF$Value, na.rm = T), linetype = "dashed", color = "black") +
    labs(x = "Percentage of Zika-related papers, post-outbreak", y = "Frequency") +
    scale_x_continuous(breaks = pretty_breaks(n = 10), expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0))
  
  p
}

plot_compare_intl_papers = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "PercPapersINTL", Class %in% c("Pre-Outbreak", "Zika")) %>%
    mutate(Value = as.numeric(Value))
  
  p1 = ggplot(DF) +
    aes(x = Class, y = Value, group = Author) +
    geom_point(size = 3, alpha = 0.5) +
    geom_line(alpha = 0.5) +
    labs(x = "Period", y = "Percentage of International Collaborations") +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
  
  p2 = ggplot(DF) +
    aes(x = Class, y = Value) +
    geom_violin() +
    # stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = "crossbar", width = 0.25, alpha = 0.5) +
    labs(x = "Period", y = "Percentage of International Collaborations") +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
  
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
    labs(x = "Period", y = "Average # of Authors per Paper") +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
  
  p2 = ggplot(DF) +
    aes(x = Class, y = Value) +
    geom_violin() +
    # stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = "crossbar", width = 0.25, alpha = 0.5) +
    labs(x = "Period", y = "Average # of Authors per Paper") +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
  
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
    labs(x = "Period", y = "Average # of Citations per Paper") +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
  
  p2 = ggplot(DF) +
    aes(x = Class, y = Value) +
    geom_violin() +
    # stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = "crossbar", width = 0.25, alpha = 0.5) +
    labs(x = "Period", y = "Average # of Citations per Paper") +
    scale_y_continuous(breaks = pretty_breaks(n = 10))
  
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
      labs(x = "Total Citations Before the Outbreak", y = "Frequency") +
      scale_y_continuous(breaks = c(10, 50, 200, 1000, 10000), trans = "log10", expand = c(0,0))
  } else {
    p = ggplot(DF) +
      aes(x = Value) +
      geom_histogram(bins = 50) +
      labs(x = "Total Citations Before the Outbreak", y = "Frequency") +
      scale_y_continuous(breaks = pretty_breaks(n = 10), expand = c(0,0))
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
    labs(x = "Year of First Publication", y = "Frequency") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(n = 10), expand = c(0,0))
  
  p
}

plot_most_common_areas = function(AUTHOR_DATA, period, min_thres = 2) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "TopField", Class %in% period) %>%
    mutate(Value = str_to_title(Value)) %>%
    count(Value) %>%
    filter(n > min_thres)
  
  p = ggplot(DF) +
    aes(x = reorder(Value, n), y = n) +
    geom_col() +
    labs(title = "Top Journal Area for the Author", x = "", y = "Frequency") +
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
    count(Value) %>%
    slice_max(n = 10, order_by = n)
  
  p = ggplot(DF) +
    aes(x = reorder(Value, n), y = n) +
    geom_col() +
    labs(title = "Top Institutions", x = "", y = "Frequency") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 9))
  
  p
}

plot_most_common_regions = function(AUTHOR_DATA) {
  DF = AUTHOR_DATA %>%
    filter(ShortName == "Region") %>%
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
    labs(title = "Top Regions", x = "", y = "Frequency") +
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
    labs(title = "Top States", x = "", y = "Frequency") +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(breaks = pretty_breaks(), expand = c(0,0)) +
    coord_flip() +
    theme(axis.text.y = element_text(size = 9))
  
  p
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
