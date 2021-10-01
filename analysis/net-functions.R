load_biblio_geo = function (filename) {
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
    return (tibble())
  }
  
  # browser()
  BD = BD %>% select(TI, DI, PY)
  
  AD = AUTHOR_DATA %>% filter(Author == author_filename)
  
  if (nrow(AD %>% filter(Class != "ERROR")) > 0) {
    BD$TargetAuthor = author_filename
    BD$Institution = AD %>% filter(ShortName == "Affiliation") %>% pull(Value)
    BD$State = AD %>% filter(ShortName == "State") %>% pull(Value)
    BD$Region = AD %>% filter(ShortName == "Region") %>% pull(Value)
  } else {
    BD$TargetAuthor = NA
    BD$Institution = NA
    BD$State = NA
    BD$Region = NA
  }
  
  BD$Class = ifelse(BD$PY < 2016, "Pre-Outbreak", "Post-Outbreak")
  
  BD
}

build_network_from_papers = function (DF, excluding = c()) {
  isFAPERJ = "FAPERJ_Network" %in% colnames(DF)
  
  print("Finding common authorship ...")
  connecting_papers = DF %>% filter(!is.na(TargetAuthor)) %>%
    mutate(PY = recode(PY, .missing = 2020)) %>%
    group_by(TI) %>%
    summarise(
      N = length(TargetAuthor %>% unique()),
      PY = max(PY, na.rm = T)
    ) %>% ungroup() %>%
    filter(N > 1)
  
  print("Building edges ...")
  edgelist = map_dfr(connecting_papers$TI, function (pub) {
    df = DF %>% filter(TI == pub)
    pairs = make_pairs(df$TargetAuthor)
    icols = c("Institution", "State", "Region")
    if (isFAPERJ) { icols = c(icols, "FAPERJ_Network") }
    edges_from_paper = map_dfr(icols, function (icol) {
        pmap_dfr(pairs, function (V1, V2) {
          pdf = df %>% filter(TargetAuthor %in% c(V1, V2))
          ps = make_pairs2(pdf[1,icol], pdf[2,icol]) %>%
            mutate(VRES1 = V1, VRES2 = V2,
                   id = paste0(V1,"---",V2))
          ps
        }) %>% mutate(Type = icol)
    })
    edges_from_paper
  }) %>%
    distinct() %>%
    filter(!(id %in% excluding))
  
  print("Building graphs ...")
  network_res = igraph::graph_from_edgelist(
    as.matrix(edgelist %>%
                select(VRES1, VRES2) %>%
                distinct()), directed = F)
  network_insts = igraph::graph_from_edgelist(
    as.matrix(edgelist %>%
                filter(Type == "Institution") %>%
                select(Va,Vb)), directed = F)
  network_states = igraph::graph_from_edgelist(
    as.matrix(edgelist %>%
                filter(Type == "State") %>%
                select(Va,Vb)), directed = F)
  network_regions = igraph::graph_from_edgelist(
    as.matrix(edgelist %>%
                filter(Type == "Region") %>%
                select(Va,Vb)), directed = F)
  if (isFAPERJ) {
    network_faperj = igraph::graph_from_edgelist(
      as.matrix(edgelist %>%
                  filter(Type == "FAPERJ_Network") %>%
                  select(Va,Vb)), directed = F)
  } else {
    network_faperj = NULL
  }

  print("Calculating metrics ...")
  metrics = rbind(
    calc_net_metrics(network_regions) %>% mutate(Type = "Regions"),
    calc_net_metrics(network_states) %>% mutate(Type = "States"),
    calc_net_metrics(network_insts) %>% mutate(Type = "Institutions"),
    calc_net_metrics(network_res) %>% mutate(Type = "Researchers")
  )
  
  if (isFAPERJ) {
    metrics = rbind(
      metrics, calc_net_metrics(network_faperj) %>% mutate(Type = "FAPERJ Network")
    )
  }
  
  list(edgelist = edgelist, nets = list(res = network_res, inst = network_insts, state = network_states, region = network_regions, faperj = network_faperj), metrics = metrics)
}

build_period_nets = function (df) {
  predf = df %>% filter(PY >= 2011 & PY < 2016)
  postdf = df %>% filter(PY >= 2016)
  
  print("Building pre-outbreak network ...")
  prenets = build_network_from_papers(predf)
  
  print("Building post-outbreak network ...")
  postnets = build_network_from_papers(postdf, prenets$edgelist$id %>% unique())
  
  list(pre = prenets, post = postnets)
}

make_pairs = function (x) {
  x = x %>% str_split(";") %>% unlist()
  dups = duplicated(x)
  while (any(dups)) {
    x[duplicated(x)] = paste0(x[duplicated(x)], ",")
    dups = duplicated(x)
  }
  if (length(x) == 1) { return (tibble()) }
  df = expand_grid(Va = x, Vb = x) %>% filter(!(Va %in% c("-","")), !(Vb %in% c("-","")), Va != Vb) %>%
    rowwise() %>% mutate(V1 = sort(c(Va,Vb))[1], V2 = sort(c(Va,Vb))[2]) %>%
    select(V1,V2) %>% distinct() %>%
    mutate(V1 = V1 %>% str_remove_all(","), V2 = V2 %>% str_remove_all(","))
  df
}

make_pairs2 = function (x, y) {
  x = x %>% str_split(";") %>% unlist()
  y = y %>% str_split(";") %>% unlist()
  df = expand_grid(Va = x, Vb = y) %>%
    filter(Va != "-", Vb != "-")
  df
}


# Function to plot the networks as matrices.
plot_collab_matrix = function (edgelist, type, include_loops = T, flevels = NULL, title = "") {
  if (type != "Researcher") {
    edgelist = edgelist %>% filter(Type == type)
  } else {
    edgelist = edgelist %>%
      distinct(id, .keep_all = T) %>%
      mutate(Va = VRES1, Vb = VRES2)
  }
  
  if (!include_loops) {
    edgelist = edgelist %>% filter(Va != Vb)
  }
  
  edgelist = edgelist %>% count(Va, Vb)
  
  if (!is.null(flevels)) {
    edgelist = edgelist %>% mutate(
      Va = factor(Va, levels = flevels),
      Vb = factor(Vb, levels = rev(flevels))
    )
  }

  ggplot(edgelist) +
    aes(x = Va, y = Vb, alpha = n, label = n) +
    geom_tile(fill = "blue") +
    geom_text(size = 4) +
    labs(x = "", y = "", title = title) +
    scale_x_discrete(drop = F, position = "top") +
    scale_y_discrete(drop = F) +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 15),
      legend.position = "none"
    )
}

# Function to calculate clustering and density of a network
calc_net_metrics = function (net) {
  tibble(
    Nodes = length(igraph::V(net)),
    Edges = length(igraph::E(net)),
    ClusteringCoef = igraph::transitivity(net, "global"),
    EdgeDensity = igraph::edge_density(
      igraph::simplify(net, remove.multiple = T, remove.loops = T), loops = F)
  )
}
