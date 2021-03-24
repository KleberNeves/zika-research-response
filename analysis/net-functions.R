library(tidyverse)
library(bibliometrix)
library(lubridate)
library(readxl)
library(metricshelpr)

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

consolidate_biblio_rows = function(DF) {
  DF %>% filter(!is.na(TargetAuthor)) %>% group_by(TI, Class) %>%
    summarise(
      TargetAuthor = paste(sort(TargetAuthor), collapse = ";"),
      Institution = paste(sort(Institution), collapse = ";"),
      State = paste(sort(State), collapse = ";"),
      Region = paste(sort(Region), collapse = ";")
    )
}

# Makes edges from a vector of characters, where each element specifies a list of nodes separated by semicolon
make_edges = function (y) {
  i = 0
  tt = length(y)
  map_dfr(y, function (x) {
    i <<- i + 1; if (i %% 100 == 0) print(paste0(i,"/",tt))
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
  })
}

# Function to build list of edges from the target author, institution, state or region columns,
# assuming they are separated by semicolons.
# It returns a data frame with a list of edges and an igraph object.
build_network = function (netcol) {
  netcol = netcol[str_detect(netcol, ";")]
  print("Building edgelist ...")
  edgelist = make_edges(netcol)
  
  print("Building graph ...")
  network = igraph::graph_from_edgelist(as.matrix(edgelist), directed = F)
  list(edgelist = edgelist, net = network)
}

# 
build_network2 = function (netcol, authorcol, excluding = c()) {
  browser()
  to_keep = str_detect(netcol, ";")
  netcol = netcol[to_keep]
  authorcol = authorcol[to_keep]
  print("Building edgelist ...")
  edgelist = make_edges(netcol)
  author_edgelist = make_edges(authorcol) %>%
    mutate(id = paste(V1, V2, sep = "---"), X = id %in% excluding)
  edgelist = edgelist[!author_edgelist$X,]
  
  print("Building graph ...")
  network = igraph::graph_from_edgelist(as.matrix(edgelist), directed = F)
  list(edgelist = edgelist, net = network)
}

build_networks = function (df, excluding = c()) {
  df = df %>% filter(str_detect(TargetAuthor, ";"))
  
  print("Building edgelists ...")
  tt = nrow(df)
  edgelist = map_dfr(1:tt, function (i) {
    i <<- i + 1; if (i %% 100 == 0) print(paste0(i,"/",tt))
    xdf = df[i,]
    res_pairs = make_pairs(xdf$TargetAuthor)
    state_pairs = make_pairs(xdf$State)
    region_pairs = make_pairs(xdf$Region)
    rdf = cbind(res_pairs, state_pairs, region_pairs)
    colnames(rdf) = c("V1RESEARCHER", "V2RESEARCHER", "V1STATE", "V2STATE", "V1REGION", "V2REGION")
    rdf
  })
  
  edgelist = edgelist %>%
    mutate(id = paste(V1RESEARCHER, V2RESEARCHER, sep = "---")) %>%
    filter(!(id %in% excluding)) %>% select(-id)
  
  print("Building graphs ...")
  network_res = igraph::graph_from_edgelist(
    as.matrix(edgelist[,c("V1RESEARCHER","V2RESEARCHER")]), directed = F)
  network_states = igraph::graph_from_edgelist(
    as.matrix(edgelist[,c("V1STATE","V2STATE")]), directed = F)
  network_regions = igraph::graph_from_edgelist(
    as.matrix(edgelist[,c("V1REGION","V2REGION")]), directed = F)
  
  edgelist = edgelist %>% mutate(i = 1:nrow(edgelist)) %>%
    pivot_longer(cols = -i) %>% mutate(Type = str_remove(name, "V[0-9]"), ij = str_extract(name, "V[0-9]")) %>% select(-name) %>% pivot_wider(names_from = ij, values_from = value) %>% select(-i)
  
  list(edgelist = edgelist, nets = list(res = network_res, states = network_states, regions = network_regions))
}

build_period_nets = function (df) {
  predf = df %>% filter(Class == "Pre")
  postdf = df %>% filter(Class == "Post")
  
  prenets = build_networks(predf)
  postnets = build_networks(postdf, edge_ids(prenets$edgelist))
  
  list(pre = prenets, post = postnets)
}

edge_ids = function (el) {
  el %>%
    filter(Type == "RESEARCHER") %>%
    mutate(id = paste(V1, V2, sep = "---")) %>%
    pull(id) %>% unique()
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
}

# Function to plot the networks as matrices.
plot_collab_matrix = function (edgelist, type, include_loops = T, flevels = NULL, title = "") {
  edgelist = edgelist %>% filter(Type == type)
  
  if (!include_loops) {
    edgelist = edgelist %>% filter(V1 != V2)
  }
  
  edgelist = edgelist %>% count(V1, V2)
  
  if (!is.null(flevels)) {
    edgelist = edgelist %>% mutate(
      V1 = factor(V1, levels = flevels),
      V2 = factor(V2, levels = rev(flevels))
    )
  }
  
  ggplot(edgelist) +
    aes(x = V1, y = V2, alpha = n, label = n) +
    geom_tile(fill = "blue") +
    # geom_text() +
    labs(x = "", y = "", title = title) +
    scale_x_discrete(drop = F, position = "top") +
    scale_y_discrete(drop = F) +
    theme(
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 15)
    )
}

# Function to subtract edges (2 - 1)
# Example: if there A and B are connected by 3 edges in edgelist2 and 1 edge in edgelist1.
# If subtract_all == T: result will be 0 connections between A and B
# If subtract_all == F: result will be 2 (== 3 - 1) connections between A and B
subtracted_network = function (edgelist1, edgelist2, subtract_all = T) {
  print("Subtracting edgelists ...")
  edgelist1 = edgelist1 %>% mutate(id = paste(V1, V2, sep = "---")) %>% count(id, V1, V2)
  edgelist2 = edgelist2 %>% mutate(id = paste(V1, V2, sep = "---")) %>% count(id, V1, V2)
  edgelist = left_join(edgelist2, edgelist1, by = "id") %>%
    mutate(n.y = ifelse(is.na(n.y), 0, n.y))
  if (subtract_all) {
    edgelist = edgelist %>% filter(n.y == 0)
  } else {
    edgelist = edgelist %>% mutate(n.x = n.x - n.y) %>%
      filter(n.x > 0)
  }
  edgelist = edgelist %>% select(V1.x, V2.x)
  colnames(edgelist) = c("V1","V2")

  print("Building graph ...")
  network = igraph::graph_from_edgelist(as.matrix(edgelist), directed = F)
  list(edgelist = edgelist, net = network)
}

# Function to obtain the overlapping edges
overlap_list = function (edgelist1, edgelist2) {
  print("Merging edgelists ...")
  edgelist1 = edgelist1 %>% mutate(id = paste(V1, V2, sep = "---")) %>% count(id, V1, V2)
  edgelist2 = edgelist2 %>% mutate(id = paste(V1, V2, sep = "---")) %>% count(id, V1, V2)
  edgelist = full_join(edgelist2, edgelist1, by = "id") %>%
    mutate(n.y = ifelse(is.na(n.y), 0, n.y), n.x = ifelse(is.na(n.x), 0, n.x))
  edgelist = edgelist %>% filter(n.y != 0 & n.x != 0)
  edgelist %>% pull(id)
}

# Function to calculate clustering and density of a network
calc_net_metrics = function (net) {
  tibble(
    ClusteringCoef = igraph::transitivity(net, "global"),
    EdgeDensity = igraph::edge_density(
      igraph::simplify(net, remove.multiple = T, remove.loops = T), loops = F)
  )
}
