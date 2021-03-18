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
      TargetAuthor = paste(TargetAuthor, collapse = ";"),
      Institution = paste(Institution, collapse = ";"),
      State = paste(State, collapse = ";"),
      Region = paste(Region, collapse = ";")
    )
}

# Function to build list of edges from the target author, institution, state or region columns,
# assuming they are separated by semicolons.
# It returns a data frame with a list of edges and an igraph object.
build_network = function (netcol) {
  i = 0
  tt = length(netcol)
  print("Building edgelist ...")
  edgelist = map_dfr(netcol, function (x) {
    i <<- i + 1; if (i %% 100 == 0) print(paste0(i,"/",tt))
    x = x %>% str_split(";") %>% unlist() %>% unique()
    if (length(x) == 1) { return (tibble()) }
    df = expand_grid(Va = x, Vb = x) %>% filter(!(Va %in% c("-","")), !(Vb %in% c("-","")))  %>%
      rowwise() %>% mutate(V1 = sort(c(Va,Vb))[1], V2 = sort(c(Va,Vb))[2]) %>%
      select(V1,V2)
    df
  })
  print("Building graph ...")
  network = igraph::graph_from_edgelist(as.matrix(edgelist), directed = F)
  list(edgelist = edgelist, net = network)
}

# Function to plot the networks as matrices.
plot_collab_matrix = function (edgelist, title = "") {
  edgelist = edgelist %>% count(V1, V2)
  ggplot(edgelist) +
    aes(x = V1, y = V2, alpha = n) +
    geom_tile(fill = "blue") +
    labs(x = "", y = "", title = title) +
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

# Function to calculate clustering and density of a network
calc_net_metrics = function (net) {
  tibble(
    ClusteringCoef = igraph::transitivity(net, "global"),
    EdgeDensity = igraph::edge_density(
      igraph::simplify(net, remove.multiple = T, remove.loops = T), loops = F)
  )
}
