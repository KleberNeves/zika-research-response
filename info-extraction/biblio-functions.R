library(tidyverse)
library(bibliometrix)
library(lubridate)
library(readxl)
library(metricshelpr)

load_biblio_author = function (filenane) {
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
  
  # Harmonize variations in author names?
  
  author_institution = AUTHOR_DATA %>%
    filter(Author == basename(filename), ShortName %in% c("Affiliation", "State", "Region")) %>%
    select(Author, ShortName, Value) %>%
    pivot_wider(id_cols = Author, names_from = ShortName, values_from = Value)
    
  BD = BD %>% mutate(Author = basename(filename)) %>%
    left_join(author_institution, by = "Author")
      
  BD
}