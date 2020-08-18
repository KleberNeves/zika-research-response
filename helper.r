# Helper functions (I'm moving them to a package, this is a placeholder)

get.bibliometrix.M = function(filenames) {
  M = convert2df(filenames, dbsource = "isi", format = "plaintext")
  M
}

get.biblio.data = function (generation) {
  # browser()
  datapath = paste0(data_folder, "/", generation)
  filenames = paste0(datapath,"/",list.files(datapath, pattern = ".txt$"))
  M = get.bibliometrix.M(filenames)
  M = M[!duplicated(M$TI),]
  class(M) = rev(class(M))
  M
}

extract.author.country.order = function (M) {
  author_country_data = ldply(1:nrow(M), function (i) {
    title = M[i, "TI"]
    affil = M[i, "C1"]
    author_field = M[i, "AU"]
    
    if (is.na(affil) | !str_detect(affil, "\\[")) {
      return (
        data.frame(Title = title,
                   Author = NA,
                   Position = NA,
                   Country = NA,
                   stringsAsFactors = F)
      )
    }
    # browser()
    # Extract author list and positions to be merged later
    author_list = unlist(str_split(author_field, ";"))
    author_list = data.frame(Author = author_list, Position = 1:length(author_list))
    author_list$NegPosition = author_list$Position - nrow(author_list) - 1
    
    # Extracts author list from affiliation text
    authors = unlist(str_extract_all(affil, "(\\[.+?\\])"))
    authors = str_replace_all(authors, "[.];", ".")
    nauthors_per_group = str_count(authors, ";") + 1
    authors = str_remove_all(authors, "\\[")
    authors = str_remove_all(authors, "\\]")
    authors = str_remove_all(authors, ",")
    authors = str_remove_all(authors, "[.]")
    authors = unlist(str_split(authors, "; "))
    
    # Extracts affiliations per group of authors
    affil = str_replace_all(affil, "; \\[", " [")
    affil = str_replace_all(affil, "(\\[.+?\\])", "]")
    affil = unlist(str_split(affil, "\\] "))
    affil = affil[2:length(affil)]
    affil = affil[affil != ""]
    
    # Extract countries from affiliations
    countries = unlist(llply(affil, function (x) {
      x = unlist(str_split(x, "; "))
      x = str_trim(x)
      x = str_remove_all(x, "[.]")
      x = str_remove(x, ".+, ")
      x[str_which(x, " USA$")] = "USA"
      x = paste(x, collapse = "; ")
    }))
    countries = rep(countries, nauthors_per_group)
    # browser()
    # Building results data frame
    R = data.frame(Title = title,
                   Author = authors,
                   Country = countries,
                   stringsAsFactors = F)
    
    RR = merge(R, author_list, by = "Author")
    
    # If merge fails, see if author list is using initials and remerge
    if (nrow(RR) == 0) {
      author_names = unlist(lapply(authors, function (x) {
        # browser()
        x = unlist(str_split(x, " "))
        first_name = x[1]
        if (first_name %in% c("DE","DOS","DAS","DA")) {
          first_name = paste(x[1:2], collapse = " ")
          x = x[3:length(x)]
        } else {
          x = x[2:length(x)]
        }
        # if (length(x) > 1) {
        if (any(x %in% c("DE","DOS","DAS","DA"))) {
          dosdas = which(x %in% c("DE","DOS","DAS","DA")) + 1
          x = x[-dosdas]
        }
        x = str_extract(x, "[A-Z]")
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