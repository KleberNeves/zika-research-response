# Helper functions
library(httr)
library(xml2)
library(rvest)
library(tidyverse)
library(glue)
library(readxl)
library(bibliometrix)

get.bibliometrix.M = function(filenames) {
  M = convert2df(filenames, dbsource = "isi", format = "plaintext")
  M
}

get.biblio.data = function (datapath) {
  filenames = paste0(datapath,"/",list.files(datapath, pattern = ".txt$"))
  M = get.bibliometrix.M(filenames)
  M = M[!duplicated(M$TI),]
  class(M) = rev(class(M))
  M
}

# Fetches the information from the PubMed API
fetch_pubmed_mesh = function (pmids) {
  id_set = paste(pmids, collapse = ",")
  
  content = xml2::read_xml(glue("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id={id_set}&rettype=abstract&retmode=xml"))
  
  xml_articles = content %>% html_elements("PubmedArticle")
  
  map_dfr(xml_articles, extract_mesh_from_xml)
}

# Extract MeSH terms from XML returned from API
extract_mesh_from_xml = function (xml_article) {
  xml_mesh_terms = xml_article %>% html_elements("MedlineCitation > MeshHeadingList > MeshHeading")
  
  mesh_terms = map_dfr(xml_mesh_terms, function (xml_mesh_term) {
    xml_mesh_heading = xml_mesh_term %>% html_elements("DescriptorName") %>% html_text()
    xml_mesh_qualifier = xml_mesh_term %>% html_elements("QualifierName") %>% html_text()
    if (length(xml_mesh_qualifier) == 0) xml_mesh_qualifier = ""
    tibble(Heading = xml_mesh_heading, Qualifier = xml_mesh_qualifier)
  })
  
  pmid = xml_article %>% html_element("MedlineCitation > PMID") %>% html_text()
  mesh_terms$pmid = pmid
  
  mesh_terms
}

# For a set of PMIDs, fetches and organizes the MeSH terms
get_mesh_terms = function (pmids) {
  pmids = pmids[!is.na(pmids)]
  parts = split(pmids, ceiling(seq_along(pmids) / 50))
  
  done = 0
  get.part = function (part) {
    pm_rec = fetch_pubmed_mesh(part)
    done <<- done + length(part)
    cat(paste0(done,"/",length(pmids),"\n"))
    print ("Sleep ...")
    Sys.sleep(2)
    pm_rec
  }
  
  print("Downloading PubMed records ...")
  all_mesh_terms = map_dfr(parts, get.part)
}

organize_mesh_terms = function (mesh_terms) {
  print("Organizing MeSH terms ...")
  collapsed_terms = mesh_terms %>%
    mutate(Term = paste0(Heading, ":", Qualifier) %>% str_remove(":$")) %>%
    group_by(pmid) %>%
    summarise(
      MeshFullTerms = paste(Term, collapse = ";"),
      MeshHeadings = paste(Heading, collapse = ";")
    )
  
  collapsed_terms
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