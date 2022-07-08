freshness = function (paper_title) {
  PAPER = ALL_PAPERS |> filter(TI == paper_title)
  author_list = PAPER |> pull(TargetAuthor) |> unique()
  authors_papers = ALL_PAPERS |> filter(TargetAuthor %in% author_list & PY < PAPER$PY)
  previous_collabs = authors_papers$TI[which(duplicated(authors_papers$TI))]
  previous_collabs = authors_papers[authors_papers$TI %in% previous_collabs,] |> select(TI, TargetAuthor)
  
  find_collab = function (a1, a2) {
    a1p = previous_collabs$TI[previous_collabs$TargetAuthor == a1]
    a2p = previous_collabs$TI[previous_collabs$TargetAuthor == a2]
    sum(a1p %in% a2p)
  }
  
  combs = as.data.frame(expand.grid(1:length(author_list), 1:length(author_list)))
  x = pmap_dbl(combs, function (Var1, Var2) {
    if (Var1 <= Var2) { return (0) }
    find_collab(author_list[Var1], author_list[Var2])
  })
  x = matrix(x, nrow = length(author_list), byrow = T)
  
  fresh_authors = sum(rowSums(x) + colSums(x) == 0)
  
  return (fresh_authors / length(author_list))
}

