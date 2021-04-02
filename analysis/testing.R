df = tibble(
 TargetAuthor = c("a;b","c;d","a;b","a;b;g;h","a;f","a"),
 State = c("AM;RJ", "PA;GO","AM;RJ","AM;RJ;SP;MS","AM;MT","AM"),
 Region = c("N;SE", "N;CO","N;SE","N;SE;SE;CO","N;CO","N"),
 Class = c("Pre","Pre","Post","Post","Post","Pre")
)

nets = build_period_nets(df)

GEO_DATA = read_excel("/home/kleber/Dropbox/Scientific Research/Projects/Zika Response/Resposta Epidemia Zika/Code/data/institutions/Institutions and States - manual curation.xlsx", sheet = 3)
states_list = GEO_DATA %>% pull(State)
regions_list = GEO_DATA %>% pull(Region) %>% unique()

plot_collab_matrix(nets$pre$edgelist, "REGION", include_loops = T, flevels = regions_list)
plot_collab_matrix(nets$post$edgelist, "REGION", include_loops = T, flevels = regions_list)





prenets = build_networks(df, c("a---b"))
postnets = build_postnets(df)

overlap = overlap_list(prenets$edgelist %>% filter(Type == "PI"),
                       postnets$edgelist %>% filter(Type == "PI"))

postnets2 = subtract_overlap(prenet$edgelist, overlap, subtract_all = T)

plot_collab_matrix(prenet$edgelist, include_loops = F)
plot_collab_matrix(postnet$edgelist, include_loops = F)
plot_collab_matrix(postnet_sT$edgelist, include_loops = F)

prenet = build_network(df %>% filter(Class == "Pre") %>% pull(Region))
postnet = build_network2(df %>% filter(Class == "Post") %>% pull(Region),
                            df %>% filter(Class == "Post") %>% pull(TargetAuthor),
                            excluding = overlap)

plot_collab_matrix(prenet$edgelist, include_loops = F)
plot_collab_matrix(postnet$edgelist, include_loops = F)
