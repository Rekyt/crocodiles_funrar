ko = readRDS("~/Downloads/dataPrepared/PCAImputedALL.rds")

all_dis = lapply(names(ko), function(x) {
  
  cat(x, "\n")
  
  y = ko[[x]]
  
  dim_table = y$PCA$scores[, 1:2]
  
  di_table = funrar::distinctiveness_global(
    as.matrix(dist(dim_table))
  )
  
  di_table$IUCN_status = y$IUCN
  
  return(di_table)
})

names(all_dis) = names(ko)

all_di = dplyr::bind_rows(all_dis, .id = "taxonomic_group")

set.seed(20230602)

all_di$extinction_risk = lapply(
  all_di$IUCN_status, predict_extinction_risk_iucn_status
) %>%
  unlist()

full_plot = all_di %>%
  ggplot(aes(global_di, extinction_risk)) +
  geom_point(shape = ".") +
  geom_smooth(method = "lm") +
  facet_wrap(vars(taxonomic_group)) +
  labs(x = "Functional Distinctiveness", y = "Extinction Risk") +
  theme_bw() +
  theme(aspect.ratio = 1)

trend_line_only = all_di %>%
  ggplot(aes(global_di, extinction_risk, color = taxonomic_group)) +
  geom_smooth(se = FALSE) +
  labs(x = "Functional Distinctiveness", y = "Extinction Risk") +
  theme_bw() +
  theme(aspect.ratio = 1)

trend_line_only

trend_line_only +
  facet_wrap(vars(taxonomic_group)) +
  geom_point(shape = ".", color = "black")

saveRDS(all_di, "data/all_di.Rds")
