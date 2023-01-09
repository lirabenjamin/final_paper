# Is this the best way to represent the data?
# Is there a better way to deal with missing data?
# How should I deal with nesting within schools?
# How could I add controls to Moran's I?
# Predict Time 4 from average of Time 1-3?
# Cross lagged panel model
# pdynmc
# Is SAR the same as Moran's I?
# Not equal for all  paper
# De Maris 2004
# Remove people who nominate themselves
# Are there differences in ys between people who nominate vs missing nominations


library(tidyverse)

clean <- read_rds("data/clean.l.rds")
attr <- read_rds("data/clean.rds")

from_ids <- clean %>%
  pull(StudyID) %>%
  unique()
to_ids <-
  clean %>%
  select(pn_bf1:pn_scw2) %>%
  pivot_longer(everything()) %>%
  filter(!is.na(value)) %>%
  pull(value) %>%
  unique()

length(from_ids)
length(to_ids)
missing_ids <- to_ids[!to_ids %in% from_ids]
missing_attr <- attr[seq_along(missing_ids), ] %>%
  mutate_all(~NA) %>%
  mutate(StudyID = missing_ids)
attr <- rbind(attr, missing_attr)
colnames(clean)

edgelist <-
  clean %>%
  # filter(time == "T1", !is.na(scw_s)) %>%
  select(StudyID, time, pn_bf1, pn_bf2, pn_scw, pn_scw2)

edgelist <-
  edgelist %>%
  pivot_longer(pn_bf1:pn_scw2, names_to = "pn", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    pn = str_replace(pn, "pn_", ""),
    pn = tm::removeNumbers(pn)
  ) %>%
  select(from = StudyID, to = value, time, pn)

library(igraph)
g <- graph_from_data_frame(edgelist, directed = TRUE, vertices = attr)

filter_graph <- function(graph = g, type = "bf", time = "T1") {
  time_ids <- which(E(graph)$time == time)
  g_r <- subgraph.edges(g, time_ids, delete.vertices = FALSE)
  type_ids <- which(E(g_r)$pn == type)
  g_r <- subgraph.edges(g_r, type_ids, delete.vertices = FALSE)
  return(g_r)
}

g

write_rds(g, "data/graph.rds")

filter_graph(graph = g, type = "scw")

