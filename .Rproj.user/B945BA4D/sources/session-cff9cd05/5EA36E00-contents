# Test correlations between matrices

# Load libraries
library(igraph)
library(tidyverse)
library(sna)
filter_graph <- function(graph = g, type = "bf", time = "T1") {
  time_ids <- which(E(graph)$time == time)
  g_r <- subgraph.edges(g, time_ids, delete.vertices = FALSE)
  type_ids <- which(E(g_r)$pn == type)
  g_r <- subgraph.edges(g_r, type_ids, delete.vertices = FALSE)
  return(g_r)
}

# Load data
g <- read_rds("data/graph.rds")

as.matrix(get.adjacency(filter_graph(g, type = "scw", time = "T3"))) %>%
  as_tibble() %>%
  mutate(id = colnames(.)) %>%
  select(id, everything()) %>%
  pivot_longer(-id, names_to = "id2", values_to = "value") %>%
  ggplot(aes(x = id, y = id2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "white", mid = "grey", high = "black",
    na.value = "white"
  ) +
  theme_void() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(title = "BF Adjacency Matrix")
ggsave("example matrix.pdf", width = 10, height = 10)

correlate <- function(graph = g, time = "T1") {
  gcor(
    as.matrix(get.adjacency(filter_graph(g, type = "bf", time = time))),
    as.matrix(get.adjacency(filter_graph(g, type = "scw", time = time))),
    mode = "digraph"
  )
}

correlate(time = "T1")
correlate(time = "T2")
correlate(time = "T3")
correlate(time = "T4")

# Significance testing
correlate_sig <- function(graph = g, time = "T1", replications = 10000, seed = 36) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  stack <- array(dim = c(2, 1136, 1136))
  stack[1, , ] <- as.matrix(get.adjacency(filter_graph(g, type = "bf", time = time)))
  stack[2, , ] <- as.matrix(get.adjacency(filter_graph(g, type = "scw", time = time)))
  cortest <- qaptest(stack, gcor, g1 = 1, g2 = 2, reps = replications) # 10K replications
  return(cortest)
}

cortest <-
  enframe(c("T1", "T2", "T3", "T4")) %>%
  mutate(
    cortest = map(
      value,
      ~ correlate_sig(graph = g, time = .x, replications = 10, seed = 36)
    ),
    testval = map_dbl(cortest, ~ .$testval),
    dist = map(cortest, ~ .$dist)
  )

# Plot distributions
cortest %>% 
  mutate(name = glue::glue("{value} correlation = {Ben::numformat(testval, 2)}, p < .001")) |> 
  unnest(dist) %>%
  ggplot(aes(dist))+
  geom_histogram(bins = 100) +
  geom_vline(aes(xintercept= testval), col = "red") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Distribution of correlation values")
ggsave("figures/correlation distributions.png", width = 6, height = 4)


cortest |> 
  ggplot(aes(value, testval))+
  geom_point()+
  geom_line(group = 1)+
  geom_text(aes(label = Ben::numformat(testval)),vjust= -.8)+
  labs(x = "Time", y = "Correlation between friendship and exemplar networks",
       caption = "All correlations are significant beyond the .001 level")
ggsave("figures/correlation_line.png", width = 6, height = 4)

# Percentile test
cortest %>%
  mutate(p1 = map_dbl(dist, ~ quantile(., 0.025)),
         p2 = map_dbl(dist, ~ quantile(., 0.975)),
         sig = testval < p1 | testval > p2)
