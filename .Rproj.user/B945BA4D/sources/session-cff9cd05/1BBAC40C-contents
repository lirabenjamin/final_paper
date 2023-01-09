library(glue)
library(tidyverse)
library(spatialreg)
library(spdep)
library(igraph)
library(classInt)
library(RColorBrewer)
library(ggrepel)

# read data
g <- read_rds("data/graph.rds")
clean <- read_rds("data/clean.rds")

# Set default ggplot theme
library(egg)
theme_set(theme_article() +
  theme(
    panel.border = element_blank(),
    axis.line = element_line(linewidth = .25)
  ))

filter_graph <- function(graph = g, type = "bf", time = "T1") {
  time_ids <- which(E(graph)$time == time)
  g_r <- subgraph.edges(g, time_ids, delete.vertices = FALSE)
  type_ids <- which(E(g_r)$pn == type)
  g_r <- subgraph.edges(g_r, type_ids, delete.vertices = FALSE)
  return(g_r)
}

run_moran_i <- function(graph = g, time = "T1", type = "bf",
                        outcome = "scw_s_T1", plot.width = 6, plot.height = 6) {
  # Required packages
  require(tidyverse)

  # Filter graph
  test_g <- g %>%
    # Keep only the relevant time and type
    filter_graph(type = type, time = time) %>%
    # delete vertices with no data
    delete_vertices(which(is.na(eval(parse(text = glue("V(.)${outcome}"))))))

  y <- eval(parse(text = glue("V(test_g)${outcome}")))

  # Create adjacency matrix
  adjacency <-
    test_g %>%
    as_adjacency_matrix() %>%
    as.matrix()

  # Transforming the matrices to spatial form
  # (row normalized) as shown in equation (21)
  adjacency <- adjacency / rowSums(adjacency)

  # Replacing potential NAN to zeros as shown in equation (22)
  adjacency[is.na(adjacency)] <- 0

  # Creating the listw object
  list_weights <- mat2listw(adjacency)

  # Testing influence
  mt <- moran.test(y,
    list_weights,
    zero.policy = TRUE,
    na.action = na.omit
  )

  # Plotting
  mp <- moran.plot(
    y, list_weights,
    zero.policy = TRUE, na.action = na.omit,
    labels = V(test_g)$StudyID
  )

  mp %>%
    tibble::as_tibble() %>%
    ggplot2::ggplot(ggplot2::aes(x, wx)) +
    ggplot2::geom_jitter(width = .05, height = .05, alpha = .3) +
    ggplot2::geom_smooth(method = "lm", se = TRUE) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = mean(wx)),
      linetype = "dashed"
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(x)),
      linetype = "dashed"
    ) +
    geom_text_repel(
      data = mp %>%
        dplyr::slice_max(cook.d, n = 50),
      aes(x, wx, label = labels),
      size = 3,
      color = "red",
      segment.color = "red",
      segment.size = 0.5,
      nudge_x = 0.1,
      nudge_y = 0.1
    ) +
    ggplot2::labs(x = "Self-Value", y = "Spatial Lag of Self-Value")
  ggplot2::ggsave(
    glue("figures/moran_i_{outcome}_{time}_{type}.png"),
    width = plot.width, height = plot.height
  )
  m <- (list(mt = mt, mp = mp))
  glance <- m$mt$estimate %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    pivot_wider(names_from = 1, values_from = 2) %>%
    mutate(statistic = m$mt$statistic, p.value = m$mt$p.value)
  return(tibble(glance = list(glance), tidy = list(mp %>% as_tibble())))
}

moran_i_results <-
  expand_grid(
    time = c("T1", "T2", "T3", "T4"),
    type = c("bf", "scw"),
    outcome = c("scw_s", "coregpa", "avg_scw_tr")
  ) %>%
  filter(!(time == "T2" & outcome == "avg_scw_tr")) %>%
  mutate(outcome = paste(outcome, time, sep = "_")) %>%
  mutate(m = pmap(list(time, type, outcome), function(x, y, z) {
    run_moran_i(time = x, type = y, outcome = z)
  }))

moran_i_results %>%
  unnest(m) %>%
  unnest(glance) %>%
  mutate(
    outcome = str_replace(outcome, "_T1", ""),
    outcome = str_replace(outcome, "_T2", ""),
    outcome = str_replace(outcome, "_T3", ""),
    outcome = str_replace(outcome, "_T4", "")
  ) %>%
  ggplot(aes(x = time, y = `Moran I statistic`, color = type)) +
  geom_point() +
  geom_line(aes(group = paste(type, outcome))) +
  facet_grid(~outcome) +
  scale_color_brewer(palette = "Set1")
ggsave("figures/moran_i.png", width = 10, height = 4)