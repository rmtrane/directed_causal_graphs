source(paste0(here::here(), "/code/misc.R"))

library(tidyverse)

DAG <- read_rds("dag.Rds")

DAG_plot <- plot_network(network_structure(DAG)) + theme_void()

ggsave(DAG_plot, filename = "DAG_figure.png")
