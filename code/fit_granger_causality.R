library(tidyverse)

source(paste0(here::here(), "/code/granger_causality.R"))

input_folder <- ifelse(interactive(), "data/from_sparse", commandArgs(TRUE)[1])
network_type <- ifelse(interactive(), "from_sparse", str_split(input_folder, "/", simplify = T)[,2])

output_folder <- paste(here::here(), "results",
                       ifelse(interactive(), "results/from_sparse", network_type), sep = "/")

input_datafile <- paste(here::here(), input_folder, "sim_data_normalized.csv", sep = "/")


#input_networkfile <- paste(input_folder, "sparse_network.tsv", sep = "/")

if(!interactive())
  plan(multiprocess)

sim_data <- read_csv(file = input_datafile)

lm_fits <- granger_causality(sim_data, gene_cols = colnames(sim_data[,-c(1,2)]), ID_col = "ID") %>%
  unnest()

network <- lm_fits %>%
  filter(p.value < 0.05,
         !str_detect(term, "ID")) %>%
  select(node2 = child_node, node1 = term, estimate) %>%
  mutate(node1 = str_remove_all(node1, pattern = "lag\\(|\\)"),
         edges = paste(node1, node2, sep = "-"))

if(!dir.exists(output_folder))
  dir.create(output_folder, recursive = TRUE)

write_tsv(network, path = paste0(output_folder, "/granger_causal_network.tsv"))

