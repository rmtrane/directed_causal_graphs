arguments <- R.utils::commandArgs(trailingOnly = TRUE, asValues = TRUE)

true_network_file <- arguments["true_network"]
other_networks <- arguments[which(names(arguments) == "")]

library(tidyverse)

true_network <- read_tsv(arguments[["true_network"]]) %>%
  mutate(edges = paste(node1, node2, sep = "-"))

results <- tibble(network_file = unlist(other_networks)) %>%
  mutate(network = map(network_file, ~read_tsv(.x)),
         Precision = map_dbl(network, ~length(intersect(.x$edges, true_network$edges))/nrow(.x)),
         Recall = map_dbl(network, ~length(intersect(.x$edges, true_network$edges))/nrow(true_network)),
         `F score` = ( 2*Precision*Recall ) / (Precision + Recall)) %>%
  select(-network)

write_tsv(results, path = "summarized_results.tsv")