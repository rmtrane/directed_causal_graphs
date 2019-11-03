##
plot_network <- function(network, node1 = "node1", node2 = "node2", layout = 'nicely'){
  tmp <- paste0("dag{", paste(paste(network[[node1]], network[[node2]], sep = "->"), collapse = ";"), "}")

  dagitty::dagitty(tmp) %>%
    ggdag::ggdag()
}

## Extract network structure from simcausal::DAG
network_structure <- function(DAG){
  tmp <- tibble(nodes = DAG, nodenames = names(DAG)) %>%
    separate(nodenames, into = c("nodename", "time"), sep = '_') %>%
    filter(time == 1) %>%
    mutate(parents = map(paste(nodename, time, sep = "_"), function(x){
      tmp <- simcausal::parents(DAG, x)[[1]] %>% str_split(string = ., pattern = "_", simplify = TRUE)
      tmp[,1]
    })) %>%
    select(node1 = parents, node2 = nodename) %>%
    unnest_longer("node1")

  return(tmp)
}

# plot_network(network_structure(DAG), layout)
