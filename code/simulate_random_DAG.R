## Create random network by specifying number of nodes and either sparsity (value between 0 and 1 that
## determines the cutoff for when an edge should be included) or number of edges.
random_network <- function(n_nodes = 6, sparsity = NULL, n_edges = NULL){
  n_edges <- n_edges + n_nodes

  if(is.null(sparsity) == is.null(n_edges)){
    stop("Exactly one of sparsity and n_edges must be specified.")
  }

  tmp <- matrix(runif(n = n_nodes^2, 0, 1),nrow = n_nodes, ncol = n_nodes,
                dimnames = list(NULL, paste0("X", 1:n_nodes)))

  diag(tmp) <- 1

  tmp <- tmp %>%
    as_tibble() %>%
    mutate(node1 = paste0("X", 1:n_nodes)) %>%
    gather(key = 'node2', value = 'value', -node1)

  if(!is.null(sparsity)){
    out <- tmp %>%
      filter(abs(value) > 1-sparsity) %>%
      arrange(node1, node2) %>% select(-value)
  }

  if(!is.null(n_edges)){
    out <- tmp %>%
      arrange(abs(value)) %>%
      top_n(n_edges) %>%
      arrange(node1, node2) %>% select(-value)
  }

  return(out)
}


## Create Dense Network
network <- random_network(n_nodes = 10, n_edges = 10)
write_tsv(x = network, path = "data/from_dense/network.tsv")

## Create Sparse Network
network <- random_network(n_nodes = 100, n_edges = 100)
write_tsv(x = network, path = "data/from_sparse/network.tsv")
