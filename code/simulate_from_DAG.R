if(!"/Users/ralphtrane/RPackages" %in% .libPaths())
  .libPaths("/workspace/rtrane/Rpackages")

library(simcausal)
library(tidyverse)
library(parallel)

## If run using R CMD BATCH, use '--args n_cores=x input_file=y n_samples=z t_max=T' to
## specify parameters
if(!interactive()){
  arguments <- R.utils::commandArgs(trailingOnly = TRUE, asValues = TRUE)
  n_cores <- arguments[["n_cores"]]
  n_samples <- arguments[["n_samples"]]
  t_max <- arguments[["t_max"]]
  input_file <- arguments[["input_file"]]
} else {
  n_cores <- 1
  input_file <- "dense_network.tsv"
  n_samples <- 100
  t_max <- 40
}

## Read network, and get uniform coefficient
network <- read_tsv(file = input_file,
                    col_names = T) %>%
  mutate(coefs = if_else(node1 == node2, 2*rbern(1, prob = 0.5)-1,
                         runif(n = nrow(.), min = -1, max = 1)),
         coefs_to_use = if_else(coefs > 0,
                                paste0("+", coefs),
                                as.character(coefs)))

## Nest by "to nodes". This is used in the for loop later
for_edges <- network %>%
  select(node1, node2, coefs_to_use) %>%
  group_by(node2) %>%
  nest()

## Set standard deviation for rnorms
SD <- 1

## Initiate DAG
D0 <- DAG.empty()

## Get all initial nodes, and all nodes that do not show up as end of any edges
initial_nodes <- mclapply(unique(c(network$node1, network$node2)), FUN = function(i){

  ## Nodes at time t = 0
  nodes <- node(i, t = 0, distr = "rnorm", sd = SD)

  ## If node does not have any parents (i.e. if it is not in the column "node 2"),
  ## we need to add all future nodes right away, since we won't encounter it later on,
  ## otherwise.
  if(!i %in% unique(network$node2)){
    nodes <- nodes +
      node(i, t=1:t_max, distr = "rnorm", sd = SD,
           params = list(mean = paste0(i, "[t-1]")))
  }

  return(nodes)
}, mc.cores = n_cores)

## Add all nodes to DAG
D_from_lists <- add.nodes(D0, flatten(initial_nodes))


## Get all future nodes that correspond to children in the network.
## We do so one child at a time
nodes_from_edges <- mclapply(1:nrow(for_edges), FUN = function(i){
  ## Expand to get all edges with node as child.
  cur_edges <- for_edges[i,] %>%
    unnest(cols = data)

  ## Get the child node
  to_node <- for_edges[i,]$node2

  ## Create the mean formula
  m <- paste(paste0(cur_edges$coefs_to_use, "*", cur_edges$node1, "[t-1]"), collapse = "")

  ## Create node
  output <- node(to_node, t = 1:t_max, distr = "rnorm",
                 params = list(mean = m, sd = SD))

  return(output)
}, mc.cores = n_cores)

## Add nodes to DAG
D_from_lists <- add.nodes(D_from_lists, flatten(nodes_from_edges))

## Set DAG and simulate data
DAG <- set.DAG(D_from_lists)

n_per_group <- ceiling(n_samples/n_cores)
n_per_group <- rep(1:n_cores, each = n_per_group)[c(1:n_samples)] %>% table() %>% c()

sim_data_list <- mclapply(X = n_per_group,
                          FUN = function(x){
                            sim(DAG, n = x, rndseed = 101010, wide = FALSE)
                          })

sim_data <- map2(sim_data_list, 1:n_cores, function(x,y){
  x %>% mutate(ID = ID + (y-1)*n_per_group[1])
}) %>%
  bind_rows()

#sim_data <- sim(DAG, n = n_samples, rndseed = 101010, wide = F)

## Save DAG as figure
source(paste0(here::here(), "/code/misc.R"))

DAG_plot <- plot_network(network_structure(DAG)) + theme_void()
ggsave(DAG_plot, filename = "DAG_figure.png")


## Write data to .csv file.
write_csv(sim_data, paste("sim_data.csv", sep = "/"))

## Write DAG to file
write_rds(DAG, paste("dag.Rds", sep = "/"))