library(simcausal)
library(tidyverse)
library(glue)

network <- read_delim(file = "net3_goldstandard.tsv",
                      col_names = F, delim = "\t") %>%
  mutate(coefs = runif(n = nrow(.), min = -1, max = 1),
         coefs_to_use = if_else(coefs > 0,
                                paste0("+", coefs),
                                as.character(coefs)))

t_max <- 2

D <- DAG.empty()

for (i in unique(c(network$X1, network$X2))){
  D <- D +
    node(i, t = 0, distr = "rnorm")

  if(!i %in% unique(network$X1)){
    D <- D +
      node(i, t=1:t_max, distr = "rnorm",
           params = list(mean = paste0(i, "[t-1]")))
  }
}


for_edges <- network %>%
  select(-X3, -coefs) %>%
  nest(data = c(X2, coefs_to_use))

D1 <- D

for (i in 1:nrow(for_loop)){
  cur_edges <- for_edges[i,] %>%
    unnest(cols = data)

  cur_node <- for_edges[i,]$X1

  to_edges <- paste(glue("{cur_edges$coefs_to_use}*{cur_edges$X2}[t-1]"), collapse = "")

  m <- paste0(cur_node, "[t-1]", if_else(str_starts(to_edges, pattern = "\\-"), '', '+'), to_edges)

  D1 <- D1 +
    node(cur_node, t = 1:t_max, distr = "rnorm",
         params = list(mean = m))
}


DAG <- set.DAG(D1)

tmp <- sim(DAG, n = 2)

D <- DAG.empty() +
  node("G189", t = 0, distr = "rnorm") +
  node("G4047", t = 0, distr = "rnorm") +
  node("G4134", t = 0, distr = "rnorm") +
  node("G189", t = 1:5, distr = "rnorm",
       params = list(mean = "G189[t-1]"))

  node("B", t = 0, distr = "rnorm")

D <- D +
  node("A", t = 1:5, distr = "rnorm",
       mean = B[t-1]) +
  node("B", t = 1:5, distr = "rnorm")

set.DAG(D)

D <- D +
  node("TF1", t = 1:t_max, distr = "rbern",
       prob = ifelse(TF1[t-1] == 0, 0.3, 0.7))

for (i in 1:5){
  D <- D +
    node(paste0("N",i), t = 1:t_max, distr = "rnorm",
         mean = runif(n = 1,
                      min = TF1[t-1] - 1,
                      max = TF1[t-1]),
         sd = 1)
}


DAG <- set.DAG(D, vecfun = 'runif')

D <- D +
  node("L2", t = 0, distr = "rbern",
       prob = 0.05) +
  node("L1", t = 0, distr = "rbern",
       prob = ifelse(L2[0] == 1, 0.5, 0.1)) +
  node("A1", t = 0, distr = "rbern",
       prob = ifelse(L1[0] == 1 & L2[0] == 0, 0.5,
                     ifelse(L1[0] == 0 & L2[0] == 0, 0.1,
                            ifelse(L1[0] == 1 & L2[0] == 1, 0.9, 0.5)))) +
  node("A2", t = 0, distr = "rbern",
       prob = 0, EFU = TRUE)

t.end <- 16
D <- D +
  node("Y", t = 1:t.end, distr = "rbern",
       prob =
         plogis(-6.5 + L1[0] + 4 * L2[t-1] + 0.05 * sum(I(L2[0:(t-1)] == rep(0, t)))),
       EFU = TRUE) +
  node("L2", t = 1:t.end, distr = "rbern",
       prob =
         ifelse(A1[t-1] == 1, 0.1,
                ifelse(L2[t-1] == 1, 0.9, min(1, 0.1 + t / 16)))) +
  node("A1", t = 1:t.end, distr = "rbern",
       prob = ifelse(A1[t-1] == 1, 1,
                     ifelse(L1[0] == 1 & L2[t] == 0, 0.3,
                            ifelse(L1[0] == 0 & L2[t] == 0, 0.1,
                                   ifelse(L1[0] == 1 & L2[t] == 1, 0.7, 0.5))))) +
  node("A2", t = 1:t.end, distr = "rbern",
       prob = if_else(t == 16, 1, 0),
       EFU = TRUE)
lDAG <- set.DAG(D)

plotDAG(lDAG, xjitter = 0.3, yjitter = 0.01)


D1 <- DAG.empty()

D1 <- D1 +
  node("A", t = 0, distr = "rbern",
       prob = 0.2) +
  node("A", t = 1:10, distr = "rbern",
       prob = A[t-1]*0.2 + (1-A[t-1])*0.6) +
  node("B", t = 0, distr = "rcat.b1",
       probs = c(0.2, 0.7, 0.1)) +
  node("B", t = 1:10, distr = "rcat.b1",
       probs = c(A[t-1]*0.2 + (1-A[t-1])*0.33,
                 A[t-1]*0.7 + (1-A[t-1])*0.33,
                 A[t-1]*0.1 + (1-A[t-1])*0.34))

DAG1 <- set.DAG(D1)

simulated_data <- sim(DAG1, n = 1000, wide = FALSE)

library(bnlearn)

for_hc <- simulated_data %>% mutate_all(as.factor) %>% select(-ID, -t)

net <- hc(for_hc)

graphviz.plot(net)
