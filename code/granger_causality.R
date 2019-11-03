library(furrr)
## Uncomment to run in parallel
#plan(multiprocess)

granger_causality <- function(input_data, non_gene_cols){
  ## Get data with only potential nodes -- most commonly, we remove t and ID
  potential_nodes <- input_data %>% select(-c({non_gene_cols}))

  ## Create RHS of lm formula
  rhs <- glue::glue_collapse(glue::glue("lag({colnames(potential_nodes)})"), sep = "+")

  ## Create tibble with one row for each potential node, and two columns:
  ## 1) child_node: indicate the child node we consider
  ## 2) lms: linear models where child_node is outcome, and other potential nodes lagged are explanatory
  out <- tibble(child_node = colnames(potential_nodes)) %>%
    mutate(lms = future_map(child_node, function(x) {
      lm(data = input_data,
         glue::glue("{x} ~ {rhs} - 1")) %>%
        broom::tidy()
    }))

  return(out)
}
