library(furrr)

granger_causality <- function(input_data, gene_cols, ID_col = NULL){

  ## Create RHS of lm formula
  rhs <- glue::glue_collapse(glue::glue("lag({gene_cols})"), sep = "+")

  ## Create tibble with one row for each potential node, and two columns:
  ## 1) child_node: indicate the child node we consider
  ## 2) lms: linear models where child_node is outcome, and other potential nodes lagged are explanatory
  out <- tibble(child_node = gene_cols) %>%
    mutate(lms = future_map(child_node, function(x) {
      lm(data = input_data,
         glue::glue("{x} ~ {rhs} - 1 + {ID_col}")) %>%
        broom::tidy()
    }))

  return(out)
}
