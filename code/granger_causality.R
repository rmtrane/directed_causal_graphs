library(furrr)
library(glmnetUtils)

granger_causality <- function(input_data, gene_cols, ID_col = NULL){

  ## Create RHS of lm formula
  rhs <- glue::glue_collapse(glue::glue("lag({gene_cols})"), sep = "+")

  ## Create tibble with one row for each potential node, and two columns:
  ## 1) child_node: indicate the child node we consider
  ## 2) lms: linear models where child_node is outcome, and other potential nodes lagged are explanatory
  out <- tibble(child_node = gene_cols) %>%
    mutate(lms = future_map(child_node, function(x) {

      # X <- input_data[-1, ] %>% select(-!!x, -time)
      # Y <- input_data[-nrow(input_data), x]
      # cvg <- cv.glmnet(as.formula(glue::glue("{x} ~ {rhs} - 1 + {ID_col}")),
      #                  alpha = 0,
      #                  data = cbind(X, Y))
      #
      # glmnet_fit <- glmnet(as.formula(glue::glue("{x} ~ {rhs} - 1 + {ID_col}")),
      #                      data = cbind(X,Y),
      #                      alpha = 0,
      #                      lambda = cvg$lambda.min)

      lm(data = input_data,
         as.formula(glue::glue("{x} ~ {rhs} - 1 + {ID_col}"))) %>%
      #glmnet_fit %>%
        broom::tidy()
    }))

  return(out)
}
