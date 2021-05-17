# Function modified from https://daranzolin.github.io/2018-01-07-probability-trees/
bayes_probability_tree <- function(prior, true_positive, true_negative, 
                                   edges_names = c(
                                     " ", " ", 
                                     " ", " ", 
                                     " ", " ",
                                     " ", " ", 
                                     " ", " "
                                     ),
                                   node_lab,
                                   digitos = 2
                                   ) {
  
  if (!all(c(prior, true_positive, true_negative) > 0) && !all(c(prior, true_positive, true_negative) < 1)) {
    stop("probabilities must be greater than 0 and less than 1.",
         call. = FALSE)
  }
  c_prior <- 1 - prior
  c_tp <- 1 - true_positive
  c_tn <- 1 - true_negative
  
  round4 <- purrr::partial(round, digits = digitos)
  
  b1 <- round4(prior * true_positive)
  b2 <- round4(prior * c_tp)
  b3 <- round4(c_prior * c_tn)
  b4 <- round4(c_prior * true_negative)
  
  bp <-  round4(b1/(b1 + b3))
  
  labs <- node_lab
  
  if (length(node_lab) == 1){
    if (node_lab == TRUE){
      labs <- c("X", prior, c_prior, true_positive, c_tp, true_negative, c_tn, b1, b2, b4, b3)
    } else if (node_lab == FALSE) {
      labs <- rep('', 11)
    }
  }
    
  
  
  tree <-
    create_graph() %>%
    add_n_nodes(
      n = 11,
      type = "path",
      label = labs,
      node_aes = node_aes(
        shape = "circle",
        height = 1,
        width = 1,
        x = c(0, 3, 3, 6, 6, 6, 6, 8, 8, 8, 8),
        y = c(0, 2, -2, 3, 1, -3, -1, 3, 1, -3, -1))) %>% 
    add_edge(
      from = 1,
      to = 2,
      edge_aes = edge_aes(
        label = edges_names[1]
      )
    ) %>% 
    add_edge(
      from = 1, 
      to = 3,
      edge_aes = edge_aes(
        label = edges_names[2]
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 4,
      edge_aes = edge_aes(
        label = edges_names[3]
      )
    ) %>% 
    add_edge(
      from = 2,
      to = 5,
      edge_aes = edge_aes(
        label = edges_names[4]
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 7,
      edge_aes = edge_aes(
        label = edges_names[5]
      )
    ) %>% 
    add_edge(
      from = 3,
      to = 6,
      edge_aes = edge_aes(
        label = edges_names[6]
      )
    ) %>% 
    add_edge(
      from = 4,
      to = 8,
      edge_aes = edge_aes(
        label = edges_names[7]
      )
    ) %>% 
    add_edge(
      from = 5,
      to = 9,
      edge_aes = edge_aes(
        label = edges_names[8]
      )
    ) %>% 
    add_edge(
      from = 7,
      to = 11,
      edge_aes = edge_aes(
        label = edges_names[9]
      )
    ) %>% 
    add_edge(
      from = 6,
      to = 10,
      edge_aes = edge_aes(
        label = edges_names[10]
      )
    ) 
  #message(glue::glue("The probability of having (prior) after testing positive is {bp}"))
  render_graph(tree)
  #invisible(tree)
}

