add_parameters <- function(fit, newdata = NULL) {
  # New parameters
  labels <- fit$new_parameters$labels
  values <- fit$new_parameters$values
  
  parameters <- as.list(values)
  names(parameters) <- labels
  
  estimates <- as.list(fit@output$estimate)
  
  all_parameters <- c(parameters, estimates)[!duplicated(c(names(parameters), names(estimates)))]
  
  # Algebras
  algebras <- list()
  for (i in names(fit@algebras)) {
    algebras[[i]] <- fit@algebras[[i]]$formula
  }
  
  # Get or set data
  data <- fit@data$observed
  if (!is.null(newdata)) {
    data <- newdata
  }
  
  names(data) <- paste0("data.", names(data))
  
  # Extract moderator data
  values <- as.list(data)
  
  # Bind together
  parameters_values <- c(all_parameters, values)
  
  # To remove problems with pure constraints
  # TODO: remove error
  for (i in names(algebras)) {
    data[[i]] <- eval(algebras[[i]], parameters_values)
  }
  
  names(data) <- gsub(pattern = "data\\.", replacement = "", x = names(data))
  
  return(data)
}

add_boot <- function(fit, newdata = NULL) {
  # New parameters
  labels <- fit$new_parameters$labels
  values <- fit$new_parameters$values
  
  parameters <-
    fit$compute$output$raw %>%
    as.list() %>%
    list_transpose()
  
  # Algebras
  algebras <- list()
  for (i in names(fit@algebras)) {
    algebras[[i]] <- fit@algebras[[i]]$formula
  }
  
  # Get or set data
  data <- fit@data$observed
  if (!is.null(newdata)) {
    data <- newdata
  }
  
  names(data) <- paste0("data.", names(data))
  values <- as.list(data)
  
  boot_list <- vector(mode = "list", length = length(parameters))
  
  # Extract moderator data
  
  for (p in seq_along(parameters)) {
    # Bind together
    parameters_values <- c(parameters[[p]], values)
    
    boot_list[[p]] <- data
    boot_list[[p]]$boot_index <- p
    
    # To remove problems with pure constraints
    # TODO: remove error
    for (i in names(algebras)) {
      boot_list[[p]][[i]] <- eval(algebras[[i]], parameters_values)
    }
  }
  
  
  output <- do.call(rbind, boot_list)
  names(output) <- gsub(pattern = "data\\.", replacement = "", x = names(output))
  
  return(output)
}
