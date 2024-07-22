 myStepBeta <- function (object, k = 2, dispersion = T) 
{
  if (requireNamespace("betareg")) {
    if (!inherits(object, "betareg")) {
      step(object, direction = "both")
    }
    else {
      full_model <- object
      beta_control_link_mean <- full_model$link$mean$name
      beta_control_link_phi <- full_model$link$precision$name
      beta_control_type <- full_model$type
      beta_control_offset_mean <- full_model$offset$mean
      beta_control_weights <- full_model$weights
      formula_full_model <- check_formula_terms(full_model)
      if (!identical(grep("\\|", as.character(formula_full_model[3])), 
                     integer(0))) {
        full_model_VertBar <- paste("|", gsub(".*\\|", 
                                              "", as.character(formula_full_model[3])))
      } else {
        full_model_VertBar <- ""
      }
      if (full_model_VertBar == "") {
        dispersion_model <- NULL
      } else {
        dispersion_model <- 1
      }
      if (dispersion == T & !is.null(dispersion_model)) {
        dispersion_terms <- dispersion_formula_terms(object)
      } else if (dispersion == F & !is.null(dispersion_model)) {
        dispersion_terms <- full_model_VertBar
      } else {
        dispersion_terms <- ""
      }
      full_AIC <- abs(AIC(full_model, k = k))
      Final_Results <- as.data.frame(matrix(ncol = 2))
      colnames(Final_Results) <- c("Model", "AIC")
      g <- 1
      while (g < length(dispersion_terms) + 1) {
        print(paste(round(g/length(dispersion_terms) * 
                            100, 2), "% of the process"))
        formula_NoInt <- remove_formula_interactions(formula_full_model)
        starting_NoInt_AIC <- 0
        Terms <- attr(object$terms$mean, "term.labels")
        is.Terms_NoInt <- Terms[-c(grep(":", Terms))]
        if (length(is.Terms_NoInt) > 0) {
          Terms_NoInt <- is.Terms_NoInt
        }
        else {
          Terms_NoInt <- Terms
        }
        Results <- as.data.frame(matrix(ncol = 2))
        names(Results) <- c("Model", "AIC")
        models <- list()
        starting_variables <- NULL
        diff <- 1
        while (diff > 5e-15) {
          i <- 1
          while (i < length(Terms_NoInt) + 1) {
            new_formula <- keep_formula_terms(formula_NoInt, 
                                              c(unique(Terms_NoInt[c(starting_variables, 
                                                                     i)])))
            new_formula <- as.formula(paste(new_formula[2], 
                                            new_formula[1], new_formula[3], dispersion_terms[g]))
            mod_updated <- try(betareg(new_formula, data = object$model, 
                                       weights = beta_control_weights, link = beta_control_link_mean, 
                                       link.phi = beta_control_link_phi, type = beta_control_type, 
                                       if (!is.null(beta_control_offset_mean)) {
                                         offset = c(beta_control_offset_mean)
                                       }), silent = T)
            if (isTRUE(class(mod_updated) == "try-error")) {
              models[[i]] <- NA
              i <- i + 1
              next
            }
            if (i == 2 & sum(unlist(lapply(models, is.na)) == 
                             2)) {
              g <- g + 1
              next
            }
            models[[i]] <- mod_updated
            Results[i, 1] <- as.character(new_formula)[3]
            Results[i, 2] <- abs(AIC(mod_updated, k = k))
            i <- i + 1
          }
          Var_reduced <- which.min(Results[, 2])
          if (identical(Var_reduced, integer(0))) {
            g <- g + 1
            next
          }
          starting_variables <- c(starting_variables, 
                                  Var_reduced)
          AIC_reduced <- Results[Var_reduced, 2]
          diff <- abs(starting_NoInt_AIC - AIC_reduced)
          starting_NoInt_AIC <- AIC_reduced
        }
        mod_reduced <- models[[which.min(Results[, 2])]]
        new_formula <- formula(mod_reduced)
        starting_IntAic <- AIC_reduced
        Terms_Int <- attr(full_model$terms$full, "term.labels")
        Terms_Int <- Terms_Int[-c(1:length(Terms_NoInt))]
        if (identical(Terms_Int, character(0))) {
          final_model <- mod_reduced
        }
        else if (!identical(Terms_Int, character(0))) {
          Results <- as.data.frame(matrix(ncol = 2))
          names(Results) <- c("Model", "AIC")
          models <- list()
          starting_variables <- NULL
          diff <- 1
          while (diff > 5e-15) {
            i <- 1
            while (i < length(Terms_Int) + 1) {
              new_formula <- keep_formula_terms(formula_full_model, 
                                                c(unique(Terms_Int[c(starting_variables, 
                                                                     i)])))
              new_formula <- as.formula(paste(new_formula[2], 
                                              new_formula[1], gsub("\\|.*", "", as.character(formula(mod_reduced)[3])), 
                                              "+", new_formula[3], dispersion_terms[g]))
              mod_updated <- try(betareg(new_formula, 
                                         data = object$model, weights = beta_control_weights, 
                                         link = beta_control_link_mean, link.phi = beta_control_link_phi, 
                                         type = beta_control_type, if (!is.null(beta_control_offset_mean)) {
                                           offset = c(beta_control_offset_mean)
                                         }), silent = T)
              if (isTRUE(class(mod_updated) == "try-error")) {
                i <- i + 1
                models[[i]] <- NA
                next
              }
              models[[i]] <- mod_updated
              Results[i, 1] <- as.character(new_formula)[3]
              Results[i, 2] <- abs(AIC(mod_updated, k = k))
              i <- i + 1
            }
            Results[i, 1] <- as.character(formula(mod_reduced)[3])
            Results[i, 2] <- AIC_reduced
            models[[i]] <- mod_reduced
            Var_reduced <- which.min(Results[, 2])
            if (identical(Var_reduced, integer(0))) {
              g <- g + 1
              next
            }
            starting_variables <- c(starting_variables, 
                                    Var_reduced)
            AIC_reduced <- Results[Var_reduced, 2]
            diff <- abs(starting_NoInt_AIC - AIC_reduced)
            starting_NoInt_AIC <- AIC_reduced
          }
        }
        Final_Results <- rbind(Final_Results, Results)
        g <- g + 1
      }
    }
  }
  final_formula <- (Final_Results$Model[which.min(Final_Results$AIC)])
  final_formula <- as.formula(paste(new_formula[2], new_formula[1], 
                                    final_formula))
  final_model <- betareg(final_formula, data = object$model, 
                         weights = beta_control_weights, link = beta_control_link_mean, 
                         link.phi = beta_control_link_phi, type = beta_control_type, 
                         if (!is.null(beta_control_offset_mean)) {
                           offset = c(beta_control_offset_mean)
                         })
  final_model$call <- paste("betareg(formula = ", formula(final_model)[2], 
                            formula(final_model)[1], formula(final_model)[3], "data =", 
                            object$call$data, ")")
  return(list("model" = final_model, 
              "modelList" = Final_Results))
 }
 