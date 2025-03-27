## adapted from Martin Holdrege 
library(patchwork)
library(dtplyr)

transform_funs <- list()

add_constant_string <- function(x) {
  # adding only a small ammount to prcpPropSum so that wouldn't run into
  # problems if got a 0. But for annuals (and other variables) want all the log transformations
  # to be positive (and a less sharp bend) so adding 1 instead. 
  out <- ifelse('prcpPropSum' == x, paste0(x,  "+0.001"), paste0(x, "+1"))
  paste0('I(', out, ')')
}

transform_funs$convert_sqrt <- function(x) {
  p <- paste0("sqrt(", x, ")")
  list(main = p, # transformation for main term
       inter = p) # transformation for term if it is in an interaction
}

# transform_funs$convert_sq <- function(x) paste0("I(", x, "^2)")

# adding x^2 term to the model (in addition to x) i.e. to allow for parabola
#transform_funs$add_sq <- function(x) paste0(x, "+ I(", x, "^2)")

# adding small constant so not taking log of 0 (this is a problem
# for about 300k afg observations)
transform_funs$convert_log10 <- function(x) {
  x <- add_constant_string(x)
  p <- paste0("log10(", x, ")")
  list(main = p,
       inter = p) 
}

# transform_funs$convert_exp <- function(x) paste0("exp(", x, ")") # removing b/ selected

transform_funs$convert_poly2 <- function(x) {
  p <- paste0("stats::poly(", x, ",2, raw = TRUE)")
  list(main = p,
       inter = x) # not changing the interaction term
}

transform_funs$convert_poly2sqrt <- function(x) {
  p <- paste0("stats::poly(I(sqrt(", x, ")),2, raw = TRUE)")
  list(main = p,
       inter = paste0("sqrt(", x, ")")) 
}

transform_funs$convert_poly2log10 <- function(x) {
  x <- add_constant_string(x)
  p <- paste0("stats::poly(I(log10(", x, ")),2, raw = TRUE)")
  list(main = p,
       inter = paste0("log10(", x, ")")) 
}

#' flatten and rename list elements
#'
#' @param x a list where each list item is a sublist
#'
#' @return a list where each item are elements of the original sublist,
#' but with the names of the two levels of the original list pasted together
#' @examples
#' x = list(upper1 = list('a' = 1), upper2 = list('a' = 21, 'b' = 27))
#' flatten_rename(x)
flatten_rename <- function(x) {
  out <- purrr::flatten(x) # flatten list
  top_names <- names(x) # names of top level of list
  n_lower <- map_dbl(x, length)
  n_top <- length(x)
  # repeat the name of the highest list level, for each
  # component of the sublist
  rep_top_names <- top_names[rep.int(1:n_top, times = n_lower)] 
  lower_names <- names(out)
  stopifnot(length(lower_names) == length(rep_top_names))
  names(out) <- paste(rep_top_names, lower_names, sep = "_")
  out
}

#' @param preds vector of predictor variables
#'
#' @return vector where each element returned is a string, that can be used as 
#' the right hand side of a model formula. One additional element is transformed
#' @example 
#' preds <- c('afgAGB', 'prcpPropSum', 'afgAGB:prcpPropSum')
#' names(preds) <- preds
#' transform_preds(preds) # the interaction term is also transformed, depending on the function
#' transform_preds(c(a = 'a', b = 'b', `a:b` = '(a:b)'))
#' transform_preds(c(a = 'sqrt(a)', b = 'b', `a:b`= 'sqrt(a):b'))
transform_preds <- function(preds) {
  stopifnot(
    is.character(preds),
    !is.null(names(preds)), # needs to be a named vector
    is.list(transform_funs) # list of functions created above
  )
  out <- map(transform_funs, function(f) {
    stopifnot(
      is.function(f)
    )
    out <- list()
    # main effects (i.e. not interactions)
    preds_main <- str_subset(preds, pattern = "[:]", negate = TRUE)
    for (var in preds_main) {
      tmp_vars <- preds
      
      # transform main effects
      main_to_transform <- var == preds & str_detect(preds, "\\(|\\:", negate = TRUE)
      tmp_vars[main_to_transform] <- f(tmp_vars[main_to_transform])$main
      
      # transform interactions
      inter_to_transform <- str_detect(preds, paste0("(", var, ":)|(:", var, ")")) & # interaction term
        # not already transformed?
        # if it isn't allowed to be transformed then the variable will have a (
        # before it or a ')' after it
        str_detect(preds, paste0('(\\(',var, ")|(", var, "\\))"), negate = TRUE)
      
      tmp_vars[inter_to_transform] <- str_replace(tmp_vars[inter_to_transform],
                                                  var,
                                                  f(var)$inter)
      
      out[[var]] <- paste(tmp_vars, collapse = " + ") %>% 
        paste("~", .)
    }
    out
  }) 
  # if there are already transformed variables in preds
  # those will be repeated multiple times in the output (so removing)
  out <- flatten_rename(out)
  # by putting convert_none first, it is kept, no matter one
  # even if later elements are duplicates 
  out <- c(convert_none = paste(preds, collapse = " + ") %>% 
             paste("~", .),
           out)
  out <- out[!duplicated(out)]
  out
}

## function to turn predicted variables into deciles
predvars2deciles <- function(df, response_vars, pred_vars,
                             filter_var = FALSE,
                             filter_vars = NULL,
                             weighted_mean = FALSE,
                             add_mid = FALSE,
                             cut_points = seq(0, 1, 0.01)) {
  
  stopifnot(
    is.logical(filter_var)
  )
  
  if (filter_var) {
    stopifnot(is.character(filter_vars))
    df <- filter_by_climate(df, add_mid = add_mid,
                            filter_vars = filter_vars)
  }
  
  # longformat df
  long_df <- predvars2long(df, response_vars = response_vars, 
                           pred_vars = pred_vars,
                           filter_var = filter_var)
  # mean of deciles
  out <- longdf2deciles(long_df, response_vars = response_vars,
                        filter_var = filter_var,
                        weighted_mean = weighted_mean,
                        cut_points = cut_points)
  out
}

#' Make long format dataframe with predictor variable becoming a column
#'
#' @param df dataframe (could be output from filter_by_climate)
#' @param response_vars names of response variables
#' @param pred_vars names of predictor variables
#' @param filter_var logical--whether this dataframe also includes 
#' filter_var and percentile_category columns that should be kept
#'
#' @return longform dataframe
predvars2long <- function(df, response_vars, 
                          pred_vars = c("afgAGB", "pfgAGB", "MAT", "MAP", 
                                        "prcpPropSum"),
                          filter_var = FALSE) {
  
  #stopifnot(c('afgAGB', 'pfgAGB') %in% names(df))
  
  new_pred_vars <- c(pred_vars)
  # creating total herbacious biomass category
  # if(!'herbAGB'%in% pred_vars) {
  #   df$herbAGB <- df$afgAGB + df$pfgAGB
  #   new_pred_vars <- c(new_pred_vars, 'herbAGB')
  # }
  # 
  # new_pred_vars <- c(pred_vars, 'herbAGB')
  # 
  select_cols <- c(new_pred_vars, response_vars)
  
  if(filter_var) {
    select_cols <- c(select_cols, c("filter_var", "percentile_category"))
  }
  
  # for weighted means (if relevant)
  
  if('weight' %in% names(df)){
    select_cols <- c(select_cols, 'weight')
  } else if('numYrs' %in% names(df)){
    select_cols <- c(select_cols, 'numYrs')
  }
  
  
  out <- df[, select_cols] %>% 
    # unname used here b/ https://github.com/tidyverse/tidyr/issues/1481
    pivot_longer(cols = all_of(unname(new_pred_vars)))
  
  # turn into an ordered factor
  ordered <- c("swe_meanAnnAvg_5yr", "tmin_meanAnnAvg_5yr", "prcp_meanAnnTotal_5yr",
               "precip_Seasonality_meanAnnAvg_5yr", "PrecipTempCorr_meanAnnAvg_5yr")
  
  if(all(new_pred_vars %in% ordered) & all(ordered %in% new_pred_vars)) {
    out$name <- factor(out$name, levels = ordered)
    
    # otherwise just order the veg variables
  } else if(all(ordered[1:3] %in% new_pred_vars)) {
    levels <- c(ordered[1:3], new_pred_vars[!new_pred_vars %in% ordered[1:3]])
    out$name <- factor(out$name, levels = levels)
  }
  out
}

#' convert longform dataframe to means of predictor quantiles
#'
#' @param df with columns of name (name of predictor variable), 'value'
#' (value of predictor variable), and 1 or more response variable columns.
#' The output of predvars2long() creates a correctly formatted df to use here
#' @param response_vars character vector, names of the response variables
#' @param filter_var logical--whether this dataframe also includes 
#' filter_var and percentile_category columns that should be kept
#' @param weighted_mean logical, whether to take the weighted mean
#' of the observed fire probability (currently requires presence of
#' weight column).
#' @param return_mean logical. if false return the dataframe before
#' means have been calculated for each quantile
#' 
#' @return For each predictor variable calculate the mean of each decile
#' and the corresponding mean (of those same rows) of the response variable
longdf2deciles <- function(df, response_vars, filter_var = FALSE,
                           weighted_mean = FALSE,
                           return_means = TRUE,
                           cut_points = seq(0, 1, 0.01)) {
  
  # computed weighted mean based on the weight column
  # numYrs column also allowed for legacy reasons
  if(weighted_mean) {
    if('weight' %in% names(df)) {
      weight_var <- "weight"
      # commenting out for now--b/ easier to miss mistakes when numYrs allowed
      # } else if ('numYrs' %in% names(df)) {
      #   weight_var <- "numYrs"
    } else {
      stop('weight column not present (needed for weighted mean)')
    }
  }
  
  stopifnot(c("name", "value", response_vars) %in% names(df))
  
  group_vars <- 'name'
  if(filter_var) {
    group_vars <- c(group_vars, c("filter_var", "percentile_category"))
  } 
  
  if(!filter_var & 'filter_var' %in% names(df)) {
    warning('dataframe includes a column named filter_var, the
            filter_var argument should probably be set to TRUE')
  }
  
  out0 <- df %>% 
    # the named vector in select was selecting by the names
    # not the vector values!
    select(all_of(group_vars), value, unname(response_vars), 
           # using matches here b/ if column not present
           # this will still work
           matches('numYrs'), matches('weight')) %>% 
    group_by(across(all_of(group_vars))) %>% 
    #lazy_dt() %>% 
    nest() %>% 
    # empirical cdf
    # calculate the percentile of each data point based on the ecdf
    mutate(percentile = map(data, function(df) ecdf(df$value)(df$value))) %>% 
    # as_tibble() %>% 
    unnest(cols = c("data", "percentile")) %>% 
    group_by(across(all_of(group_vars))) %>% 
    mutate(decile = cut(percentile, cut_points,
                        labels = 1:(length(cut_points) - 1))) %>% 
    # calculate mean of response variables for each decile of each predictor
    # variable
    group_by(across(all_of(c(group_vars, 'decile'))), arrange = FALSE) %>% 
    lazy_dt() # this speeds up the code ~3x
  
  
  if(!return_means) {
    return(as_tibble(out0))
  }
  
  if(weighted_mean) {
    out <- out0 %>% 
      summarize(across(unname(response_vars), weighted.mean, w = .data[[weight_var]]),
                mean_value = weighted.mean(value, w = .data[[weight_var]]), 
                # mean of predictor for that decile
                .groups = 'drop')
    
  } else {
    out <- out0 %>% 
      summarize(across(unname(response_vars), ~mean(.x, na.rm = TRUE)),
                mean_value = mean(value, na.rm = TRUE), # mean of predictor for that decile
                .groups = 'drop')
  }
  
  as_tibble(out)
}


#' filter rows by climate variables
#' 
#' @description filters the dataframe by percentiles of the climate variable
#' columns. So the output includes rows corresponding the bottom 2 deciles and 
#' the top two deciles of each climate variable. Note this function has
#' been updated to filter by an arbitrary number of columns (not just climate)
#'
#' @param df dataframe that needs to have MAP, MAT, and prcpPropSum column
#' @param add_mid also add a seperate category of the center 2 deciles of 
#' each climate variable
#' @param filter_vars variables (usually climate variables), to split the others
#' into response and predicted
#'
#' @return dataframe with same columns as df but also filter_var,
#' percentile_category, which give the names of the climate variable filtered 
#' by and the percentile cut-off used that the given row fits in

filter_by_climate <- function(df, add_mid = FALSE,
                              filter_vars = c('MAT', 'MAP', 'prcpPropSum')) {
  
  # percentile cuttoffs to use, keeping values below low, and above high
  low <- 0.2
  high <- 0.8
  
  # # creating total herbacious biomass category
  # df$herbAGB <- df$afgAGB + df$pfgAGB
  # 
  names(filter_vars) <- filter_vars
  stopifnot(filter_vars %in% names(df))
  
  # fitting empirical cdf's so that percentiles of the climate variables
  # can be computed
  ecdf_list <- map(df[filter_vars], ecdf)
  
  # dataframe, where each column provides the percentiles of a climate variable
  # percentiles correspond to rows in df
  percentiles <- map_dfc(filter_vars, function(var) {
    ecdf_list[[var]](df[[var]])
  })
  names(percentiles) <- filter_vars
  
  # only keep rows falling in the low or high category, for each climate var
  df_filtered <- map_dfr(filter_vars, function(var) {
    df_low <- df[percentiles[[var]] < low, ]
    if (nrow(df_low)>0) {
      df_low$percentile_category <- paste0("<", low*100, "th")
    }
   
    df_high <- df[percentiles[[var]] > high, ] 
    df_high$percentile_category <- paste0(">", high*100, "th")
    
    out <- bind_rows(df_low, df_high)
    
    out$percentile_category <- as.factor(out$percentile_category)
    
    # adding separate category for the middle of the data
    if(add_mid){
      df_mid <- df[percentiles[[var]] < .6 & percentiles[[var]] > .4, ]
      df_mid$percentile_category <- "40th-60th"
      
      # create correct factor order
      levels <- levels(out$percentile_category)
      levels <- c(levels[1], "40th-60th", levels[2])
      # convert to character for binding
      out$percentile_category <- as.character(out$percentile_category)
      out <- bind_rows(out, df_mid)
      out$percentile_category <- factor(out$percentile_category,
                                        levels = levels)
      
    }
    out$filter_var <- var
    
    out
  })
  df_filtered$filter_var <- factor(df_filtered$filter_var, levels = filter_vars)
  df_filtered
}

