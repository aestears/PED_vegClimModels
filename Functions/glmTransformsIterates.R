## adapted from code from Martin Holdrege 

# names elements of x with the values of x
self_name <- function(x) {
  if(!is.null(names(x))){
    warning('x is already named are you sure you want to rename')
  }
  names(x) <- x
  x
}

#' Fit a number of beta glms
#'
#' @param forms character vector where each element can be parsed to a 
#' model formula
#' @param df dataframe to fit the model on
#'
#' @return list of models, one for each formula
fit_beta_glms <- function(forms, df, weights_col = NULL) {
  stopifnot(weights_col %in% names(df))
  
  glm_list <- map(forms, function(form) {
    char_form <- form
    form <- as.formula(form)
    # some of these won't fit so returns NA if throws error
    # not using purrr::safely() didn't seem to work, maybe b/ 
    # of environment issues?
    #betareg(PerennialHerbGram_dec ~ swe_meanAnnAvg_30yr + tmean_meanAnnAvg_30yr + prcp_meanAnnTotal_30yr + PrecipTempCorr_meanAnnAvg_30yr + isothermality_meanAnnAvg_30yr + annWetDegDays_meanAnnAvg_30yr,
            #data = df, link = c("logit"), link.phi = NULL, type = c("ML"))
      out <- tryCatch(betareg(formula = form, data = df, 
                              , link = c("logit"), link.phi = NULL,
                              type = c("ML")),
                      error = function(e) NA)

    
    if (any(is.na(out))) message(paste(char_form,  "model couldn't fit \n"))
    butcher::butcher(out) # to save memory
  })
  # removing models that couldn't be fit b/ they threw an error
  out <- keep(glm_list, function(x) all(!is.na(x)))
  out
}
# fit beta regression models w/ different transforms
glmTransformsIterates <- function(preds, df, response_var,
                                    max_steps = NULL, delta_aic = 4,
                                    fit_mod = fit_beta_glms, 
                                    ...) {
  stopifnot(
    is.data.frame(df),
    is.character(preds),
    is.character(response_var)
  )
  # max steps just makes sure the while loop doesn't explode
  # (run for ever) if something goes wrong
  if (is.null(max_steps)) {
    max_steps <- 30
  }
  if (is.na(max_steps) | max_steps > 30) {
    stop("two many iterations will be required consider shorter preds vector")
  }
  
  out <- list()
  i = 1 # this is the step number
  
  # iterating through number of total variables transformed in the model
  steps_left <- length(preds)
  while (i <= max_steps & steps_left > 0) {
    
    step_name <- paste0('step', i)
    print(step_name)
    out[[step_name]] <- list() # list of output for this step
    
    # model formulas with an additional predictor variable transformed
    pred_transforms1 <- transform_preds(preds = preds)
    # pasting in response variable
    pred_transforms2 <- paste(response_var,  pred_transforms1) 
    names(pred_transforms2) <- names(pred_transforms1)
    
    # fitting glm's for each formula (all model objects)
    glm_list <- fit_mod(forms = pred_transforms2,df = df) #, ...)
    
    # sorting AIC
    aic_sorted <- map_dbl(glm_list, AIC) %>% 
      sort() 
    
    # putting output into list
    best_mod <- names(aic_sorted[1])# name of model with lowest aic
    
    # model with no transformations is considered best unless other
    ## if the model w/ no transformations didn't converge, then we assign it an extremely high AIC value 
    if (is.na(aic_sorted['convert_none'])) {
      aic_sorted <- c(aic_sorted, "convert_none" = 10000000) 
    }
    # model is delta_aic better
    cat('\n', aic_sorted['convert_none'], "\n")
    cat(aic_sorted[best_mod], "\n")
    cat('delta aic cutoff', delta_aic, "\n")
    if ((aic_sorted['convert_none'] - aic_sorted[best_mod]) < delta_aic) {
      best_mod <- 'convert_none'
    }
    out[[step_name]]$best <- best_mod 
    # for memory reasons now not saving the actually model objects
    #out[[step_name]]$glm <- glm_list # model objects
    out[[step_name]]$aic <- aic_sorted # AIC values sorted
    
    # preparing for next cycle through the loop
    i <- i + 1
    
    # parsing the predictor variables
    # of the best model into a vector
    preds_out <- pred_transforms1[[best_mod]] %>% 
      str_replace_all("[ ]|~", "") %>%  # remove spaces and ~
      # split based on presence of + (but b/ add constants in some places
      # don't split + if followed by a digit)
      str_split("\\+(?!\\d)") %>% 
      unlist() %>% 
      self_name()
    
    # transformation that took place this step. 
    # transformation of interactions depends on what transformation
    # happend to the main term, and is done in the same step
    # matching single but not double ::, (because e.g. stats::poly() is ok)
    preds_main <- str_subset(preds, pattern = '(?<!\\:)\\:(?!\\:)', negate = TRUE) # not interaction terms
    preds_out_main <- str_subset(preds_out, pattern = '(?<!\\:)\\:(?!\\:)', negate = TRUE) # not interaction terms
    if(all(preds_out_main %in% preds_main)) {
      diff = NA_character_
    } else {
      diff <- preds_out_main[!preds_out_main %in% preds_main]
      if(length(diff) > 1) {
        stop('issue with figuring out which var was transformed')
      }
    }
    out[[step_name]]$var_transformed <- diff
    
    # check if variable parsing worked
    # this line now commented out so that transformation 
    # can include replacing x with x +x^2
    # stopifnot(length(preds_out) == length(preds))
    preds <- preds_out
    
    # how many untransformed variables are left?
    
    steps_left <- sum(str_detect(preds, "\\(", negate = TRUE))
    # determine whether to go to the next step
    # if the best model is the one where no more  transformations
    # were done then don't continue
    
    if(out[[step_name]]$best == 'convert_none') {
      break
    }
  }
  # the formula of the best model in the final step
  out$final_formula <- pred_transforms2[best_mod]
  out
}

#' publication quality  dotplot of data summarized to quantiles
#'
#' @param df dataframe longform with 'mean_value' column
#' (i.e. mean value of the predictor variable for the given decile) and 'name' 
#' column which gives the name of the predictor variable
decile_dotplot_pq <- function(df, size = 0.5, response = response, IQR = FALSE, CI = FALSE) {
  
  if('filter_var' %in% names(df)) {
    stop('filter_var column present, you should used decile_dotplot_filtered()')
  } 
  
  # if there are deciles that have been calculated for the observed values, then remove then from the input data.frame
  if(c("newObservedVal") %in% df$name) {
    df <- df %>% 
      filter(name != "newObservedVal")
  }
  
  df2 <- df %>% 
    #filter(!name %in% response) %>% 
    mutate(name = var2lab(name, units_md = TRUE)) %>% 
    arrange(name)
  
  # fix the levels to correspond to the predictor values we actually use in this model
  df2$name <-factor(as.character(df2$name))
  
  yvar <- response[1]
  yvar_pred <-  response[2]
  
  fig_letters <- str_to_upper(c("a", "b", "c", "d", "e", "f", "g", "f", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q","r", "s", "t", "u", "v", "w", "x", "y", "z"))
  letter_df <- tibble(
    letter = fig_letters[1:length(unique(df2$name))],
    name = (as.factor(unique(df2$name))),
    x = -Inf,
    y = Inf
  )
  
  if (IQR == TRUE) {
    g <- ggplot(df2, aes(x = mean_value, y = .data[[yvar]])) +
      geom_ribbon(aes(ymin = .data[[paste0(yvar, "_IQR_low")]], 
                      ymax = .data[[paste0(yvar, "_IQR_high")]], 
                      #color = 'Observed', 
                      fill = 'Observed'), 
                  alpha = .2) + 
      geom_ribbon(aes(ymin = .data[[paste0(yvar_pred, "_IQR_low")]], 
                      ymax = .data[[paste0(yvar_pred, "_IQR_high")]], 
                      #color = 'Predicted', 
                      fill = 'Predicted'),
                  alpha = .2) + 
      geom_point(aes(color = "Observed", shape = "Observed"),
                 #size = size, 
                 alpha = 0.6) +
      geom_point(aes(y = .data[[yvar_pred]], color = 'Predicted',
                     shape = 'Predicted'), size = size, alpha = 0.75) +
      geom_text(data = letter_df, aes(x = x, y = y, label = letter),
                hjust = -0.8,
                vjust = 1) +
      facet_wrap(~name, scales = 'free', strip.position = "bottom") +
      # using annotate to add in line segements because lemon package (facet_rep_wrap)
      # isn't being maintained anymore
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1) +
      # adding line segment on the right, for secondary axis
      annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, size = 1) +
      labs(#y = "Total Tree Cover per decile" ,
        x = "mean climate predictor value per decile") +
      # theme(legend.position = 'top',
      #       #legend.title = element_blank(),
      #       #strip.text = element_text(),
      #       strip.background = element_blank(),
      #       strip.text = ggtext::element_markdown(margin = margin()),
      #       strip.placement = "outside",
      #       axis.title.x = element_blank(),
      #       axis.line.y = element_blank()) +
      scale_color_manual(name = 'legend', values = c("Observed" = "black", "Predicted" = "#2b8cbe")) +
      scale_fill_manual(name = 'legend', values = c("Observed" = "black", "Predicted" = "#2b8cbe")) +
      scale_shape_manual(name = 'legend', values =  c("Observed" = 19, "Predicted" = 17)) #+
    #sec_axis_fri() # adding second fire return interval axis
    
  } else  if (CI == TRUE) {
    g <- ggplot(df2, aes(x = mean_value, y = .data[[yvar]])) +
      geom_ribbon(aes(ymin = .data[[paste0(yvar, "_CI_lower")]], 
                      ymax = .data[[paste0(yvar, "_CI_upper")]], 
                      #color = 'Observed', 
                      fill = 'Observed'), 
                  alpha = .2) + 
      geom_ribbon(aes(ymin = .data[[paste0(yvar_pred, "_CI_lower")]], 
                      ymax = .data[[paste0(yvar_pred, "_CI_upper")]], 
                      #color = 'Predicted', 
                      fill = 'Predicted'),
                  alpha = .2) + 
      geom_point(aes(color = "Observed", shape = "Observed"),
                 #size = size, 
                 alpha = 0.6) +
      geom_point(aes(y = .data[[yvar_pred]], color = 'Predicted',
                     shape = 'Predicted'), size = size, alpha = 0.75) +
      geom_text(data = letter_df, aes(x = x, y = y, label = letter),
                hjust = -0.8,
                vjust = 1) +
      facet_wrap(~name, scales = 'free_x', strip.position = "bottom") +
      # using annotate to add in line segements because lemon package (facet_rep_wrap)
      # isn't being maintained anymore
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1) +
      # adding line segment on the right, for secondary axis
      annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, size = 1) +
      labs(#y = "Total Tree Cover per decile" ,
        x = "mean climate predictor value per decile") +
      # theme(legend.position = 'top',
      #       #legend.title = element_blank(),
      #       #strip.text = element_text(),
      #       strip.background = element_blank(),
      #       strip.text = ggtext::element_markdown(margin = margin()),
      #       strip.placement = "outside",
      #       axis.title.x = element_blank(),
      #       axis.line.y = element_blank()) +
      scale_color_manual(name = 'legend', values = c("Observed" = "black", "Predicted" = "#2b8cbe")) +
      scale_fill_manual(name = 'legend', values = c("Observed" = "black", "Predicted" = "#2b8cbe")) +
      scale_shape_manual(name = 'legend', values =  c("Observed" = 19, "Predicted" = 17)) #+
    #sec_axis_fri() # adding second fire return interval axis
    
  } else  {
    g <- ggplot(df2, aes(x = mean_value, y = .data[[yvar]])) +
     geom_point(aes(color = "Observed", shape = "Observed"),
                 #size = size, 
                 alpha = 0.6) +
      geom_point(aes(y = .data[[yvar_pred]], color = 'Predicted',
                     shape = 'Predicted'), size = size, alpha = 0.75) +
      geom_text(data = letter_df, aes(x = x, y = y, label = letter),
                hjust = -0.8,
                vjust = 1) +
      facet_wrap(~name, scales = 'free_x', strip.position = "bottom") +
      # using annotate to add in line segements because lemon package (facet_rep_wrap)
      # isn't being maintained anymore
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 1) +
      # adding line segment on the right, for secondary axis
      annotate("segment", x=Inf, xend=Inf, y=-Inf, yend=Inf, size = 1) +
      labs(#y = "Total Tree Cover per decile" ,
        x = "mean climate predictor value per decile") +
      # theme(legend.position = 'top',
      #       #legend.title = element_blank(),
      #       #strip.text = element_text(),
      #       strip.background = element_blank(),
      #       strip.text = ggtext::element_markdown(margin = margin()),
      #       strip.placement = "outside",
      #       axis.title.x = element_blank(),
      #       axis.line.y = element_blank()) +
      scale_color_manual(name = 'legend', values = c("Observed" = "black", "Predicted" = "#2b8cbe")) +
      scale_fill_manual(name = 'legend', values = c("Observed" = "black", "Predicted" = "#2b8cbe")) +
      scale_shape_manual(name = 'legend', values =  c("Observed" = 19, "Predicted" = 17)) #+
    #sec_axis_fri() # adding second fire return interval axis
    
  }
  
  g
}

#' convert variable abbreviation to a better label
#'
#' @param x character vector
#' @param units_md whehter to include units in markdown format
#' @param add_letters whether to add letters to factor levels
#' @param include_hmod whether to include human modification as a level
#' 
#' @return vector same length as x, with fuller labels. Except if x 
#' is null, then just the lookup vector returned
#'
#' @examples
#'  var2lab(rev(factor(c("afgAGB", "prcpPropSum", 'MAT', 'MAP', 'pfgAGB'))))
#'  var2lab("prcpPropSum")
var2lab <- function(x = NULL, units_md = FALSE, add_letters = FALSE,
                    include_hmod = FALSE) {
  
  
  # Including units that are written using markdown formatting

  lookup_md <- c(
    "tmean_s" = "Mean ann. temp.",
    "prcpTempCorr_s"  = "Prcp. - temp. correlation",
    "isothermality_s" = "isothermality",
    "annWatDef_s" = "Ann. water deficit",
    "prcp_s" = "Prcp.",
    "prcp_seasonality_s" = "Prcp. seasonality",
    "prcp_dry_s" = "Prcp. of driest month", 
    "annWetDegDays_s" = "Ann. wet degree days", 
    "t_warm_s" = "Temp. of warmest month", 
    "t_cold_s" = "Temp. of coldest month", 
    "prcp_wet_s" = "Prcp. of wettest month", 
    "soilDepth_s" = "Soil depth", 
    "sand_s" = "Sand", 
    "coarse_s" = "Coarse",
    "AWHC_s" = "Avg. Wat. Holding Cap.", 
    "clay_s" = "Clay", 
    "carbon_s" = "Carbon", 
    "AWHC" = "AWHC",
    "VPD_mean_anom" = "VPD mean ANOM", 
    "VPD_max" = "VPD max CLIM",
    "VPD_max_95_anom" = "VPD max 95 CLIM",
    "VPD_min_anom" = "VPD min ANOM",
    "aboveFreezingMonth_anom" = "aboveFreezing ANOM",
    "annWatDef" = "Ann WatDef CLIM", 
    "annWatDef_anom" = "AnnWatDef ANOM", 
    "annWetDegDays_anom" = "AnnWetDegDays ANOM", 
    "annWetDegDays" = "AnnWetDegDays CLIM",
    "coarse" = "coarse", 
    "clay" = "clay", 
    "frostFreeDays_5_anom" = "FrostFreeDays 5 ANOM",
    "isothermality" = "isothermality CLIM",
    "isothermality_anom" = "isothermality ANOM", 
    "prcp" = "Precip CLIM",
    "prcp_dry" = "PrecipDriestMonth CLIM",
    "prcpTempCorr" = "PrecipTempCorr CLIM",
    "prcpTempCorr_anom" = "PrecipTempCorr ANOM",
    "prcp_seasonality" = "PrecipSeasonality CLIM", 
    "prcp_seasonality_anom" = "PrecipSeasonality ANOM",
    "prcp_wet_anom" = "precipWettest ANOM", 
    "precp_dry_anom" = "precipDriest ANOM", 
    "prcp_anom" = "precip ANOM",
    "sand" = "sand", 
    "carbon" = "carbon", 
    "t_cold_anom" = "tempColdest ANOM",
    "t_warm_anom" = "tempWarmest ANOM",
    "tmax_anom" = "maxTemp ANOM Ann. maximum Temp. - 3 yr. anomaly", 
    "tmean" = "meanTemp CLIM",
    "tmin_anom" = "tmin ANOM",
    "prcp_wet" = "prcp_wet",
    "t_warm" = "t_warm",
    "soilDepth" = "soilDepth", 
    "t_cold" = "t_cold"
  )
  
  lookup_name_only <- c(
    "tmean_s" = "Mean ann. temp.",
    "prcpTempCorr_s"  = "Prcp. - temp. correlation",
    "isothermality_s" = "isothermality",
    "annWatDef_s" = "Ann. water deficit",
    "prcp_s" = "Prcp.",
    "prcp_seasonality_s" = "Prcp. seasonality",
    "prcp_dry_s" = "Prcp. of driest month", 
    "annWetDegDays_s" = "Ann. wet degree days", 
    "t_warm_s" = "Temp. of warmest month", 
    "t_cold_s" = "Temp. of coldest month", 
    "prcp_wet_s" = "Prcp. of wettest month", 
    "soilDepth_s" = "Soil depth", 
    "sand_s" = "Sand", 
    "coarse_s" = "Coarse",
    "AWHC_s" = "AWHC", 
    "clay_s" = "Clay", 
    "carbon_s" = "Carbon",
    "AWHC" = "Average Soil Water Holding Capacity",
    "VPD_max" = "VPD Ann. maximum - 30 yr. mean",
    "VPD_mean_anom" = "VPD mean - 3 yr. anomaly", 
    "VPD_max_95_anom" = "VPD Ann. maximum - 30 yr. 95th percentile",
    "VPD_min_anom" = "VPD Ann. minimum - 3 yr. anomaly",
    "aboveFreezingMonth_anom" = "Month above freezing - 3 yr. anomaly",
    "annWatDef" = "Ann. water deficit - 30 yr. mean", 
    "annWatDef_anom" = "Ann. water deficit - 3 yr. anomaly",
    "annWetDegDays" = "Ann. wet degree days - 30 yr. mean",
    "annWetDegDays_anom" = "Ann. wet degree days - 3 yr. anomaly", 
    "coarse" = "% of soil as coarse fragments", 
    "frostFreeDays_5_anom" = "Frost free days - 5th percentile over 3 yr. anomaly",
    "isothermality" = "isothermality - 30 yr. mean",
    "isothermality_anom" = "isothermality - 3 yr. anomaly", 
    "prcp" = "Ann. Precip. - 30 yr. mean",
    "prcp_dry" = "Precip. of Driest Month - 30 yr. mean",
    "prcpTempCorr" = "Correlation of Precip. and Temp. - 30 yr. mean",
    "prcpTempCorr_anom" = "Correlation of Precip. and Temp. - 3 yr. anomaly",
    "prcp_seasonality" = "Precip. seasonality - 30 yr. mean", 
    "prcp_seasonality_anom" = "Precip. seasonality - 3 yr. anomaly",
    "prcp_wet_anom" = "Precip. of the wettest month - 3 yr. anomaly", 
    "precp_dry_anom" = "Precip. of the driest month - 3 yr. anomaly", 
    "sand" = "% of soil as sand", 
    "carbon" = "% carbon", 
    "clay" = "% clay", 
    "t_cold_anom" = "Temp. of the coldest month - 3 yr. anomaly",
    "t_warm_anom" = "Temp. of the warmest month - 3 yr. anomaly",
    "tmax_anom" = "Ann. maximum Temp. - 3 yr. anomaly", 
    "tmean" = "Ann. mean Temp. - 30 yr. mean",
    "tmin_anom" = "Ann. minimum Temp. - 3 yr. anomaly",
    "prcp_anom" = "Ann. total precip. - 3 yr. anomaly",
    "prcp_wet" = "Precip of wettest month - 30 yr. mean",
    "t_warm" = "temp. of warmest month - 30 yr. mean",
    "soilDepth" = "soilDepth", 
    "t_cold" = "t_cold"
  )
  
  # if human modification layer included in the input,
  # add it here (if not present, it won't be included as 
  # a factor level)
  if("hmod" %in% x | include_hmod) {
    lookup_md <- c(lookup_md, "hmod" = "Human modification")
    lookup_name_only <- c(lookup_name_only, "hmod" = "Human modification")
    
  }
  stopifnot(as.character(x) %in% names(lookup_md))
  
  
  
  lookup <- if(units_md) {
    lookup_md
  } else {
    lookup_name_only
  }
  
  if(add_letters) {
    new_lookup <- paste(fig_letters[1:length(lookup)], lookup)
    names(new_lookup) <- names(lookup)
    lookup <- new_lookup
  }
  
  if(is.null(x)) {
    return(lookup)
  }
  
  out <- lookup[as.character(x)]
  
  # convert to a factor (for ordering in figures)
  out <- factor(out, levels = lookup)
  
  out
}

#' add observed vs predicted inset to quantile dotplot
#'
#' @param g ggplot object created in decile_dotplot_pq
#' @param df dataframe (same dataframe used for decile_dotplot_pq)
#' @param add_smooth whether to add a smoother to the inset
#' @param ... arguments passed to geom_smooth
add_dotplot_inset <- function(g, df, response, dfRaw = NULL, add_smooth = FALSE,  deciles = TRUE, ...) {
  fig_letters <- str_to_upper(c("a", "b", "c", "d", "e", "f", "g", "f", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q","r", "s", "t", "u", "v", "w", "x", "y", "z"))
  yvar <- response[1]
  max <- df %>% 
    dplyr::select(all_of(yvar), all_of(response[2])) %>% 
    max #* 100
  if (deciles == TRUE) {
    # 
    # inset <- ggplot(df, aes(y = .data[[yvar]], x = .data[[response[2]]])) +
    #   geom_point(shape = 4, alpha = 0.2, size = 0.3) +
    #   geom_abline(slope = 1, size = 0.5, col = "red") +
    #   labs(y = "Observed Values",
    #        x = "Predicted Values") +
    #   # geom_text(data = tibble(
    #   #   letter = fig_letters[6],
    #   #   x = -Inf,
    #   #   y = Inf),
    #   #   aes(x = x, y = y, label = letter),
    #   #   hjust = -0.8, vjust = 1) +
    #   theme(axis.title = element_text(size = 6),
    #         axis.text = element_text(size = 6)#,
    #         #plot.margin = margin()
    #   )#+
    # #coord_cartesian(xlim = c(0, max),
    # #ylim = c(0, max))
    # 
    # if(add_smooth) {
    #   inset <- inset +
    #     geom_smooth(se = FALSE, color = 'gray')
    # }
    # 
    # library(ggpubr)
    # g2 <- ggarrange(g, ggarrange(inset, ggplot(), widths = c(1,2)), heights = c(3, 1), nrow = 2)
    # # inset_element(inset, left = .75,
    # #               bottom = -.08,
    # #               right = 1, top = 0.14)
  } else {
    inset_raw <- ggplot(dfRaw, aes(y = .data[[yvar]], x = .data[[response[2]]])) +
      geom_point(shape = 4, alpha = 0.2, size = 0.3) +
      geom_abline(slope = 1, size = 0.5, col = "red") +
      labs(y = "Observed Values",
           x = "Predicted Values") +
      theme(axis.title = element_text(size = 6),
            axis.text = element_text(size = 6)#,
            #plot.margin = margin()
      )#+
    
    if(add_smooth) {
      inset_raw <- inset_raw +
        geom_smooth(se = TRUE, color = 'gray')
    }
    
    # 
    # inset <- ggplot(df, aes(y = .data[[yvar]], x = .data[[response[2]]])) +
    #   geom_point(shape = 4, alpha = 0.2, size = 0.3) +
    #   geom_abline(slope = 1, size = 0.5, col = "red") +
    #   labs(y = "Observed Values",
    #        x = "Predicted Values") +
    #   # geom_text(data = tibble(
    #   #   letter = fig_letters[6],
    #   #   x = -Inf,
    #   #   y = Inf),
    #   #   aes(x = x, y = y, label = letter),
    #   #   hjust = -0.8, vjust = 1) +
    #   theme(axis.title = element_text(size = 6),
    #         axis.text = element_text(size = 6)#,
    #         #plot.margin = margin()
    #   )#+
    # #coord_cartesian(xlim = c(0, max),
    # #ylim = c(0, max))
    # 
    # if(add_smooth) {
    #   inset <- inset +
    #     geom_smooth(se = FALSE, color = 'gray')
    # }
    
    library(ggpubr)
    g2 <- ggarrange(g, ggarrange(#inset,
      inset_raw
                                 , ggplot(), widths = c(1,3), nrow = 1), heights = c(3, 1), nrow = 2)
    # inset_element(inset, left = .75,
    #               bottom = -.08,
    #               right = 1, top = 0.14)
  }
  
  g2
}


#' @param df dataframe longform in longform, should be output of 
#' predvars2deciles with the filter_var argument set to TRUE
#' @param add_smooth logical--whether to add splines
#' @param size size of points
#' @param xvars = vector of variables to show along the x axis
decile_dotplot_filtered_pq <- function(df,
                                       response,
                                       add_smooth = TRUE,
                                       size = 0.5,
                                       return_df = FALSE,
                                       xvars
) {
  
  yvar <- response
  
  # # convert k to c
  # df[df$name == "MAT", ]$mean_value <- 
  #   df[df$name == "MAT", ]$mean_value - 273.15
  df2 <- df %>% 
    filter(name %in% xvars) %>% 
    dplyr::select(name, filter_var, percentile_category, decile, mean_value,
           all_of(yvar), all_of(paste0(yvar, "_pred"))) %>% 
    pivot_longer(cols = all_of(c(yvar, paste0(yvar, "_pred"))),
                 names_to = 'source',
                 values_to = 'CoverProportion') %>% 
    mutate(source = ifelse(str_detect(source, "_pred$"),
                           "predicted", "observed")) %>% 
    arrange(percentile_category, source) %>%  # so factor is ordered
    mutate(percentile_category = paste0(percentile_category, " (", source, ")"),
           percentile_category = factor(percentile_category, levels = unique(percentile_category)),
           PercentCover = CoverProportion) # convert to %)
  
  letter_df <- expand_grid(filter_var = unique(df2$filter_var), 
                           name = unique(df2$name)) %>% 
    mutate(letter = seq(1:150)[1:n()],
           x = -Inf,
           y = Inf)
  
  if(return_df) {
    return(df2)
  }
  
  # more colors for when mid category is included
  if (length(unique(df2$percentile_category)) == 6) {
    # reds, greens, blues
    colors <- c("#f03b20","#feb24c", "#31a354", "#addd8e", "#0570b0", "#74a9cf")
    shapes <- c(19, 17, 19, 17, 19, 17)
  } else {
    colors <- c("#f03b20","#feb24c", "#0570b0", "#74a9cf")
    shapes <- c(19, 17, 19, 17)
  }
  
  g <- ggplot(df2, aes(x = mean_value, y = PercentCover)) +
    geom_point(aes(color = percentile_category,
                   shape = percentile_category),
               size = size) +
    facet_grid(filter_var~name, scales = 'free_x', switch = 'x'
               ,labeller = labeller(filter_var = ~var2lab(.x, units_md = TRUE),
                                   name = ~var2lab(.x, units_md = TRUE))
    ) +
    labs(x = paste0(response),
      #y = lab_fireProbPerc,
      tag = 'Filtering variable') +
    # using annotate to add in line segements because lemon package (facet_rep_wrap)
    # isn't being maintained anymore
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, size = 0.7) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size = 0.7) +
    geom_text(data = letter_df, aes(x = x, y = y, label = letter),
              hjust = -0.8,
              vjust = 1)+
    #labs(y = lab_fireProbPerc) +
    theme(legend.position = 'top',
          legend.title = element_text(size = 9),
          #strip.text = element_text(),
          strip.background = element_blank(),
          strip.text = ggtext::element_markdown(),
          strip.placement = "outside",
          axis.title.x = element_blank(),
          axis.line = element_blank(),
          plot.tag.position = c(1.01, 0.5),
          plot.tag = element_text(angle = 270),
          plot.margin = unit(c(5.5, 20, 5.5, 5.5), "points")) +
    # different colors for each combination of percentile and observed vs predicted,
    # shapes are observed (circles) vs predicted (triangles)
    scale_colour_manual(name = "Percentile of filtering variable",
                        values = colors)+
    scale_shape_manual(name = "Percentile of filtering variable",
                       values = shapes) +
    guides(colour = guide_legend(title.position="top", title.hjust = 0.5)) +
    coord_cartesian(clip = 'off')
  
  if (add_smooth) {
    g <- g +
      geom_smooth(aes(color = percentile_category),
                  se = FALSE,
                  size = 0.7)
  }
  g
}

# helper function for generating legends
legend_generator <- function(df, percentile_name, variable_name) {
  legend_name = paste(percentile_name, variable_name)
  
  colors <- c("#f03b20","#feb24c", "#0570b0", "#74a9cf")
  shapes <- c(19, 17)
  
  if(percentile_name == "Low") {
    colors <- colors[1:2]
  } else {
    colors <- colors[3:4]
  }
  
  
  g1 <- df %>% 
    filter(percentile_name == percentile_name) %>% 
    ggplot(aes(mean_value, probability, color = source, shape = source)) +
    geom_point() +
    geom_line() +
    scale_colour_manual(name = legend_name,
                        values = colors)+
    scale_shape_manual(name =  legend_name,
                       values = shapes) +
    guides(color = guide_legend(title.position = "left"),
           shape = guide_legend(title.position = "left")) +
    theme(legend.title = element_text(size = 8),
          legend.text = element_text(size = 6),
          legend.margin= margin())
  
  cowplot::get_legend(g1)
}

# generate a list of legends (to be used decile_dotplot_filter_pq2)
generate_all_legends <- function(df) {
  var_names <- var2lab(c("MAT", "MAP", "prcpPropSum"))
  percentile_names <- c("Low", "High")
  df_names <- expand_grid(var_names, percentile_names)
  
  legends <- list()
  for(i in 1:nrow(df_names)) {
    legends[[i]] <- legend_generator(df = df, 
                                     percentile_name = df_names$percentile_names[i],
                                     variable_name = df_names$var_names[i])
  }
  legends
}

