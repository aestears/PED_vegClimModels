## Functions for environmental blocking to be used in cross validation
##
## Author: Alice Stears, based on code from Gregor Siegmund
## Email: astears@gmail.com


#' Function to append climate covariates to outcome data
#' User passes outcomes and climate covariates to the function
#'
#' @param df outcome observations as a data.frame
#' @param covariates climate covariates as a data.table; one of "longtermclimate" or "annualclimate"
#'
#' @return outcome observations and climate covariates as a data.frame
#' @export
#'
#' @examples
#' #Default use
#' ...
append_covariates <- function(df = outcomes, covariates = "longtermclimate"){
  
  # - + read in table crosswalking indexed survey coordinates to original identifiers ----
  # surveyCoordsCrosswalkTable <- read.csv(here::here("03_outputs","01_surveyCoordinates","surveyCoordsCrosswalkTable.csv"))
  # crosswalk <- surveyCoordsCrosswalkTable %>%
  #   dplyr::select(sourceID,index) %>%
  #   dplyr::distinct()
  
  if(covariates=='longtermclimate'){
    climate <- readRDS(here::here("03_outputs","05_appendEnvironmentalCovariates","meanLongtermClimateAdjustedWaterYear-20241216.rds"))
    climate_df <- as.data.frame(climate) 
    
    df_new <- df %>%
      # dplyr::left_join(crosswalk) %>%
      dplyr::left_join(climate_df,by=c('index'='id','Trt_ID')) %>%
      dplyr::relocate(index,.before=yearFire)
    
  } else if(covariates=='annualclimate'){
    climate <- readRDS(here::here("03_outputs","05_appendEnvironmentalCovariates","meanAnnualClimateAdjustedWaterYear-20241216.rds"))
    climate_df <- as.data.frame(climate) 
    
    # compute anomalies here if working with those
    # or move this to outside the function
    
    # need to modify the below if working with something other than year of seeding
    df_new <- df %>%
      dplyr::mutate(waterYearSeeding = EflowStats::get_waterYear(dateSeeding,wyMonth=9L)) %>%
      dplyr::mutate(waterYearSeeding = case_when(month(dateSeeding)==8~waterYearSeeding+1,
                                                 month(dateSeeding)!=8~waterYearSeeding,
                                                 .default=waterYearSeeding)) %>%
      dplyr::left_join(climate_df,by=c('index'='id','Trt_ID','waterYearSeeding'='waterYear')) %>%
      dplyr::relocate(index,.before=yearFire)
  }
  
  return(df_new)
}


#' Function to assign data to training and testing/hold-out set using rsample 
#' User passes data.frame with outcome observations and climate covariates, and parameters for performing the hold-out
#'
#' @param df outcomes and climate covariates as a data.frame, inherit from append_covariates()
#' @param holdout_method method to use for holding out data. 'randomsurveys' holds out p percent of the surveys at random. 'randomseedings' holds out p percent of the seedings at random. 'randomyears' holds out p percent of the seeding years at random. 'lastyears' holds out the lastnyears of the data. 'none' assigns all data to training (none is held out).
#' @param p percent to hold out. For 'randomsurveys', this is p percent of observations. For 'randomseedings' this is p percent of seedings. For 'randomyears' this is p percent of years.
#' @param lastnyears final n years of observations to hold out. Only used if holdout_method=='lastyears'.
#'
#' @return outcome observations and climate covariates, with variable for whether observation is in training or test/hold-out set as a data.frame
#' @export
#'
#' @examples
#' #Default use
#' df_split <- hold_out_data(df = df,holdout_method="randomsurveys",p=0.20)
hold_out_data_rsample <- function(df = data, holdout_method = 'randomsurveys', n_folds = 10, n_repeats = 1, lastnyears = 5){
  
  if(holdout_method == "randomsurveys"){
    
    # n_folds = 1/p
    data_cv<-rsample::vfold_cv(data = df, v = n_folds, repeats = n_repeats)
    
  } else if(holdout_method == "randomseedings"){
    
    # n_folds = 1/p
    data_cv<-rsample::group_vfold_cv(data = df, v = n_folds, group = "Trt_ID", balance = 'groups', repeats = n_repeats)
    
  } else if(holdout_method == "randomyears"){
    
    # n_folds = 1/p
    data_cv<-rsample::group_vfold_cv(data = df, v = n_folds, group = "yearSeeding", balance = 'groups', repeats = n_repeats)
    
  } else if(holdout_method == "lastyears"){
    
    df_nested <- df %>%
      tidyr::nest(data = -c(yearSeeding)) %>%
      dplyr::arrange(yearSeeding)
    data_cv<-rsample::rolling_origin(data = df_nested,cumulative = TRUE,assess = lastnyears, initial=nrow(df_nested)-lastnyears)
    
  } else 
    if(holdout_method == 'none'){
      
      # hacky solution to get all data into the same format
      data_cv<-rsample::vfold_cv(data=df, v = 2, repeats = 1)[1,]
      data_cv$splits[[1]]$in_id <- 1:nrow(df)
      
    } else if (holdout_method =="environmentalblock"){
      
      # data_cv<-rsample::clustering_cv(data=data_tmp, v = n_folds, repeats = n_repeats,
      #                                 vars = all_of(covarsToFit))
      # data_cv<-rsample::nested_cv(data=data_tmp, outside = outer_folds, inside = rsample::clustering_cv(v = n_folds, repeats = n_repeats, vars = all_of(covarsToFit)))
      x_tmp <- rsample::clustering_cv(data = df, vars = names(prednames)[1:8], v = 5#n_folds
                                     # , repeats = n_repeats
                                     )
      # x_tmp<-list()
      # for(j in 1:nrow(outer_folds)){
      #   x_nogeom<-rsample::analysis(outer_folds$splits[[j]]) %>% sf::st_drop_geometry()
      #   x_tmp[[j]] <- rsample::clustering_cv(data=x_nogeom, v = n_folds, repeats = n_repeats, vars = all_of(prednames))
      # }
      outer_folds$inner_resamples <- x_tmp
      data_cv <- outer_folds
      
      # check output
      #  data_folds<-bind_rows(lapply(data_cv$splits,rsample::assessment),.id='fold')
      
      # # read in all the coordinate data
      # allCoordinatesConus <- readRDS(here::here("03_outputs","01_surveyCoordinates","surveyCoordsTableAllDataSourcesSpatialObject.RDS"))
      # 
      # # grab the variables necessary here; index and geometry 
      # surveyCoordinates <- allCoordinatesConus %>%
      #   dplyr::select(sourceID,geometry)
      # 
      # # join to the data
      # data_spatial <- data_folds %>%
      #   dplyr::left_join(surveyCoordinates) %>%
      #   sf::st_as_sf()
      # 
      # ggplot() +
      #   geom_sf(data=data_spatial,aes(geometry=geometry,color=fold)) +
      #   geom_point()
      # 
      # data_folds %>%
      #   tidyr::pivot_longer(Sim_SoilTemp_C_min_010_cm:Input_PPT_mm) %>% 
      # ggplot(aes(x=value,fill=fold)) +
      #   geom_density() +
      #   facet_wrap(~name,scales='free')
      
      # clusterList <- list()
      # for(j in 1:n_repeats){
      # x_vals <- data_tmp %>% dplyr::select(all_of(covarsToFit))
      # kms <- stats::kmeans(x_vals, centers = n_folds, iter.max = 500, 
      #                      nstart = 25)
      # fold <- kms$cluster
      # clusterList[[j]] <- fold
      # }
      # str(data_cv$splits[[1]])
    }
  
  # df_model <- df %>%
  #   dplyr::mutate(model_set = case_when(sourceID %in% oos_survey~"validation",
  #                                       !(sourceID %in% oos_survey)~"training",
  #                                       .default=NA))
  
  return(data_cv)
}



# #' Function to perform outer split and prepare inner split for model-fitting
# #' User passes data.frame from hold_out_data()
# #'
# #' @param df outcomes and climate covariates with test/train variable as a data.frame, inherit from hold_out_data()
# #'
# #' @return list with (1) data frame of outcomes, scaled and centered covariates, sourceID, Trt_ID, and yearSeeding; (2) vector of outcomes; (3) matrix of scaled and covariates; and (4) vector of covariates to fit
# #' @export
# #'
# #' @examples
# #' #Default use
# #' df_split <- hold_out_data(x = df,holdout_method="randomsurveys",p=0.20)
# #' data <- outer_split(x = df_split)
# outer_split <- function(df = data){

#   # - Select training split ----
#   df_fit <- df

#   # - Select all covariates ----

#   covarsToFit <- colnames(df_split)[!(colnames(df_split)%in%c('model_set',"species",
#                                                               "Trt_ID","locationID",
#                                                               "sourceID","index",'yearFire',
#                                                               "yearSeeding",'timeFireSeeding',
#                                                               "Occurrence_allSurvey_binary"))]


#   # - Prepare for observations and covariates for model fitting ----

#   y <- df_fit$Occurrence_allSurvey_binary # response
#   X <- as.matrix(dplyr::select(df_fit, all_of(covarsToFit))) # covariate matrix

#   # - + check for missing data in covariates ----
#   # and drop missing data
#   test_vec <- as.numeric(apply(X, MARGIN = 2, FUN = "mean"))
#   nacols <- which(is.na(test_vec))
#   if(length(nacols) > 0) X <- X[,-nacols]

#   # - + standardize covariates in TRAINING data only ----
#   X <- scale(X, center = TRUE, scale = TRUE)

#   data.object <- data.frame(y_obs = y, sourceID = df_fit$sourceID, 
#                             Trt_ID = df_fit$Trt_ID,
#                             yearSeeding = df_fit$yearSeeding, X)
#   return(list(df = data.object, outcome = y, covariates_scaled = X, covariates = covarsToFit))
# }


# #' Function to assign data to analysis and assessment sets for cross-validation 
# #' User passes list from outer_split(), method for cross-validation, and cv parameters.
# #' The function uses the *rsample* package to assign data to folds
# #'
# #' @param x list of data frame for cross-validation, and other outputs inherited from outer_split()
# #' @param cv_method method to use for assigning data to cross-validation folds. 'kfold' randomly assigns observations to folds. 'kfold_seeding' randomly assigns seeding treatments to folds. 'kfold_year' randomly assigns years to folds. 'loo' assigns each observation to a fold. 
# #' @param n_folds number of folds
# #' @param n_repeats number of times to repeat fold assignment
# #'
# #' @return list of splits, one per repeat/fold, that holds the data to be fit
# #' @export
# #'
# #' @examples
# #' #Default use
# #' data_cv <- assign_data_cv(x = data, cv_method = 'kfold', n_folds = 5, n_repeats = 1)
# assign_data_cv = function(x = data, cv_method = "kfold", n_folds = 5,n_repeats = 1){

#   data_tmp <- x[[1]]

#   if(cv_method=='kfold'){
#     data_cv<-rsample::vfold_cv(data=data_tmp, v = n_folds, repeats = n_repeats)
#   } else if(cv_method=='kfold_seeding'){
#     data_cv<-rsample::group_vfold_cv(data=data_tmp, v = n_folds, group = "Trt_ID", balance = 'observations', repeats=n_repeats)
#   } else if(cv_method=='kfold_year'){
#     data_cv<-rsample::group_vfold_cv(data=data_tmp, v = n_folds, group = "yearSeeding", balance = 'observations', repeats=n_repeats)
#   } else if(cv_method=='loo'){
#     data_cv<-rsample::loo_cv(data_tmp)
#   } else if(cv_method=="spatialblock"){

#     # read in all the coordinate data
#     allCoordinatesConus <- readRDS(here::here("03_outputs","01_surveyCoordinates","surveyCoordsTableAllDataSourcesSpatialObject.RDS"))

#     # grab the variables necessary here; index and geometry 
#     surveyCoordinates <- allCoordinatesConus %>%
#       dplyr::select(sourceID,geometry)

#     # join to the data
#     data_spatial <- data_tmp %>%
#       dplyr::left_join(surveyCoordinates) %>%
#       sf::st_as_sf()

#     # use spatialsample to construct spatial blocks
#     data_cv<-spatialsample::spatial_block_cv(data=data_spatial, v = n_folds, repeats = n_repeats)

#     # remove spatial objects to save space 
#     rm(allCoordinatesConus,surveyCoordinates)

#   } else if(cv_method=="environmentalblock"){

#     data_cv<-rsample::clustering_cv(data=data_tmp, v = n_folds, repeats = n_repeats,
#                                     vars = all_of(covarsToFit))

#     # check output
#     #  data_folds<-bind_rows(lapply(data_cv$splits,rsample::assessment),.id='fold')

#     # # read in all the coordinate data
#     # allCoordinatesConus <- readRDS(here::here("03_outputs","01_surveyCoordinates","surveyCoordsTableAllDataSourcesSpatialObject.RDS"))
#     # 
#     # # grab the variables necessary here; index and geometry 
#     # surveyCoordinates <- allCoordinatesConus %>%
#     #   dplyr::select(sourceID,geometry)
#     # 
#     # # join to the data
#     # data_spatial <- data_folds %>%
#     #   dplyr::left_join(surveyCoordinates) %>%
#     #   sf::st_as_sf()
#     # 
#     # ggplot() +
#     #   geom_sf(data=data_spatial,aes(geometry=geometry,color=fold)) +
#     #   geom_point()
#     # 
#     # data_folds %>%
#     #   tidyr::pivot_longer(Sim_SoilTemp_C_min_010_cm:Input_PPT_mm) %>% 
#     # ggplot(aes(x=value,fill=fold)) +
#     #   geom_density() +
#     #   facet_wrap(~name,scales='free')

#     # clusterList <- list()
#     # for(j in 1:n_repeats){
#     # x_vals <- data_tmp %>% dplyr::select(all_of(covarsToFit))
#     # kms <- stats::kmeans(x_vals, centers = n_folds, iter.max = 500, 
#     #                      nstart = 25)
#     # fold <- kms$cluster
#     # clusterList[[j]] <- fold
#     # }
#     # str(data_cv$splits[[1]])
#   }

#   return(data_cv)
# }

#' Function to assign data to analysis and assessment sets for cross-validation 
#' User passes list from outer_split(), method for cross-validation, and cv parameters.
#' The function uses the *rsample* package to assign data to folds
#'
#' @param x list of data frame for cross-validation, and other outputs inherited from outer_split()
#' @param cv_method method to use for assigning data to cross-validation folds. 'kfold' randomly assigns observations to folds. 'kfold_seeding' randomly assigns seeding treatments to folds. 'kfold_year' randomly assigns years to folds. 'loo' assigns each observation to a fold. 
#' @param n_folds number of folds
#' @param n_repeats number of times to repeat fold assignment
#'
#' @return list of splits, one per repeat/fold, that holds the data to be fit
#' @export
#'
#' @examples
#' #Default use
#' data_cv <- assign_data_cv(x = data, cv_method = 'kfold', n_folds = 5, n_repeats = 1)
inner_split_cv = function(x = data, outer_folds = outer_split, cv_method = "kfold", n_folds = 5, n_repeats = 1){
  
  data_tmp <- x
  
  if(cv_method=='kfold'){
    data_cv<-rsample::nested_cv(data_tmp, outside = outer_folds, inside = rsample::vfold_cv(v = n_folds, repeats = n_repeats))
  } else if(cv_method=='kfold_seeding'){
    data_cv<-rsample::nested_cv(data_tmp, outside = outer_folds, inside = rsample::group_vfold_cv(v = n_folds, group = "Trt_ID", balance = 'groups', repeats=n_repeats))
  } else if(cv_method=='kfold_year'){
    data_cv<-rsample::nested_cv(data_tmp, outside = outer_folds, inside = rsample::group_vfold_cv(v = n_folds, group = "yearSeeding", balance = 'groups', repeats=n_repeats))
  } else if(cv_method=='loo'){
    data_cv<-rsample::nested_cv(data_tmp, outside = outer_folds, inside = rsample::loo_cv())
  } else if(cv_method=="spatialblock"){
    
    data_cv<-rsample::nested_cv(data=data_tmp, outside = outer_folds, inside = spatialsample::spatial_block_cv(v = n_folds, repeats = n_repeats))
    
    # use spatialsample to construct spatial blocks
    #  data_cv<-spatialsample::spatial_block_cv(data=data_spatial, v = n_folds, repeats = n_repeats)
    #data_cv<-spatialsample::spatial_block_cv(data=data_spatial, v = n_folds, repeats = n_repeats)
    
    # remove spatial objects to save space 
    # rm(allCoordinatesConus,surveyCoordinates)
    
  } else if(cv_method=="environmentalblock"){
    
    # data_cv<-rsample::clustering_cv(data=data_tmp, v = n_folds, repeats = n_repeats,
    #                                 vars = all_of(covarsToFit))
    # data_cv<-rsample::nested_cv(data=data_tmp, outside = outer_folds, inside = rsample::clustering_cv(v = n_folds, repeats = n_repeats, vars = all_of(covarsToFit)))
    x_tmp<-list()
    for(j in 1:nrow(outer_folds)){
      x_nogeom<-rsample::analysis(outer_folds$splits[[j]]) %>% sf::st_drop_geometry()
      x_tmp[[j]] <- rsample::clustering_cv(data=x_nogeom, v = n_folds, repeats = n_repeats, vars = all_of(covarsToFit))
    }
    outer_folds$inner_resamples <- x_tmp
    data_cv <- outer_folds
    
    # check output
    #  data_folds<-bind_rows(lapply(data_cv$splits,rsample::assessment),.id='fold')
    
    # # read in all the coordinate data
    # allCoordinatesConus <- readRDS(here::here("03_outputs","01_surveyCoordinates","surveyCoordsTableAllDataSourcesSpatialObject.RDS"))
    # 
    # # grab the variables necessary here; index and geometry 
    # surveyCoordinates <- allCoordinatesConus %>%
    #   dplyr::select(sourceID,geometry)
    # 
    # # join to the data
    # data_spatial <- data_folds %>%
    #   dplyr::left_join(surveyCoordinates) %>%
    #   sf::st_as_sf()
    # 
    # ggplot() +
    #   geom_sf(data=data_spatial,aes(geometry=geometry,color=fold)) +
    #   geom_point()
    # 
    # data_folds %>%
    #   tidyr::pivot_longer(Sim_SoilTemp_C_min_010_cm:Input_PPT_mm) %>% 
    # ggplot(aes(x=value,fill=fold)) +
    #   geom_density() +
    #   facet_wrap(~name,scales='free')
    
    # clusterList <- list()
    # for(j in 1:n_repeats){
    # x_vals <- data_tmp %>% dplyr::select(all_of(covarsToFit))
    # kms <- stats::kmeans(x_vals, centers = n_folds, iter.max = 500, 
    #                      nstart = 25)
    # fold <- kms$cluster
    # clusterList[[j]] <- fold
    # }
    # str(data_cv$splits[[1]])
  }
  
  return(data_cv)
}

# use auc from glmnet internal functions
# https://github.com/cran/glmnet/tree/master/R
auc.glmnet=function(y,prob,w){
  if(missing(w))
    survival::concordance(y~prob)$concordance
  else survival::concordance(y~prob,weights=w)$concordance
}

# nested_splits$inner_resamples[[1]]


replicateRuns <- function(nested_splits = nested_split_data ,hmethod = "randomseedings",cvmethod='loo'){
  
  pen_facts <- rep(1, length(covarsToFit)) # penalize all covariates
  lambdas <- 10^seq(0, -8, by = -.01) # sequence of penalties to test
  
  n_resamples <- nrow(nested_splits)
  
  outer_rep.out <- outer_coefs.out <- total_inner.cv <- list()
  
  # loop over resamples (repeats*outer folds)
  for(r_i in 1:n_resamples){
    
    outer_set <- nested_splits[r_i,] %>% dplyr::select(id,id2)
    data_inner <- nested_splits$splits[[r_i]]
    data_cv <- nested_splits$inner_resamples[[r_i]]
    
    #if(cvmethod!="loo"){
    
    reps <- data_cv$id %>% unique
    
    inner_rep.out<-list()
    # loop over folds in inner loop (repeats*inner folds)
    for(rep_i in 1:length(reps)){
      
      data_model <- data_cv %>%
        dplyr::filter(id==reps[rep_i])
      
      # get data in 
      data_folds<-dplyr::bind_rows(lapply(data_model$splits,assessment),.id='fold')
      
      # if this was a spatial split, drop geometry
      if("sf" %in% class(data_folds)){
        data_folds <- data_folds %>%
          sf::st_drop_geometry() 
      }
      
      fold_vec = data_folds$fold %>%
        as.integer()
      y = data_folds$y_obs %>%
        as.vector()
      X = data_folds %>% 
        dplyr::select(all_of(covarsToFit)) %>%
        as.matrix()
      
      # scale predictors
      X <- scale(X, center = TRUE, scale = TRUE)
      
      # model with regularization via LASSO, following implementation in Tredennick et al. 2021
      lasso_tmp <- cv.glmnet(x = X, 
                             y = y, 
                             lambda = lambdas,
                             penalty.factor = pen_facts,
                             family = "binomial", 
                             alpha = 1, # 0 for ridge, 1 for lasso, 0.5 for elastic net 
                             standardize = FALSE, 
                             type.measure = "auc",
                             foldid = fold_vec)
      
      # + Collect results into data frames 
      lambdas <- lasso_tmp$lambda
      auc_scores <- lasso_tmp$cvm
      
      inner_rep.out[[rep_i]] <- data.frame(hmethod = hmethod, cvmethod = cvmethod,
                                           outer_rep=outer_set$id,outer_fold=outer_set$id2,
                                           inner_rep=reps[rep_i],
                                           lambda.min = lasso_tmp$lambda.min,
                                           lambda.1se = lasso_tmp$lambda.1se,
                                           lambda=lambdas,
                                           metric=auc_scores)
      
    }
    auc_lasso <- do.call(rbind,inner_rep.out)
    
    # following yates 2022
    # lambda value at best mean AUC estimate across reps
    lambda_best_lasso <- auc_lasso %>% 
      dplyr::group_by(lambda) %>%  
      dplyr::summarise(metric = mean(metric)) %>% 
      dplyr::filter(metric == max(metric, na.rm = T)) %>% 
      dplyr::filter(lambda == min(lambda, na.rm = T)) %>% 
      dplyr::pull(lambda)
    
    # AUC scores for all reps at best lambda value
    auc_best_lasso <- auc_lasso %>% 
      dplyr::filter(lambda == lambda_best_lasso) %>%
      dplyr::arrange(lambda,inner_rep) %>% 
      dplyr::pull(metric)
    
    # now refit all the data 
    data_training <- analysis(data_inner) %>% sf::st_drop_geometry() 
    data_testing <- assessment(data_inner) %>% sf::st_drop_geometry() 
    y_train = data_training$y_obs %>%
      as.vector()
    X_train = data_training %>% 
      dplyr::select(all_of(covarsToFit)) %>%
      as.matrix()
    
    # scale predictors
    X_train <- scale(X_train, center = TRUE, scale = TRUE)
    
    X_train.mu<-attributes(X_train)$`scaled:center`
    X_train.sd<-attributes(X_train)$`scaled:scale`
    
    mod1<-glmnet(x = X_train, 
                 y = y_train, 
                 lambda = lambdas,
                 penalty.factor = pen_facts,
                 family = "binomial", 
                 alpha = 1, # 0 for ridge, 1 for lasso, 0.5 for elastic net 
                 standardize = FALSE, 
                 type.measure = "auc")
    
    beta.coefs<-mod1$beta %>% 
      as.matrix %>% 
      t %>% 
      as_tibble() %>% 
      dplyr::mutate(lambda = lambdas) %>% 
      tidyr::pivot_longer(!any_of(c("lambda","metric")), values_to = "estimate", names_to = "coefficient") %>% 
      dplyr::filter(estimate != 0 | coefficient == "cn") %>%
      dplyr::filter(lambda %in% c(lambda_best_lasso) ) %>%
      dplyr::mutate(holdout=hmethod,cv=cvmethod,
                    outer_rep=outer_set$id,outer_fold=outer_set$id2,
                    lambda_choice="best")
    
    alpha.coefs <- mod1$a0 %>% 
      as_tibble() %>% 
      dplyr::mutate(lambda = lambdas) %>% 
      rename(intercept=value) %>%
      tidyr::pivot_longer(!any_of(c("lambda","metric")), values_to = "estimate", names_to = "coefficient") %>% 
      dplyr::filter(estimate != 0 | coefficient == "cn") %>%
      dplyr::filter(lambda %in% c(lambda_best_lasso) ) %>%
      dplyr::mutate(holdout=hmethod,cv=cvmethod,
                    outer_rep=outer_set$id,outer_fold=outer_set$id2,
                    lambda_choice="best")
    
    coef.estimates <- bind_rows(alpha.coefs,beta.coefs)
    
    # get AUC on inner data
    
    # training data
    preds <- predict.glmnet(mod1,newx=X_train,type='response') %>% boot::inv.logit()
    # just best lambda
    in_output <- data.frame(testy = y_train, predyp= preds[,lambdas == lambda_best_lasso])
    trainroc <- pROC::roc(in_output$testy, in_output$predyp, direction = "<", quiet = TRUE)
    
    # test ROC
    # outer assessment 
    if(hmethod=='none'){
      testroc <- data.frame(auc=NA)
    } else if(hmethod!='non'){
      outer_assessment_x <- data_testing %>% dplyr::select(all_of(covarsToFit)) %>% as.matrix()
      
      X_test.scaled <- scale(outer_assessment_x, center = X_train.mu, scale = X_train.sd)
      X_test.df <- data.frame(X_test.scaled) %>% dplyr::select(all_of(covarsToFit)) %>% as.matrix()
      
      preds <- predict.glmnet(mod1,newx=X_test.df,type='response') %>% boot::inv.logit()
      out_output <- data.frame(testy = data_testing$y_obs, predyp= preds[,lambdas == lambda_best_lasso])
      testroc <- pROC::roc(out_output$testy, out_output$predyp, direction = "<", quiet = TRUE)
    }
    
    # + collect results into data frames -
    
    outer_rep.out[[r_i]] <- data.frame(hmethod = hmethod, cvmethod = cvmethod,
                                       outer_rep=outer_set$id,outer_fold=outer_set$id2,
                                       lambda.min = lambda_best_lasso,
                                       lambda.1se = NA,
                                       train_auc = trainroc$auc,
                                       test_auc = testroc$auc)
    outer_coefs.out[[r_i]] <- coef.estimates
    total_inner.cv[[r_i]] <- auc_lasso
    
  }
  
  outer_cv <- do.call(rbind,outer_rep.out)
  outer_coefs <- do.call(rbind,outer_coefs.out)
  inner_cv <- do.call(rbind,total_inner.cv)
  
  return(list(outer_cv.results = outer_cv,
              outer_cv.coefs = outer_coefs,
              inner_cv.results = inner_cv,
              data = nested_splits
  ))
  
}

#}

#  data_inner <- outer_split(df = df_split)

# Assign folds and reps here -
#  data_cv <- assign_data_cv(x = data_inner, cv_method = cvmethod, n_folds = 10, n_repeats = 2)



# if(cvmethod!="loo"){
#   
#   reps <- data_cv$id %>% unique
#   rep.out<-list()
#   for(i in 1:length(reps)){
#     data_model <- data_cv %>%
#       dplyr::filter(id==reps[i])
#     
#     # get data in 
#     data_folds<-dplyr::bind_rows(lapply(data_model$splits,assessment),.id='fold')
#     
#     # if this was a spatial split, drop geometry
#     if("sf" %in% class(data_folds)){
#       data_folds <- data_folds %>%
#         sf::st_drop_geometry() 
#     }
#     
#     fold_vec = data_folds$fold %>%
#       as.integer()
#     y = data_folds$y_obs %>%
#       as.vector()
#     X = data_folds %>% 
#       dplyr::select(all_of(covarsToFit)) %>%
#       as.matrix()
#     
#     # model with regularization via LASSO, following implementation in Tredennick et al. 2021
#     lasso_tmp <- cv.glmnet(x = X, 
#                            y = y, 
#                            lambda = lambdas,
#                            penalty.factor = pen_facts,
#                            family = "binomial", 
#                            alpha = 1, # 0 for ridge, 1 for lasso, 0.5 for elastic net 
#                            standardize = FALSE, 
#                            type.measure = "auc",
#                            foldid = fold_vec)
#     
#     for(j in 1:length(unique(fold_vec))){
#       
#       train_dat = data_folds[data_folds$fold!=j,]
#       test_dat = data_folds[data_folds$fold==j,]
#       
#       y = train_dat$y_obs %>%
#         as.vector()
#       X = train_dat %>% 
#         dplyr::select(all_of(covarsToFit)) %>%
#         as.matrix()
#       
#       X <- scale(X, center = TRUE, scale = TRUE)
#       
#       lasso_mod <- glmnet(x = X, 
#                           y = y, 
#                           lambda = lambdas,
#                           penalty.factor = pen_facts,
#                           family = "binomial", 
#                           alpha = 1, # 0 for ridge, 1 for lasso, 0.5 for elastic net 
#                           standardize = FALSE)
#       
#       #select(-y) %>% as.matrix()
#       preds <- predict.glmnet(lasso_mod,newx=X,type='response') %>% boot::inv.logit()
#       in_output <- data.frame(predyp = preds)
#       trainroc<-c()
#       for(k in 1:ncol(in_output)){
#         trainroc<-c(trainroc, pROC::roc(y, preds[,k], direction = "<", quiet = TRUE)$auc)
#       }
#       
#       # test
#       test_dat = data_folds[data_folds$fold==j,]
#       
#       y_test = test_dat$y_obs %>%
#         as.vector()
#       X_test = test_dat %>% 
#         dplyr::select(all_of(covarsToFit)) %>%
#         as.matrix()
#       
#       X.mu<-attributes(X)$`scaled:center`
#       X.sd<-attributes(X)$`scaled:scale`
#       
#       X_test_scaled <- scale(X_test, center = X.mu, scale = X.sd)
#       X_test_scaled <- data.frame(X_test_scaled)%>% select(all_of(covarsToFit)) %>% as.matrix()
#       
#       
#       preds <- predict.glmnet(lasso_mod,newx=X_test_scaled,type='response') %>% boot::inv.logit()
#       in_output <- data.frame(predyp = preds)
#       testroc<-c()
#       for(k in 1:ncol(in_output)){
#         testroc<-c(testroc, pROC::roc(y_test, preds[,k], direction = "<", quiet = TRUE)$auc)
#       }
#       
#       plot(lambdas,trainroc);
#       plot(lambdas,testroc)
#       
#     }
#     
#     # Collect results into data frames -
#     
#     lambdas <- lasso_tmp$lambda
#     auc_scores <- lasso_tmp$cvm
#     
#     rep.out[[i]] <- data.frame(rep=reps[i],lambda=lambdas,metric=auc_scores)
#     
#   }
#   auc_lasso<-do.call(rbind,rep.out)
#   
#   # following yates 2022
#   # lambda value at best mean MCC estimate
#   lambda_best_lasso <- auc_lasso %>% 
#     dplyr::group_by(lambda) %>%  
#     dplyr::summarise(metric = mean(metric)) %>% 
#     dplyr::filter(metric == max(metric, na.rm = T)) %>% 
#     dplyr::pull(lambda)
#   
#   # AUC scores for all reps at best lambda value
#   auc_best_lasso <- auc_lasso %>% 
#     dplyr::filter(lambda == lambda_best_lasso) %>%
#     dplyr::arrange(lambda,rep) %>% 
#     dplyr::pull(metric)
#   
#   # compute modified standard errors
#   # metric_plot_data <- auc_lasso %>% 
#   #   group_by(lambda) %>% 
#   #   arrange(lambda,rep) %>%
#   #   summarise(se_ose = sd(metric),
#   #             se_diff  = sd(metric - auc_best_lasso),
#   #             se_best = sd(auc_best_lasso),
#   #             rho_best_m = (se_diff^2 - se_ose^2 - se_best^2)/(-2*se_ose*se_best),
#   #             se_mod = sqrt(1-(rho_best_m))*se_best,
#   #             metric_diff = mean(metric - auc_best_lasso),
#   #             metric = mean(metric))
#   # 
#   # metric_mod_ose <- metric_plot_data %>% 
#   #   filter(metric + se_mod > max(metric, na.rm = T)) %>% 
#   #   filter(lambda == max(lambda))
#   # 
#   # metric_ose <- metric_plot_data %>% 
#   #   filter(se_ose >= abs(metric_diff)) %>% 
#   #   filter(lambda == max(lambda))
#   
#   # now refit all the data 
#   fit_glmnet <- glmnet(x = data_inner$covariates_scaled %>% as.data.frame() %>% dplyr::select(all_of(covarsToFit)) %>% makeX(),
#                        y = data_inner$outcome,
#                        family = "binomial",
#                        lambda = lambdas,
#                        alpha = 1)
#   
#   out.obj1<-fit_glmnet$beta %>% 
#     as.matrix %>% 
#     t %>% 
#     as_tibble() %>% 
#     dplyr::mutate(lambda = lambdas) %>% 
#     tidyr::pivot_longer(!any_of(c("lambda","metric")), values_to = "estimate", names_to = "coefficient") %>% 
#     dplyr::filter(estimate != 0 | coefficient == "cn") %>%
#     dplyr::filter(lambda==lambda_best_lasso ) %>%
#     dplyr::mutate(holdout=hmethod,cv=cvmethod,lambda_choice="best")
#   
#   # get AUC on inner data
#   # train ROC
#   # outer assessment 
#   inner_analysis_x <- data_inner$covariates_scaled %>% as.data.frame() %>% dplyr::select(all_of(covarsToFit)) %>% makeX()
#   
#   X.mu<-attributes(X)$`scaled:center`
#   X.sd<-attributes(X)$`scaled:scale`
#   
#   X.new.scaled <- scale(inner_analysis_x, center = X.mu, scale = X.sd)
#   X.new.df <- data.frame(X.new.scaled)%>% select(all_of(covarsToFit)) %>% as.matrix()
#   
#   #select(-y) %>% as.matrix()
#   preds <- predict.glmnet(mod1,newx=X.new.df,type='response') %>% boot::inv.logit()
#   in_output <- data.frame(testy = inner_analysis$Occurrence_allSurvey_binary, predyp= preds[,1])
#   trainroc <- pROC::roc(in_output$testy, in_output$predyp, direction = "<", quiet = TRUE)
#   
#   
#   # get AUC on outer data
#   
#   # out.obj2<-fit_glmnet$beta %>%
#   #   as.matrix %>%
#   #   t %>%
#   #   as_tibble() %>%
#   #   mutate(lambda = lambdas) %>%
#   #   pivot_longer(!any_of(c("lambda","metric")), values_to = "estimate", names_to = "coefficient") %>%
#   #   filter(estimate != 0 | coefficient == "cn") %>%
#   #   dplyr::filter(lambda==metric_mod_ose$lambda ) %>%
#   #   dplyr::mutate(holdout=hmethod,cv=cvmethod,lambda_choice="mod_ose")
#   
#   return(list(out.obj1,auc_best_lasso,fit_glmnet,data_cv))
#   
#   # + LOO -
#   
# } else if(cvmethod=="loo"){
#   
#   # # seedingOutcomeObs <- readRDS(here::here("03_outputs","06_modelData","achy-outcomes.RDS"))
#   # # df <- append_covariates(df = seedingOutcomeObs, covariates = "longtermclimate")
#   # # df_split <- hold_out_data(df = df,holdout_method=hmethod,p=0.20)
#   # 
#   # + Assign folds and reps here -
#   # data_cv <- assign_data_cv(x = data_inner, cv_method = cvmethod)
#   # covarsToFit <- colnames(df_split)[!(colnames(df_split)%in%c("model_set","species",
#   #                                                             "Trt_ID","locationID",
#   #                                                             "sourceID","index",'yearFire',
#   #                                                             "yearSeeding",'timeFireSeeding',
#   #                                                             "Occurrence_allSurvey_binary"))]
#   # covarsToFit<-covarsToFit[grep("min|max",covarsToFit,invert=TRUE)]
#   
#   data_model <- data_cv
#   
#   # get data in
#   data_folds<-bind_rows(lapply(data_model$splits,assessment),.id='fold')
#   fold_vec = data_folds$fold %>%
#     as.integer()
#   y = data_folds$y_obs %>%
#     as.vector()
#   X = data_folds %>%
#     dplyr::select(all_of(covarsToFit)) %>%
#     as.matrix()
#   # model with regularization via LASSO, following implementation in Tredennick et al. 2021
#   
#   # pen_facts <- rep(1, ncol(X)) # penalize all covariates
#   # lambdas <- 10^seq(0, -4, by = -.01) # sequence of penalties to test
#   # lambdas <- seq(-1.6,-10, length.out = 100)
#   
#   lasso_preds <- matrix(NA,nrow=length(fold_vec),ncol=length(lambdas))
#   for(i in 1:length(fold_vec)){
#     lasso_tmp <- glmnet(x = X[-i,],
#                         y = y[-i],
#                         lambda = lambdas,
#                         penalty.factor = pen_facts,
#                         family = "binomial",
#                         alpha = 1, # 0 for ridge, 1 for lasso, 0.5 for elastic net
#                         standardize = FALSE)
#     lasso_preds[i,] <- predict(lasso_tmp,newx=X[i,],type='response')
#   }
#   
#   auc_scores=c()
#   for(j in 1:length(lambdas)){
#     auc_scores[j]=auc.glmnet(y,lasso_preds[,j])
#   }
#   # plot(lambdas,aucEst)
#   
#   # + Collect results into data frames -
#   # lambdas <- lasso_tmp$lambda
#   # auc_scores <- lasso_tmp$cvm
#   
#   df.out <- data.frame(rep=1,lambda=lambdas,metric=auc_scores)
#   
#   auc_lasso<-df.out
#   
#   # QUESTION: I currently calculate AUC over all observations
#   # this means that I don't have estimates of AUC for multiple folds
#   # so the 'mean' lambda is the overall lambda and I don't estimate SEs below yet
#   # following yates 2022
#   # lambda value at best mean MCC estimate
#   lambda_best_lasso <- auc_lasso %>% 
#     group_by(lambda) %>%  
#     summarise(metric = mean(metric)) %>% 
#     filter(metric == max(metric, na.rm = T)) %>% 
#     filter(lambda == min(lambda, na.rm = T)) %>%
#     pull(lambda)
#   
#   # AUC scores for all reps at best lambda value
#   auc_best_lasso <- auc_lasso %>% 
#     filter(lambda == lambda_best_lasso) %>%
#     arrange(lambda,rep) %>% pull(metric)
#   
#   # compute modified standard errors
#   # metric_plot_data <- auc_lasso %>% 
#   #   group_by(lambda) %>% 
#   #   arrange(lambda,rep) %>%
#   #   summarise(se_ose = sd(metric),
#   #             se_diff  = sd(metric - auc_best_lasso),
#   #             se_best = sd(auc_best_lasso),
#   #             rho_best_m = (se_diff^2 - se_ose^2 - se_best^2)/(-2*se_ose*se_best),
#   #             se_mod = sqrt(1-(rho_best_m))*se_best,
#   #             metric_diff = mean(metric - auc_best_lasso),
#   #             metric = mean(metric))
#   # 
#   # metric_mod_ose <- metric_plot_data %>% 
#   #   filter(metric + se_mod > max(metric, na.rm = T)) %>% 
#   #   filter(lambda == max(lambda))
#   # 
#   # metric_ose <- metric_plot_data %>% 
#   #   filter(se_ose >= abs(metric_diff)) %>% 
#   #   filter(lambda == max(lambda))
#   
#   # now refit all the data 
#   fit_glmnet <- glmnet(x = data_inner$covariates_scaled %>% 
#                          as.data.frame() %>% 
#                          dplyr::select(all_of(covarsToFit)) %>% 
#                          makeX(),
#                        y = data_inner$outcome,
#                        family = "binomial",
#                        lambda = lambdas,
#                        alpha = 1)
#   
#   out.obj1<-fit_glmnet$beta %>% 
#     as.matrix %>% 
#     t %>% 
#     as_tibble() %>% 
#     mutate(lambda = lambdas) %>% 
#     pivot_longer(!any_of(c("lambda","metric")), values_to = "estimate", names_to = "coefficient") %>% 
#     filter(estimate != 0 | coefficient == "cn") %>%
#     dplyr::filter(lambda==lambda_best_lasso ) %>%
#     dplyr::mutate(holdout=hmethod,cv=cvmethod,lambda_choice="best")
#   
#   # out.obj2<-fit_glmnet$beta %>% 
#   #   as.matrix %>% 
#   #   t %>% 
#   #   as_tibble() %>% 
#   #   mutate(lambda = lambdas) %>% 
#   #   pivot_longer(!any_of(c("lambda","metric")), values_to = "estimate", names_to = "coefficient") %>% 
#   #   filter(estimate != 0 | coefficient == "cn") %>%
#   #   dplyr::filter(lambda==metric_mod_ose$lambda ) %>%
#   #   dplyr::mutate(holdout=hmethod,cv=cvmethod,lambda_choice="mod_ose")
#   
#   return(list(out.obj1,auc_best_lasso,fit_glmnet,data_cv))
# }