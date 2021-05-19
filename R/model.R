# In  this R file, it is possible to:
# - define the variables that are object of the ML algorithms. 
# - define the ML models: with or without neighboring effects,
#   specifying different spatal cross validation (1, 5, 9 )-fold cross validation, and 
#   the season that will be investigated (winter, summer)


# the file where the output in  R format (XXXXXX.RData) will be saved is here defined.
data_file <- '{output_dir}/RF_{year_from}_{year_test - 1}.RData' %>% g

# the input factor names are given by the names() of the dataframe of points points_df. 
# among  them, the ones representing convoluted information (information on neighbours) are
# represented by the field "perc_cols" 
all_cols <- names(points_df)
perc_cols <- all_cols %>% subset(all_cols %>% startsWith("perc_"))

# exclude some variable in the ML algorithm. 

if(is_Bulgaria){
    excluded_cols_base_rf_bg           <- c("point_index", "row", "col", "box", "veg_mask", "veg_freq", perc_cols)
    excluded_cols_base_no_clim_rf_bg   <- c("point_index", "prec", "temp", "row", "col", "box", "veg_mask", "veg_freq", perc_cols)
    excluded_cols_base_xy_rf_bg        <- c('x', 'y', "point_index", "row", "col", "box", "veg_mask", "veg_freq", perc_cols)                  # coords are excluded
    excluded_cols_base_nothing_rf_bg   <- c('x', 'y', "point_index", "prec", "temp", "row", "col", "box", "veg_mask", "veg_freq", perc_cols)  # both coords and climate are excluded
    
  }else{
    excluded_cols_base         <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq")
    excluded_cols_base_rf      <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq")
    excluded_cols_base_ruf     <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq")
    excluded_cols_base_rf_nop  <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq", perc_cols) 
    excluded_cols_base_ruf_nop <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq", perc_cols) 
    excluded_cols_base_mlp     <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg",     "veg_freq") 
    excluded_cols_base_svm     <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg",     "veg_freq")
    #excluded_cols_base        <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq")
}


# for every fold number, we can consider or exclude the neighbourhood (perc_cols) in the variable selection. 

# No perc (nop) => test the model without the neighbours effect. (it is referenced to as "STANDARD MODEL")
# Perc => test the model with the neighbours effect  

# for Bulgaria perc_cols are always ecluded


#  HOW TO BUILD THE NAME OF THE MODEL FOR LABELING PURPOSES

#  {onefold, fivefolds, ninefolds}_{std,perc}_{w,s}
#  
#  where:
#  - one, five, or nine  folds is the number of folds in the cross validation process.
#  - std is the standard model without neighbouring effects.
#  - perc stands for the model with neighbours effect. 
#  - w stands for winter (stagione (season) = 1  ) and s for summer (stagione (season) = 2) 
#  
# If you want to remember a single model test, it is  a good practice to change the following  'type' parameter, e.g. 
# type = 'my_personal_experiment'


if(batch_test == TRUE){
  results_list = list()
  nfold_l = c(5)             # c(1,5)
  algo_l = c("randomForest") # c("randomForest","svm","mlp_rminer")
  season_l = c(2)            # c(1,2)
  type = "nothing"           # can be 'std', 'perc', or a custom name
  
  prod = product(nfolds= nfold_l, algo = algo_l, season= season_l)
  
  it <- ihasNext(prod)
  it_no = 1 
  while (hasNext(it)){
    param <- nextElem(it)
    # cat(sprintf('a = %d, b = %d\n', x$a, x$b))}   name <- 'ruf_onefold_std_s'
    name_string = paste( param$algo,
                 param$nfolds%>%as.character() ,"fold",
                 type,"sea",
                 param$season%>%as.character(), sep="_")  

    if(param$algo %in% c("randomForest", "ruf")){
      
    my_excluded_cols = excluded_cols_base_nothing_rf_bg  # excluded_cols_base_rf_bg and excluded_cols_base_xy_rf_bg if xy are excluded 
    
    }else{
    my_excluded_cols = excluded_cols_base_mlp
    } 
      print("algo is ...")
      print(param$algo)
      print("nfold is ")
      print(param$nfolds)
      print("season is...")
      print(param$season)
      print("name is...")
      print(name_string)
      # apply the model defined in model_functions.R
      experiment_t =   do_experiment(
      points_df, fires_df, my_excluded_cols, param$season, 
      year_from, year_test, box_dimension, param$nfolds,
      mtry, ntree, nodesize, name_string, resolution,param$algo, name_string
    ) 
    # assign(name_string,experiment_t)
    #eval(parse(text =" paste(risultati[it_no] <- name_string    )"))
    results_list[it_no]  <- experiment_t 
    it_no <- it_no+1
  }  
  
# some personalized configuration (not used for Bulgaria)
}else{


nfolds <- 1
#-------------------------------------------------------------------------------
# perc
# select columns
# 


season <- 2
name <- 'onefolds_std_w'

#onefold_std_w <- do_experiment(
# points_df, fires_df, excluded_cols, season, 
# year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution
#)

season <- 2

algo="ruf"
name <- 'ruf_onefold_std_s'
#ruf_onefold_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_ruf, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
#)


algo="svm"
name <- 'svm_onefold_std_s'

#svm_onefold_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_svm, season, #points_df
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
#  )

algo = "mlp_rminer"
name <- 'mlpr_onefold_std_s'
#mlpr_onefold_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_mlp, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
#)


# 
#-------------------------------------------------------------------------------
# no perc
# select columns

excluded_cols <- excluded_cols_base

season <- 1
name <- 'fivefolds_perc_w'

#fivefolds_perc_w <- do_experiment(
# points_df, fires_df, excluded_cols, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution
#)

season <- 2
name <- 'fivefolds_perc_s'
#fivefolds_perc_s <- do_experiment(
#  points_df, fires_df, excluded_cols, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution)


}

# Saving the data inside of Rdata files

# Insert just the experiments
# you want to save  for
# further R  reanalyses.               


if(batch_test == TRUE){
  # save list
  save(results_list, file = data_file)
  
}else{
# exapmpe for saving a different configuration
save(

  rf_fivefolds_std_s,

  file = data_file
)

}
