# model.R. In  this R file, we can 
# - define the variable that are object of the  ML algorithms. 
# - define the ML models that will be tried out: with or without neighboring effect,
#   specifying different spatal cross validation (1, 5, 9 )-fold cross validation, and 
#   the season that will be investigated (winter, summer)


# 
# the file where the output in  R format (XXXXXX.RData) will be saved is here defined.
data_file <- '{output_dir}/RF_{year_from}_{year_test - 1}.RData' %>% g

#the input factor names are given by the names() of the dataframe of points points_df. 
# among  them, the ones representing convoluted information (information on neighbours) are
# represented by the field "perc_cols" 
all_cols <- names(points_df)
perc_cols <- all_cols %>% subset(all_cols %>% startsWith("perc_"))


# if you want to exclude  some variable in the ML algorithm, you can 
# change the content of excluded_cols_base, for the Liguria case or the Sicilia-Puglia case,
# respectively. 
if(is_Liguria){
excluded_cols_base <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq")
}else{
excluded_cols_base <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq")
}

# from here onwards, we try different folding cross validation  nfolds = 1, 5 or 9.
# for every fold number, we can consider or exclude the neighbourhood (perc_cols) in the variable selection. 
# The variables  "row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq" must be always discarded for Sicilia and Puglia.
# For Liguria, the variable "veg" must be discarded instead of veg_type 
# 
# 
# 
# No perc => test the model without the neighbours effect. (it is referenced to as "STANDARD MODEL")
# Perc => test the model with the neighbours effect  
# 
#  HOW TO BUILD THE NAME OF THE MODEL FOR LABELING PURPOSES
# 
#  {onefold, fivefolds, ninefolds}_{std,perc}_{w,s}
#  
#  where:
#  - one, five, or nine  folds is the number of folds in the cross validation process.
#  - std is the standard model without neighbouring effects.
#  - perc stands for the model with neighbours effect. 
#  - w stands for winter (stagione (season)   = 1  ) and s for summer (stagione (season)   = 2) 
#  
# If you want to remember a single model test, it is  a good practice to change   the first part of  the name, e.g. 
# name <- 'my_experiment_onefold_std_w'
#  
# ------------------------------------------------------------------------ #
nfolds <- 1

#-------------------------------------------------------------------------------
# no perc
# select columns


excluded_cols <- c(excluded_cols_base, perc_cols)

season <- 1
name <- 'onefold_std_w'

#onefold_std_w <- do_experiment(
#  points_df, fires_df, excluded_cols, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution
#)

season <- 2
name <- 'onefold_std_s'
#onefold_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution
#)

#-------------------------------------------------------------------------------
# perc
# select columns
# 
# 
excluded_cols <- excluded_cols_base

season <- 1
name <- 'onefold_perc_w'

#onefold_perc_w <- do_experiment(
 # points_df, fires_df, excluded_cols, season, 
  #year_from, year_test, box_dimension, nfolds,
  #mtry, ntree, nodesize, name, resolution
#)

season <- 2
name <- 'onefold_perc_s'
onefold_perc_s <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)


#-------------------------------------------------------------------------------
nfolds <- 5
#-------------------------------------------------------------------------------
# no perc
# select columns
#
excluded_cols <- c(excluded_cols_base, perc_cols)

season <- 1
name <- 'fivefolds_std_w'

#fivefolds_std_w <- do_experiment(
 # points_df, fires_df, excluded_cols, season, 
 # year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution
#)

season <- 2
name <- 'fivefolds_std_s'
#fivefolds_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution
#)


#-------------------------------------------------------------------------------
# perc
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
#  mtry, ntree, nodesize, name, resolution
#)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# perc 
# select columns
# 
nfolds <- 9
excluded_cols <- excluded_cols_base

season <- 1
name <- 'ninefolds_perc_w'

#ninefolds_perc_w <- do_experiment(
#  points_df, fires_df, excluded_cols, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution
#)

season <- 2
name <- 'ninefolds_perc_s'
#ninefolds_perc_s <- do_experiment(
#  points_df, fires_df, excluded_cols, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution
#)

##################################################
##################################################
#   Saving the data inside of Rdata files    #####
#
#           Insert just the experiments
#           you want to save  for
#           further R  reanalyses.               
##################################################

save(
  #onefold_std_w,
  #onefold_perc_w,
  # onefold_freq_w,
  #fivefolds_std_w,
  #fivefolds_perc_w,
  # fivefolds_freq_w,
  #ninefolds_perc_w,
  
  #onefold_std_s,
  onefold_perc_s,
  # onefold_freq_s,
  #fivefolds_std_s,
  #fivefolds_perc_s,
  # fivefolds_freq_s,
  #ninefolds_perc_s,

  file = data_file
)

