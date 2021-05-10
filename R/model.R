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

# FOR SARA: PLEASE CHECK 
# if you want to exclude  some variable in the ML algorithm, you can 
# change the content of excluded_cols_base, for the Liguria case or the Sicilia-Puglia case,
# respectively. 
  if(is_Liguria){              #database liguria non ha veg, ha veg_type... nel dubbio elimino anche il veg. 
    excluded_cols_base         <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq_new") 
    excluded_cols_base_rf      <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq_new")
    excluded_cols_base_ruf     <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq_new")
    excluded_cols_base_rf_nop  <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq_new", perc_cols)
    excluded_cols_base_ruf_nop <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq_new", perc_cols)
    excluded_cols_base_mlp     <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type","veg", "veg_freq_new") 
    excluded_cols_base_svm     <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type","veg", "veg_freq_new") 
  }else if(is_Liguria_s | is_Sicilia_s|is_Liguria_Sicilia){
    excluded_cols_base         <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq")
    excluded_cols_base_rf      <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq")
    excluded_cols_base_rf_xy   <- c("row", "col", "box", "veg_agg", "veg_type", "veg_freq")
    excluded_cols_base_ruf     <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq")
    # this one below is "std_"
    excluded_cols_base_rf_nop  <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq", perc_cols)
    # this one below is "std_xy_"
    excluded_cols_base_rf_xy_nop <- c("row", "col", "box", "veg_agg", "veg_type", "veg_freq", perc_cols)
    excluded_cols_base_ruf_nop <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg_freq", perc_cols) 
    excluded_cols_base_mlp     <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg",     "veg_freq") 
    excluded_cols_base_svm     <- c("row", "col", "x", "y", "box", "veg_agg", "veg_type", "veg",     "veg_freq")
    
  }else if(is_Bulgaria){
    excluded_cols_base_rf_bg           <- c("point_index", "row", "col", "box", "veg_mask", "veg_freq", perc_cols)
    excluded_cols_base_no_clim_rf_bg   <- c("point_index", "prec", "temp", "row", "col", "box", "veg_mask", "veg_freq", perc_cols)
    excluded_cols_base_xy_rf_bg        <- c('x', 'y', "point_index", "row", "col", "box", "veg_mask", "veg_freq", perc_cols)
    excluded_cols_base_nothing_rf_bg   <- c('x', 'y', "point_index", "prec", "temp", "row", "col", "box", "veg_mask", "veg_freq", perc_cols)
    
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



if(batch_test == TRUE){
  results_list = list()
  nfold_l = c(5)#c(1,5)
  algo_l = c("randomForest") #c("randomForest","svm","mlp_rminer")
  season_l = c(2)#c(1,2)
  type = "nothing" #can be 'std', 'perc', 'xy' or 'xy_perc + % rows if not 99.99% 
  
  prod = product(nfolds= nfold_l, algo = algo_l, season= season_l)
  
  it <- ihasNext(prod)
  it_no = 1 
  while (hasNext(it)){
    param <- nextElem(it)
    #cat(sprintf('a = %d, b = %d\n', x$a, x$b))}   name <- 'ruf_onefold_std_s'
    name_string = paste( param$algo,
                 param$nfolds%>%as.character() ,"fold",
                 type,"sea",
                 param$season%>%as.character(), sep="_")  #type: std --> perc
    # for liguria (with  no neighbouring) just two options:
    # to use vegetation like in rf and ruf, or to use
    # neighboring as vegetation (mlp and svm)
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
      experiment_t =   do_experiment(
      points_df, fires_df, my_excluded_cols, param$season, 
      year_from, year_test, box_dimension, param$nfolds,
      mtry, ntree, nodesize, name_string, resolution,param$algo, name_string
    ) 
    #assign(name_string,experiment_t)
    #eval(parse(text =" paste(risultati[it_no] <- name_string    )"))
    results_list[it_no]  <- experiment_t 
    it_no <- it_no+1
  }  
  
  
}else{









# ------------------------------------------------------------------------ #
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
name <- 'ruf_nop_onefold_std_s'
#ruf_nop_onefold_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_ruf_nop, season, 
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

algo="rf"
name <- 'rf_onefold_std_s'
#rf_onefold_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_rf, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
#)



name <- 'rf_nop_onefold_std_s'

#rf_nop_onefold_std_s <- do_experiment(
#  points_df, fires_df,excluded_cols_base_rf_nop, season, #points_df
#    year_from, year_test, box_dimension, nfolds,
#    mtry, ntree, nodesize, name, resolution,algo
#)

algo="mlp"
name <- 'mlp_onefold_std_s'
#mlp_onefold_std_s <- do_experiment(
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
#  mtry, ntree, nodesize, name, resolution
#)---------------
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
algo = "svm"

#onefold_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_svm, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, 
#  ntree, nodesize, name, resolution, algo
#)



#-------------------------------------------------------------------------------
nfolds <- 5# debug 
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

algo="ruf"
name <- 'ruf_fivefolds_std_s'
#ruf_fivefolds_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_ruf, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
#)
name <- 'ruf_nop_fivefolds_std_s'
#ruf_nop_fivefolds_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_ruf_nop, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
#)


algo="svm"
name <- 'svm_fivefolds_std_s'

#svm_fivefolds_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_svm, season, #points_df
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
#)

algo = "mlp_rminer"
name <- 'mlpr_fivefolds_std_s'
#mlpr_fivefolds_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_mlp, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
#)

algo="randomForest"
name <- 'rf_fivefolds_std_s'
rf_fivefolds_std_s <- do_experiment(
  points_df, fires_df, excluded_cols_base_rf, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution,algo
)



name <- 'rf_nop_fivefolds_std_s'
#rf_nop_fivefolds_std_s <- do_experiment(
#  points_df, fires_df,excluded_cols_base_rf_nop, season, #points_df
#    year_from, year_test, box_dimension, nfolds,
#    mtry, ntree, nodesize, name, resolution,algo
#)

algo="mlp"
name <- 'mlp_fivefolds_std_s'
#mlp_fivefolds_std_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_mlp, season, 
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo
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
#
#AT: HERE I PUT THE NEWEST ALGORITHMS...
#
#
#kernel support vector machine
season <- 2
name <- 'onefold_ksvm_s'
nfolds <- 1
algo = 'svm'
#FR mangia matrici, mentre l'altro df

#onefold_ksvm_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_svm, season, #stagione sempre 2
#  year_from, year_test, box_dimension, nfolds,
#  mtry, ntree, nodesize, name, resolution,algo#tipo di algoritmo
#)
#...gli mangia tutto 



season <- 2
name <- 'onefold_mlp_s'#multi layer perceptron
nfolds <- 1
algo <- "mlp"
#onefold_mlp_s <- do_experiment(
#  points_df, fires_df, excluded_cols_base_mlp, season, 
#  year_from, year_test, box_dimension, nfolds,
 # mtry, ntree, nodesize, name, resolution,algo
#)


}

##################################################
##################################################
#   Saving the data inside of Rdata files    #####
#
#           Insert just the experiments
#           you want to save  for
#           further R  reanalyses.               
##################################################

if(batch_test == TRUE){
  #save list
  save(results_list, file = data_file)
  
}else{
save(
  #onefold_std_w,
  #onefold_perc_w,
  # onefold_freq_w,
  #fivefolds_std_w,
  #fivefolds_perc_w,
  # fivefolds_freq_w,
  #ninefolds_perc_w,
  
  #onefold_std_s,
  #onefold_perc_s,
  # onefold_freq_s,
  #ruf_fivefolds_std_s,
  #ruf_nop_fivefolds_std_s,
  #svm_fivefolds_std_s,
  #mlpr_fivefolds_std_s,
  rf_fivefolds_std_s,
  #rf_nop_fivefolds_std_s,
  #mlp_fivefolds_std_s,
  #fivefolds_perc_s,
  # fivefolds_freq_s,
  #ninefolds_perc_s,
  #onefold_ksvm_s,
  #onefold_mlp_s,
  #ruf_onefold_std_s,
  #ruf_nop_onefold_std_s,
  #svm_onefold_std_s,
  #mlpr_onefold_std_s,
  #rf_onefold_std_s,
  #rf_nop_onefold_std_s,
  #mlp_onefold_std_s,

  file = data_file
)

}
