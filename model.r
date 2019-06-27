source('R/model_functions.R')

resolution = 100
box_dimension <- 15000
ntree <- 750
nodesize <- 50

year_from <- 1996
year_test <- 2016


# ------------------------------------------------------------------------ #
# load data
data_dir = 'data_{resolution}m' %>% g
points_df <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
fires_df <- read.csv('{data_dir}/fires.csv' %>% g)
fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
fires_df$month = month(fires_df$data)


all_cols <- names(points_df)
perc_cols <- all_cols %>% subset(all_cols %>% startsWith("perc_"))

nfolds <- 1

#-------------------------------------------------------------------------------
# no perc, no veg_freq
# select columns
excluded_cols <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq")
excluded_cols <- c(excluded_cols, perc_cols)

season <- 1
name <- 'onefold_std_w'

onefold_std_w <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)

season <- 2
name <- 'onefold_std_s'
onefold_std_s <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)

#-------------------------------------------------------------------------------
# perc, no veg_freq
# select columns
excluded_cols <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq")

season <- 1
name <- 'onefold_perc_w'

onefold_perc_w <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)

season <- 2
name <- 'onefold_perc_s'
onefold_perc_s <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)

#-------------------------------------------------------------------------------
# no perc, veg_freq
# select columns
excluded_cols <- c("row", "col", "x", "y", "box", "veg_agg", "veg")
excluded_cols <- c(excluded_cols, perc_cols)

season <- 1
name <- 'onefold_freq_w'

onefold_freq_w <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)


season <- 2
name <- 'onefold_freq_s'
onefold_freq_s <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)


#-------------------------------------------------------------------------------
nfolds <- 5
#-------------------------------------------------------------------------------
# no perc, no veg_freq
# select columns
excluded_cols <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq")
excluded_cols <- c(excluded_cols, perc_cols)

season <- 1
name <- 'fivefolds_std_w'

fivefolds_std_w <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)

season <- 2
name <- 'fivefolds_std_s'
fivefolds_std_s <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)


#-------------------------------------------------------------------------------
# no perc, no veg_freq
# select columns
excluded_cols <- c("row", "col", "x", "y", "box", "veg_agg", "veg", "veg_freq")

season <- 1
name <- 'fivefolds_perc_w'

fivefolds_perc_w <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)

season <- 2
name <- 'fivefolds_perc_s'
fivefolds_perc_s <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)

#-------------------------------------------------------------------------------
# no perc, no veg_freq
# select columns
excluded_cols <- c("row", "col", "x", "y", "box", "veg_agg", "veg")
excluded_cols <- c(excluded_cols, perc_cols)

season <- 1
name <- 'fivefolds_freq_w'

fivefolds_freq_w <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)


season <- 2
name <- 'fivefolds_freq_s'
fivefolds_freq_s <- do_experiment(
  points_df, fires_df, excluded_cols, season, 
  year_from, year_test, box_dimension, nfolds,
  mtry, ntree, nodesize, name, resolution
)

