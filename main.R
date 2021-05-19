# load model functions
source('R/model_functions.R')

# load analysis functions
source('R/analysis_functions.R')

# select the region 
is_Bulgaria <- T
region_name = 'Bulgaria'


# Spatial resolution and cross validation resolution

# the spatial resolution  of the map
resolution = 500

# The box dimension is the side of the squares ([m]) in which the spatial domain 
# is discretized before regrouping such boxes into a number of folds for the cross validation. 
box_dimension <- 15000

# parameters for Random Forest

# ntree  is the number of the trees used by the RandomForest algorithm. default = 750
ntree <- 750 

# nodesize is Minimum size of terminal nodes. 
# Setting this number larger causes smaller trees to be grown (and thus take less time).
# Setting this number larger may affect overall accuracy of the method. default = 50
nodesize <- 50

# advanced settings for specific tests

#set use clustering
user_clustering=FALSE   # not used for Bulgaria

# set use of batch list of experiment, allow to change a lot of input parameters for extensive
# experiment creation
batch_test = TRUE

# loading  the data
# define the data directory then read the geographical and fire occurrence databases 
  
if (is_Bulgaria) {
  data_dir = '{region_name}' %>% g
  # read the geographical points info (points.csv) and the fire info (fires.csv)
  # dataset for extracting the training dataset
  points_df <- read.csv('/home/gruppo4/Bulgaria/bg_dati_per_cluster/points_train_2.csv' %>% g, row.names="point_index")
  # the entire geographical dataset
  all_points_df <- read.csv('/home/gruppo4/Bulgaria/bg_dati_per_cluster/all_points_2.csv' %>% g, row.names="point_index")
  points_df$veg <- as.factor(points_df$veg)
  points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
  all_points_df$veg <- as.factor(all_points_df$veg)
  all_points_df <- SpatialPointsDataFrame(all_points_df[c("x", "y")], all_points_df)
  # fires dataset
  fires_df <- read.csv('/home/gruppo4/Bulgaria/bg_dati_per_cluster/fires_no_park_2.csv' %>% g)
  fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
  fires_df$anno = fires_df$random_yea
  fires_df$anno <- as.numeric(fires_df$anno)
  fires_df$year = fires_df$random_yea
  fires_df$year <- as.numeric(fires_df$year)
  
  # unique value is selected for the season (no seasonal analisys will be computed)
  fires_df$stagione = 2
}
  

# the time domain of the analysis. the years from 
# year_from to year_test-1 are usually used for training.
# The years from year_test to the end of the available database are usually used for testing.

# please be aware that for Bulgaria the years are randomized 
if (is_Bulgaria) {
  year_from <- 2006
  year_test <- 2014 #until 2016
}


# Define the output folder where results will be saved
output_dir <- 'output_{region_name}/{year_from}_{year_test - 1}' %>% g


# load the model and the analysis 
source('R/model.R')
source('R/analysis.R')

