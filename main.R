# load model functions
source('R/model_functions.R')

# load analysis functions
source('R/analysis_functions.R')

############### SPATIAL RESOLUTION AND CROSS VALIDATION RESOLUTION
# the spatial resolution  of the map. Default: 100 m
resolution = 100
# The box dimension is the side of the squares ([m]) in which the spatial domain 
# is discretized before regrouping such boxes into a number of  folds for the cross validation. 
box_dimension <- 15000

##################################################
######   PARAMETERS  FOR RANDOM  FOREST ##########
##################################################
# ntree  is the number of the trees used by the RandomForest algorithm. default = 750
ntree <- 100
#nodesize is Minimum size of terminal nodes. 
#Setting this number larger causes smaller trees to be grown (and thus take less time).
#Setting this number larger may affect overall accuracy of the method. default = 50
nodesize <- 60



# loading  the data
# we  define the data directory, we read the geographical and fire  occurrence data bases 
data_dir = 'puglia_trial'
#data_dir = 'data_{resolution}m' %>% g
points_df <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
fires_df <- read.csv('{data_dir}/fires.csv' %>% g)
fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
fires_df$month = month(fires_df$data)

# the time domain of the analysis. the years from 
# year_from to year_test-1 are used for training.
# The years from year_test to the end of the available database are used for testing.
#default  year from 1997  year test 2012 
year_from <-  2007 
year_test <- 2015
#puglia data  from 2007 to 2017,  year test 2015  
#the ML model is built and analysis is performed.  
source('R/model.R')
source('R/analysis.R')