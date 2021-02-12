# load model functions
source('R/model_functions.R')

# load analysis functions
source('R/analysis_functions.R')

###### SETTING THE  EXPERIMENT #######
###### If we want to practice with Liguria region instead of 
###### going through the project (Puglia or Sicilia), the 
###### variable is_Liguria needs to be set to True (T), otherwise to False (F)

is_Liguria <- T
is_Puglia <- F
is_Sardegna <- F
is_Sicilia <- F

# please select your region, 'puglia' or 'sicilia'
#  

#region_name = 'puglia'
#region_name = 'sardegna'
region_name = 'liguria'
#region_name = 'sicilia'

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
ntree <- 750#300#750#/3
#nodesize is Minimum size of terminal nodes. 
#Setting this number larger causes smaller trees to be grown (and thus take less time).
#Setting this number larger may affect overall accuracy of the method. default = 50
nodesize <- 50

##################################################
# ADVANCED SETTINGS FOR SPECIFIC TESTS 
#set use clustering
user_clustering=TRUE #  not used in reality...
#set use of batch list of experiment, to span a lot of input parameters for extensive
#experiment creation
batch_test = TRUE

# loading  the data
# we  define the data directory, we read the geographical and fire  occurrence data bases 

if (is_Liguria) {
	data_dir = 'data_{resolution}m' %>% g
	# we read the geographical points info (points.csv) and the fire info (fires.csv)
	points_df <- read.csv('{data_dir}/points_new_veg.csv' %>% g, row.names="point_index")
	points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
	fires_df <- read.csv('{data_dir}/fires.csv' %>% g)
	fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
	fires_df$month = month(fires_df$data)
} else if (is_Sardegna) {
	data_dir = '{region_name}_trial' %>% g
	# we read the geographical points info (points.csv) and the fire info (fires.csv)
	points_df <- read.csv('{data_dir}/points_sar_aggiornati.csv' %>% g, row.names="point_index")
	points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
	fires_df <- read.csv('{data_dir}/fires_sar_aggiornati.csv' %>% g)
	fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
	fires_df$month = month(fires_df$data)
}else if (is_Puglia) {
	data_dir = '{region_name}_trial' %>% g
	# we read the geographical points info (points.csv) and the fire info (fires.csv)
	points_df <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
	points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
	fires_df <- read.csv('{data_dir}/fires.csv' %>% g)
	fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
	fires_df$month = month(fires_df$data)
}else if (is_Sicilia) {
	data_dir = '{region_name}_trial' %>% g
	# we read the geographical points info (points.csv) and the fire info (fires.csv)
	points_df <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
	points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
	fires_df <- read.csv('{data_dir}/fires.csv' %>% g)
	fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
	fires_df$month = month(fires_df$data)
}

# Idee Andrea: se la liguria ha fuochi dal 1997 al 2017...year test 2014 ---> 20% di test...
# la Sardegna ha fuochi dal 2007 al 2018 compreso... year test 2016

# the time domain of the analysis. the years from 
# year_from to year_test-1 are used for training.
# The years from year_test to the end of the available database are used for testing.
#default  year from 1997  year test 2012 
if (is_Liguria) {
  user_clustering=TRUE
  print("I already chose the test  years...")
  #Winter:# 
  yearVectorTest_w <- c(2003,2007,2015)
  #Summer:# 
  yearVectorTest_s <- c(2001,2010,2016)
	year_from <- 2000#1997    
	year_test <- 2017#2015
} else if (is_Sardegna) {
	year_from <-  2007 
	year_test <- 2017   
}else if (is_Puglia){
	year_from <-  2007 
	year_test <- 2015
}else if (is_Sicilia) {
	year_from <-  2007 
	year_test <- 2015
}


#puglia data  from 2007 to 2017,  year_from  2007 year_test = 2015
#sicilia data from 2007 to 2017,  year_from = 2007, year_test = 2015  
#the ML model is built and analysis is performed.  
#
#
#
#
#NOW  WE DEFINE THE OUTPUT FOLDER WHERE RESULTS WILL BE SAVED
#
#
if(is_Liguria){
  output_dir <- 'output/{year_from}_{year_test - 1}' %>% g
}else{
  output_dir <- 'output_{region_name}/{year_from}_{year_test - 1}' %>% g
}


source('R/model.R')
source('R/analysis.R')