# load model functions
source('R/model_functions.R')

# load analysis functions
source('R/analysis_functions.R')

###### SETTING THE  EXPERIMENT #######
###### If we want to practice with Liguria region instead of 
###### going through the project (Puglia or Sicilia), the 
###### variable is_Liguria needs to be set to True (T), otherwise to False (F)

is_Liguria <- F
is_Puglia <- F
is_Sardegna <- F
is_Sicilia <- F
is_Liguria_s <-F # Liguria Simple --> study of  CLC CORINE 3rd level for DPC
is_Sicilia_s <-F#....
is_Liguria_Sicilia <- F #Liguria and Sicilia union of points to have crossed analysis

is_Bulgaria <- T





# please select your region, 'puglia' or 'sicilia'
#  

#region_name = 'puglia'
#region_name = 'sardegna'
#region_name  = "liguria"
#region_name = 'liguria_s'
#region_name = 'sicilia_s'
#region_name = 'liguria_sicilia'
region_name = 'Bulgaria'

############### SPATIAL RESOLUTION AND CROSS VALIDATION RESOLUTION
# the spatial resolution  of the map. Default: 100 m
resolution = 500
# The box dimension is the side of the squares ([m]) in which the spatial domain 
# is discretized before regrouping such boxes into a number of  folds for the cross validation. 
box_dimension <- 15000

##################################################
######   PARAMETERS  FOR RANDOM  FOREST ##########
##################################################
# ntree  is the number of the trees used by the RandomForest algorithm. default = 750

ntree <- 750 #300#750#/3

#nodesize is Minimum size of terminal nodes. 
#Setting this number larger causes smaller trees to be grown (and thus take less time).
#Setting this number larger may affect overall accuracy of the method. default = 50
nodesize <- 50

##################################################
# ADVANCED SETTINGS FOR SPECIFIC TESTS 
#set use clustering

user_clustering=FALSE #  not used in reality...
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
}else if (is_Liguria_s) {
  data_dir = "data_lig_s"
  # we read the geographical points info (points.csv) and the fire info (fires.csv)
  points_df <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
  points_df$veg <- as.factor(points_df$veg)
  points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
  fires_df <- read.csv('{data_dir}/fires.csv' %>% g)
  fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
  fires_df$year = strtoi(fires_df$year)
  fires_df$month = strtoi(fires_df$month)
  #strategy 1: every fire is a summer fire  ---> not good for liguria
  #fires_df$stagione = 2 # can be improved!
  #strategy 2: use transform but loose  class 
  #fires_df <- transform(fires_df,stagione = ifelse(is_in(month, 5:10), 2, 1) )
  #strategy 3
  fires_df$stagione  <- 1
  fires_df@data[is_in(fires_df@data$month, 5:10), "stagione"] <- 2 #it keeps the spatialpointssdataframe class
  
}else if (is_Sicilia_s) {
  data_dir = "data_sicilia_s"
  # we read the geographical points info (points.csv) and the fire info (fires.csv)
  points_df <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
  points_df$veg <- as.factor(points_df$veg)
  points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
  fires_df <- read.csv('{data_dir}/fires.csv' %>% g)
  fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
  fires_df$year = strtoi(fires_df$year)
  fires_df$month = strtoi(fires_df$month)
  fires_df$stagione = 1 # da migliorare!!!
  fires_df@data[is_in(fires_df@data$month, 5:10), "stagione"] <- 2 #it keeps the spatialpointssdataframe class
  
  
}else if (is_Liguria_Sicilia) {
  print("sono dentro Liguria Sicilia")
  data_dir = "data_lig_s"
  # we read the geographical points info (points.csv) and the fire info (fires.csv)
  points_df1 <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
  points_df1 <- points_df1[,1:13]
  
  print(dim(points_df1)[1])
  len_df1 = dim(points_df1)[1]
  fires_df1 <- read.csv('{data_dir}/fires.csv' %>% g)

  data_dir = "data_sicilia_s"
  points_df2 <- read.csv('{data_dir}/points.csv' %>% g, row.names="point_index")
  points_df2 <- points_df2[,1:13]
  print(dim(points_df2)[1])
  len_df2 = dim(points_df2)[1]
  fires_df2 <- read.csv('{data_dir}/fires.csv' %>% g)
  # region 2 fires  need to find region 2 points in the  final database. I need to change their index
  fires_df2$point_index = fires_df2$point_index + len_df1
  
  points_df <- rbind(points_df1, points_df2, makeUniqueIDs = TRUE) 
  fires_df <- rbind(fires_df1,  fires_df2, makeUniqueIDs = TRUE)
  
  
  points_df$veg <- as.factor(points_df$veg)
  points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
  fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
  fires_df$year = strtoi(fires_df$year)
  fires_df$month = strtoi(fires_df$month)
  fires_df$stagione = 1 # da migliorare!!!
  fires_df@data[is_in(fires_df@data$month, 5:10), "stagione"] <- 2 #it keeps the spatialpointssdataframe class
  
}else if (is_Bulgaria) {
  data_dir = '{region_name}' %>% g
  # we read the geographical points info (points.csv) and the fire info (fires.csv)
  points_df <- read.csv('/home/gruppo4/Bulgaria/bg_dati_per_cluster/points_train_2.csv' %>% g, row.names="point_index")
  all_points_df <- read.csv('/home/gruppo4/Bulgaria/bg_dati_per_cluster/all_points_2.csv' %>% g, row.names="point_index")
  points_df$veg <- as.factor(points_df$veg)
  points_df <- SpatialPointsDataFrame(points_df[c("x", "y")], points_df)
  all_points_df$veg <- as.factor(all_points_df$veg)
  all_points_df <- SpatialPointsDataFrame(all_points_df[c("x", "y")], all_points_df)
  fires_df <- read.csv('/home/gruppo4/Bulgaria/bg_dati_per_cluster/fires_no_park_2.csv' %>% g)
  fires_df <- SpatialPointsDataFrame(fires_df[c("x", "y")], fires_df)
  fires_df$anno = fires_df$random_yea
  fires_df$anno <- as.numeric(fires_df$anno)
  fires_df$year = fires_df$random_yea
  fires_df$year <- as.numeric(fires_df$year)
  
  fires_df$stagione = 2
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
	year_from <- 1997#1997    
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
}else if (is_Liguria_s) {
  year_from <-  2007 
  year_test <- 2018
}else if (is_Sicilia_s) {
  year_from <-  2007 
  year_test <- 2018
}else if (is_Liguria_Sicilia) {
  year_from <-  2007 
  year_test <- 2018
}else if (is_Bulgaria) {
  year_from <- 2006
  year_test <- 2014 #until 2016
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

