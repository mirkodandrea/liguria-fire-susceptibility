#!/usr/bin/env python
# coding: utf-8

# Importing the libraries
import os
import rasterio as rio
import numpy as np
import geopandas as gpd
import pandas as pd
from osgeo import gdal
import matplotlib.pyplot as plt
from progressbar import ProgressBar

# utils contains most of the ad-hoc routines to read 
# and save raster data, managing  boxes and cordinates  

from utils import *

# specify the names of folders, tiff (existing or to be created) and existing shapefiles

data_dir = "/home/gruppo4/Bulgaria/bg_dati_per_cluster"

dem_file            = f'{data_dir}/dem_500_wgs84_35N.tiff'
slope_file          = f'{data_dir}/slope_500_int.tif'
aspect_file         = f'{data_dir}/aspect_500_int.tif'
northing_file       = f'{data_dir}/northing_500.tif'
easting_file        = f'{data_dir}/easting_500.tif'

urban_distance_file = f'{data_dir}/urban_distance_500.tiff'
roads_distance_file = f'{data_dir}/roads_distance_500.tiff'
tracks_distance_file= f'{data_dir}/tracks_distance_500.tiff'
crops_distance_file = f'{data_dir}/crops_distance_500.tiff'
parks_raster_file = f'{data_dir}/parks_500.tiff'
vegetation_agg_file = f'{data_dir}/vegetation_agg.tiff'
vegetation_file     = f'{data_dir}/vegetation.tiff'
vegetation_type_file     = f'{data_dir}/vegetation_type.tiff'
vegetation_mask_file =f'{data_dir}/vegetation_mask.tiff'  #noveg,1 if no veg
parks_exc_file = f'{data_dir}/parks_exc.tiff'

#adding climate rasters
precipitation_file = f'{data_dir}/rr_95_20_repr_res_interpol_ext.tif'
temperature_file = f'{data_dir}/T_95_20_repr_res_interpol_ext.tif'

#add park to exclude
parks_exc_shp = f'{data_dir}/National_parks_and_reserves_m.shp'

#path of the neeeded shapefiles
fires_shp           = f'{data_dir}/merge_fires_diss_ry_wgs84_35N.shp'
vegetation_shp      = f'{data_dir}/CLC2018.shp'
roads_shp           = f'{data_dir}/roads_filtered_wgs84_35N.shp'
urban_shp           =f'{data_dir}/Pop_places_poly_wgs84_35N.shp'
tracks_shp          = f'{data_dir}/tracks_wgs84_35N.shp'
parks_shp           = f'{data_dir}/zpo_wgs84_35N.shp'



# open dem
# dem is used as a reference
with rio.open(dem_file) as src:
    print(f'Reading dem file {dem_file}')
    dem = src.read(1, masked = True)
    for i, dtype, nodataval in zip(src.indexes, src.dtypes, src.nodatavals):
        print(i, dtype, nodataval)
    print(dem.__class__)
    print(dem.shape)
    #dem = float(dem)
    #dem[dem == 65535] = np.NaN
_, dx, _, _, _, dy = src.transform.to_gdal()    
bbox = src.bounds

#to show the dem file in a graphical way
#plt.matshow(dem)


# PREPROCESSING

# evaluate slope, northing and easting tiff files
if not os.path.isfile(slope_file):
    print(f'Creating slope file {slope_file}')
    gdal.DEMProcessing(slope_file, dem_file, 'slope')

    
if not os.path.isfile(northing_file) or not os.path.isfile(easting_file):

    if not os.path.isfile(aspect_file):
        print(f'Creating aspect file {aspect_file}')
        gdal.DEMProcessing(aspect_file, dem_file, 'aspect')

    with rio.open(aspect_file) as f:
        print(f'Calculating northing and easting files')
        print(f'Reading aspect file {aspect_file}')
        aspect = f.read(1,   masked = True)
        #aspect[aspect <= -9999] = np.NaN

    northing = np.cos(aspect * np.pi/180.0)
    print(f'Saving northing file {northing_file}')
    save_raster_as(northing, northing_file, aspect_file)
    del northing 
    print(f'Saving easting file {easting_file}')    
    easting = np.sin(aspect * np.pi/180.0)
    save_raster_as(easting, easting_file, aspect_file)
    del easting



print(f'Reading vegetation shp {vegetation_shp}')
vegetation = gpd.read_file(vegetation_shp)

# define a dictionary with not burnable CORINE class code
dict_not_burnable={"111": "not_burnable",
"112": "not_burnable",
"121": "not_burnable",
"122": "not_burnable",
"123": "not_burnable",
"124": "not_burnable",
"131": "not_burnable",
"132": "not_burnable",
"133": "not_burnable",
"141": "not_burnable",
"142": "not_burnable",
"141": "not_burnable",
"142": "not_burnable",
"331": "not_burnable",
"332": "not_burnable",
"334": "not_burnable",
"335": "not_burnable",
"411": "not_burnable",
"412": "not_burnable",
"421": "not_burnable",
"422": "not_burnable",
"423": "not_burnable",
"511": "not_burnable",
"512": "not_burnable",
"521": "not_burnable",
"522": "not_burnable",
"523": "not_burnable"}


# change the names of not burnable classes
vegetation = vegetation.replace({"CODE_18":dict_not_burnable})
vegetation['CODE_18_INT'] = vegetation.apply(lambda r: 0 if r.CODE_18  == "not_burnable"   else int(r.CODE_18), axis =1 )

# mask the vegetation areas
vegetation['is_vegetated'] = vegetation.apply(lambda r: 0 if r.CODE_18  == "not_burnable"   else 1, axis =1 )

# create a raster with the vegetation classes 
if not os.path.isfile(vegetation_file):
    print(f'Rasterizing vegetation')
    vegetation_raster = rasterize_numerical_feature(vegetation, dem_file, 'CODE_18_INT')
    print(f'Writing vegetation_raster file {vegetation_file}')    
    save_raster_as(vegetation_raster, vegetation_file, dem_file)
    

# create a binary vegetated tiff file 
if not os.path.isfile(vegetation_mask_file):
    print(f'Rasterizing vegetation_mask')    
    vegetation_mask_raster = rasterize_numerical_feature(vegetation, dem_file, 'is_vegetated')
    print(f'Writing vegetation_mask_raster file {vegetation_mask_file}')
    save_raster_as_noforce(vegetation_mask_raster, vegetation_mask_file, dem_file)


vegetation_mask_raster = read_tiff(vegetation_mask_file)
mask = vegetation_mask_raster ==1    
indices = np.argwhere(mask)
coordinates = extract_coordinates(indices, src)

# selecting the corine classes corresponding to crops
vector_crops = ["211","212","213","221","222","223","241","242", "243"] 


# rasterize the excluded parks  --> they will be not considered in the training dataset
if not os.path.isfile(parks_exc_file):
    print("I am doing the excluded parks")
    
    parks_e  = gpd.read_file(parks_exc_shp)
    
    parks_e_raster = rasterize_numerical_feature(parks_e, dem_file, 'is_exc_par')
    save_raster_as_noforce(parks_e_raster, parks_exc_file, dem_file)

# create the remaining tiff files for the final dataset 

vegetation['Crops']=0
vegetation['Crops'] = vegetation.apply(lambda r: 1 if r.CODE_18 in vector_crops  else 0, axis =1 )
if not os.path.isfile(crops_distance_file):
    print("I  am  cropping")
    crops = vegetation.query(f'Crops == 1')
    crops_raster_file = f'{data_dir}/crops.tiff'
    crops_raster = rasterize_numerical_feature(crops, dem_file)
    save_raster_as_noforce(crops_raster, crops_raster_file, dem_file)

    write_distance_raster(crops_raster_file, crops_distance_file)


if not os.path.isfile(urban_distance_file):
    print("I am doing the urban")
    #urban = vegetation.query(f'Urbano == 1')
    urban  = gpd.read_file(urban_shp)
    urban_raster_file = f'{data_dir}/urban.tiff'
    urban_raster = rasterize_numerical_feature(urban, dem_file)
    save_raster_as_noforce(urban_raster, urban_raster_file, dem_file)

    urban_distance_file = f'{data_dir}/urban_distance.tiff'            
    write_distance_raster(urban_raster_file, urban_distance_file)



if not os.path.isfile(roads_distance_file):
    print(f'Reading roads shp {roads_shp}')
    roads = gpd.read_file(roads_shp)

    roads_raster = rasterize_numerical_feature(roads, dem_file)
    roads_raster_file = f'{data_dir}/roads.tiff'
    save_raster_as_noforce(roads_raster, roads_raster_file, dem_file)

    write_distance_raster(roads_raster_file, roads_distance_file)
    

if not os.path.isfile(tracks_distance_file):
    print(f'Reading tracks shp {tracks_shp}')
    tracks = gpd.read_file(tracks_shp)

    tracks_raster = rasterize_numerical_feature(tracks, dem_file)
    tracks_raster_file = f'{data_dir}/tracks.tiff'
    save_raster_as_noforce(tracks_raster, tracks_raster_file, dem_file)
    write_distance_raster(tracks_raster_file, tracks_distance_file)
    

if not os.path.isfile(parks_raster_file):
    print(f'Reading parks shp {parks_shp}')
    parks = gpd.read_file(parks_shp)

    parks_raster = rasterize_numerical_feature(parks, dem_file)
    save_raster_as_noforce(parks_raster, parks_raster_file, dem_file)



# DATA PREPARATION

# load data 

print('loading climate data')
prec = rio.open(precipitation_file)
temp = rio.open(temperature_file)

print(f'Reading precipitation file {precipitation_file}')
prec = read_tiff(precipitation_file)

print(f'Reading temperature file {temperature_file}')
temp = read_tiff(temperature_file)

print(f'Reading excluded parks file {parks_exc_file}')
parks_exc_ = read_tiff(parks_exc_file)

print(f'Reading slope file {slope_file}')
slope = read_tiff(slope_file)

print(f'Reading northing file {northing_file}')
northing = read_tiff(northing_file)
    
print(f'Reading easting file {easting_file}') 
easting = read_tiff(easting_file)

print(f'Reading vegetation_raster file {vegetation_file}')            
vegetation_raster = read_tiff(vegetation_file)
    
print(f'Reading vegetation mask  file {vegetation_mask_file}')          
vegetation_mask_raster = read_tiff(vegetation_mask_file)

print(f'Reading urban distance file {urban_distance_file}')
urban_distance = read_tiff(urban_distance_file)

print(f'Reading roads distance file {roads_distance_file}')
roads_distance = read_tiff(roads_distance_file)

print(f'Reading crops distance file {crops_distance_file}')
crops_distance = read_tiff(crops_distance_file)

print(f'Reading tracks distance file {tracks_distance_file}')
tracks_distance = read_tiff(tracks_distance_file)

print(f'Reading parks file {parks_raster_file}')
parks_raster = read_tiff(parks_raster_file)

print(f'Reading vegetation shp {vegetation_shp}')

vegetation = gpd.read_file(vegetation_shp)
vegetation = vegetation.replace({"CODE_18":dict_not_burnable})



print("Doing  the mask")



# masking vegetation raster excluding non-vegetated areas. 
mask = (vegetation_mask_raster == 1)  & (prec > -100) & (temp > -100) & ( ~dem.mask) & (~slope.mask) & (~northing.mask) & (~easting.mask) & (parks_exc_ != 1)
indeces = np.argwhere(mask)
coordinates = extract_coordinates(indeces, src)
points_geom = [Point(*p) for p in coordinates.T]

print("creating  points geo data frame")

points = gpd.GeoDataFrame(pd.DataFrame(indeces, columns=['row', 'col']), geometry=points_geom, crs =src.crs.to_string() )  
points['x'] = points.geometry.x
points['y'] = points.geometry.y


print(f'Reading fires shp {fires_shp}')
fires = gpd.read_file(fires_shp)

# create a fire dataset
print("before doing points copy for envelopes")
points_envelopes = points.copy()
print("I did the envelopes of pixels")
points_envelopes.geometry = points.geometry.buffer(dx/2).envelope
print(f' Just assigned geometry to the envelopes of pixels.  Before doing sjoin ')

# fires and point with envelopes must have same crs
fires = fires.to_crs(points_envelopes.crs.srs)   
presences = gpd.sjoin(points_envelopes, fires).loc[:, ('x', 'y', 'index_right','area_ha', 'random_yea')]
presences.index.name = 'point_index'
presences.rename(columns={'index_right': 'fire_index'}, inplace=True)

presences.to_csv(f'{data_dir}/fires_no_park_2.csv')

print(f'created csv of fires ')



# Extract veg type density for each point neighborood

from scipy import signal
from scipy import misc

window_size = 2
M = vegetation_raster
types = np.unique(vegetation_raster)

types_presence = {}

counter = np.ones((window_size*2+1, window_size*2+1))
counter = counter / np.sum(counter)

bar = ProgressBar()
for t in bar(types):
    density_entry = 'perc_' + str(int(t)) 
    types_presence[density_entry] = 100 * signal.convolve2d(M==t, counter, boundary='fill', mode='same')
    

veg_freq = np.array(list(types_presence.values())).argmax(0)


# Merge datasets to points.csv file

vegetation_raster_string =  vegetation_raster.astype(int)
vegetation_raster_string = vegetation_raster.astype(str)

# create a dictonary that will be used for defining the new variables of the dataset 
data_dict = {
    'dem': dem,
    'prec': prec,
    'temp': temp,
    'slope': slope,
    'north': northing,
    'east': easting,
    'veg': vegetation_raster_string,
    'veg_mask': vegetation_mask_raster,
    'urban_d': urban_distance,
    'roads_d': roads_distance,
    'crops_d': crops_distance,
    'park': parks_raster,
    'tracks_d': tracks_distance,
    'veg_freq': veg_freq
}
data_dict.update(types_presence)

mappings = {
#    'veg': mapping,
#    'veg_freq': mapping
}

# dataset generation 
bar = ProgressBar()
for k, v in bar(data_dict.items()):
    points[k] = v[points.row.values, points.col.values]
    if k in mappings:
        points[k] = points[k].apply(lambda v: mappings[k][v])
        


points.loc[points['north'].isna(), 'north'] = 0
points.loc[points['east'].isna(), 'east'] = 0
points.loc[points['roads_d'].isna(), 'roads_d'] = -9999   

points.geometry = points.loc[points.index].geometry
points.index.name = 'point_index'

points_csv = points.copy()
points_csv['x'] = points_csv.geometry.x
points_csv['y'] = points_csv.geometry.y
points_csv = points_csv.drop('geometry', axis=1)

points_csv[['x', 'y', 'row', 'col'] + list(data_dict.keys())]

points_csv.to_csv(f'{data_dir}/points_train_2.csv')


