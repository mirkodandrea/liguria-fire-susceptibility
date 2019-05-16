import os
import rasterio as rio
import numpy as np
import geopandas as gpd
import pandas as pd
from osgeo import gdal

from progressbar import ProgressBar

from sklearn import preprocessing
from rasterio import features
from shapely.geometry import Point, GeometryCollection

def extract_coordinates(indices, src):
    indices_t = indices.T[::-1, :]

    coordinates = np.stack(src.transform * indices_t)

    _, dx, _, _, _, dy = src.transform.to_gdal()
    coordinates = coordinates + np.array([[dx, dy]]).T/2

    #points = [Point(*p) for p in coordinates.T]

    return coordinates


def encode_feature(gdf, column):
    encoder = preprocessing.LabelEncoder()
    encoder.fit(gdf[column])
    encoded_column = encoder.transform(gdf[column])
    mapping = dict(zip(range(len(encoder.classes_)), encoder.classes_,))
    return encoded_column, mapping


def rasterize_numerical_feature(gdf, reference_file, column=None):
    with rio.open(reference_file) as f:
        out = f.read(1)
        out_array = np.empty(out.shape)
        # this is where we create a generator of geom, value pairs to use in rasterizing
        if column is not None:
            shapes = ((geom, value) for geom, value in zip(gdf.geometry, gdf[column]))
        else:
            shapes = ((geom, 1) for geom in gdf.geometry)

        burned = features.rasterize(shapes=shapes, fill=np.NaN, out=out_array, transform=f.transform)
    #    out.write_band(1, burned)

    return burned

def save_raster_as(array, output_file, reference_file, **kwargs):
    with rio.open(reference_file) as f:
        profile = f.profile
        profile.update(**kwargs)

        with rio.open(output_file, 'w', **profile) as dst:
            dst.write(array.astype(profile['dtype']), 1)
            
def read_tiff(tiff_file):
    with rio.open(tiff_file) as f:
        print(f'Reading file {tiff_file}')        
        data = f.read(1)
    return data



def calculate_box_grid(bbox, dx, dy, box_dim):
    lon_min, lon_max, lon_step = bbox.left   +dx/2 + box_dim/2, bbox.right +dx/2 - box_dim/2, box_dim
    lat_min, lat_max, lat_step = bbox.bottom +dy/2 + box_dim/2, bbox.top   +dy/2 - box_dim/2, box_dim
    return lon_min, lon_max, lon_step, lat_min, lat_max, lat_step


def build_boxes(lon_min, lon_max, lon_step, lat_min, lat_max, lat_step):
    box_lon = np.arange(lon_min, lon_max +lon_step/2, lon_step)
    box_lat = np.arange(lat_min, lat_max +lat_step/2, lat_step)
    box_lons, box_lats = np.meshgrid(box_lon, box_lat)

    return box_lons, box_lats
    
def get_boxes_rc(coordinates, lon_min, lon_step, lat_min, lat_step):
    C = (np.round((coordinates[0, :] - lon_min) / lon_step)).astype('int')
    R = (np.round((coordinates[1, :] - lat_min) / lat_step)).astype('int')
    RC = np.array((R, C))
    return RC

def calculate_box_intersections(geometry, box_lons, box_lats, RC):
    unique_rc = np.unique(RC, axis=1)

    box_cuts = {}
    bar = ProgressBar()
    for rc in bar(unique_rc.T):
        lon, lat = box_lons[rc[0], rc[1]], box_lats[rc[0], rc[1]]
        box = Point(lon, lat).buffer(box_dim + buffer_dim).envelope
        box_cut = [box.intersection(g) for g in geometry]
        box_cuts[(rc[0], rc[1])] = box_cut
    
    return box_cuts

def calculate_distances_on_boxes(coordinates, RC, box_cuts, outside_value=-9999):
    bar = ProgressBar()
    distances = []
    points = []
    for p, rc in bar(zip(coordinates.T, RC.T)):
        point = Point(*p)
        p_distances = [
            point.distance(g) 
            if not g.is_empty 
            else np.NaN 
            for g in box_cuts[(rc[0], rc[1])]
        ] 
        d = np.nanmin(p_distances)
        d = outside_value if d > buffer_dim else d
        distances.append(d)
        points.append(point)
        
    return distances, points


def write_distance_raster(raster_file, dst_file, column=None):
    src_ds = gdal.Open(raster_file)
    srcband=src_ds.GetRasterBand(1)


    drv = gdal.GetDriverByName('GTiff')
    dst_ds = drv.Create( dst_file,
                         src_ds.RasterXSize, src_ds.RasterYSize, 1,
                         gdal.GetDataTypeByName('Float32'))

    dst_ds.SetGeoTransform( src_ds.GetGeoTransform() )
    dst_ds.SetProjection( src_ds.GetProjectionRef() )

    dstband = dst_ds.GetRasterBand(1)
    gdal.ComputeProximity(srcband, dstband, ["DISTUNITS=GEO"])