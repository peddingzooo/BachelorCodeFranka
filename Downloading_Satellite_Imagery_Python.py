

###############################################################################################################################
#                                     Automatic Download of Satellite Imagery                                                 #
###############################################################################################################################


import sys
import os
import numpy as np
import pandas as pd
import geopandas as gpd
import rasterio
from rasterio.transform import from_bounds
from rasterio.merge import merge
from rasterio.enums import Resampling
from rasterio.warp import reproject, Resampling
from sentinelhub import (
    SHConfig, SentinelHubRequest, DataCollection, MimeType,
    MosaickingOrder, CRS, BBoxSplitter, bbox_to_dimensions
)
from shapely.errors import ShapelyDeprecationWarning
import warnings
import time

warnings.filterwarnings("ignore", category=ShapelyDeprecationWarning)

CLIENT_ID = "41dc00b2-6262-4c40-8d4c-4ec1bea55b56"
    #"a2ba8787-3594-474f-a785-f4c155630a66"
    #"8881a25a-4a45-4d6d-bc41-089fd1901c36"
CLIENT_SECRET = "KCyEZahGF1kskDMibKWQjtcmCCBuRSHr"
    #"svjFd6SphoAaWb7FxNjisExpGgXPNHFQ"
    #"vk9cTxTVDLepXvDH3od6ueMy6Z7TYbbr"
config = SHConfig()
config.sh_client_id = CLIENT_ID
config.sh_client_secret = CLIENT_SECRET

shapefile_path = "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/M5_vorläufig.shp"
gdf = gpd.read_file(shapefile_path)
if gdf.empty:
    raise ValueError("Das Shapefile enthält keine Geometrien!")
gdf = gdf.to_crs(epsg=4326)
geometry = gdf.geometry.union_all()

resolution = 10

date_range = pd.date_range(start="2017-01-01", end="2018-12-31", freq='5D').strftime('%Y-%m-%d').tolist()

bbox_splitter = BBoxSplitter([geometry], CRS.WGS84, (5, 5))
bboxes = bbox_splitter.get_bbox_list()

output_dir = "C:/Users/braun/iCloudDrive/Geographie/5. Semester_Bachelorarbeit/M5/SentinelData"

def check_image_availability(date, bbox, config):
    request = SentinelHubRequest(
        evalscript=""" 
        //VERSION=3
        function setup() { return {input: [{bands: ["B01"]}], output: {bands: 1}}; }
        function evaluatePixel(sample) { return [1]; }
        """,
        input_data=[SentinelHubRequest.input_data(
            data_collection=DataCollection.SENTINEL2_L2A,
            time_interval=(date, date),
            mosaicking_order=MosaickingOrder.LEAST_CC,
            other_args={"maxCloudCoverage": 30}  # Wolkenbedeckung <= 30%
        )],
        responses=[SentinelHubRequest.output_response("default", MimeType.TIFF)],
        bbox=bbox,
        size=(10, 10),
        config=config
    )
    try:
        response = request.get_data()
        return response is not None and len(response) > 0
    except Exception:
        return False


def download_images_for_bbox_and_date(date, bbox, resolution, config, tile_index, downloaded_tiles):
    if not check_image_availability(date, bbox, config):
        print(f"Keine Bilddaten für {date} in BBox {bbox}")
        return
    width, height = bbox_to_dimensions(bbox, resolution=resolution)
    evalscript = """
    //VERSION=3
    function setup() {
        return {input: [{bands: ["B02", "B03", "B04", "B08", "B11", "B12", "SCL"]}], output: {bands: 6}};
    }
    function evaluatePixel(sample) {
        if (sample.SCL == 3 || sample.SCL == 8 || sample.SCL == 9 || sample.SCL == 1 || sample.SCL == 2) {
            return [0, 0, 0, 0, 0, 0];
        }
        return [sample.B02, sample.B03, sample.B04, sample.B08, sample.B11, sample.B12];
    }
    """
    request = SentinelHubRequest(
        evalscript=evalscript,
        input_data=[SentinelHubRequest.input_data(
            data_collection=DataCollection.SENTINEL2_L2A,
            time_interval=(date, date),
            mosaicking_order=MosaickingOrder.LEAST_CC
        )],
        responses=[SentinelHubRequest.output_response("default", MimeType.TIFF)],
        bbox=bbox,
        size=(width, height),
        config=config
    )
    try:
        response = request.get_data()
        for idx, data in enumerate(response):
            output_path = os.path.join(output_dir, f"{date}_tile{tile_index}.tif")
            image_data = np.moveaxis(data, -1, 0)

            transform = from_bounds(*bbox, image_data.shape[2], image_data.shape[1])
            with rasterio.open(
                    output_path, "w", driver="GTiff",
                    height=image_data.shape[1], width=image_data.shape[2],
                    count=6, dtype=image_data.dtype,
                    crs="EPSG:4326", transform=transform
            ) as dst:
                dst.write(image_data)
            downloaded_tiles.append(output_path)
            print(f"Gespeichert: {output_path}")
    except Exception as e:
        print(f"Fehler beim Abrufen der Daten für BBox {bbox} am {date}: {e}")

def merge_tiles(downloaded_tiles, date):
    if len(downloaded_tiles) > 1:
        src_files_to_mosaic = [rasterio.open(f) for f in downloaded_tiles]
        mosaic, out_trans = merge(src_files_to_mosaic)
        out_meta = src_files_to_mosaic[0].meta.copy()
        out_meta.update({
            "driver": "GTiff",
            "count": 6,
            "dtype": "float32",
            "crs": src_files_to_mosaic[0].crs,
            "transform": out_trans,
            "width": mosaic.shape[2],
            "height": mosaic.shape[1]
        })
        output_merged_path = os.path.join(output_dir, f"merged_{date}.tif")
        with rasterio.open(output_merged_path, "w", **out_meta) as dest:
            dest.write(mosaic)
        print(f"Gespeichertes zusammengeführtes Bild: {output_merged_path}")
        for f in src_files_to_mosaic:
            f.close()
        for tile in downloaded_tiles:
            try:
                os.remove(tile)
                print(f"Gelöscht: {tile}")
            except PermissionError:
                print(f"Warnung: Datei {tile} konnte nicht gelöscht werden.")

for date in date_range:
    downloaded_tiles = []
    for i, bbox in enumerate(bboxes):
        download_images_for_bbox_and_date(date, bbox, resolution, config, i, downloaded_tiles)
    merge_tiles(downloaded_tiles, date)







































