# Import necessary libraries
import geopandas as gpd
import matplotlib.pyplot as plt

# Read the GeoJSON file into a GeoDataFrame
dis_df = gpd.read_file("datasrc/map_data/geojson/districts_latlon.txt.geojson")
mkm_df = gpd.read_file("datasrc/map_data/geojson/mukims_latlon.txt.geojson")
kpg_df = gpd.read_file("datasrc/map_data/geojson/kampongs_latlon.txt.geojson")

# Export
dis_df.to_file("datasrc/map_data/geojson_shp/district.shp")
mkm_df.to_file("datasrc/map_data/geojson_shp/mukim.shp")
kpg_df.to_file("datasrc/map_data/geojson_shp/kampong.shp")

# Plot the full GeoDataFrame
plt.figure(figsize=(10, 10))
kpg_df.boundary.plot()
plt.show()
