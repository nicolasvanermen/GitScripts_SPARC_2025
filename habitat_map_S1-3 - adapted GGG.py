# ---------------------------------------------------------------------------------------------------------
# Authors: Amber Mertens & Anne-Lie Van Praet (19/03/2025), adapted by Nicolas Vanermen (versie 10/07/2025)
# ---------------------------------------------------------------------------
# Description: steps 1-3 for the creation of ecotope maps for SPARC CRT-areas
# ------------------
# Version: Python 27
# ------------------

# -------------------
# ----- IMPORTS -----
# -------------------

# modules
import arcpy
from arcpy.sa import *
from arcpy import env
import functionsGGG as f

# check out Spatial Analyst extension
if arcpy.CheckExtension("Spatial") == "Available":
    arcpy.CheckOutExtension("Spatial")
else:
    print("Spatial Extension not available")
    exit()

# ------------------
# ----- INPUTS -----
# ------------------

# paths
main_dir = "Q:\\Projects\\PRJ_Schelde\\SPARC"
gis_path = main_dir + "\\03_habitats\\GIS"
output_path = gis_path + "\\SPARC_maps_TEST\\"
bathymetry_path = gis_path + "\\Geodatabanken\\SPARC_bathymetry_20250613_masked_DTMs.gdb"

# parameters
area = "Vlassenbroek"
year = "2022"
print("Processing S1-3 " + area + " " + year)

# tidal data table
table_path = main_dir + "\\02_getij\\Exports 2025 07 09"
excel_table = table_path + "\\DD_percentielen_GGG_SPARC.xls"

# source DTM
source_dtm = bathymetry_path + "\\MASK_DTM2022_SPARC_ETRS_inv"

# area_boundary layer:
area_boundary_lyr = gis_path + "\\Layers\\" + area + ".lyr"

# area_boundary shapefile
area_boundary = gis_path + "\\Geodatabanken\\SPARC_areas_hertekend_ETRS.gdb\\" + area

# geodatabase to store results per area
db_name = "SPARC_habitat_maps_" + area + year + ".gdb"

if not arcpy.Exists(output_path + db_name):
    geodatabase = arcpy.CreateFileGDB_management((output_path), db_name).getOutput(0)
else:
    geodatabase = output_path + db_name

# nr of decimals to round the input data to:
# (this is for the 'reclassify' function which only works on integer rasters)
round_dec = 2  # 2 decimals
round_multipl_list = ["1"]  # a 1 here to which the correct amount of zeros will be added
for i in range(0, round_dec):
    round_multipl_list.append("0")
round_multiplier = int(''.join(round_multipl_list))

# set environment settings (scratch workspace same as current workspace to avoid issues with map algebra)
env.workspace = env.scratchWorkspace = geodatabase
env.overwriteOutput = True

# ----------------------
# ------ Step 1-3 ------
# ----------------------
# deze 1e drie stappen maken rasters aan met relatieve waterhoogtes van overspoelingsdata en waterstanden

# perform a mask:
area_raster = "SPARC_" + area + year
arcpy.gp.ExtractByMask_sa(source_dtm,
                          area_boundary,
                          geodatabase + "\\" + area_raster)

# DTM:
dtm = geodatabase + "\\" + area_raster

# set local parameters
dd_percentages = ["DD_04", "DD_35", "DD_60", "DD_85", "DD_995"]

# loop through percentages
for p in dd_percentages:
    print("Currently processing " + p)

    # input
    sheet_name = "hoogte_" + p

    # output
    out_ras = geodatabase + "\\" + p + "_raster_t"

    # process: create raster with relative values from Excel sheet and save the result
    f.table_to_raster(area_boundary_lyr, "Gebied", excel_table, sheet_name, "Area",
                      "hoogte", out_ras)

    raw_raster = Raster(geodatabase + "\\" + p + "_raster_t")

    # round off the values
    rounded_raster = Float(Int(raw_raster * round_multiplier)) / round_multiplier

    # make values relative by subtracting dtm
    result = f.subtract_dtm(rounded_raster, dtm)
    result.save(geodatabase + "\\rel_grens_" + p + "_boz_f")  # relatieve grens
