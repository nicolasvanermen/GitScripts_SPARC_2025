# ---------------------------------------------------------------------------------------------------------
# Authors: Amber Mertens & Anne-Lie Van Praet (19/03/2025), adapted by Nicolas Vanermen (versie 10/07/2025)
# --------------------------------------------------------------------------------
# Description: steps 1-3 for the creation of ecotope maps for SPARC de-embankments
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
import functions as f

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
bathymetry_path = gis_path + "\\Geodatabanken\\SPARC_bathymetry_20250707_2050.gdb"

# parameters
area = "GrootSchoorHamme"
year = "2050"
print("Processing S1-3 " + area + " " + year)

if area in ["GrootBroek","KleinBroek","DeBunt"]:
    river = "Durme"
else:
    river = "Zeeschelde"
    
# tidal data table path
table_path = main_dir + "\\02_getij\\Exports 2025 05 28"

# source DTM
source_dtm = bathymetry_path + "\\GrootSchoorHamme_2050"

# allocatie layer:
allocatie_lyr = gis_path + "\\Layers\\AllocatieAspntScheldeETRS.lyr"

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
                          gis_path + "\\Geodatabanken\\SPARC_areas_hertekend_ETRS.gdb\\" + area,
                          geodatabase + "\\" + area_raster)

# DTM:
dtm = geodatabase + "\\" + area_raster

# set local parameters
dd_percentages = ["DD_25", "DD_75"]
HWLW_percentiles = ["OF_LW_10", "OF_HW_90", "OF_BE"]

# data to process this way
data_to_process = ["overspoeling", "waterstand"]

for d in data_to_process:

    if d == "overspoeling":
        percs = dd_percentages
        print("Step 1: \"droogvalduur\" (\"overspoeling\")")
    elif d == "waterstand":
        percs = HWLW_percentiles
        print("Step 2: \"waterstand\"")

    # loop through percentages
    for p in percs:
        print("Current file is " + d + " " + p)

        # input
        table_name = "interpolatie_" + d + "_4QN+QE_" + river + ".xls"
        sheet_name = "hoogte_" + p
        excel_table = table_path + "\\" + table_name

        # output
        out_ras = geodatabase + "\\" + p + "_raster_t"

        # process: create raster with relative values from Excel sheet and save the result
        f.table_to_raster(allocatie_lyr, "AllocNR", excel_table, sheet_name, "AllocNR",
                          "hoogte", out_ras)

        raw_raster = Raster(geodatabase + "\\" + p + "_raster_t")

        # round off the values
        rounded_raster = Float(Int(raw_raster * round_multiplier)) / round_multiplier

        # make values relative by subtracting dtm
        result = f.subtract_dtm(rounded_raster, dtm)
        result.save(geodatabase + "\\rel_grens_" + p + "_boz_f")  # relatieve grens
