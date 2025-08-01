# ---------------------------------------------------------------------------------------------------------
# Authors: Amber Mertens & Anne-Lie Van Praet (19/03/2025), adapted by Nicolas Vanermen (versie 10/07/2025)
# ---------------------------------------------------------------------------
# Description: steps 4-9 for the creation of ecotope maps for SPARC CRT-areas
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

# ------------------
# ----- INPUTS -----
# ------------------

# paths
main_dir = "Q:\\Projects\\PRJ_Schelde\\SPARC"
gis_path = main_dir + "\\03_habitats\\GIS"
output_path = gis_path + "\\SPARC_maps_TEST\\"

# parameters
area = "Vlassenbroek"
year = "2022"
print("Processing S4-9 " + area + " " + year)

# geodatabase to store results per area:
db_name = "SPARC_habitat_maps_" + area + year + ".gdb"
geodatabase = output_path + db_name

# DTM
dtm = geodatabase + "\\SPARC_" + area + year

# set environment settings (scratch workspace should be the same as current workspace to avoid issues with map algebra)
env.workspace = env.scratchWorkspace = geodatabase
env.overwriteOutput = True

# ------------------------------------------------------------------------
# ------ Step 4-9: Reclassify and combine layers into fysiotope map ------
# ------------------------------------------------------------------------

# note: Reclassify in arcpy works only with integer rasters (unlike the tool in ArcMap), therefore the floating point
# rasters needs to be transformed into integers, by first multiplying everything with 1000 to avoid losing detail

# input rasters
files_to_edit = arcpy.ListRasters("rel_grens*boz_f")

# set reclassify ranges for each file: [startvalue,endvalue, newvalue]
range_DD_35 = [
    [-50, 0, 0],
    [0, 50, 1]
]
range_DD_60 = [
    [-50, 0, 2],
    [0, 50, 3]
]
range_DD_04 = [
    [-50, 0, 0],
    [0, 2, 1],
    [2, 5, 2],
    [5, 10, 3],
    [10, 50, 4]
]
range_DD_85 = [
    [-100, 0, 100],
    [0, 300, 0]
]
range_DD_995 = [
    [-100, 0, 1000],
    [0, 300, 0]
]

# loop through all the files
for fte in range(0, len(files_to_edit)):
    print("current file is: " + files_to_edit[fte])

    current_file = files_to_edit[fte]
    file_suffix = current_file.split("grens_")[1].split("_boz")[0]
    ranges = eval("range_" + file_suffix)

    raster_file = Raster(current_file)

    # process: Reclassify float raster using custom function
    reclass_to_save = geodatabase + "\\reclass_" + file_suffix
    reclass = f.reclassify_float_raster(raster_file, ranges, reclass_to_save)

    # process: Apply majority filter to smoothen out the raster
    maj_rast = MajorityFilter(reclass, "EIGHT", "MAJORITY")
    maj_rast.save(geodatabase + "\\reclass_maj_" + file_suffix)  # save the result

# load all results as rasters
# (this step is probably not necessary, but added to be sure it is correct)
DD04 = Raster(geodatabase + "\\reclass_maj_DD_04")
DD35 = Raster(geodatabase + "\\reclass_maj_DD_35")
DD60 = Raster(geodatabase + "\\reclass_maj_DD_60")
DD85 = Raster(geodatabase + "\\reclass_maj_DD_85")
DD995 = Raster(geodatabase + "\\reclass_maj_DD_995")

# this step may give some issues - in which case try to execute it in the python window in Arcmap
# (remember to first run all the imports)
# combine "slik" layers DD25 & DD75
slik = (DD35 + DD60) * 10
slik.save(geodatabase + "\\slik")

# combine all fysiotopes (slik, onderwater, schor, buitengrens estuarium)
fysiotopes = slik + DD04 + DD85 + DD995

fysiotopes.save(geodatabase + "\\fysiotopes_temp")  # initial (temporary) fysiotope map

# fill up the empty cells 3 times as an iterative process
for c in range(1, 4, 1):
    fill_ras = Con(IsNull(fysiotopes), FocalStatistics(fysiotopes, NbrRectangle(5, 5, "CELL"), "MEDIAN"), fysiotopes)
    fill_ras.save(geodatabase + "\\fill_ras_" + str(c))
    fysiotopes = fill_ras

# specify the reclassify range
range_fysio = RemapRange([
    [20, 25, 20],  # hoog slik
    [25, 34, 30],  # middelhoog slik
    [34, 40, 40],  # laag slik
    [40, 41, 41],  # ondiep subtidaal
    [41, 42, 42],  # matig diep subtidaal
    [42, 43, 43],  # diep subtidaal
    [43, 44, 44],  # zeer diep subtidaal
    [44, 49, 20],  # ?
    [49, 450, 120],  # supralitoraal
    [450, 1120, 1120]  # buiten estuarien gebied
])

# reclassify, mask and save the final fysiotope raster
recl_fysiotopes = Reclassify(fysiotopes, "VALUE", range_fysio, "DATA")

arcpy.gp.ExtractByMask_sa(recl_fysiotopes,
                          gis_path + "\\Geodatabanken\\SPARC_areas_hertekend_ETRS.gdb\\" + area,
                          geodatabase + "\\fysiotopes")

recl_fysiotopes = geodatabase + "\\fysiotopes"

# process: Create polygon layer from final raster
out_poly_fysio = geodatabase + "\\fysiotopes_p"  # final polygon fysitope map
arcpy.RasterToPolygon_conversion(recl_fysiotopes, out_poly_fysio, "NO_SIMPLIFY", "VALUE")
arcpy.AlterField_management(out_poly_fysio, "gridcode", "gridcode_fysio", "gridcode_fysio")

# assign fysiotope values
# input
fysiotopes = geodatabase + "\\fysiotopes_p"  # final fysiotope map (output previous step)

# add field fysio
arcpy.AddField_management(fysiotopes, "Fysiotoop", "TEXT")

# fill the Fysio field with fysiotope names
codeblock = """
def getfysioclass(field_f, field_notfound):
    fysio_class = {
    20: "hoog slik",
    30: "middelhoog slik",
    40: "laag slik",
    41: "ondiep subtidaal",
    42: "matig diep subtidaal",
    43: "diep subtidaal",
    44: "zeer diep subtidaal",
    120: "supralitoraal",
    1120: "buiten estuarien gebied"
    }
    if field_f in fysio_class:
        fysio_label = fysio_class[field_f]
        return fysio_label
    else:
        return field_notfound"""

expression = "getfysioclass(!gridcode_fysio!, !Fysiotoop!)"
arcpy.CalculateField_management(fysiotopes, "Fysiotoop", expression, "PYTHON", codeblock)

