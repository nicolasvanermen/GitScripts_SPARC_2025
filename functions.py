
#
# ---------------------------------------------------------------------------
# functions.py
# Created on: 10/11/2020
# Author: Amber Mertens
#
# Description: Functions to be called in the "habitat_map" script
# Version: Python 27
# ---------------------------------------------------------------------------

# Import modules
import arcpy
from arcpy.sa import *
from arcpy import env

# Check out Spatial Analyst extension
if arcpy.CheckExtension("Spatial") == "Available":
    arcpy.CheckOutExtension("Spatial")
else:
    print("Spatial Extension not available")
    exit()

# Overwrite pre-existing files
env.overwriteOutput = True


# Functions
def table_to_raster(polygon_layer, polygon_join_field, excel_table, sheet_name, table_join_field, field_to_rasterize,
                    output_raster):
    """
    This funcions joins an Excel sheet to allocation polygons and subtracts the DTM to get the relative data.
    The resulting relative raster is returned, while the intermediate raster is saved in the gdb.
    Parameters: polygon_layer, polygon_join_field, table_to_join, table_join_field, field_to_rasterize, DTM,
        output_raster
    """

    # Process: Convert .xls sheet to table
    out_table = sheet_name
    arcpy.ExcelToTable_conversion(excel_table, out_table, sheet_name)

    # Process: Add Join
    table_to_join = out_table
    arcpy.AddJoin_management(polygon_layer, polygon_join_field, table_to_join, table_join_field, "KEEP_COMMON")

    # Process: Polygon to Raster
    table_name = (table_to_join.split("\\"))[-1]  # the table name is the last part of the path for the table_to_join
    arcpy.PolygonToRaster_conversion(polygon_layer, table_name + "." + field_to_rasterize, output_raster,
                                     "CELL_CENTER", "NONE", "25")

    # Process: Remove Join
    arcpy.RemoveJoin_management(polygon_layer)


def subtract_dtm(input_raster, dtm):
    # Process: Raster Calculator --> Subtract DTM to get relative values
    # set geoprocessing environment for calculation
    env.cellSize = dtm
    env.extent = dtm
    # calculation
    result = input_raster - dtm

    return result


# Function to reclassify a float raster
def reclassify_float_raster(reclassify_raster, reclassify_range, file_to_save):
    """
    This function reclassifies a float raster by multiplying all values with 1000 and then changing it to an integer.
    Parameters: raster to reclassify, reclassify range
    """

    # loop through the range to multiply the start- and endvalue by 1000 (but not the newvalue)
    for i in range(0, len(reclassify_range), 1):
        for j in range(0, (len(reclassify_range[i]) - 1), 1):
            reclassify_range[i][j] = reclassify_range[i][j] * 1000

    # transform the range into an arcpy "RemapRange" (input for reclassify)
    recl_range = RemapRange(reclassify_range)

    # Transform the floating point raster into an integer raster (necessary to run reclassify)
    multras = reclassify_raster * 1000  # multiply values by 1000 (to not lose valuable data by transforming into int)
    intras = Int(multras)  # transform the raster into integer
    # intras.save("int_raster")

    # Process: Reclassify the velocity raster
    reclass = Reclassify(intras, "VALUE", recl_range, "DATA")
    reclass.save(file_to_save)

    # delete intermediate files from scratch workspace
    # arcpy.Delete_management("int_raster")
    del multras
    del intras

    return reclass


def create_habitat_map(fysiotopes, velocity, mask, hard_substrate, hard_substrate_mask, geodatabase):
    """
    This function combines the fysiotope map with the max velocity, mask layer, and hard substrate to create the
    habitat map.
    Parameters: fysiotopes, velocity, mask, hard_substrate, geodatabase.
    It returns the habitat map
    """

    if "flood" in velocity:
        name_suffix = "flood"
    else:
        name_suffix = "max"

    # Process: Union to combine fysiotope map and max v class and mask and hard substrate mask
    fys_vel = geodatabase + "\\fysio_vel_" + name_suffix
    arcpy.Union_analysis([fysiotopes, velocity], fys_vel, "ALL", "", "GAPS")  # [inputs], output, parameters
    fys_vel_ip = geodatabase + "\\fysio_vel_ip_" + name_suffix
    arcpy.Union_analysis([fys_vel, mask], fys_vel_ip, "ALL", "", "GAPS")
    fys_vel_ip_hsmask = geodatabase + "\\fysio_vel_ip_hsmask_" + name_suffix
    arcpy.Union_analysis([fys_vel_ip, hard_substrate_mask], fys_vel_ip_hsmask, "ALL", "", "GAPS")

    # Process: Select based on mask
    field_names1 = [fi.name for fi in arcpy.ListFields(fys_vel_ip)]
    field1 = next((x for x in field_names1 if mask.split("\\")[-1] in x), False)
    fysio_vel_ip_inmask = geodatabase + "\\fysio_vel_ip_inmask_" + name_suffix
    where_clause1 = '"' + field1 + '"' + ' <> -1'
    arcpy.Select_analysis(fys_vel_ip_hsmask, fysio_vel_ip_inmask, where_clause1)  # input, output, where clause
    # FID intplan has a value where the mask exists, and -1 where it does not exist
    # so by selecting WHERE NOT -1, we take the places where the mask exists

    # Process: Union with hard substrate
    fysio_vel_ip_inmask_hs = geodatabase + "\\fysio_vel_ip_inmask_hs_" + name_suffix
    arcpy.Union_analysis([fysio_vel_ip_inmask, hard_substrate], fysio_vel_ip_inmask_hs, "ALL", "", "GAPS")

    # Process: Select
    field_names2 = [fi.name for fi in arcpy.ListFields(fysio_vel_ip_inmask_hs)]
    field2 = next((x for x in field_names2 if fysio_vel_ip_inmask.split("\\")[-1] in x), False)
    habitat_map = geodatabase + "\\habitat_map_" + name_suffix
    where_clause2 = '"' + field2 + '"' + ' <> -1'
    arcpy.Select_analysis(fysio_vel_ip_inmask_hs, habitat_map, where_clause2)
    # after the union, take the only the parts where the initial layer exists
    # i.e. leave out the other hard substrate parts where there is no info on the fysio_vel

    # Process: Add Field
    fields_to_add = ["Fysio", "Dyn", "Habmap"]
    for fa in fields_to_add:
        arcpy.AddField_management(habitat_map, fa, field_type="TEXT", field_is_nullable="NULLABLE")

    return habitat_map


def fill_empty_polygons(map_leeg, fc, geodatabase, source, target, where_clause):
    """ This function fills empty polygons based on neigbouring polygons (sharing the longest border).
        map_leeg = the feature class with the empty polygons in question
        fc = a filename suffix to distinguish the saved files: dyn or habmap
        source = original source layer where the empty polygons are coming from, used for clipping the neighbors
        target = final layer to put the filled polygons in
        where_clause = selection to fill (eliminate)
     """
    map_leeg_buf = geodatabase + "\\leeg_buf_" + fc
    arcpy.Buffer_analysis(map_leeg, map_leeg_buf, "2 Meters", "FULL", "FLAT", "NONE", "", "PLANAR")

    # Clip the buffered polygons shape from the original habitat_map file
    map_buf_clip = geodatabase + "\\buf_clip_" + fc  # the shape from habitat map to cover the empty polygons plus
        # a sliver of the surrounding polygons
    arcpy.Clip_analysis(source, map_leeg_buf, map_buf_clip)

    # Change the result to singlepart polygons (each individual instead of one combined multipolygon)
    try:
        map_bc_singlep = geodatabase + "\\buf_clip_singlep_" + fc  # single part file of the buffered clipped habitat map
        arcpy.MultipartToSinglepart_management(map_buf_clip, map_bc_singlep)
    except arcpy.ExecuteError:
        map_bc_singlep = map_buf_clip

    # make the singlepart polygons into a feature layer
    arcpy.MakeFeatureLayer_management(map_bc_singlep, "bc_singlep_polys")

    # select from this feature layer again where the habmap is empty (so not the surrounding bits from the buffer)
    arcpy.SelectLayerByAttribute_management("bc_singlep_polys", "NEW_SELECTION", where_clause)

    # eliminate selected (take on the value of neighbour with which it shares the largest border)
    map_buffered_polys_filled = geodatabase + "\\buffered_polys_filled_" + fc
    arcpy.Eliminate_management("bc_singlep_polys", map_buffered_polys_filled)

    # use the result before the buffer to clip out the original shape again
    map_filler_polys = geodatabase + "\\filler_polys_" + fc
    arcpy.Clip_analysis(map_buffered_polys_filled, map_leeg, map_filler_polys)

    # delete some fields
    fields_fc = [fi.name for fi in arcpy.ListFields(map_filler_polys)]
    fields_source = [fi.name for fi in arcpy.ListFields(target)]
    to_delete = []
    for field_one in fields_fc:
        if field_one not in fields_source:
            to_delete.append(field_one)
    input_del = (";".join(to_delete))
    if input_del != "":
        arcpy.DeleteField_management(map_filler_polys, input_del)

    # # select from original file the polygons where habmap field is NOT empty
    # map_notempty = geodatabase + "\\notempty_" + fc
    # arcpy.Select_analysis(source, map_notempty, where_clause_ne)

    # Then erase the filler polys from the not empty habmap file
    map_empty_erased = geodatabase + "\\empty_erased_" + fc
    arcpy.Erase_analysis(target, map_filler_polys, map_empty_erased)

    # Then append them back in - so essentially replacing the empty ones with the filled ones
    arcpy.Append_management(map_filler_polys, map_empty_erased, "TEST")

    return map_empty_erased
