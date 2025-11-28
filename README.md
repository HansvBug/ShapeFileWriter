# ShapeWriter


(Minimal) ESRI Shapefile writer for Lazarus / Free Pascal.

This is a simple program that serves as a demo for the shapewriter.pas unit.

## Features

- Produces .shp (geometry), .shx (index) and .dbf (attribute) files
- Supports Point, PolyLine and Polygon geometry types (including MULTI-PART)
- All geometry coordinates stored as Double precision (IEEE 754)
- Full Z and M coordinate support (3D and measure values)
- Proper endian handling: file headers use big-endian, geometry data uses little-endian
- Minimal DBF implementation with configurable field definitions
- Optional projection file (.prj) support with WKT strings
- Progress reporting and buffered file writing for large datasets

## Limitations

- Basic DBF field types (Character, Numeric, Date, Logical)
- UTF-8 to single-byte codepage conversion for DBF compatibility

## Notes

- No dependencies required, uses only standard FPC/Lazarus units.
