{*
 * ShapeWriter.pas
 *
 * ShapeWriter - ESRI Shapefile writer for Lazarus/Free Pascal
 * Copyright (c) 2025 Hans van Buggenum
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *}

unit ShapeWriter;

{$mode objfpc}{$H+}

{
  ShapeWriter.pas

  (Minimal) ESRI Shapefile writer for Lazarus / Free Pascal.

  Features:
  - Produces .shp (geometry), .shx (index) and .dbf (attribute) files
  - Supports Point, PolyLine and Polygon geometry types (including MULTI-PART)
  - All geometry coordinates stored as Double precision (IEEE 754)
  - Full Z and M coordinate support (3D and measure values)
  - Proper endian handling: file headers use big-endian, geometry data uses little-endian
  - Minimal DBF implementation with configurable field definitions
  - Optional projection file (.prj) support with WKT strings
  - Progress reporting and buffered file writing for large datasets

  Limitations:
  - Basic DBF field types (Character, Numeric, Date, Logical)
  - UTF-8 to single-byte codepage conversion for DBF compatibility

  Notes:
  - No dependencies required, uses only standard FPC/Lazarus units.
}

interface

uses
  Classes, SysUtils, Math;

type
  { Progress event for monitoring write operations }
  TShapeWriterProgressEvent = procedure(Sender: TObject; Current, Total: Integer; const Message: string) of object;

  { Stages of the shapefile writing process }
  TShapeWriterProgressStage = (psInitializing, psWritingSHP, psWritingSHX, psWritingDBF, psWritingPRJ, psComplete);

  { Log event for ShapeWriter notifications }
  TShapeWriterLogEvent = procedure(Sender: TObject; const Message: string) of object;

  {
    ESRI Shapefile geometry type codes
    Values correspond to the Shapefile specification:
      stNull    = 0: Null shape
      stPoint   = 1: Point geometry
      stPolyLine= 3: Polyline geometry (single or multi-part)
      stPolygon = 5: Polygon geometry (single or multi-part)

    Note: Z and M variants are automatically handled based on coordinate data presence
  }
  TEShapeType = (stNull = 0, stPoint = 1, stPolyLine = 3, stPolygon = 5); // TEShapeType (TEsriShapeType. TShapeType conflicts with extctrls.

  {
    2D/3D point with double-precision coordinates
    Supports full XYZM coordinates for 3D shapes and measure values
  }
  TPointD = record
    X, Y: Double;  // Required: X and Y coordinates (2D position)
    Z: Double;     // Optional: Z coordinate (3D height/elevation)
    M: Double;     // Optional: M measure value (linear referencing)
  end;

  { Dynamic array of TPointD for storing coordinate sequences }
  TPointDArray = array of TPointD;

  {
    DBF field definition structure
    Follows dBase III field specification limits
  }
  TDBFField = record
    Name: string;      // Field name (max 11 characters)
    FieldType: Char;   // Field type: 'C'=Character, 'N'=Numeric, 'D'=Date, 'L'=Logical
    Size: Integer;     // Field length in bytes
    Decimals: Integer; // Decimal places (for Numeric fields only)
  end;

  {
    In-memory representation of a shapefile feature
    Contains both geometry (shape) and attribute data
    Supports multi-part geometries and optional Z/M coordinates
  }
  TFeature = record
    ShapeType: TEShapeType;        // Geometry type for this feature
    Parts: array of TPointDArray;  // Array of parts (each part is a coordinate array) { #todo : rename parts }
    Values: array of string;       // Attribute values corresponding to DBF fields
  end;

  {
    Main shapefile writer class
    Collects features and writes ESRI Shapefile format (.shp, .shx, .dbf, .prj)
    Automatically detects and handles Z and M coordinates when present

    Usage example:
      writer:= TShapefileWriter.Create(stPolyLine);
      try
        writer.AddDBFField('NAME', 'C', 20);
        writer.AddDBFField('ELEVATION', 'N', 10, 2);
        writer.AddPoly(points, ['Road 1', '125.5']);
        writer.SaveTo('roads');
      finally
        writer.Free;
      end;
  }

  { TShapefileWriter }
  TShapefileWriter = class
  private
    fFeatures: array of TFeature;               // Collection of features to be written
    fShapeType: TEShapeType;                    // Base shape type for this writer instance
    fOutputDir: string;                         // Output directory for files
    fDBFEncoding: string;                       // Character encoding for DBF files (e.g., 'CP1252', 'ISO-8859-1')
    fWriteBuffer: TMemoryStream;                // Buffer for efficient file writing
    fBufferSize: Integer;                       // Size of write buffer in bytes
    fOnProgress: TShapeWriterProgressEvent;     // Event for progress reporting
    fProgressStage: TShapeWriterProgressStage;  // Current stage of writing process
    fDBFFields: array of TDBFField;             // DBF field definitions
    fOnLog: TShapeWriterLogEvent;

    { Core file writing methods }
    procedure WriteMainAndIndex(const Prefix: string);
    procedure WriteDBF(const Prefix: string);
    procedure WritePRJ(const Prefix: string; const WKT: string);

    { Header and geometry writing utilities }
    procedure WriteHeaderStream(S: TStream; ShapeTypeValue: Integer;
      FileLengthWords: Integer; const {%H-}Box: array of Double;
      const ZBox: array of Double; const MBox: array of Double);
    procedure UpdateBounds(var Box : array of Double; const P : TPointD; Row : Integer);

    { Path and encoding utilities }
    function ResolvePrefix(const Prefix: string): string;
    function UTF8ToDBFBytes(const S: string; const CodePage: string): TBytes;
    { Polygon validation and processing }
    function IsPolygonClosed(const Points: TPointDArray): Boolean;
    function HasZ: Boolean;
    function HasM: Boolean;
    procedure EnsurePolygonClosed(var Points: TPointDArray);
    function PolygonSignedArea(const Part: TPointDArray): Double;
    procedure EnsurePolygonOrientation(var Parts: array of TPointDArray);
    procedure ReversePolygon(var Part: TPointDArray);
    { Buffered writing utilities }
    procedure InitWriteBuffer(Size: Integer = 65536);
    procedure FlushWriteBuffer(var FS: TFileStream);
    { Progress reporting }
    procedure DoProgress(Current, Total: Integer; const Msg: string = '');
    procedure SetProgressStage(Stage: TShapeWriterProgressStage);
    { logging }
    procedure DoLog(const Msg: string);
    //
    function ShapeTypeToString(ShapeType: TEShapeType): string;

  public
    { Constructor requires shape type which determines geometry type for all features }
    constructor Create(ShapeType: TEShapeType);

    { DBF field management }
    function AddDBFField(const AName : string; AType : Char; ASize : Integer; ADecimals : Integer= 0) : Boolean;
    { Geometry addition methods }
    function AddPoint(X, Y : Double; const Values : array of string) : Boolean; overload;
    function AddPoint(X, Y, Z, M : Double; const Values : array of string) : Boolean; overload;
    function AddPoly(const Points : array of TPointD; const Values : array of string; Row : Integer) : Boolean;
    function AddPolyParts(const Parts : array of TPointDArray; const Values : array of string; Row : Integer) : Boolean;
    { Main writing method - generates all shapefile components }
    function SaveTo(const Prefix : string; const WKT : string= '') : Boolean;

    { Properties }
    property OutputDir: string read fOutputDir write fOutputDir;       // Output directory
    property DBFEncoding: string read fDBFEncoding write fDBFEncoding; // DBF character encoding
    property OnProgress: TShapeWriterProgressEvent read fOnProgress write fOnProgress; // Progress event
    property OnLog: TShapeWriterLogEvent read fOnLog write fOnLog;  // LogEvent
  end;

const
  // RD New / Amersfoort (EPSG:28992) - Dutch coordinate system
  WKT_28992 = 'PROJCS["Amersfoort / RD New",' +
              'GEOGCS["Amersfoort",' +
              'DATUM["Amersfoort",' +
              'SPHEROID["Bessel 1841",6377397.155,299.1528128]],' +
              'PRIMEM["Greenwich",0],' +
              'UNIT["degree",0.0174532925199433]],' +
              'PROJECTION["Oblique_Stereographic"],' +
              'PARAMETER["latitude_of_origin",52.15616055555555],' +
              'PARAMETER["central_meridian",5.38763888888889],' +
              'PARAMETER["scale_factor",0.9999079],' +
              'PARAMETER["false_easting",155000],' +
              'PARAMETER["false_northing",463000],' +
              'UNIT["metre",1]]';

  // WGS84 (EPSG:4326) - Global latitude/longitude coordinate system
  WKT_4326 =  'GEOGCS["GCS_WGS_1984",DATUM["D_WGS_1984",SPHEROID["WGS_1984",6378137,298.257223563]],PRIMEM["Greenwich",0],UNIT["Degree",0.017453292519943295]]';
  { #todo : If there are going to be more, it's better to put the WKT strings in a separate unit. }

implementation

{ ==================== Helper Functions ==================== }

{
  Converts a 32-bit integer to 4-byte little-endian format
  @param Value: Integer to convert
  @return: TBytes array in little-endian order
}
function Int32ToBytesLE(Value: Integer): TBytes;
begin
  Result:= nil;  // Initialize
  SetLength(Result, 4);
  Result[0]:= Byte(Value and $FF);
  Result[1]:= Byte((Value shr 8) and $FF);
  Result[2]:= Byte((Value shr 16) and $FF);
  Result[3]:= Byte((Value shr 24) and $FF);
end;

{
  Converts a 32-bit integer to 4-byte big-endian format
  @param Value: Integer to convert
  @return: TBytes array in big-endian order
}
function Int32ToBytesBE(Value: Integer): TBytes;
begin
  Result:= nil;
  SetLength(Result, 4);
  Result[0]:= Byte((Value shr 24) and $FF);
  Result[1]:= Byte((Value shr 16) and $FF);
  Result[2]:= Byte((Value shr 8) and $FF);
  Result[3]:= Byte(Value and $FF);
end;

{
  Converts a Double to 8-byte little-endian format
  @param Value: Double to convert
  @return: TBytes array in little-endian order
  Note: Assumes little-endian CPU architecture (x86/x64)
}
function DoubleToBytesLE(Value: Double): TBytes;
begin
  Result:= nil;
  SetLength(Result, 8);
  Move(Value, Result[0], 8);
end;

{ ==================== TShapefileWriter Implementation ==================== }

{
  Constructor - initializes shapefile writer with specific geometry type
  @param ShapeType: The type of geometry this writer will handle (points, lines, or polygons)
}
constructor TShapefileWriter.Create(ShapeType: TEShapeType);
begin
  inherited Create;
  fShapeType:= ShapeType;
  SetLength(fFeatures, 0);
  SetLength(fDBFFields, 0);
  fOutputDir:= '';
  {$IFDEF WINDOWS}
  fDBFEncoding:= 'CP1252';  // Windows Latin-1
  {$ELSE}
  fDBFEncoding:= 'UTF8';    // Linux default
  {$ENDIF}
  fWriteBuffer:= nil;
end;

{
  Adds a field definition to the DBF file structure (fDBFFields)
  @param AName: Field name (max 11 characters, will be truncated)
  @param AType: Field type ('C', 'N', 'D', 'L')
  @param ASize: Field size in bytes
  @param ADecimals: Decimal places for numeric fields
}
function TShapefileWriter.AddDBFField(const AName: string; AType: Char; ASize: Integer; ADecimals: Integer = 0): Boolean;
var
  dbfField: TDBFField;
  lName: string;
  t: Char;
begin
  Result:= True;
  // Preparing an array with field names and data and the correct data type
  // Basic name truncation to DBF limit
  lName:= AName;
  if Length(lName) > 10 then
    lName:= Copy(lName, 1, 10); // Truncate to DBF field name limit (header)

  // validate size
  if ASize < 1 then begin
    // Perhaps approach differently. For the time being, this is sufficient
    DoLog(Format('Invalid DBF field size for "%s": %d. Size must be >= 1. (Shape writer).', [AName, ASize]));
    //raise Exception.CreateFmt('Invalid DBF field size for "%s": %d. Size must be >= 1.', [AName, ASize]);
    Result:= False;
    Exit;
  end;

  // normalize and validate field type
  t:= UpCase(AType);
  if not (t in ['C', 'N', 'D', 'L']) then begin
    // fallback to Character if unknown type provided
    t:= 'C';
  end;

  // validate decimals for numeric fields
  if t = 'N' then begin
    if ADecimals < 0 then
      ADecimals:= 0;
    // decimals cannot be >= field length
    if ADecimals >= ASize then
      ADecimals:= Max(0, ASize - 1);
  end
  else
    ADecimals:= 0; // only numeric fields have decimals

  // fill record and append
  dbfField.Name:= lName;
  dbfField.FieldType:= t;
  dbfField.Size:= ASize;
  dbfField.Decimals:= ADecimals;

  SetLength(fDBFFields, Length(fDBFFields) + 1);
  fDBFFields[High(fDBFFields)]:= dbfField;
end;

{
  Adds a point feature with 2D coordinates to the shapefile
  @param X: X-coordinate of the point (longitude)
  @param Y: Y-coordinate of the point (latitude)
  @param Values: Array of attribute values corresponding to DBF field definitions
  @return Boolean: True if point was successfully added, False if shape type mismatch occurs
  @note Logs an error and returns False instead of raising an exception when shape type doesn't match
  @note The function handles both geometry data (coordinates) and attribute data (DBF values)
}
function TShapefileWriter.AddPoint(X, Y: Double; const Values: array of string): Boolean;
var
  f: TFeature;
  i: Integer;
begin
  Result:= True;
  if fShapeType <> stPoint then begin
    DoLog(Format('Writer configured for shape type: %s. Found shape type: %s. (Shape writer).', [ShapeTypeToString(stPoint), ShapeTypeToString(fShapeType)]));
    //raise Exception.Create('Writer configured for other shape type');
    Result:= False;
    Exit;
  end;

  f.ShapeType:= stPoint;
  SetLength(f.Parts, 1);
  SetLength(f.Parts[0], 1);

  f.Parts[0][0].X:= X;
  f.Parts[0][0].Y:= Y;
  f.Parts[0][0].Z:= NaN;
  f.Parts[0][0].M:= NaN;

  // safe copy of values
  SetLength(f.Values, Length(Values));
  for i:= 0 to High(f.Values) do
    f.Values[i]:= Values[i];

  // Create a list all features (data).
  SetLength(fFeatures, Length(fFeatures) + 1);
  fFeatures[High(fFeatures)]:= f;
end;

{
  Adds a point feature with full XYZM coordinates
  @param X, Y, Z, M: Point coordinates and optional Z/M values
  @param Values: Attribute values
  @return Boolean: True if point was successfully added, False if shape type mismatch occurs
  @note Logs an error and returns False instead of raising an exception when shape type doesn't match
  @note The function handles both geometry data (coordinates) and attribute data (DBF values)
}
function TShapefileWriter.AddPoint(X, Y, Z, M: Double; const Values: array of string): Boolean;
var
  f: TFeature;
  i: Integer;
begin
  Result:= True;
  if fShapeType <> stPoint then begin
    DoLog(Format('Writer configured for shape type: %s. Found shape type: %s. (Shape writer).', [ShapeTypeToString(stPoint), ShapeTypeToString(fShapeType)]));
    //raise Exception.Create('Writer configured for other shape type');
    Result:= False;
    Exit;
  end;

  f.ShapeType:= stPoint;
  SetLength(f.Parts, 1);
  SetLength(f.Parts[0], 1);

  f.Parts[0][0].X:= X;
  f.Parts[0][0].Y:= Y;
  f.Parts[0][0].Z:= Z;
  f.Parts[0][0].M:= M;

  // safe copy of values
  SetLength(f.Values, Length(Values));
  for i:= 0 to High(f.Values) do
    f.Values[i]:= Values[i];

  SetLength(fFeatures, Length(fFeatures) + 1);
  fFeatures[High(fFeatures)]:= f;
end;

{
  Adds a single-part polyline or polygon feature
  @param Points: Array of points defining the geometry
  @param Values: Attribute values
  @param Row: used for better logging (row in the CSV file)
  @return Boolean: True if point was successfully added, False if shape type mismatch occurs
  @note Logs an error and returns False instead of raising an exception when shape type doesn't match
  @note The function handles both geometry data (coordinates) and attribute data (DBF values)
}
function TShapefileWriter.AddPoly(const Points : array of TPointD; const Values : array of string; Row : Integer): Boolean;
var
  f: TFeature;
  i: Integer;
  tmpPart: TPointDArray = nil;
begin
  Result:= True;
  if not (fShapeType in [stPolyLine, stPolygon]) then begin
    DoLog(Format('Writer configured for other shape type. Expected polyline or polygon. Found type: %s. (Shape writer).', [ShapeTypeToString(fShapeType)]));
    //raise Exception.Create('Writer configured for other shape type');
    Result:= False;
    Exit;
  end;

  f.ShapeType:= fShapeType;

  SetLength(tmpPart, Length(Points));
  for i:= 0 to High(Points) do begin
    tmpPart[i]:= Points[i];
    if IsNan(tmpPart[i].Z) then tmpPart[i].Z:= NaN;
    if IsNan(tmpPart[i].M) then tmpPart[i].M:= NaN;
  end;

  if fShapeType = stPolygon then begin
    EnsurePolygonClosed(tmpPart);
    if Length(tmpPart) < 4 then begin
      DoLog(Format('Polygon part %d must have at least 3 points. Found: . Row: %d. (Shape writer).', [Length(tmpPart), Row]));
      //raise Exception.Create('Polygon must have at least 3 points.');
    end;
  end;

  SetLength(f.Parts, 1);
  f.Parts[0]:= tmpPart;

  if fShapeType = stPolygon then
    EnsurePolygonOrientation(f.Parts);

  // safe copy of values
  SetLength(f.Values, Length(Values));
  for i:= 0 to High(f.Values) do
    f.Values[i]:= Values[i];

  SetLength(fFeatures, Length(fFeatures) + 1);
  fFeatures[High(fFeatures)]:= f;
end;

{
  Adds a multi-part polyline or polygon feature
  @param Parts: Array of parts, each part is an array of points
  @param Values: Attribute values
  @return Boolean: True if the polypart was successfully added, False if shape type mismatch occurs
  @note Logs an error and returns False instead of raising an exception when shape type doesn't match
  @note The function handles both geometry data (coordinates) and attribute data (DBF values)
}
function TShapefileWriter.AddPolyParts(const Parts : array of TPointDArray; const Values : array of string; Row : Integer): Boolean;
var
  f: TFeature;
  p, i: Integer;
  tmpParts: array of TPointDArray = Nil;
begin
  Result:= True;
  if not (fShapeType in [stPolyLine, stPolygon]) then begin
    DoLog(Format('Writer configured for other shape type. Expected line or point. Found type: %s. (Shape writer).', [ShapeTypeToString(fShapeType)]));
    //raise Exception.Create('Writer configured for other shape type');
    Result:= False;
    Exit;
  end;

  f.ShapeType:= fShapeType;
  SetLength(tmpParts, Length(Parts));

  for p:= 0 to High(Parts) do begin
    SetLength(tmpParts[p], Length(Parts[p]));
    if Length(Parts[p]) > 0 then
      Move(Parts[p][0], tmpParts[p][0], Length(Parts[p]) * SizeOf(TPointD));

    for i:= 0 to High(tmpParts[p]) do begin
      if IsNan(tmpParts[p][i].Z) then tmpParts[p][i].Z:= NaN;
      if IsNan(tmpParts[p][i].M) then tmpParts[p][i].M:= NaN;
    end;

    if fShapeType = stPolygon then begin
      EnsurePolygonClosed(tmpParts[p]);
      if Length(tmpParts[p]) < 4 then
       DoLog(Format('Polygon part %d must have at least 3 points. Found: , row: %d. (Shape writer).', [p, Row]));
    end;
  end;

  if fShapeType = stPolygon then
    EnsurePolygonOrientation(tmpParts);

  f.Parts:= tmpParts;

  // safe copy of values
  SetLength(f.Values, Length(Values));
  for i:= 0 to High(f.Values) do
    f.Values[i]:= Values[i];

  SetLength(fFeatures, Length(fFeatures) + 1);
  fFeatures[High(fFeatures)]:= f;
end;

{
  Writes complete shapefile set to disk
  @param Prefix: Base filename without extension
  @param WKT: Optional Well-Known Text projection string
  @raises Exception if no features have been added
}
function TShapefileWriter.SaveTo(const Prefix: string; const WKT: string = ''): Boolean;
var
  fullPrefix: string;
begin
  Result:= False;
  if Length(fFeatures) = 0 then begin
    DoLog('No features to write - cannot create empty shapefile');
    raise Exception.Create('No features to write - cannot create empty shapefile');
  end;

  SetProgressStage(psInitializing);

  // Resolve full output path
  fullPrefix:= ResolvePrefix(Prefix);

  // Initialize write buffer for efficient file I/O. Buffer size = 65536. (It is a TMemoryStream).
  InitWriteBuffer(65536);

  try
    // Write geometry and index files
    SetProgressStage(psWritingSHP);
    WriteMainAndIndex(fullPrefix);

    // Write attribute file
    SetProgressStage(psWritingDBF);
    WriteDBF(fullPrefix);

    // Write projection file if WKT provided
    if WKT <> '' then begin
      SetProgressStage(psWritingPRJ);
      WritePRJ(fullPrefix, WKT);
    end;

    SetProgressStage(psComplete);
    Result:= True;
  finally
    // Clean up write buffer
    if fWriteBuffer <> nil then begin
      fWriteBuffer.Free;
      fWriteBuffer:= nil;
    end;
  end;
end;

{
  Updates bounding box with point coordinate if it extends current extents
  @param Box: Bounding box array [xmin, ymin, xmax, ymax]
  @param P: Point to test against bounding box
  @param Row: Row number for error reporting
}
procedure TShapefileWriter.UpdateBounds(var Box: array of Double; const P: TPointD; Row: Integer);
begin
  // Verify minimum 4 elements
   if Length(Box) < 4 then Exit;

   // Skip invalid points
   if IsNan(P.X) or IsNan(P.Y) then begin
     DoLog('Invalid point found: X = ' + FloatToStr(P.X) + ', Y = ' + FloatToStr(P.Y) + ' Row: ' + IntToStr(Row));
     Exit;
   end;
   if IsInfinite(P.X) or IsInfinite(P.Y) then begin
     DoLog('Invalid point found (Infinite). X = ' + FloatToStr(P.X) + ', Y = ' + FloatToStr(P.Y) + ' Row: ' + IntToStr(Row));
     Exit;
   end;

   // Update min/max with extra checks to avoid comparisons with NaN
   if (not IsNan(Box[0])) and (not IsInfinite(Box[0])) then begin
     if P.X < Box[0] then Box[0]:= P.X;
   end
   else
     Box[0]:= P.X;

   if (not IsNan(Box[1])) and (not IsInfinite(Box[1])) then begin
     if P.Y < Box[1] then Box[1]:= P.Y;
   end
   else
     Box[1]:= P.Y;

   if (not IsNan(Box[2])) and (not IsInfinite(Box[2])) then begin
     if P.X > Box[2] then Box[2]:= P.X;
   end
   else
     Box[2]:= P.X;

   if (not IsNan(Box[3])) and (not IsInfinite(Box[3])) then begin
     if P.Y > Box[3] then Box[3]:= P.Y;
   end
   else
     Box[3]:= P.Y;
end;

{
  Resolves full file prefix including output directory
  @param Prefix: Base filename
  @return: Full path prefix including directory
}
function TShapefileWriter.ResolvePrefix(const Prefix: string): string;
begin
  if (fOutputDir <> '') then
    Result:= IncludeTrailingPathDelimiter(fOutputDir) + Prefix
  else
    Result:= Prefix;
end;

{
  Converts UTF-8 string to specified codepage for DBF compatibility
  @param S: UTF-8 string to convert
  @param CodePage: Target codepage ('CP1252', 'ISO-8859-1', etc.)
  @return: Byte array in target encoding
}
function TShapefileWriter.UTF8ToDBFBytes(const S: string; const CodePage: string): TBytes;
var
  enc: TEncoding;
  uStr: UnicodeString;
  encWasCreated: Boolean;
begin
  if S = '' then Exit;

  // Choose encoding
  enc:= nil;
  encWasCreated:= False;
  if SameText(CodePage, 'CP1252') then begin
    enc:= TEncoding.GetEncoding(1252);
    encWasCreated:= True;
  end
  else if SameText(CodePage, 'ISO-8859-1') then begin
    enc:= TEncoding.GetEncoding(28591);
    encWasCreated:= True;
  end
  else if SameText(CodePage, 'UTF8') then
    enc:= TEncoding.UTF8
  else
    enc:= TEncoding.ANSI; // fallback

  try
    // explicit conversion from Lazarus UTF-8 'string' to UnicodeString to avoid implicit-conversion warnings
    uStr:= UTF8Decode(S);
    // encode
    Result:= enc.GetBytes(uStr);
  finally
    // Free only if we have created an encoding via GetEncoding
    if encWasCreated and (enc <> nil) then
      enc.Free;
  end;
end;

{
  Checks if polygon ring is properly closed (first point = last point)
  @param Points: Polygon ring coordinates
  @return: True if polygon is closed
}
function TShapefileWriter.IsPolygonClosed(const Points: TPointDArray): Boolean;
begin
  Result:= (Length(Points) > 0) and
           (Points[0].X = Points[High(Points)].X) and
           (Points[0].Y = Points[High(Points)].Y);
end;

{
  Checks if any feature contains Z coordinates
  @return: True if Z coordinates are present in any feature
}
function TShapefileWriter.HasZ: Boolean;
var
  i, p, k: Integer;
begin
  Result:= False;
  for i:= 0 to High(fFeatures) do
    for p:= 0 to High(fFeatures[i].Parts) do
      for k:= 0 to High(fFeatures[i].Parts[p]) do begin
        if not IsNan(fFeatures[i].Parts[p][k].Z) then begin
          Result:= True;
          Exit; // Stop at first valid Z value
        end;
      end;
end;

{
  Checks if any feature contains M values
  @return: True if M values are present in any feature
}
function TShapefileWriter.HasM: Boolean;
var
  i, p, k: Integer;
begin
  Result:= False;
  for i:= 0 to High(fFeatures) do
    for p:= 0 to High(fFeatures[i].Parts) do
      for k:= 0 to High(fFeatures[i].Parts[p]) do begin
        if not IsNan(fFeatures[i].Parts[p][k].M) then begin
          Result:= True;
          Exit; // Stop at first valid M value
        end;
      end;
end;

{
  Ensures polygon ring is closed by adding closing point if needed
  @param Points: Polygon ring (modified in place if not closed)
}
procedure TShapefileWriter.EnsurePolygonClosed(var Points: TPointDArray);
begin
  if not IsPolygonClosed(Points) then begin
    SetLength(Points, Length(Points) + 1);
    Points[High(Points)]:= Points[0]; // Close ring by duplicating first point
  end;
end;

{
  Calculates signed area of polygon ring using shoelace formula
  Positive area = counter-clockwise, Negative area = clockwise
  @param Part: Polygon ring coordinates
  @return: Signed area of polygon
}
function TShapefileWriter.PolygonSignedArea(const Part: TPointDArray): Double;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to High(Part) - 1 do
    Result:= Result + (Part[i].X * Part[i + 1].Y - Part[i + 1].X * Part[i].Y);

  Result:= Result / 2;
end;

{
  Ensures proper polygon ring orientation according to Shapefile specification:
  Outer rings clockwise, inner rings (holes) counter-clockwise
  @param Parts: Array of polygon rings (modified in place)
}
procedure TShapefileWriter.EnsurePolygonOrientation(var Parts: array of TPointDArray);
var
  i: Integer;
  area: Double;
begin
  for i:= 0 to High(Parts) do begin
    // First check if the polygon part is valid
    if Length(Parts[i]) < 3 then begin
      DoLog(Format('Warning: Polygon part %d has insufficient points (%d). Skipping orientation.', [i, Length(Parts[i])]));
      Continue;
    end;

    area:= PolygonSignedArea(Parts[i]);

    // Extended area validation
    if IsNan(area) then begin
      DoLog(Format('Error: NaN area calculated for polygon part %d. Part contains invalid coordinates.', [i]));
      Continue;
    end;

    if IsInfinite(area) then begin
      DoLog(Format('Error: Infinite area calculated for polygon part %d. Coordinate values may be too large.', [i]));
      Continue;
    end;

    // Check for degenerate polygons (very small area)
    if Abs(area) < 1E-12 then begin
      DoLog(Format('Warning: Degenerate polygon part %d detected (area: %g). Orientation correction skipped.', [i, area]));
      Continue;
    end;

    // Apply orientation correction
    if i = 0 then begin
      // Outer ring should be clockwise (negative area)
      if area > 0 then begin
        DoLog(Format('Correcting outer ring %d orientation (was CCW, making CW)', [i]));
        ReversePolygon(Parts[i]);
      end;
    end
    else begin
      // Inner rings (holes) should be counter-clockwise (positive area)
      if area < 0 then begin
        DoLog(Format('Correcting inner ring %d orientation (was CW, making CCW)', [i]));
        ReversePolygon(Parts[i]);
      end;
    end;
  end;
end;

{
  Reverses the vertex order of a polygon ring
  @param Part: Polygon ring to reverse (modified in place)
}
procedure TShapefileWriter.ReversePolygon(var Part: TPointDArray);
var
  i, j: Integer;
  pointD: TPointD;
begin
  i:= 0;
  j:= High(Part);
  while i < j do begin
    pointD:= Part[i];
    Part[i]:= Part[j];
    Part[j]:= pointD;
    Inc(i);
    Dec(j);
  end;
end;

{
  Initializes the write buffer for efficient file I/O
  @param Size: Buffer size in bytes (default 64KB)
}
procedure TShapefileWriter.InitWriteBuffer(Size: Integer);
begin
  fBufferSize:= Size;
  fWriteBuffer:= TMemoryStream.Create;
  fWriteBuffer.Size:= 0;
end;

{
  Flushes write buffer contents to file stream
  @param FS: File stream to write to
}
procedure TShapefileWriter.FlushWriteBuffer(var FS: TFileStream);
begin
  if fWriteBuffer.Size > 0 then begin
    FS.WriteBuffer(fWriteBuffer.Memory^, fWriteBuffer.Size);
    fWriteBuffer.Clear;
  end;
end;

{
  Fires progress event if assigned
  @param Current: Current progress value
  @param Total: Total progress value
  @param Msg: Progress message
}
procedure TShapefileWriter.DoProgress(Current, Total: Integer; const Msg: string);
begin
  if Assigned(fOnProgress) then
    fOnProgress(Self, Current, Total, Msg);
end;

{
  Updates progress stage and fires progress event
  @param Stage: New progress stage
}
procedure TShapefileWriter.SetProgressStage(Stage: TShapeWriterProgressStage);
const
  StageNames: array[TShapeWriterProgressStage] of string = (
    'Initializing...', 'Writing SHP...', 'Writing SHX...',
    'Writing DBF...', 'Writing PRJ...', 'Complete'
  );
begin
  fProgressStage:= Stage;
  DoProgress(0, 0, StageNames[Stage]);
end;

{
  Fires log event if assigned
  @param Msg: Log message
}
procedure TShapefileWriter.DoLog(const Msg : string);
begin
  if Assigned(fOnLog) then
    fOnLog(Self, Msg);
end;

{
  Converts shape type enum to string representation
  @param ShapeType: Shape type enum value
  @return: String representation of shape type
}
function TShapefileWriter.ShapeTypeToString(ShapeType: TEShapeType): string;
begin
  case ShapeType of
    stNull: Result:= 'Null';
    stPoint: Result:= 'Point';
    stPolyLine: Result:= 'PolyLine';
    stPolygon: Result:= 'Polygon';
    else Result:= 'Unknown';
  end;
end;

{
  Writes 100-byte Shapefile header to stream
  @param S: Target stream
  @param ShapeTypeValue: Shape type code (including Z/M variants: 11,13,15,21,23,25)
  @param FileLengthWords: File length in 16-bit words
  @param Box: Bounding box [xmin, ymin, xmax, ymax]
  @param ZBox: Z value range [zmin, zmax] (included if Z coordinates present)
  @param MBox: M value range [mmin, mmax] (included if M values present)
}
procedure TShapefileWriter.WriteHeaderStream(S: TStream; ShapeTypeValue: Integer;
  FileLengthWords: Integer; const {%H-}Box: array of Double;
  const ZBox: array of Double; const MBox: array of Double);
var
  buf: TBytes = Nil;
  i: Integer;
  beFileCode, leVersion, leShapeType: TBytes;
  xminBytes, yminBytes, xmaxBytes, ymaxBytes: TBytes;
  zminBytes, zmaxBytes, mminBytes, mmaxBytes: TBytes;
  zmin, zmax, mmin, mmax: Double;
begin
  // Initialize 100-byte header buffer
  SetLength(buf, 100);
  for i:= 0 to 99 do buf[i]:= 0;

  // File code (big-endian)
  beFileCode:= Int32ToBytesBE(9994);
  Move(beFileCode[0], buf[0], 4);

  // File length (big-endian)
  Move(Int32ToBytesBE(FileLengthWords)[0], buf[24], 4);

  // Version and shape type (little-endian)
  leVersion:= Int32ToBytesLE(1000);
  Move(leVersion[0], buf[28], 4);

  leShapeType:= Int32ToBytesLE(ShapeTypeValue);
  Move(leShapeType[0], buf[32], 4);

  // Bounding box coordinates (little-endian)
  xminBytes:= DoubleToBytesLE(Box[0]);
  yminBytes:= DoubleToBytesLE(Box[1]);
  xmaxBytes:= DoubleToBytesLE(Box[2]);
  ymaxBytes:= DoubleToBytesLE(Box[3]);

  Move(xminBytes[0], buf[36], 8);
  Move(yminBytes[0], buf[44], 8);
  Move(xmaxBytes[0], buf[52], 8);
  Move(ymaxBytes[0], buf[60], 8);

  // Z range (bytes 68-83) - included when Z coordinates present
  if (Length(ZBox) >= 2) then begin
    zmin:= ZBox[0];
    zmax:= ZBox[1];
  end
  else begin
    zmin:= 0.0;
    zmax:= 0.0;
  end;

  // M range (bytes 84-99) - included when M values present
  if (Length(MBox) >= 2) then begin
    mmin:= MBox[0];
    mmax:= MBox[1];
  end
  else begin
    mmin:= 0.0;
    mmax:= 0.0;
  end;

  zminBytes:= DoubleToBytesLE(zmin);
  zmaxBytes:= DoubleToBytesLE(zmax);
  mminBytes:= DoubleToBytesLE(mmin);
  mmaxBytes:= DoubleToBytesLE(mmax);

  Move(zminBytes[0], buf[68], 8);
  Move(zmaxBytes[0], buf[76], 8);
  Move(mminBytes[0], buf[84], 8);
  Move(mmaxBytes[0], buf[92], 8);

  S.WriteBuffer(buf[0], Length(buf));
end;

{
  Writes main geometry (.shp) and index (.shx) files
  Automatically detects and handles Z and M coordinates
  @param Prefix: Base filename without extension
}
procedure TShapefileWriter.WriteMainAndIndex(const Prefix: string); { #todo : This function is way to long. Split into helper functions }
var
  shpStream, shxStream: TFileStream;
  i, recNum: Integer;
  shpRecStartOffsetBytes: Integer;
  offsets: array of Integer = nil;
  lengthsWords: array of Integer = nil;
  headerBox: array[0..3] of Double;
  shapeTypeValue: Integer;
  recContent: TBytes = Nil;
  recContentLenWords: Integer;
  recordNumberBE, contentLengthBE: TBytes;
  partIdx, part, pntIdx: Integer;
  xmin, ymin, xmax, ymax: Double;
  shxFileLengthWords, shpFileLengthWords: Integer;
  offBE, lenBE: TBytes;
  zeroByte: Byte;
  numParts, totalPoints: Integer;
  partStartIndices: array of Integer = Nil;
  currentWritePos: Integer;
  shpName, shxName: string;
  lHasZ, lHasM: Boolean;
  zmin, zmax, mmin, mmax: Double; // File-level Z/M bounds
  fzmin, fzmax, fmmin, fmmax: Double; // Per-feature Z/M bounds
begin
  // STEP 1: Detect if any features have Z (3D) or M (measure) coordinates
  // This determines which shapefile format variant to use
  lHasZ:= HasZ;
  lHasM:= HasM;

  // STEP 2: Determine the appropriate shape type code according to ESRI specification
  // Shapefile types vary based on geometry type and presence of Z/M coordinates:
  // Point: 1=2D, 11=3D, 21=2D+M
  // PolyLine: 3=2D, 13=3D, 23=2D+M
  // Polygon: 5=2D, 15=3D, 25=2D+M
  case fShapeType of
    stPoint:
      if lHasZ then shapeTypeValue:= 11      // PointZ (3D points)
      else if lHasM then shapeTypeValue:= 21 // PointM (2D points with measures)
      else shapeTypeValue:= 1;               // Point (2D points)
    stPolyLine:
      if lHasZ then shapeTypeValue:= 13      // PolyLineZ (3D lines)
      else if lHasM then shapeTypeValue:= 23 // PolyLineM (2D lines with measures)
      else shapeTypeValue:= 3;               // PolyLine (2D lines)
    stPolygon:
      if lHasZ then shapeTypeValue:= 15      // PolygonZ (3D polygons)
      else if lHasM then shapeTypeValue:= 25 // PolygonM (2D polygons with measures)
      else shapeTypeValue:= 5;               // Polygon (2D polygons)
  else
    shapeTypeValue:= 0; // Null shape type for unsupported types
  end;

  // STEP 3: Prepare output filenames
  shpName:= Prefix + '.shp'; // Main geometry file
  shxName:= Prefix + '.shx'; // Index file

  // STEP 4: Create file streams for both .shp and .shx files
  shpStream:= TFileStream.Create(shpName, fmCreate);  // fmCreate overwrites existing files
  shxStream:= TFileStream.Create(shxName, fmCreate);
  try
    // STEP 5: Initialize bounding box variables to extreme values
    // These will be updated as we process each feature to find the actual bounds
    headerBox[0]:= 1e300;   // minX - start with very large positive number
    headerBox[1]:= 1e300;   // minY
    headerBox[2]:= -1e300;  // maxX - start with very large negative number
    headerBox[3]:= -1e300;  // maxY
    zmin:= 1e300;  // Minimum Z coordinate (height/elevation)
    zmax:= -1e300; // Maximum Z coordinate
    mmin:= 1e300;  // Minimum M coordinate (measure value, e.g., route position)
    mmax:= -1e300; // Maximum M coordinate

    // STEP 6: Write placeholder headers (100 bytes each)
    // We can't write the final header yet because we don't know the file bounds or length
    fWriteBuffer.Clear;
    SetLength(recContent, 100);       // Shapefile header is exactly 100 bytes
    FillChar(recContent[0], 100, 0);  // Fill with zeros as placeholder
    fWriteBuffer.WriteBuffer(recContent[0], 100); // Write to .shp
    FlushWriteBuffer(shpStream);

    // Repeat for .shx file
    fWriteBuffer.Clear;
    fWriteBuffer.WriteBuffer(recContent[0], 100);
    FlushWriteBuffer(shxStream);

    // STEP 7: Initialize arrays to track record positions and lengths
    // These arrays will store information needed for the .shx index file
    SetLength(offsets, Length(fFeatures));      // Starting position of each record
    SetLength(lengthsWords, Length(fFeatures)); // Length of each record in 16-bit words

    // STEP 8: Set progress stage for UI feedback
    SetProgressStage(psWritingSHP);

    // STEP 9: Process and write each feature to the .shp file
    recNum:= 1; // Shapefile record numbers start at 1
    for i:= 0 to High(fFeatures) do begin
      // Calculate current position in file (including buffer content)
      shpRecStartOffsetBytes:= shpStream.Position + fWriteBuffer.Size;
      // Convert to 16-bit words (Shapefile requirement for offsets)
      offsets[i]:= shpRecStartOffsetBytes div 2;

      // STEP 9a: Progress reporting for UI
      if (i mod 10 = 0) or (i = High(fFeatures)) then
        DoProgress(i + 1, Length(fFeatures),
          Format('Writing SHP: %d of %d features', [i + 1, Length(fFeatures)]));

      // STEP 9b: Build record content based on geometry type
      if fFeatures[i].ShapeType = stPoint then begin
        // POINT GEOMETRY: Handle point features with Z/M support
        if lHasZ then begin
          // PointZ record: type(4) + X(8) + Y(8) + Z(8) + M(8) = 36 bytes
          SetLength(recContent, 4 + 8*4);
          Move(Int32ToBytesLE(11)[0], recContent[0], 4); // Shape type PointZ
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].X)[0], recContent[4], 8);
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].Y)[0], recContent[12], 8);
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].Z)[0], recContent[20], 8);
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].M)[0], recContent[28], 8);

          // Update file-level Z/M bounds
          if not IsNan(fFeatures[i].Parts[0][0].Z) and not IsInfinite(fFeatures[i].Parts[0][0].Z) then begin
            if fFeatures[i].Parts[0][0].Z < zmin then zmin:= fFeatures[i].Parts[0][0].Z;
            if fFeatures[i].Parts[0][0].Z > zmax then zmax:= fFeatures[i].Parts[0][0].Z;
          end;
          if not IsNan(fFeatures[i].Parts[0][0].M) and not IsInfinite(fFeatures[i].Parts[0][0].M) then begin
            if fFeatures[i].Parts[0][0].M < mmin then mmin:= fFeatures[i].Parts[0][0].M;
            if fFeatures[i].Parts[0][0].M > mmax then mmax:= fFeatures[i].Parts[0][0].M;
          end;
        end
        else if lHasM then begin
          // PointM record: type(4) + X(8) + Y(8) + M(8) = 28 bytes
          SetLength(recContent, 4 + 8*3);
          Move(Int32ToBytesLE(21)[0], recContent[0], 4); // Shape type PointM
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].X)[0], recContent[4], 8);
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].Y)[0], recContent[12], 8);
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].M)[0], recContent[20], 8);

          if not IsNan(fFeatures[i].Parts[0][0].M) and not IsInfinite(fFeatures[i].Parts[0][0].M) then begin
            if fFeatures[i].Parts[0][0].M < mmin then mmin:= fFeatures[i].Parts[0][0].M;
            if fFeatures[i].Parts[0][0].M > mmax then mmax:= fFeatures[i].Parts[0][0].M;
          end;
        end
        else begin
          // Basic Point record: type(4) + X(8) + Y(8) = 20 bytes
          SetLength(recContent, 4 + 8*2);
          Move(Int32ToBytesLE(1)[0], recContent[0], 4); // Shape type Point
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].X)[0], recContent[4], 8);
          Move(DoubleToBytesLE(fFeatures[i].Parts[0][0].Y)[0], recContent[12], 8);
        end;
        // Update the overall spatial bounds for this point
        UpdateBounds(headerBox, fFeatures[i].Parts[0][0], i+2);  // + 2 because header is in CSV but not in loop =+1, and counting is 0-indexed = +1
      end
      else if (fFeatures[i].ShapeType in [stPolyLine, stPolygon]) then begin
        // POLYLINE/POLYGON GEOMETRY: Handle complex geometries with multiple parts
        // Count total parts and points in this feature
        numParts:= Length(fFeatures[i].Parts);
        totalPoints:= 0;
        for part:= 0 to numParts - 1 do
          Inc(totalPoints, Length(fFeatures[i].Parts[part]));

        if totalPoints = 0 then begin
          // Skip empty geometries
          DoLog(Format('Warning: Feature %d has no points. Skipping.', [i+1]));
          SetLength(recContent, 0);
        end
        else begin
          currentWritePos:= 0;
          // Base size without Z/M blocks:
          // type(4) + bbox(32) + numParts(4) + numPoints(4) + partsArray(4*numParts) + points(16*totalPoints)
          SetLength(recContent, 4 + 32 + 4 + 4 + (4 * numParts) + (16 * totalPoints));

          // Write shape type
          Move(Int32ToBytesLE(Integer(fFeatures[i].ShapeType))[0], recContent[currentWritePos], 4);
          Inc(currentWritePos, 4);

          // STEP 9c: Compute feature-specific bounding box and Z/M ranges
          xmin:= 1e300; ymin:= 1e300; xmax:= -1e300; ymax:= -1e300;
          fzmin:= 1e300; fzmax:= -1e300; // Feature-level Z bounds
          fmmin:= 1e300; fmmax:= -1e300; // Feature-level M bounds

          // Iterate through all points in all parts to find bounds WITH NaN CONTROLS
          for part:= 0 to numParts - 1 do
            for pntIdx:= 0 to High(fFeatures[i].Parts[part]) do begin
              // Update overall file bounds
              UpdateBounds(headerBox, fFeatures[i].Parts[part][pntIdx], i+2);  // + 2 because header is in CSV but not in loop =+1, and counting is 0-indexed = +1

              // Update feature-specific XY bounds WITH EXPLICIT NaN AND INFINITE CHECKS
              if not IsNan(fFeatures[i].Parts[part][pntIdx].X) and not IsInfinite(fFeatures[i].Parts[part][pntIdx].X) then begin
                if fFeatures[i].Parts[part][pntIdx].X < xmin then xmin:= fFeatures[i].Parts[part][pntIdx].X;
                if fFeatures[i].Parts[part][pntIdx].X > xmax then xmax:= fFeatures[i].Parts[part][pntIdx].X;
              end;

              if not IsNan(fFeatures[i].Parts[part][pntIdx].Y) and not IsInfinite(fFeatures[i].Parts[part][pntIdx].Y) then begin
                if fFeatures[i].Parts[part][pntIdx].Y < ymin then ymin:= fFeatures[i].Parts[part][pntIdx].Y;
                if fFeatures[i].Parts[part][pntIdx].Y > ymax then ymax:= fFeatures[i].Parts[part][pntIdx].Y;
              end;

              // Update Z/M bounds for 3D and measure values WITH NaN CONTROLS
              if lHasZ and (not IsNan(fFeatures[i].Parts[part][pntIdx].Z)) and not IsInfinite(fFeatures[i].Parts[part][pntIdx].Z) then begin
                if fFeatures[i].Parts[part][pntIdx].Z < fzmin then fzmin:= fFeatures[i].Parts[part][pntIdx].Z;
                if fFeatures[i].Parts[part][pntIdx].Z > fzmax then fzmax:= fFeatures[i].Parts[part][pntIdx].Z;
                // Also update file-level Z bounds
                if fFeatures[i].Parts[part][pntIdx].Z < zmin then zmin:= fFeatures[i].Parts[part][pntIdx].Z;
                if fFeatures[i].Parts[part][pntIdx].Z > zmax then zmax:= fFeatures[i].Parts[part][pntIdx].Z;
              end;

              if lHasM and (not IsNan(fFeatures[i].Parts[part][pntIdx].M)) and not IsInfinite(fFeatures[i].Parts[part][pntIdx].M) then begin
                if fFeatures[i].Parts[part][pntIdx].M < fmmin then fmmin:= fFeatures[i].Parts[part][pntIdx].M;
                if fFeatures[i].Parts[part][pntIdx].M > fmmax then fmmax:= fFeatures[i].Parts[part][pntIdx].M;
                // Also update file-level M bounds
                if fFeatures[i].Parts[part][pntIdx].M < mmin then mmin:= fFeatures[i].Parts[part][pntIdx].M;
                if fFeatures[i].Parts[part][pntIdx].M > mmax then mmax:= fFeatures[i].Parts[part][pntIdx].M;
              end;
            end;

          // VALIDATE BOUNDS: Check if we found any valid coordinates
          if (xmin = 1e300) or (xmax = -1e300) or (ymin = 1e300) or (ymax = -1e300) then begin
            // No valid XY coordinates found in this feature, use fallback values
            DoLog(Format('Warning: Feature %d has no valid XY coordinates. Using fallback bounds.', [i+1]));
            xmin := 0.0; ymin := 0.0; xmax := 0.0; ymax := 0.0;
          end;

          // Validate Z bounds
          if lHasZ and ((fzmin = 1e300) or (fzmax = -1e300)) then begin
            fzmin := 0.0; fzmax := 0.0;
          end;

          // Validate M bounds
          if lHasM and ((fmmin = 1e300) or (fmmax = -1e300)) then begin
            fmmin := 0.0; fmmax := 0.0;
          end;

          // Write feature bounding box (minX, minY, maxX, maxY)
          Move(DoubleToBytesLE(xmin)[0], recContent[currentWritePos], 8); Inc(currentWritePos, 8);
          Move(DoubleToBytesLE(ymin)[0], recContent[currentWritePos], 8); Inc(currentWritePos, 8);
          Move(DoubleToBytesLE(xmax)[0], recContent[currentWritePos], 8); Inc(currentWritePos, 8);
          Move(DoubleToBytesLE(ymax)[0], recContent[currentWritePos], 8); Inc(currentWritePos, 8);

          // Write part count and total point count
          Move(Int32ToBytesLE(numParts)[0], recContent[currentWritePos], 4); Inc(currentWritePos, 4);
          Move(Int32ToBytesLE(totalPoints)[0], recContent[currentWritePos], 4); Inc(currentWritePos, 4);

          // STEP 9d: Compute part start indices (cumulative point counts)
          SetLength(partStartIndices, numParts);
          partIdx:= 0;
          for part:= 0 to numParts - 1 do begin
            partStartIndices[part]:= partIdx;
            Inc(partIdx, Length(fFeatures[i].Parts[part]));
          end;

          // Write parts array (starting indices for each part)
          for part:= 0 to numParts - 1 do begin
            Move(Int32ToBytesLE(partStartIndices[part])[0], recContent[currentWritePos], 4);
            Inc(currentWritePos, 4);
          end;

          // STEP 9e: Write all points (X,Y coordinates)
          for part:= 0 to numParts - 1 do
            for pntIdx:= 0 to High(fFeatures[i].Parts[part]) do begin
              // Write coordinates even if they are NaN (maintain data structure)
              // The bounds calculation above already handled invalid coordinates
              Move(DoubleToBytesLE(fFeatures[i].Parts[part][pntIdx].X)[0], recContent[currentWritePos], 8);
              Inc(currentWritePos, 8);
              Move(DoubleToBytesLE(fFeatures[i].Parts[part][pntIdx].Y)[0], recContent[currentWritePos], 8);
              Inc(currentWritePos, 8);
            end;

          // STEP 9f: Append Z coordinates if present (for 3D shapes)
          if lHasZ then begin
            // Extend array for Z block: Zmin(8) + Zmax(8) + Zarray(8*totalPoints)
            SetLength(recContent, Length(recContent) + 16 + (8 * totalPoints));
            Move(DoubleToBytesLE(fzmin)[0], recContent[currentWritePos], 8); Inc(currentWritePos, 8);
            Move(DoubleToBytesLE(fzmax)[0], recContent[currentWritePos], 8); Inc(currentWritePos, 8);

            // Write Z coordinate for each point
            for part:= 0 to numParts - 1 do
              for pntIdx:= 0 to High(fFeatures[i].Parts[part]) do begin
                // Write 0.0 for NaN Z values (maintains data structure consistency)
                if IsNan(fFeatures[i].Parts[part][pntIdx].Z) or IsInfinite(fFeatures[i].Parts[part][pntIdx].Z) then
                  Move(DoubleToBytesLE(0.0)[0], recContent[currentWritePos], 8)
                else
                  Move(DoubleToBytesLE(fFeatures[i].Parts[part][pntIdx].Z)[0], recContent[currentWritePos], 8);

                Inc(currentWritePos, 8);
              end;
          end;

          // STEP 9g: Append M values if present (for measure values)
          if lHasM then begin
            // Extend array for M block: Mmin(8) + Mmax(8) + Marray(8*totalPoints)
            SetLength(recContent, Length(recContent) + 16 + (8 * totalPoints));
            Move(DoubleToBytesLE(fmmin)[0], recContent[currentWritePos], 8); Inc(currentWritePos, 8);
            Move(DoubleToBytesLE(fmmax)[0], recContent[currentWritePos], 8); Inc(currentWritePos, 8);

            // Write M value for each point
            for part:= 0 to numParts - 1 do
              for pntIdx:= 0 to High(fFeatures[i].Parts[part]) do begin
                if IsNan(fFeatures[i].Parts[part][pntIdx].M) or IsInfinite(fFeatures[i].Parts[part][pntIdx].M) then
                  Move(DoubleToBytesLE(0.0)[0], recContent[currentWritePos], 8)
                else
                  Move(DoubleToBytesLE(fFeatures[i].Parts[part][pntIdx].M)[0], recContent[currentWritePos], 8);

                Inc(currentWritePos, 8);
              end;
          end;
        end;
      end
      else begin
        // Unsupported shape type - write empty record
        DoLog(Format('Warning: Unsupported shape type for feature %d. Writing empty record.', [i+1]));
        SetLength(recContent, 0);
      end;

      // STEP 10: Compute content length in 16-bit words (Shapefile requirement)
      recContentLenWords:= Length(recContent) div 2;
      if (Length(recContent) mod 2) <> 0 then Inc(recContentLenWords);

      // STEP 11: Write record header (big-endian format required by Shapefile spec)
      recordNumberBE:= Int32ToBytesBE(recNum);      // Record number (1-based)
      contentLengthBE:= Int32ToBytesBE(recContentLenWords); // Content length in words

      fWriteBuffer.WriteBuffer(recordNumberBE[0], 4);
      fWriteBuffer.WriteBuffer(contentLengthBE[0], 4);

      // Write actual record content
      if Length(recContent) > 0 then
        fWriteBuffer.WriteBuffer(recContent[0], Length(recContent));

      // STEP 12: Pad to even byte boundary if needed (Shapefile requirement)
      if (Length(recContent) mod 2) <> 0 then begin
        zeroByte:= 0;
        fWriteBuffer.WriteBuffer(zeroByte, 1);
      end;

      // STEP 13: Flush buffer to disk if it reaches the buffer size limit
      if fWriteBuffer.Size >= fBufferSize then
        FlushWriteBuffer(shpStream);

      // Store record information for index file
      lengthsWords[i]:= recContentLenWords;
      Inc(recNum); // Move to next record number
    end;

    // STEP 14: Final flush of .shp buffer to ensure all data is written
    FlushWriteBuffer(shpStream);

    // STEP 15: Compute final file length and write correct header with actual bounds
    shpFileLengthWords:= (shpStream.Size) div 2;
    if (shpStream.Size mod 2) <> 0 then Inc(shpFileLengthWords);

    // Rewind to beginning and write the real header with calculated bounds
    shpStream.Position:= 0;
    if lHasZ then
      WriteHeaderStream(shpStream, shapeTypeValue, shpFileLengthWords, headerBox, [zmin, zmax], [mmin, mmax])
    else if lHasM then
      WriteHeaderStream(shpStream, shapeTypeValue, shpFileLengthWords, headerBox, [], [mmin, mmax])
    else
      WriteHeaderStream(shpStream, shapeTypeValue, shpFileLengthWords, headerBox, [], []);

    // STEP 16: Write .shx index file
    SetProgressStage(psWritingSHX);
    fWriteBuffer.Clear;
    for i:= 0 to High(offsets) do begin
      // Progress reporting for index writing
      if (i mod 100 = 0) or (i = High(offsets)) then
        DoProgress(i + 1, Length(offsets),
          Format('Writing SHX: %d of %d records', [i + 1, Length(offsets)]));

      // Write offset and length (big-endian) for each record
      offBE:= Int32ToBytesBE(offsets[i]);     // Offset in 16-bit words
      lenBE:= Int32ToBytesBE(lengthsWords[i]); // Length in 16-bit words
      fWriteBuffer.WriteBuffer(offBE[0], 4);
      fWriteBuffer.WriteBuffer(lenBE[0], 4);

      // Flush buffer if needed
      if fWriteBuffer.Size >= fBufferSize then
        FlushWriteBuffer(shxStream);
    end;

    FlushWriteBuffer(shxStream);

    // STEP 17: Compute .shx file length and write its header
    shxFileLengthWords:= (shxStream.Size) div 2;
    if (shxStream.Size mod 2) <> 0 then Inc(shxFileLengthWords);
    shxStream.Position:= 0;
    if lHasZ then
      WriteHeaderStream(shxStream, shapeTypeValue, shxFileLengthWords, headerBox, [zmin, zmax], [mmin, mmax])
    else if lHasM then
      WriteHeaderStream(shxStream, shapeTypeValue, shxFileLengthWords, headerBox, [], [mmin, mmax])
    else
      WriteHeaderStream(shxStream, shapeTypeValue, shxFileLengthWords, headerBox, [], []);

  finally
    // STEP 18: Clean up - always free the streams, even if an exception occurs
    shpStream.Free;
    shxStream.Free;
  end;
end;

{
  Writes dBase III format attribute file (.dbf)
  @param Prefix: Base filename without extension
}
procedure TShapefileWriter.WriteDBF(const Prefix: string);
const
  BufferSize = 65536; // 64KB buffer for efficient file writing
var
  dbf: TFileStream;
  i, numRecords, headerLength, recordLength, pos, fIdx: Integer;
  header: array[0..31] of Byte = (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
  recBuf: TBytes = Nil;
  b: Byte;
  y, m, d: Word;
  fieldCount: Integer;
  eofMarker: Byte;
  fieldOffsets: array of Integer = Nil;
  fld: TDBFField;
  val: string;
  valBytes: TBytes;
  numStr: string;
  writePos: Integer;
  nameBuf: array[0..10] of Byte = (0,0,0,0,0,0,0,0,0,0,0);  // Initialize with zeros
  offset0: Integer;
  copyCnt: Integer;
begin
  // STEP 1: Create the .dbf file stream
  dbf:= TFileStream.Create(Prefix + '.dbf', fmCreate);
  try
    // STEP 2: Initialize or clear the write buffer for efficient I/O
    if fWriteBuffer = nil then
      InitWriteBuffer(BufferSize)
    else
      fWriteBuffer.Clear;

    // STEP 3: Calculate basic file structure information
    numRecords:= Length(fFeatures);  // Number of records (features)
    fieldCount:= Length(fDBFFields); // Number of fields (columns)

    // STEP 4: Calculate header and record lengths
    // Header structure: 32 bytes main header + 32 bytes per field + 1 byte terminator
    headerLength:= 32 + (32 * fieldCount) + 1;

    // Record length: 1 byte deletion flag + sum of all field sizes
    recordLength:= 1; // Start with deletion flag byte
    SetLength(fieldOffsets, fieldCount); // Track where each field starts in the record

    // Calculate total record length and field offsets
    for fIdx:= 0 to fieldCount - 1 do
      recordLength:= recordLength + fDBFFields[fIdx].Size;

    // Safety check
    if recordLength < 1 then begin
      DoLog(Format('Invalid DBF recordLength Length. Length found: %d. Minimal Length is 1.', [recordLength]));
      raise Exception.Create('Invalid DBF record length.');
    end;

    // STEP 5: Allocate buffer for one record
    SetLength(recBuf, recordLength);

    // =========================================================================
    // SECTION A: WRITE DBF HEADER
    // =========================================================================

    // STEP A1: Prepare main header (32 bytes)
    FillChar(header[0], SizeOf(header), 0); // Clear header buffer
    header[0]:= $03; // dBASE III format identifier
    // Write current date (year, month, day)
    DecodeDate(Date, y, m, d);
    header[1]:= Byte(y mod 100); // Last 2 digits of year
    header[2]:= Byte(m);         // Month
    header[3]:= Byte(d);         // Day
    // Write record count (4 bytes, little-endian)
    Move(Int32ToBytesLE(numRecords)[0], header[4], 4);
    // Write header length (2 bytes, little-endian)
    header[8]:= Byte(headerLength and $FF);        // Low byte
    header[9]:= Byte((headerLength shr 8) and $FF); // High byte
    // Write record length (2 bytes, little-endian)
    header[10]:= Byte(recordLength and $FF);        // Low byte
    header[11]:= Byte((recordLength shr 8) and $FF); // High byte
    // Set encoding/language driver
    case UpperCase(fDBFEncoding) of
      'CP1252': header[29]:= $03;     // Windows ANSI
      'ISO-8859-1': header[29]:= $01; // ISO Latin-1
    else
      header[29]:= $03; // Default to Windows ANSI
    end;

    // Write main header to buffer
    fWriteBuffer.WriteBuffer(header[0], 32);

    // STEP A2: Write field descriptors (32 bytes per field)
    writePos:= 1; // First byte position after deletion flag in records
    for fIdx:= 0 to fieldCount - 1 do begin
      fld:= fDBFFields[fIdx];

      // Field name (11 bytes, null-padded)
      FillChar(nameBuf[0], SizeOf(nameBuf), 0); // Clear name buffer
      if fld.Name <> '' then
        // Copy field name (max 11 characters)
        Move(fld.Name[1], nameBuf[0], Min(Length(fld.Name), 11));
      fWriteBuffer.WriteBuffer(nameBuf[0], 11);

      // Field type (1 byte: C=Character, N=Numeric, D=Date, L=Logical)
      b:= Byte(Ord(fld.FieldType));
      fWriteBuffer.WriteBuffer(b, 1);

      // 4 reserved bytes (set to zero)
      b:= 0;
      for pos:= 1 to 4 do fWriteBuffer.WriteBuffer(b, 1);

      // Field size and decimal places (1 byte each)
      b:= Byte(fld.Size);
      fWriteBuffer.WriteBuffer(b, 1);
      b:= Byte(fld.Decimals);
      fWriteBuffer.WriteBuffer(b, 1);

      // 14 reserved bytes (set to zero)
      b:= 0;
      for pos:= 1 to 14 do fWriteBuffer.WriteBuffer(b, 1);

      // Remember this field's position within each record
      fieldOffsets[fIdx]:= writePos;
      Inc(writePos, fld.Size); // Move to next field position
    end;

    // STEP A3: Write header terminator and flush to file
    b:= $0D; // Header terminator byte
    fWriteBuffer.WriteBuffer(b, 1);
    FlushWriteBuffer(dbf);

    // =========================================================================
    // SECTION B: WRITE RECORD DATA
    // =========================================================================

    // STEP B1: Process each record (feature)
    for i:= 0 to High(fFeatures) do begin
      // Initialize record buffer with spaces and deletion flag
      recBuf[0]:= Byte(' '); // Space = active record (not deleted)
      if recordLength > 1 then
        FillChar(recBuf[1], recordLength - 1, Byte(' ')); // Fill rest with spaces

      // STEP B2: Process each field in this record
      for fIdx:= 0 to fieldCount - 1 do begin
        fld:= fDBFFields[fIdx];

        // Validate field position and size
        offset0:= fieldOffsets[fIdx]; // Starting position of this field in record
        if (offset0 < 1) or (offset0 + fld.Size - 1 > recordLength - 1) then begin
          DoLog(Format('DBF field "%s" has invalid offset/size (record length mismatch).', [fld.Name]));
          raise Exception.CreateFmt('DBF field "%s" has invalid offset/size (record length mismatch).', [fld.Name]);
        end;

        // Get field value (use empty string if no value exists)
        if fIdx <= High(fFeatures[i].Values) then
          val:= fFeatures[i].Values[fIdx]
        else
          val:= '';

        // STEP B3: Handle different field types according to dBase III spec
        case fld.FieldType of
          'C': // CHARACTER FIELD - Left-justified, space-padded
            begin
              // Convert string to appropriate encoding
              valBytes:= UTF8ToDBFBytes(val, fDBFEncoding);
              copyCnt:= Min(fld.Size, Length(valBytes));
              if copyCnt > 0 then
                Move(valBytes[0], recBuf[offset0], copyCnt); // Copy data
              // Pad remaining space with spaces
              if fld.Size - copyCnt > 0 then
                FillChar(recBuf[offset0 + copyCnt], fld.Size - copyCnt, Byte(' '));
            end;

          'N': // NUMERIC FIELD - Right-justified, space-padded
            begin
              if val = '' then
                numStr:= ''
              else
                numStr:= val;

              // Convert numeric string to bytes
              valBytes:= UTF8ToDBFBytes(numStr, fDBFEncoding);
              copyCnt:= Min(fld.Size, Length(valBytes));

              // Fill entire field with spaces first
              if fld.Size > 0 then
                FillChar(recBuf[offset0], fld.Size, Byte(' '));

              // Copy number to right-aligned position
              if copyCnt > 0 then begin
                Move(valBytes[0], recBuf[offset0 + (fld.Size - copyCnt)], copyCnt);
              end;
            end;

          'D': // DATE FIELD - Format: YYYYMMDD, Left-justified
            begin
              valBytes:= UTF8ToDBFBytes(val, fDBFEncoding);
              copyCnt:= Min(fld.Size, Length(valBytes));
              if copyCnt > 0 then
                Move(valBytes[0], recBuf[offset0], copyCnt);
              // Pad with spaces
              if fld.Size - copyCnt > 0 then
                FillChar(recBuf[offset0 + copyCnt], fld.Size - copyCnt, Byte(' '));
            end;

          'L': // LOGICAL FIELD - Single character: T/F/Y/N/?
            begin
              if fld.Size < 1 then begin
                DoLog(Format('Logical field "%s" must have size >= 1.', [fld.Name]));
                raise Exception.CreateFmt('Logical field "%s" must have size >= 1.', [fld.Name]);
              end;
              // Store first character of value (uppercase) or '?' for unknown
              if (val <> '') then
                recBuf[offset0]:= Byte(UpCase(val[1]))
              else
                recBuf[offset0]:= Byte('?');
              // Pad remaining space with spaces
              if fld.Size > 1 then
                FillChar(recBuf[offset0 + 1], fld.Size - 1, Byte(' '));
            end
        else
          // DEFAULT: Treat unknown field types as character
          begin
            valBytes:= UTF8ToDBFBytes(val, fDBFEncoding);
            copyCnt:= Min(fld.Size, Length(valBytes));
            if copyCnt > 0 then
              Move(valBytes[0], recBuf[offset0], copyCnt);
            if fld.Size - copyCnt > 0 then
              FillChar(recBuf[offset0 + copyCnt], fld.Size - copyCnt, Byte(' '));
          end;
        end; // case field type
      end; // for each field

      // STEP B4: Write completed record to buffer
      fWriteBuffer.WriteBuffer(recBuf[0], recordLength);

      // Progress reporting
      if (i mod 50 = 0) or (i = High(fFeatures)) then
        DoProgress(i + 1, Length(fFeatures), Format('Writing DBF: %d of %d records', [i + 1, Length(fFeatures)]));

      // Flush buffer to disk if approaching capacity
      if fWriteBuffer.Size >= BufferSize - recordLength then
        FlushWriteBuffer(dbf);
    end; // for each record

    // STEP B5: Final flush and write EOF marker
    FlushWriteBuffer(dbf);
    eofMarker:= $1A; // dBase III end-of-file marker
    dbf.WriteBuffer(eofMarker, 1);

  finally
    // STEP 6: Cleanup - always free resources, even if errors occur
    fWriteBuffer.Free;
    fWriteBuffer:= nil;
    dbf.Free;
  end;
end;

{
  Writes projection file (.prj) with Well-Known Text string
  @param Prefix: Base filename without extension
  @param WKT: Well-Known Text projection string
}
procedure TShapefileWriter.WritePRJ(const Prefix: string; const WKT: string);
var
  prj: TFileStream;
  fullName: string;
begin
  fullName:= Prefix + '.prj';
  prj:= TFileStream.Create(fullName, fmCreate);
  try
    prj.WriteBuffer(PAnsiChar(AnsiString(WKT))^, Length(WKT));
  finally
    prj.Free;
  end;
end;

end.
