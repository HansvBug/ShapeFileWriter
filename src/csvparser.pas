{ Copyright ©2025 Hans van Buggenum }
unit CSVParser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  ShapeWriter; // Assumes TShapefileWriter, TPointD, TPointDArray etc. are defined here

type
  { alias for array of parts (each part is a TPointDArray) }
  TPointDArrayArray = array of TPointDArray;
  { Progress event; Transmitting progress to the calling form }
  TCSVProgressEvent = procedure(Sender: TObject; Current, Total: Integer; const Message: string) of object;
  { Log event for CSV parsing notifications }
  TCSVLogEvent = procedure(Sender: TObject; const Message: string) of object;

  { TCSVParser }
  TCSVParser = class
  private
    fOnProgress: TCSVProgressEvent;
    fOnLog: TCSVLogEvent;

    procedure DoProgress(Current, Total: Integer; const Msg: string);
    procedure DoLog(const Msg: string);
    procedure ParseCSVLineToTStringList(const Line: string; StringList: TStringList; Delim: Char);
    function DetectGeometryTypeFromData(CSVFile: TStringList; const Delim: Char; xIdx, yIdx, wktIdx: Integer): TEShapeType;
    function ShapeTypeToString(ShapeType: TEShapeType): string; // Also exists in shapewriter.pas. Duplicate.
  public
    function LoadCSVToLists(const AFileName: string; const Delim: Char;
      out CSVFile, HeaderList, DataList: TStringList): Boolean;

    function AddDBFFieldsFromHeader(ShapeWriter : TObject; const CSVFile,
      HeaderList : TStringList; const Delim : Char; const fs : TFormatSettings;
  xIdx, yIdx, zIdx, mIdx, wktIdx : Integer) : Boolean;

    procedure ProcessCSVRows(ShapeWriter: TObject;
      CSVFile, HeaderList: TStringList; const Delim: Char;
      const fs: TFormatSettings; shapeType: TEShapeType;
      xIdx, yIdx, zIdx, mIdx, wktIdx: Integer);

    procedure ProcessPointRow(ShapeWriter : TObject; const FieldList,
      HeaderList : TStringList; const fs : TFormatSettings;
      lineIndex : Integer; xIdx, yIdx, zIdx, mIdx, wktIdx : Integer;
      var statusText : string);

    procedure ProcessPolyRow(ShapeWriter: TObject;
      const FieldList, HeaderList: TStringList; const fs: TFormatSettings;
      lineIndex: Integer; xIdx, yIdx, zIdx, mIdx, wktIdx: Integer);

    function NormalizeNumberString(const S: string; const fs: TFormatSettings): string;

    { Determine the column index in which the coordinates or the wkt geometry are located. }
    procedure IdentifyCoordinateIndices(const HeaderList: TStringList;
      out xIdx, yIdx, zIdx, mIdx, wktIdx: Integer);

    function GetDelimiter(const AText: string): Char;

    property OnProgress: TCSVProgressEvent read fOnProgress write fOnProgress;
    property OnLog: TCSVLogEvent read fOnLog write fOnLog;
  end;

implementation

{ -------------------- Helpers (implementation only) -------------------- }

// Removes outer parentheses from a string if present
function TrimParens(const S: string): string;
var
  t: string;
begin
  t:= Trim(S);
  if (Length(t) >= 2) and (t[1] = '(') and (t[Length(t)] = ')') then
    Result:= Copy(t, 2, Length(t) - 2)
  else
    Result:= t;
end;

// Splits a string on commas but only at top-level (parentheses depth = 0)
function SplitTopLevelItems(const S: string): TStringList;
var
  i, depth: Integer;
  cur: string;
  ch: Char;
begin
  Result:= TStringList.Create;
  cur:= '';
  depth:= 0;
  for i:= 1 to Length(S) do begin
    ch:= S[i];
    if ch = '(' then begin
      Inc(depth);
      cur:= cur + ch;
    end
    else if ch = ')' then begin
      Dec(depth);
      cur:= cur + ch;
    end
    else if (ch = ',') and (depth = 0) then begin
      Result.Add(Trim(cur));
      cur:= '';
    end
    else
      cur:= cur + ch;
  end;
  if Trim(cur) <> '' then
    Result.Add(Trim(cur));
end;

// Parses a coordinate pair/triplet string into X, Y, Z values
// Expected formats: 'x y' or 'x y z' or 'x,y,z' (without parentheses)
// Uses invariant format settings ('.' as decimal separator) because WKT always uses '.'
procedure ParseCoordinatePair(const Token: string; out X, Y, Z: Double);
var
  parts: TStringList;
  s: string;
  invFS: TFormatSettings;
begin
  X:= NaN;
  Y:= NaN;
  Z:= NaN;
  parts:= TStringList.Create;
  try
    s:= Trim(Token);
    // Replace commas with spaces to use a single delimiter
    s:= StringReplace(s, ',', ' ', [rfReplaceAll]);
    parts.Delimiter:= ' ';
    parts.StrictDelimiter:= True;
    parts.DelimitedText:= s;

    // Invariant format settings: '.' as decimal separator
    invFS:= DefaultFormatSettings;
    invFS.DecimalSeparator:= '.';

    if parts.Count >= 1 then
      TryStrToFloat(parts[0], X, invFS);
    if parts.Count >= 2 then
      TryStrToFloat(parts[1], Y, invFS);
    if parts.Count >= 3 then
      TryStrToFloat(parts[2], Z, invFS);
  finally
    parts.Free;
  end;
end;

// Extracts base geometry type from raw type string
// Extracts 'POINT' from 'POINT', 'POINT Z', 'POINTZ', 'POINT ZM' etc.
function ExtractBaseGeomType(const RawType: string): string;
var
  s: string;
  p: Integer;
begin
  s:= Trim(RawType);
  // Remove possible extra words after space
  p:= Pos(' ', s);
  if p > 0 then
    Result:= Copy(s, 1, p - 1)
  else
    Result:= s;

  // Also remove any trailing Z/M letters (e.g., 'POINTZ' or 'POINTM')
  // However, often 'POINTZ' would be 'POINTZ'; we want 'POINT'
  // Simpler approach: only keep leading alphabetic characters
  result:= UpperCase(Result);
  // Trim non-alpha at end
  while (Length(Result) > 0) and not (Result[Length(Result)] in ['A'..'Z']) do
    Delete(Result, Length(Result), 1);
end;

// Parses WKT string into geometry parts
// Supports: POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON
function ParseWKTToParts(const AWKT: string; out geomType: string;
  out Parts: TPointDArrayArray): Boolean;
var
  s, up, inner, t, rawType, baseType: string;
  i, j, k: Integer;
  tokens, ringTokens, coordTokens: TStringList;
  coordPair: string;
  partList: array of TPointDArray = nil;  // Initialize to nil
  partCount: Integer;
  x, y, z: Double;
begin
  Result:= False;
  geomType:= '';
  Parts:= nil;
  if Trim(AWKT) = '' then Exit(False);

  s:= Trim(AWKT);
  up:= UpperCase(s);

  // Ignore SRID=xxxx; prefix
  if Pos('SRID=', up) = 1 then begin
    i:= Pos(';', up);
    if i > 0 then
      s:= Trim(Copy(s, i + 1, MaxInt));
  end;

  // Find type and inner text
  i:= Pos('(', s);
  if i = 0 then Exit(False);
  rawType:= Trim(Copy(s, 1, i - 1));
  baseType:= ExtractBaseGeomType(rawType);
  geomType:= baseType;
  inner:= Trim(Copy(s, i, MaxInt)); // Contains leading '('
  inner:= TrimParens(inner);

  tokens:= nil;
  ringTokens:= nil;
  coordTokens:= nil;

  try
    tokens:= TStringList.Create;

    if baseType = 'POINT' then begin
      SetLength(partList, 1);
      SetLength(partList[0], 1);
      ParseCoordinatePair(inner, x, y, z);
      partList[0][0].X:= x;
      partList[0][0].Y:= y;
      partList[0][0].Z:= z;
      partList[0][0].M:= NaN;
      SetLength(Parts, 1);
      Parts[0]:= partList[0];
      Result:= True;
      // NO Exit here - let finally block execute
    end
    else if (baseType = 'MULTIPOINT') or (baseType = 'LINESTRING') then begin
      // Could be: MULTIPOINT((x y),(x y)) or MULTIPOINT(x y, x y)
      if (Length(inner) >= 1) and (inner[1] = '(') then
        inner:= TrimParens(inner);

      // Replace tokens with new SplitTopLevelItems result
      FreeAndNil(tokens); // First free old tokens
      tokens:= SplitTopLevelItems(inner);

      SetLength(partList, 1);
      SetLength(partList[0], tokens.Count);
      for i:= 0 to tokens.Count - 1 do begin
        coordPair:= tokens[i];
        ParseCoordinatePair(coordPair, x, y, z);
        partList[0][i].X:= x;
        partList[0][i].Y:= y;
        partList[0][i].Z:= z;
        partList[0][i].M:= NaN;
      end;
      SetLength(Parts, 1);
      Parts[0]:= partList[0];
      Result:= True;
      // NO Exit here - let finally block execute
    end
    else if (baseType = 'MULTILINESTRING') or (baseType = 'POLYGON') or (baseType = 'MULTIPOLYGON') then begin
      // Replace tokens with new SplitTopLevelItems result
      FreeAndNil(tokens); // First free old tokens
      tokens:= SplitTopLevelItems(inner);

      partCount:= 0;
      for i:= 0 to tokens.Count - 1 do begin
        t:= tokens[i];
        if baseType = 'MULTIPOLYGON' then begin
          // Token contains polygon (possibly ((ring),(ring)))
          t:= TrimParens(t);
          ringTokens:= SplitTopLevelItems(t);
          try
            for j:= 0 to ringTokens.Count - 1 do begin
              coordTokens:= SplitTopLevelItems(TrimParens(ringTokens[j]));
              try
                SetLength(partList, partCount + 1);
                SetLength(partList[partCount], coordTokens.Count);
                for k:= 0 to coordTokens.Count - 1 do begin
                  ParseCoordinatePair(coordTokens[k], x, y, z);
                  partList[partCount][k].X:= x;
                  partList[partCount][k].Y:= y;
                  partList[partCount][k].Z:= z;
                  partList[partCount][k].M:= NaN;
                end;
                Inc(partCount);
              finally
                FreeAndNil(coordTokens); // Always free
              end;
            end;
          finally
            FreeAndNil(ringTokens); // Always free
          end;
        end
        else begin
          // Token is a ring or linestring
          coordTokens:= SplitTopLevelItems(TrimParens(t));
          try
            SetLength(partList, partCount + 1);
            SetLength(partList[partCount], coordTokens.Count);
            for j:= 0 to coordTokens.Count - 1 do begin
              ParseCoordinatePair(coordTokens[j], x, y, z);
              partList[partCount][j].X:= x;
              partList[partCount][j].Y:= y;
              partList[partCount][j].Z:= z;
              partList[partCount][j].M:= NaN;
            end;
            Inc(partCount);
          finally
            FreeAndNil(coordTokens); // Always free
          end;
        end;
      end;

      SetLength(Parts, partCount);
      for i:= 0 to partCount - 1 do
        Parts[i]:= partList[i];
      Result:= True;
      // NO Exit here - let finally block execute
    end;

  finally
    // Always free all TStringList objects
    FreeAndNil(tokens);
    FreeAndNil(ringTokens);
    FreeAndNil(coordTokens);
  end;
end;

{ -------------------- TCSVParser implementations -------------------- }

// Fires the OnProgress event if assigned
procedure TCSVParser.DoProgress(Current, Total : Integer; const Msg : string);
begin
  if Assigned(fOnProgress) then
    fOnProgress(Self, Current, Total, Msg);
end;

// Fires the OnLog event if assigned
procedure TCSVParser.DoLog(const Msg : string);
begin
  if Assigned(fOnLog) then
    fOnLog(Self, Msg);
end;

// Parses a CSV line into a TStringList using the specified delimiter
procedure TCSVParser.ParseCSVLineToTStringList(const Line: string; StringList: TStringList; Delim: Char);
var
  i: Integer;
begin
  if Trim(Line) = '' then
    Exit;

  StringList.Clear;
  StringList.StrictDelimiter:= True;
  StringList.Delimiter:= Delim;
  StringList.QuoteChar:= '"';  { #todo : Needs testing }
  // Note: DelimitedText uses QuoteChar and Delimiter
  StringList.DelimitedText:= Line;
  for i:= 0 to StringList.Count - 1 do
    StringList[i]:= Trim(StringList[i]);
end;

// Detects the geometry type from the CSV data by analyzing first 100 rows
function TCSVParser.DetectGeometryTypeFromData(CSVFile : TStringList;
  const Delim : Char; xIdx, yIdx, wktIdx : Integer) : TEShapeType;
var
  lineCount: Integer;
  fieldList: TStringList;
  wktText, geomType: string;
  partsArr: TPointDArrayArray;
  pointCount, lineCountData, polygonCount: Integer;
begin
  Result:= stNull;
  pointCount:= 0;
  lineCountData:= 0;
  polygonCount:= 0;

  fieldList:= TStringList.Create;
  try
    // Analyze only first 100 rows for efficiency (or all rows if less than 100)
    for lineCount:= 1 to Min(100, CSVFile.Count - 1) do begin
      ParseCSVLineToTStringList(CSVFile[lineCount], fieldList, Delim);
      if fieldList.Count < 1 then
        Continue;

      // Check WKT column first (most reliable)
      if (wktIdx <> -1) and (wktIdx < fieldList.Count) then begin
        wktText:= Trim(fieldList[wktIdx]);
        if wktText <> '' then begin
          partsArr:= nil;
          if ParseWKTToParts(wktText, geomType, partsArr) then begin
            geomType:= UpperCase(Trim(geomType));
            if (geomType = 'POINT') or (geomType = 'MULTIPOINT') then
              Inc(pointCount)
            else if (geomType = 'LINESTRING') or (geomType = 'MULTILINESTRING') then
              Inc(lineCountData)
            else if (geomType = 'POLYGON') or (geomType = 'MULTIPOLYGON') then
              Inc(polygonCount);
          end;
          Continue;
        end;
      end;

      // Fallback: based on X/Y columns and shape type logic
      // If WKT is present, ignore this fallback
      if (wktIdx = -1) and (xIdx <> -1) and (yIdx <> -1) then begin
        // Simple assumption: if there's only one point per row, then points
        // In practice, more complex logic could be added here
        Inc(pointCount);
      end;
    end;

    // Determine dominant geometry type
    if (polygonCount > lineCountData) and (polygonCount > pointCount) then
      Result:= stPolygon
    else if (lineCountData > pointCount) and (lineCountData > polygonCount) then
      Result:= stPolyLine
    else if pointCount > 0 then
      Result:= stPoint
    else
      Result:= stNull;

  finally
    fieldList.Free;
  end;
end;

// Converts shape type enum to string representation
function TCSVParser.ShapeTypeToString(ShapeType : TEShapeType) : string;
begin
  case ShapeType of
    stNull:      Result:= 'Null';
    stPoint:     Result:= 'Point';
    stPolyLine:  Result:= 'Line';
    stPolygon:   Result:= 'Polygon';
    else begin
        DoLog('Shape is Unknown. (CSV parser).');
        Result:= 'Unknown';
      end;
    end;
end;

// Loads CSV file into string lists and extracts headers
function TCSVParser.LoadCSVToLists(const AFileName: string; const Delim: Char;
  out CSVFile, HeaderList, DataList: TStringList): Boolean;
begin
  Result:= False;
  CSVFile:= TStringList.Create;
  HeaderList:= TStringList.Create;
  DataList:= TStringList.Create;
  try
    CSVFile.LoadFromFile(AFileName); // Load the CSV file into the TStringList
    if CSVFile.Count = 0 then begin
      FreeAndNil(CSVFile);
      FreeAndNil(HeaderList);
      FreeAndNil(DataList);
      DoLog('CSV file is empty. No data to process.');
      Exit(False);
    end;

    ParseCSVLineToTStringList(CSVFile[0], HeaderList, Delim); // Read the first line. It must contain headers

    Result:= True;
  except

    //CSVFile.Free;
    //HeaderList.Free;
    //DataList.Free;
    raise;
  end;
end;

// Adds DBF fields based on CSV header information
function TCSVParser.AddDBFFieldsFromHeader(ShapeWriter: TObject;
  const CSVFile, HeaderList: TStringList; const Delim: Char;
  const fs: TFormatSettings; xIdx, yIdx, zIdx, mIdx, wktIdx: Integer): Boolean;
var
  i: Integer;
  fieldName: string;
  fieldType: Char;
  fieldSize: Integer;
  fieldDecimals: Integer;
  firstDataLineExists: Boolean;
  dataList: TStringList;
  dataValue, normValue: string;
  tempDouble: Double;
begin
  Result:= True;
  firstDataLineExists:= False;
  dataList:= TStringList.Create;
  try
    if CSVFile.Count > 1 then
    begin  // Is there a row with data available?
      ParseCSVLineToTStringList(CSVFile[1], dataList, Delim);
      firstDataLineExists:= True;
    end
    else begin
      DoLog('The CSV file does not contain any data. (CSV parser).');
      Result:= False;
      Exit;
    end;

    // Preparing an array with field names and data and the correct data type.
    for i:= 0 to HeaderList.Count - 1 do
    begin
      fieldName:= Trim(HeaderList[i]); // Determine the column in which the data will be dropped/stored.

      // Skip the geometry column(s). It will not be in the dbf file. (x/y/z/m/wkt)
      if (i = xIdx) or (i = yIdx) or (i = zIdx) or (i = mIdx) or (i = wktIdx) then
        Continue;

      if fieldName = '' then  // No field name, create a fictitious field name.
        fieldName:= 'FIELD_' + IntToStr(i + 1);

      // Initialize default field type and size.
      // Most of the fields will be a character field.
      fieldType:= 'C';
      fieldSize:= 50;
      fieldDecimals:= 0;

      if firstDataLineExists and (i < dataList.Count) then
      begin
        dataValue:= Trim(dataList[i]);  // Read the content of the field.
        normValue:= dataValue;
        // normalize decimal separators to FS decimal sep
        normValue:= StringReplace(normValue, '.', fs.DecimalSeparator, [rfReplaceAll]);
        normValue:= StringReplace(normValue, ',', fs.DecimalSeparator, [rfReplaceAll]);

        // Based on the content, determine the field type and adjust the character field to the correct field type..
        if TryStrToFloat(normValue, tempDouble, fs) then
        begin
          fieldType:= 'N';
          fieldSize:= 15;
          fieldDecimals:= 6;
        end
        else if (Length(dataValue) <= 1) then
        begin
          fieldType:= 'L';
          fieldSize:= 1;
        end
        else if (Length(dataValue) = 10) and (Pos('/', dataValue) > 0) then
        begin
          fieldType:= 'D';
          fieldSize:= 8;
        end;
      end;

      Result:= TShapefileWriter(ShapeWriter).AddDBFField(fieldName, fieldType, fieldSize, fieldDecimals);
    end;
  finally
    dataList.Free;
  end;
end;

// Processes all CSV rows and converts them to shapefile features
procedure TCSVParser.ProcessCSVRows(ShapeWriter : TObject; CSVFile,
  HeaderList : TStringList; const Delim : Char; const fs : TFormatSettings;
  shapeType : TEShapeType; xIdx, yIdx, zIdx, mIdx, wktIdx : Integer);
var
  lineCount: Integer;
  fieldList: TStringList;
  statusText: string;
  wktText: string;
  geomType: string;
  partsArr: TPointDArrayArray = Nil;  // Initialize to nil
  i: Integer;
  attrArr: array of string = Nil;
  totalRows: Integer;
  success: Boolean;
  detectedShapeType: TEShapeType;
begin
  // CHECK: Compare selected shape type with data in CSV
  detectedShapeType:= DetectGeometryTypeFromData(CSVFile, Delim, xIdx, yIdx, wktIdx);

  if (detectedShapeType <> stNull) and (detectedShapeType <> shapeType) then begin
    DoLog(Format('Geometry type mismatch! CSV contains %s data, but export is configured for %s. (CSV parser).',
      [ShapeTypeToString(detectedShapeType), ShapeTypeToString(shapeType)]));
    raise Exception.CreateFmt('Geometry type mismatch! CSV contains %s data, but export is configured for %s.',
      [ShapeTypeToString(detectedShapeType), ShapeTypeToString(shapeType)]);
  end;

  fieldList:= TStringList.Create;
  try
    totalRows:= CSVFile.Count - 1; // Number of data rows (excluding header)

    for lineCount:= 1 to totalRows do begin
      ParseCSVLineToTStringList(CSVFile[lineCount], fieldList, Delim);
      if fieldList.Count < 1 then
        Continue;

      // Report progress every 100 rows or at the end
      if (lineCount mod 100 = 0) or (lineCount = totalRows) then begin
        DoProgress(lineCount, totalRows, Format('Processing CSV row %d of %d', [lineCount, totalRows]));
      end;

      // Check WKT column (if present)
      if (wktIdx <> -1) and (wktIdx < fieldList.Count) then begin
        wktText:= Trim(fieldList[wktIdx]);
        if wktText <> '' then begin
          if ParseWKTToParts(wktText, geomType, partsArr) then begin
            geomType:= UpperCase(Trim(geomType));

            // Build attribute array: all fields except geometry columns
            SetLength(attrArr, 0);
            for i:= 0 to HeaderList.Count - 1 do begin
              if (i = xIdx) or (i = yIdx) or (i = zIdx) or (i = mIdx) or (i = wktIdx) then
                Continue;

              SetLength(attrArr, Length(attrArr) + 1);
              if i < fieldList.Count then
                attrArr[High(attrArr)]:= fieldList[i]
              else
                attrArr[High(attrArr)]:= '';
            end;

            // Depending on geomType: call the appropriate shapewriter function and give attrArr
            if geomType = 'POINT' then begin
              try
                // partsArr[0][0] must exist
                if (Length(partsArr) >= 1) and (Length(partsArr[0]) >= 1) then begin
                  if (not IsNan(partsArr[0][0].Z)) then begin
                    success:= TShapefileWriter(ShapeWriter).AddPoint(partsArr[0][0].X, partsArr[0][0].Y,
                      partsArr[0][0].Z, partsArr[0][0].M, attrArr);
                    if not success then begin
                      DoLog(Format('Row %d: Processing stopped - incompatible shape type. (CSV parser).', [lineCount + 1]));
                      Break;
                    end;
                  end
                  else begin
                    success:= TShapefileWriter(ShapeWriter).AddPoint(partsArr[0][0].X, partsArr[0][0].Y, attrArr);
                    if not success then begin
                      DoLog(Format('Row %d: Processing stopped - incompatible shape type. (CSV parser).', [lineCount + 1]));
                      Break;
                    end;
                  end;
                end;
              except
                on E: Exception do
                begin
                  // Log the error with line number for point validation errors
                  DoLog(Format('Row %d: Point skipped - %s. (CSV parser).', [lineCount + 1, E.Message]));
                end;
              end;
            end

            else if geomType = 'MULTIPOINT' then begin
              try
                if (Length(partsArr) >= 1) then
                  TShapefileWriter(ShapeWriter).AddPoly(partsArr[0], attrArr, lineCount);
              except
                on E: Exception do
                begin
                  // Log the error with line number for multipoint validation errors
                  DoLog(Format('Row %d: MultiPoint skipped - %s. (CSV parser).', [lineCount + 1, E.Message]));
                end;
              end;
            end

            else if geomType = 'LINESTRING' then begin
              try
                if (Length(partsArr) >= 1) then
                  TShapefileWriter(ShapeWriter).AddPoly(partsArr[0], attrArr, lineCount);
              except
                on E: Exception do begin
                  // Log the error with line number for line validation errors
                  DoLog(Format('Row %d: LineString skipped - %s. (CSV parser).', [lineCount + 1, E.Message]));
                end;
              end;
            end

            else if geomType = 'MULTILINESTRING' then begin
              try
                if Length(partsArr) > 0 then begin
                  success:= TShapefileWriter(ShapeWriter).AddPolyParts(partsArr, attrArr, lineCount);
                  if not success then begin
                    DoLog(Format('Row %d: Processing stopped - incompatible shape type. (CSV parser).', [lineCount + 1]));
                    Break;
                  end;
                end;
              except
                on E: Exception do begin
                  // Log the error with line number for multi line validation errors
                  DoLog(Format('Row %d: MultiLineString skipped - %s. (CSV parser).', [lineCount + 1, E.Message]));
                end;
              end;
            end

            else if geomType = 'POLYGON' then begin
              try
                if Length(partsArr) = 1 then begin
                  success:= TShapefileWriter(ShapeWriter).AddPoly(partsArr[0], attrArr, lineCount);
                  if not success then begin
                    DoLog(Format('Row %d: Processing stopped - incompatible shape type. (CSV parser).', [lineCount + 1]));
                    Break;
                  end;
                end
                else if Length(partsArr) > 1 then
                  TShapefileWriter(ShapeWriter).AddPolyParts(partsArr, attrArr, lineCount);
              except
                on E: Exception do begin
                  // Log the error with line number for polygon validation errors
                  DoLog(Format('Row %d: Polygon skipped - %s. (CSV parser).', [lineCount + 1, E.Message]));
                end;
              end;


            end
            else if geomType = 'MULTIPOLYGON' then begin
              try
                if Length(partsArr) > 0 then
                  TShapefileWriter(ShapeWriter).AddPolyParts(partsArr, attrArr, lineCount);
              except
                on E: Exception do begin
                  // Log the error with line number for polygon validation errors
                  DoLog(Format('Row %d: Multipoint skipped - %s. (CSV parser).', [lineCount + 1, E.Message]));
                end;
              end;
            end;

            Continue;
          end;
          // If ParseWKTToParts returned false -> fall back to X/Y (below)
        end;
      end;

      // fallback: X/Y columns (standard processing)
      statusText:= '';
      case shapeType of
        stPoint: ProcessPointRow(ShapeWriter, fieldList, HeaderList, fs, lineCount, xIdx, yIdx, zIdx, mIdx, wktIdx, statusText);  { #todo : Remove statusText; is not used }
        stPolyLine, stPolygon: ProcessPolyRow(ShapeWriter, fieldList, HeaderList, fs, lineCount, xIdx, yIdx, zIdx, mIdx, wktIdx);
      end;
    end;

    // Report completion
    DoProgress(totalRows, totalRows, 'CSV processing complete');

  finally
    fieldList.Free;
  end;
end;

// Processes a single row as a point feature
procedure TCSVParser.ProcessPointRow(ShapeWriter: TObject;
  const FieldList, HeaderList: TStringList; const fs: TFormatSettings;
  lineIndex: Integer; xIdx, yIdx, zIdx, mIdx, wktIdx: Integer; var statusText: string);
var
  i: Integer;
  pointValues: array of string = Nil;
  x, y, z, m: Double;
  hasZ, hasM: Boolean;
  sVal: string;
begin
  statusText:= '';
  hasZ:= (zIdx <> -1) and (zIdx < FieldList.Count);
  hasM:= (mIdx <> -1) and (mIdx < FieldList.Count);

  // Build attributes: all fields except x/y/z/m/wkt
  SetLength(pointValues, 0);
  for i:= 0 to HeaderList.Count - 1 do begin
    if (i = xIdx) or (i = yIdx) or (i = zIdx) or (i = mIdx) or (i = wktIdx) then
      Continue;
    SetLength(pointValues, Length(pointValues) + 1);
    if i < FieldList.Count then
      pointValues[High(pointValues)]:= FieldList[i]
    else
      pointValues[High(pointValues)]:= '';
  end;

  // X
  if (xIdx < 0) or (xIdx >= FieldList.Count) then begin
    statusText:= Format('Row %d: Missing X coordinate', [lineIndex]);
    Exit;
  end;
  sVal:= NormalizeNumberString(FieldList[xIdx], fs);
  if not TryStrToFloat(sVal, x, fs) then begin
    statusText:= Format('Row %d: Invalid X coordinate: %s', [lineIndex, FieldList[xIdx]]);
    Exit;
  end;

  // Y
  if (yIdx < 0) or (yIdx >= FieldList.Count) then begin
    statusText:= Format('Row %d: Missing Y coordinate', [lineIndex]);
    Exit;
  end;
  sVal:= NormalizeNumberString(FieldList[yIdx], fs);
  if not TryStrToFloat(sVal, y, fs) then begin
    statusText:= Format('Row %d: Invalid Y coordinate: %s', [lineIndex, FieldList[yIdx]]);
    Exit;
  end;

  // Z (optional)
  if hasZ then begin
    if not TryStrToFloat(NormalizeNumberString(FieldList[zIdx], fs), z, fs) then
      z:= 0;
  end
  else
    z:= 0;

  if hasM then begin
    if not TryStrToFloat(NormalizeNumberString(FieldList[mIdx], fs), m, fs) then
      m:= 0;
  end
  else
    m:= 0;

  // Write point with attributes
  if (hasZ) then
    TShapefileWriter(ShapeWriter).AddPoint(x, y, z, m, pointValues)
  else
    TShapefileWriter(ShapeWriter).AddPoint(x, y, pointValues);
end;

// Processes a single row as a polygon/polyline feature
procedure TCSVParser.ProcessPolyRow(ShapeWriter: TObject;
  const FieldList, HeaderList: TStringList; const fs: TFormatSettings;
  lineIndex: Integer; xIdx, yIdx, zIdx, mIdx, wktIdx: Integer);
var
  points: array of TPointD = Nil;
  x, y, z, m: Double;
  i: Integer;
  attrArr: array of string = Nil;
begin
  // Expect at least X and Y
  if (xIdx < 0) or (yIdx < 0) then
    Exit;
  if (xIdx >= FieldList.Count) or (yIdx >= FieldList.Count) then
    Exit;

  SetLength(points, 1);
  if not TryStrToFloat(NormalizeNumberString(FieldList[xIdx], fs), x, fs) then Exit;
  if not TryStrToFloat(NormalizeNumberString(FieldList[yIdx], fs), y, fs) then Exit;

  points[0].X:= x;
  points[0].Y:= y;

  if (zIdx <> -1) and (zIdx < FieldList.Count) then begin
    if TryStrToFloat(NormalizeNumberString(FieldList[zIdx], fs), z, fs) then
      points[0].Z:= z
    else
      points[0].Z:= NaN;
  end
  else
    points[0].Z:= NaN;

  if (mIdx <> -1) and (mIdx < FieldList.Count) then begin
    if TryStrToFloat(NormalizeNumberString(FieldList[mIdx], fs), m, fs) then
      points[0].M:= m
    else
      points[0].M:= NaN;
  end
  else
    points[0].M:= NaN;

  // attributes - skip geometry columns
  SetLength(attrArr, 0);
  for i:= 0 to HeaderList.Count - 1 do begin
    if (i = xIdx) or (i = yIdx) or (i = zIdx) or (i = mIdx) or (i = wktIdx) then
      Continue;
    SetLength(attrArr, Length(attrArr) + 1);
    if i < FieldList.Count then
      attrArr[High(attrArr)]:= FieldList[i]
    else
      attrArr[High(attrArr)]:= '';
  end;

  TShapefileWriter(ShapeWriter).AddPoly(points, attrArr, lineIndex);
end;

// Normalizes number strings by replacing common decimal separators
function TCSVParser.NormalizeNumberString(const S: string; const fs: TFormatSettings): string;
begin
  Result:= StringReplace(S, '.', fs.DecimalSeparator, [rfReplaceAll]);
  Result:= StringReplace(Result, ',', fs.DecimalSeparator, [rfReplaceAll]);
end;

// Identifies which columns contain coordinate data or WKT geometry
procedure TCSVParser.IdentifyCoordinateIndices(const HeaderList: TStringList;
  out xIdx, yIdx, zIdx, mIdx, wktIdx: Integer);
var
  i: Integer;
  header: string;
begin
  xIdx:= -1;
  yIdx:= -1;
  zIdx:= -1;
  mIdx:= -1;
  wktIdx:= -1;

  for i:= 0 to HeaderList.Count - 1 do
  begin
    header:= UpperCase(Trim(HeaderList[i]));

    if (header = 'X') or (header = 'LON') or (header = 'LONG') or (header = 'LONGITUDE') then
      xIdx:= i
    else if (header = 'Y') or (header = 'LAT') or (header = 'LATITUDE') then
      yIdx:= i
    else if (header = 'Z') or (header = 'ELEVATION') or (header = 'ALT') or (header = 'ALTITUDE') then
      zIdx:= i
    else if (header = 'M') or (header = 'MEASURE') then
      mIdx:= i
    else if (header = 'WKT') or (header = 'GEOM') or (header = 'GEOMETRY') or (header = 'SHAPE') then
      wktIdx:= i;
  end;
end;

// Detects the delimiter used in a CSV line by analyzing common separators
function TCSVParser.GetDelimiter(const AText: string): Char;
var
  commaCount, semicolonCount, tabCount: Integer;
  i: Integer;
begin
  commaCount:= 0;
  semicolonCount:= 0;
  tabCount:= 0;

  for i:= 1 to Length(AText) do
  begin
    case AText[i] of
      ',': Inc(commaCount);
      ';': Inc(semicolonCount);
      #9: Inc(tabCount);
    end;
  end;

  // Return the most common delimiter
  if (commaCount >= semicolonCount) and (commaCount >= tabCount) then
    Result:= ','
  else if (semicolonCount >= commaCount) and (semicolonCount >= tabCount) then
    Result:= ';'
  else
    Result:= #9; // Tab
end;

end.
