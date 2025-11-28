{ Copyright ©2025 Hans van Buggenum }

unit BasicDemoShape;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Return a message to the form }
  TShapefileExportEvent = procedure(Sender: TObject; const Message: string) of object;
  { Progress }
  TShapefileProgressEvent = procedure(Sender: TObject; Current, Total: Integer; const Message: string) of object;

  { TBasicDemoShape }
  TBasicDemoShape = class
    private
      fOnExportComplete: TShapefileExportEvent;
      fOnProgress: TShapefileProgressEvent;
    public
      procedure WriteBasicDemoShape(const FullFileName, shapeType, CoordSystem : String);
      property OnExportComplete: TShapefileExportEvent read fOnExportComplete write fOnExportComplete;
      property OnProgress: TShapefileProgressEvent read fOnProgress write fOnProgress;
  end;


implementation
uses ShapeWriter;

{ TBasicDemoShape }
procedure TBasicDemoShape.WriteBasicDemoShape(const FullFileName, shapeType, CoordSystem: String);
var
  shapeWriter, shapeWriterZM: TShapefileWriter;
  outDir, FilePrefix: string;
  lPoints: array of TPointD = nil;
  lShapeType: TEShapeType;
  parts: array of TPointDArray = nil;
  i, j: Integer;

  // Helper: define standard fields (ID and NAME). Can be expanded if necessary, but sufficient for small demo..
  procedure DefineDefaultFields(aWriter: TShapefileWriter);
  begin
    aWriter.AddDBFField('ID', 'N', 9, 0);
    aWriter.AddDBFField('NAME', 'C', 50);
  end;

  // Helper: add 2D point with (integer) ID and name string
  procedure AddPoint2D(aWriter: TShapefileWriter; X, Y: Double; ID: Integer; const AName: string);
  var
    vals: array of string = Nil;
  begin
    SetLength(vals, 2);
    vals[0]:= IntToStr(ID);
    vals[1]:= AName;
    aWriter.AddPoint(X, Y, vals);
  end;

  // Helper: add Z/M point (Z and M doubles) with attributes
  procedure AddPointZM(aWriter: TShapefileWriter; X, Y, Z, M: Double; ID: Integer; const AName: string);
  var
    vals: array of string = nil;
  begin
    SetLength(vals, 2);
    vals[0]:= IntToStr(ID);
    vals[1]:= AName;
    aWriter.AddPoint(X, Y, Z, M, vals);
  end;

  // Helper: add single-part polyline/polygon 2D
  procedure AddPoly2D(aWriter: TShapefileWriter; const Points: array of TPointD; ID: Integer; const AName: string);
  var
    vals: array of string = nil;
  begin
    SetLength(vals, 2);
    vals[0]:= IntToStr(ID);
    vals[1]:= AName;
    aWriter.AddPoly(Points, vals, -1);
  end;

  // Helper: Add Multipart Polyline/Polygon
  procedure AddPolyParts2D(aWriter: TShapefileWriter; const PartsArr: array of TPointDArray; ID: Integer; const AName: string);
  var
    vals: array of string = nil;
  begin
    SetLength(vals, 2);
    vals[0]:= IntToStr(ID);
    vals[1]:= AName;
    aWriter.AddPolyParts(PartsArr, vals, -1);
  end;

  // Helper: add single-part polyline/polygon with Z/M
  procedure AddPolyZM(aWriter: TShapefileWriter; const Points: array of TPointD; ID: Integer; const AName: string);
  var
    vals: array of string = nil;
  begin
    SetLength(vals, 2);
    vals[0]:= IntToStr(ID);
    vals[1]:= AName;
    aWriter.AddPoly(Points, vals, -1);
  end;

  // Helper: multipart Z/M
  procedure AddPolyPartsZM(aWriter: TShapefileWriter; const PartsArr: array of TPointDArray; ID: Integer; const AName: string);
  var
    vals: array of string = nil;
  begin
    SetLength(vals, 2);
    vals[0]:= IntToStr(ID);
    vals[1]:= AName;
    aWriter.AddPolyParts(PartsArr, vals, -1);
  end;
begin
  // Shape type
  case ShapeType of
    'Point': lShapeType:= stPoint;
    'PolyLine': lShapeType:= stPolyLine;
    'Polygon': lShapeType:= stPolygon;
  else
    lShapeType:= stPoint; // Eigenlijk zinloos. 33% kans dat dit juist is. { #todo : Nog een keer aanpassen }
  end;

  if FullFileName = '' then Exit;

  outDir:= ExtractFilePath(FullFileName);
  if outDir = '' then outDir := GetCurrentDir;
  if not DirectoryExists(outDir) then ForceDirectories(outDir);
  FilePrefix:= ChangeFileExt(ExtractFileName(FullFileName), '');

  // --- 2D writer ---
  shapeWriter:= TShapefileWriter.Create(lShapeType);
  try
    shapeWriter.OutputDir:= outDir;
    shapeWriter.OnProgress:= fOnProgress; // Link progress event to ShapeWriter (pass it on)

    // Create default fields, only for the demo (ID, NAME)
    DefineDefaultFields(shapeWriter);

    case lShapeType of
      stPoint:
        begin
          AddPoint2D(shapeWriter, 4.895168, 52.370216, 1, 'Amsterdam');
          AddPoint2D(shapeWriter, 5.121420, 52.090737, 2, 'Utrecht');
          AddPoint2D(shapeWriter, 4.477733, 51.924420, 3, 'Rotterdam');
        end;

      stPolyLine:
        begin
          // single-part
          SetLength(lPoints, 3);
          lPoints[0].X:= 4.8; lPoints[0].Y:= 52.3;
          lPoints[1].X:= 4.9; lPoints[1].Y:= 52.35;
          lPoints[2].X:= 5.0; lPoints[2].Y:= 52.4;
          AddPoly2D(shapeWriter, lPoints, 100, 'DemoLine_SinglePart');

          // multipart
          SetLength(parts, 2);
          SetLength(parts[0], 3);
          parts[0][0].X:= 4.8; parts[0][0].Y:= 52.30;
          parts[0][1].X:= 4.85; parts[0][1].Y:= 52.31;
          parts[0][2].X:= 4.9; parts[0][2].Y:= 52.32;

          SetLength(parts[1], 4);
          parts[1][0].X:= 5.0; parts[1][0].Y:= 52.40;
          parts[1][1].X:= 5.05; parts[1][1].Y:= 52.41;
          parts[1][2].X:= 5.1; parts[1][2].Y:= 52.42;
          parts[1][3].X:= 5.15; parts[1][3].Y:= 52.43;

          AddPolyParts2D(shapeWriter, parts, 101, 'DemoLine_MultiPart');
        end;

      stPolygon:
        begin
          // single-part polygon
          SetLength(lPoints, 5);
          lPoints[0].X := 4.8; lPoints[0].Y := 52.3;
          lPoints[1].X := 4.9; lPoints[1].Y := 52.3;
          lPoints[2].X := 4.9; lPoints[2].Y := 52.4;
          lPoints[3].X := 4.8; lPoints[3].Y := 52.4;
          lPoints[4]:= lPoints[0];
          AddPoly2D(shapeWriter, lPoints, 200, 'DemoPoly_SinglePart');

          // multipart polygon (outer + hole)
          SetLength(parts, 2);
          SetLength(parts[0], 5);
          parts[0][0].X:= 4.7; parts[0][0].Y:= 52.2;
          parts[0][1].X:= 4.95; parts[0][1].Y:= 52.2;
          parts[0][2].X:= 4.95; parts[0][2].Y:= 52.45;
          parts[0][3].X:= 4.7; parts[0][3].Y:= 52.45;
          parts[0][4]:= parts[0][0];

          SetLength(parts[1], 5);
          parts[1][0].X:= 4.8; parts[1][0].Y:= 52.3;
          parts[1][1].X:= 4.85; parts[1][1].Y:= 52.3;
          parts[1][2].X:= 4.85; parts[1][2].Y:= 52.35;
          parts[1][3].X:= 4.8; parts[1][3].Y:= 52.35;
          parts[1][4]:= parts[1][0];

          // safety close-check
          for i:= 0 to High(parts) do begin
            if Length(parts[i]) > 0 then begin
              j:= High(parts[i]);

              if not ((parts[i][0].X = parts[i][j].X) and (parts[i][0].Y = parts[i][j].Y)) then begin
                SetLength(parts[i], Length(parts[i]) + 1);
                parts[i][High(parts[i])]:= parts[i][0];
              end;
            end;
          end;

          AddPolyParts2D(shapeWriter, parts, 201, 'DemoPoly_MultiPart_Hole');
        end;
    end;

    if CoordSystem = '4326' then
      if shapeWriter.SaveTo(FilePrefix, WKT_4326) then begin
        // Send notification via event
        if Assigned(fOnExportComplete) then
          fOnExportComplete(Self, Format('Shapefile exported, location: %s',
            [ExtractFilePath(FullFileName) + FilePrefix + '.shp']));
      end
    else begin
      fOnExportComplete(Self, 'Unexpected error, the shape was not exported.');
    end;
  finally
    shapeWriter.Free;
  end;

  // --- Z/M writers (if desired) ---
  // Punt Z/M
  if lShapeType = stPoint then begin
    shapeWriterZM:= TShapefileWriter.Create(stPoint);
    try
      shapeWriterZM.OutputDir:= outDir;
      shapeWriterZM.OnProgress:= fOnProgress;

      DefineDefaultFields(shapeWriterZM);

      AddPointZM(shapeWriterZM, 4.895168, 52.370216, 10.5, 123.0, 1001, 'Amsterdam_ZM');
      AddPointZM(shapeWriterZM, 5.121420, 52.090737, 7.2, 98.4, 1002, 'Utrecht_ZM');
      AddPointZM(shapeWriterZM, 4.477733, 51.924420, 2.0, 210.7, 1003, 'Rotterdam_ZM');

      if CoordSystem = '4326' then
        if shapeWriter.SaveTo(FilePrefix + '_ZM_point', WKT_4326) then begin
          // Send notification via event
          if Assigned(fOnExportComplete) then
            fOnExportComplete(Self, Format('Shapefile exported, location: %s',
              [ExtractFilePath(FullFileName) + FilePrefix + '_ZM_point.shp']));
        end
      else begin
        fOnExportComplete(Self, 'Unexpected error, the shape was not exported.');
      end;

    finally
      shapeWriterZM.Free;
    end;
  end;

  // Line Z/M
  if lShapeType = stPolyLine then begin
    shapeWriterZM:= TShapefileWriter.Create(stPolyLine);
    try
      shapeWriterZM.OutputDir:= outDir;
      shapeWriterZM.OnProgress:= fOnProgress;

      DefineDefaultFields(shapeWriterZM);

      SetLength(lPoints, 3);
      lPoints[0].X:= 4.8; lPoints[0].Y:= 52.3; lPoints[0].Z:= 12.0; lPoints[0].M:= 0.0;
      lPoints[1].X:= 4.9; lPoints[1].Y:= 52.35; lPoints[1].Z:= 15.5; lPoints[1].M:= 50.0;
      lPoints[2].X:= 5.0; lPoints[2].Y:= 52.4; lPoints[2].Z:= 20.2; lPoints[2].M:= 100.0;
      AddPolyZM(shapeWriterZM, lPoints, 1100, 'DemoLineZ_SinglePart');

      SetLength(parts, 2);
      SetLength(parts[0], 3);
      parts[0][0].X:= 4.8; parts[0][0].Y:= 52.30; parts[0][0].Z:= 5.0; parts[0][0].M:= 1.0;
      parts[0][1].X:= 4.85; parts[0][1].Y:= 52.31; parts[0][1].Z:= 6.0; parts[0][1].M:= 2.0;
      parts[0][2].X:= 4.9; parts[0][2].Y:= 52.32; parts[0][2].Z:= 7.0; parts[0][2].M:= 3.0;

      SetLength(parts[1], 4);
      parts[1][0].X:= 5.0; parts[1][0].Y:= 52.40; parts[1][0].Z:= 8.0; parts[1][0].M:= 10.0;
      parts[1][1].X:= 5.05; parts[1][1].Y:= 52.41; parts[1][1].Z:= 9.0; parts[1][1].M:= 20.0;
      parts[1][2].X:= 5.1; parts[1][2].Y:= 52.42; parts[1][2].Z:= 10.0; parts[1][2].M:= 30.0;
      parts[1][3].X:= 5.15; parts[1][3].Y:= 52.43; parts[1][3].Z:= 11.0; parts[1][3].M:= 40.0;

      AddPolyPartsZM(shapeWriterZM, parts, 1101, 'DemoLineZ_MultiPart');

      if CoordSystem = '4326' then
        if shapeWriter.SaveTo(FilePrefix + '_ZM_line', WKT_4326) then begin
          // Send notification via event
          if Assigned(fOnExportComplete) then
            fOnExportComplete(Self, Format('Shapefile exported, location: %s',
              [ExtractFilePath(FullFileName) + FilePrefix + '_ZM_line.shp']));
        end
      else begin
        fOnExportComplete(Self, 'Unexpected error, the shape was not exported.');
      end;

    finally
      shapeWriterZM.Free;
    end;
  end;

  // Poly Z/M
  if lShapeType = stPolygon then begin
    shapeWriterZM:= TShapefileWriter.Create(stPolygon);
    try
      shapeWriterZM.OutputDir:= outDir;
      shapeWriterZM.OnProgress:= fOnProgress;

      DefineDefaultFields(shapeWriterZM);

      SetLength(lPoints, 5);
      lPoints[0].X:= 4.8; lPoints[0].Y:= 52.3; lPoints[0].Z:= 1.0; lPoints[0].M:= 10.0;
      lPoints[1].X:= 4.9; lPoints[1].Y:= 52.3; lPoints[1].Z:= 1.5; lPoints[1].M:= 20.0;
      lPoints[2].X:= 4.9; lPoints[2].Y:= 52.4; lPoints[2].Z:= 2.0; lPoints[2].M:= 30.0;
      lPoints[3].X:= 4.8; lPoints[3].Y:= 52.4; lPoints[3].Z:= 2.5; lPoints[3].M:= 40.0;
      lPoints[4]:= lPoints[0];
      AddPolyZM(shapeWriterZM, lPoints, 1200, 'DemoPolyZ_SinglePart');

      SetLength(parts, 2);
      SetLength(parts[0], 5);
      parts[0][0].X:= 4.7; parts[0][0].Y:= 52.2; parts[0][0].Z:= 0.5; parts[0][0].M:= 5.0;
      parts[0][1].X:= 4.95; parts[0][1].Y:= 52.2; parts[0][1].Z:= 0.6; parts[0][1].M:= 6.0;
      parts[0][2].X:= 4.95; parts[0][2].Y:= 52.45; parts[0][2].Z:= 0.7; parts[0][2].M:= 7.0;
      parts[0][3].X:= 4.7; parts[0][3].Y:= 52.45; parts[0][3].Z:= 0.8; parts[0][3].M:= 8.0;
      parts[0][4]:= parts[0][0];

      SetLength(parts[1], 5);
      parts[1][0].X:= 4.8; parts[1][0].Y:= 52.3; parts[1][0].Z:= 0.9; parts[1][0].M:= 9.0;
      parts[1][1].X:= 4.85; parts[1][1].Y:= 52.3; parts[1][1].Z:= 1.0; parts[1][1].M:= 10.0;
      parts[1][2].X:= 4.85; parts[1][2].Y:= 52.35; parts[1][2].Z:= 1.1; parts[1][2].M:= 11.0;
      parts[1][3].X:= 4.8; parts[1][3].Y:= 52.35; parts[1][3].Z:= 1.2; parts[1][3].M:= 12.0;
      parts[1][4]:= parts[1][0];

      AddPolyPartsZM(shapeWriterZM, parts, 1201, 'DemoPolyZ_MultiPart_Hole');

      if CoordSystem = '4326' then
        if shapeWriter.SaveTo(FilePrefix + '_ZM_poly', WKT_4326) then begin
          // Send notification via event
          if Assigned(fOnExportComplete) then
            fOnExportComplete(Self, Format('Shapefile exported, location: %s',
              [ExtractFilePath(FullFileName) + FilePrefix + '_ZM_poly.shp']));
        end
      else begin
        fOnExportComplete(Self, 'Unexpected error, the shape was not exported.');
      end;

    finally
      shapeWriterZM.Free;
    end;
  end;
end;

end.

