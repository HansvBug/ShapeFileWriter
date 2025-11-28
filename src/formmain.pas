{ Copyright ©2025 Hans van Buggenum }
unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons;

type
  TShapefileProgressEvent = procedure(Sender: TObject; Current, Total: Integer; const Message: string) of object;

  { TFrmMain }
  TFrmMain = class(TForm)
    btnClose : TButton;
    btnBDExportShape : TButton;
    btnFDExportShape : TButton;
    cbBDShapeType : TComboBox;
    cbDelimiter : TComboBox;
    cbGeometryType : TComboBox;
    edtBDCoordSystem : TEdit;
    edtBDOutDir : TEdit;
    edtCSVFile : TEdit;
    gbBasicDemo : TGroupBox;
    gbCsvDemo : TGroupBox;
    lblBDCoordinateSystem : TLabel;
    lblBDShapeType : TLabel;
    lblBDOutDir : TLabel;
    lblCSVdelimiter : TLabel;
    lblCSVfile : TLabel;
    lblStatus : TLabel;
    memoProgress : TMemo;
    memoError : TMemo;
    PageControl1 : TPageControl;
    Panel1 : TPanel;
    ProgressBar1 : TProgressBar;
    sbtnBDSelectOutputDir : TSpeedButton;
    sbtnSelectCSVFile : TSpeedButton;
    StatusBar1 : TStatusBar;
    tsFullDemo : TTabSheet;
    tsBasicDemo : TTabSheet;
    procedure btnBDExportShapeClick(Sender : TObject);
    procedure btnCloseClick(Sender : TObject);
    procedure btnFDExportShapeClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure sbtnBDSelectOutputDirClick(Sender : TObject);
    procedure sbtnSelectCSVFileClick(Sender : TObject);
  private
    procedure ControlsBDOnChanged(Sender: TObject);  // Basic Demo tabpage
    procedure ControlsFDOnChanged(Sender: TObject);  // Full Demo tabpage
    procedure HandleExportComplete(Sender: TObject; const Message: string);
    procedure HandleProgress(Sender: TObject; Current, Total: Integer; const Message: string);
    procedure HandleCSVLog(Sender: TObject; const Message: string);

    procedure WriteBasicDemoShape(FullFileName: String);
    function ShowSaveDialogForPrefix(var DefaultPrefix, OutDir, OutPrefix : string): Boolean;
  public

  end;

var
  FrmMain : TFrmMain;

implementation
uses BasicDemoShape, CSVParser, ShapeWriter;

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.btnCloseClick(Sender : TObject);
begin
  Close;
end;

procedure TFrmMain.btnFDExportShapeClick(Sender : TObject);
var
  CSVFile, headerList, dataList: TStringList;
  outDir: string = '';
  filePrefix: string;
  Delim: Char;
  fs: TFormatSettings;
  xIdx, yIdx, zIdx, mIdx, wktIdx: Integer;
  lShapeWriter: TShapefileWriter;
  shapeType: TEShapeType;
  csvParser: TCSVParser;
begin
  // basic checks
  if edtCSVFile.Text = '' then begin
    ShowMessage('First select a CSV file');
    Exit;
  end;
  if not FileExists(edtCSVFile.Text) then
  begin
    ShowMessage('CSV does not exist: ' + edtCSVFile.Text);
    Exit;
  end;

  StatusBar1.SimpleText:= '';
  // Reset progress bar
  ProgressBar1.Position:= 0;
  ProgressBar1.Max:= 100;

  csvParser:= TCSVParser.Create;
  try
    btnFDExportShape.Enabled:= False;
    csvParser.OnProgress:= @HandleProgress;
    // delimiter and locale
    Delim:= csvParser.GetDelimiter(cbDelimiter.Text);
    fs:= FormatSettings;

    // Determine the geometry type to be applied
    case cbGeometryType.ItemIndex of
      0: shapeType:= stPoint;
      1: shapeType:= stPolyLine;
      2: shapeType:= stPolygon;
    else
      ShowMessage('Shape type does not exist.');
      Exit; // csvParser wordt in outer finally vrijgegeven
    end;

    filePrefix:= ChangeFileExt(ExtractFileName(edtCSVFile.Text), '');

    if not ShowSaveDialogForPrefix(filePrefix, outDir, filePrefix) then
      Exit; // csvParser wordt in outer finally vrijgegeven

    // Initialize
    CSVFile:= nil;
    headerList:= nil;
    dataList:= nil;
    lShapeWriter:= nil;

    try
      try
        Screen.Cursor:= crHourGlass;
        lShapeWriter:= TShapefileWriter.Create(shapeType);  // Create shapewriter
        lShapeWriter.OutputDir:= outDir;
        // bind the process and log events
        lShapeWriter.OnProgress:= @HandleProgress;
        lShapeWriter.OnLog:= @HandleCSVLog;
        csvParser.OnLog:= @HandleCSVLog;

        // load csv en header
        if not csvParser.LoadCSVToLists(edtCSVFile.Text, Delim, CSVFile, headerList, dataList) then
        begin
          ShowMessage('CSV file is empty or could not be loaded.');
          Exit;
        end;

        lblStatus.Caption:= 'Processing CSV data...';
        Application.ProcessMessages;

        // Identify coordinate indices (= Column index)
        csvParser.IdentifyCoordinateIndices(headerList, xIdx, yIdx, zIdx, mIdx, wktIdx);

        // Vereiste: ofwel X/Y aanwezig ofwel WKT-kolom aanwezig
        if ((xIdx = -1) or (yIdx = -1)) and (wktIdx = -1) then
        begin
          ShowMessage('Could not identify coordinate columns or WKT geometry column. Provide X/Y columns or a WKT geometry column. Check the headernames and the delimiter.');
          Exit;
        end;

        // Voeg DBF velden toe (negeer WKT/geometry kolom als attribuutkolom)
        // Eerst de header maken. Als dat niet kan dan stoppen.
        if csvParser.AddDBFFieldsFromHeader(lShapeWriter, CSVFile, headerList, Delim, fs, xIdx, yIdx, zIdx, mIdx, wktIdx) then begin
          // Verwerk alle rijen (geeft prioriteit aan WKT kolom indien aanwezig)
      // Deze duurt lang bij polygon geometrie
          csvParser.ProcessCSVRows(lShapeWriter, CSVFile, headerList, Delim, fs, shapeType, xIdx, yIdx, zIdx, mIdx, wktIdx);

          // // Save dbf file. The data has been prepared by the CSVParser.
          lblStatus.Caption:= 'Writing shapefile...';
          Application.ProcessMessages;
          //TShapefileWriter(lShapeWriter).SaveTo(filePrefix, WKT_4326);     { #todo : Hoe te bepalen wat nodig is??? combobox voor maken? }
          TShapefileWriter(lShapeWriter).SaveTo(filePrefix, WKT_28992);      { #todo : Hoe te bepalen wat nodig is??? combobox voor maken? }

          lblStatus.Caption := 'Conversion complete! Shapefile saved.';

          ShowMessage('Conversion complete!' + sLineBreak +
                      'Shapefile saved as: ' + filePrefix + '.shp');

        end;
      except
        on E: Exception do begin
          lblStatus.Caption:= 'Error: ' + E.Message;
          ShowMessage('Error during conversion: ' + E.Message);
        end;
      end;
    finally
      // Opruimen — zorg dat alles precies één keer wordt vrijgegeven
      if Assigned(lShapeWriter) then
        FreeAndNil(lShapeWriter);

      FreeAndNil(dataList); //
      FreeAndNil(headerList);
      FreeAndNil(CSVFile);
      btnFDExportShape.Enabled:= True;
    end;

  finally
    // csvParser één keer vrijgeven in outer finally
    FreeAndNil(csvParser);
    Screen.Cursor:= crDefault;
  end;
end;

procedure TFrmMain.btnBDExportShapeClick(Sender : TObject);
var
  saveDialog: TSaveDialog;
begin
  saveDialog := TSaveDialog.Create(nil);
  memoProgress.Clear;
  memoError.Clear;
  try
    saveDialog.Title := Format('Save %s Shapefile', [cbBDShapeType.Text]);
    saveDialog.DefaultExt := 'shp';
    saveDialog.Filter := 'ESRI Shapefile (*.shp)|*.shp';
    saveDialog.Options := [ofOverwritePrompt, ofPathMustExist];

    if not saveDialog.Execute then Exit;
      WriteBasicDemoShape(saveDialog.FileName);
  finally
    saveDialog.Free;
  end;
end;

procedure TFrmMain.FormCreate(Sender : TObject);
begin
  Self.Color:= clWindow;
  btnBDExportShape.Enabled:= False;
  memoProgress.Clear;
  memoError.Clear;
  cbDelimiter.ItemIndex:= 2;
  btnFDExportShape.Enabled:= False;

  cbBDShapeType.OnChange:= @ControlsBDOnChanged;
  edtBDOutDir.OnChange:= @ControlsBDOnChanged;

  edtCSVFile.OnChange:= @ControlsFDOnChanged;

  cbDelimiter.OnChange:= @ControlsFDOnChanged;
  cbGeometryType.OnChange:= @ControlsFDOnChanged;
end;

procedure TFrmMain.sbtnBDSelectOutputDirClick(Sender : TObject);
var
  selectDirectoryDlg: TSelectDirectoryDialog;
begin
  selectDirectoryDlg:= TSelectDirectoryDialog.Create(Self);
  try
    selectDirectoryDlg.Title := 'Selecteer output directory';

    {$IFDEF WINDOWS}
    selectDirectoryDlg.InitialDir:= GetEnvironmentVariable('USERPROFILE') + PathDelim + 'Documents';
    {$ENDIF}
    {$IFDEF LINUX}
    selectDirectoryDlg.InitialDir:= GetEnvironmentVariable('HOME') + PathDelim + 'Documents';
    {$ENDIF}

    if selectDirectoryDlg.Execute then begin
      edtBDOutDir.Text:= IncludeTrailingPathDelimiter(selectDirectoryDlg.FileName);
    end;
  finally
    selectDirectoryDlg.Free;
  end;
end;

procedure TFrmMain.sbtnSelectCSVFileClick(Sender : TObject);
var
  openDlg: TOpenDialog;
begin
  openDlg:= TOpenDialog.Create(Self);
  with openDlg do
  try
    Title:= 'Select CSV file';
    Filter:= 'CSV files|*.csv|All files|*.*';
    DefaultExt:= 'csv';

    if edtCSVFile.Text = '' then begin
      {$IFDEF WINDOWS}
      openDlg.InitialDir:= GetEnvironmentVariable('USERPROFILE') + PathDelim + 'Documents';
      {$ENDIF}
      {$IFDEF LINUX}
      openDlg.InitialDir:= GetEnvironmentVariable('HOME') + PathDelim + 'Documents';
      {$ENDIF}
    end
    else
      openDlg.InitialDir:= edtCSVFile.Text;

    if Execute then
      edtCSVFile.Text:= FileName;
  finally
    Free;
  end;
end;

procedure TFrmMain.ControlsBDOnChanged(Sender : TObject);
begin
  btnBDExportShape.Enabled:= (cbBDShapeType.Text <> '') and (edtBDOutDir.Text <> '');
end;

procedure TFrmMain.ControlsFDOnChanged(Sender : TObject);
begin
  btnFDExportShape.Enabled:= (edtCSVFile.Text <> '') and {(edtOutDir.Text <> '') and}
                             (cbDelimiter.Text <> '') and (cbGeometryType.Text <> '');
end;

procedure TFrmMain.HandleExportComplete(Sender : TObject; const Message : string);
begin
  memoProgress.Lines.Add(Message);
end;

procedure TFrmMain.HandleProgress(Sender : TObject; Current, Total : Integer; const Message : string);
begin
  // Update progress bar
  if Total > 0 then begin
    ProgressBar1.Max:= Total;
    ProgressBar1.Position:= Current;
  end;

  // update Memo
  if Message <> '' then
    memoProgress.Lines.Add(Format('[%d/%d] %s', [Current, Total, Message]));

  // Force UI update
  Application.ProcessMessages;
end;

procedure TFrmMain.HandleCSVLog(Sender : TObject; const Message : string);
begin
  memoError.Lines.Add(Message);
end;

procedure TFrmMain.WriteBasicDemoShape(FullFileName : String);
var
  basicDemoShape: TBasicDemoShape;
begin
  StatusBar1.SimpleText:= '';
  // Reset progress bar
  ProgressBar1.Position:= 0;
  ProgressBar1.Max:= 100;

  basicDemoShape:= TBasicDemoShape.Create;
  try
    // Connect events
    basicDemoShape.OnExportComplete:= @HandleExportComplete;
    basicDemoShape.OnProgress:= @HandleProgress;
    // write the demo shape(s)
    basicDemoShape.WriteBasicDemoShape(FullFileName, cbBDShapeType.Text, edtBDCoordSystem.Text);
  finally
    basicDemoShape.Free;
    // Reset progress bar after completion
    ProgressBar1.Position:= 0;
    StatusBar1.SimpleText:= '  Export completed';
  end;
end;

function TFrmMain.ShowSaveDialogForPrefix(var DefaultPrefix, OutDir,
  OutPrefix : string) : Boolean;
var
  saveDlg: TSaveDialog;
  s: string;
begin
  Result:= False;
  OutPrefix:= DefaultPrefix;
  saveDlg:= TSaveDialog.Create(Self);
  try
    saveDlg.Title:= 'Save as Shapefile';
    saveDlg.Filter:= 'Shapefile|*.shp';
    saveDlg.DefaultExt:= 'shp';
    //saveDlg.InitialDir:=
    saveDlg.FileName:= DefaultPrefix;

    if not saveDlg.Execute then
      Exit(False);

    // Pick up the filename of saveFileDialog and not the pre-prepared filename
    s := ChangeFileExt(ExtractFileName(saveDlg.FileName), '');
    if s <> '' then
      OutPrefix := s;

    s := ExtractFilePath(saveDlg.FileName);
    if s <> '' then
      OutDir := s;

    Result := True;
  finally
    saveDlg.Free;
  end;
end;

end.

