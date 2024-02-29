unit SimThyrGUIServices;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 5.0.0 (Mirage) }

{ (c) J. W. Dietrich, 1994 - 2024 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

{ This unit provides some global functions for use by other units }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}


interface

uses
  Classes, SysUtils, Grids, StdCtrls, Dialogs, Forms, SimThyrTypes,
  SimThyrResources, UnitConverter, DIFSupport, DOM, FileUtil, DateUtils
  {$IFDEF WINDOWS}
  , Windows, Win32Proc, registry
  {$ENDIF}
  {$IFDEF DARWIN}
  , MacOSAll
  {$ENDIF}
  {$IFDEF LCLCocoa}
  , CocoaAll, CocoaUtils
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF}
  , EnvironmentInfo, SimThyrBaseServices;

const
  iuSystemScript = -1;
  iuCurrentScript = -2;
  iuWordSelectTable = 0;
  iuWordWrapTable = 1;
  iuNumberPartsTable = 2;
  iuUnTokenTable = 3;
  iuWhiteSpaceList = 4;

type
  tSaveMode = (TimeSeries, Plot);

var
  gSaveMode: tSaveMode;

procedure bell;
procedure ClearResultContents(var theContents: tResultContent);
procedure writeaTableCell(theTable: TStringGrid; theCell: TableCell; theString: Str255);
procedure writeTableCells(theTable: TStringGrid; theContents: tResultContent);
procedure SaveGridToFile(theTable: TStringGrid; theFileName: string;
  theDelimiter: char; colNames, rowNames: boolean; var ReturnCode: integer);
procedure SetStatusBarPanel0(curr, max: string);
procedure writeaMemoLine(theMemo: TMemo; theString: Str255);
function DarkTheme: boolean;
procedure SetFileName(theForm: TForm; const FileName: string);
procedure ShowImplementationMessage;
procedure ShowImplementationMessage(theText: string);
procedure ShowFormatMessage;
procedure ShowFileError;
procedure ShowVersionError;
procedure ShowURLStatus(theCode: integer);
procedure ShowSaveError;
procedure ShowMemoryError;
procedure ShowPrereleaseWarning;

implementation

uses SimThyrLog;

procedure bell; {platform-independent implementation of acustical warning}
var
  s: longint;
begin
  {$IFDEF WINDOWS}
  MessageBeep(0);
  {$ELSE}
  {$IFDEF LCLCarbon}
  SysBeep(30);
  {$ELSE}
  s := fpSystem('echo -ne ''\007''');
  {s := fpSystem('echo -ne "\a"');}
  {s := fpSystem('tput bel');}
  {beep;}
    {$ENDIF}
  {$ENDIF}
end;

procedure ClearResultContents(var theContents: tResultContent);
{deletes a row}
var
  counter: integer;
begin
  for counter := 1 to RES_MAX_COLS do
    theContents[counter] := ' ';
end;


procedure writeaTableCell(theTable: TStringGrid; theCell: TableCell; theString: Str255);
{set the conents of a cell in a StringGrid to the contents of a string}
begin
  theTable.Cells[theCell.x, theCell.y] := theString;
end;


procedure writeTableCells(theTable: TStringGrid; theContents: tResultContent);
{writes a vector of values to the last line of a StringGrid}
var
  j: integer;
  theCell: TableCell;
  theString: Str255;
begin
  theTable.Tag := theTable.Tag + 1; // stores last line
  if theTable.Tag > GridRows - 1 then
    theTable.RowCount := theTable.RowCount + 1;
  theCell.y := theTable.Tag;
  for j := 0 to RES_MAX_COLS - 1 do
  begin
    theString := theContents[j];
    theCell.x := j;
    writeaTableCell(theTable, theCell, theString);
  end;
end;

procedure SaveGridToFile(theTable: TStringGrid; theFileName: string;
  theDelimiter: char; colnames, rowNames: boolean; var ReturnCode: integer);
{saves the contents of a string grid}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theString: string;
  r, c: integer;
  startC: integer;
  theContents: TStringList;
  doc: TDIFDocument;
  theCode: integer;
begin
  if rowNames then
    startC := 0
  else
    startC := 1;
  if theDelimiter = 'd' then
  begin {DIF file handling}
    theCode := 0;
    try
      doc := TDIFDocument.Create;
      doc.SetHead('SimThyr');

      if colNames then
      begin
        doc.NewTuple;
        theString := '';
        for c := startC to theTable.ColCount - 1 do
        begin
          theString := theTable.Cells[c, 0];
          Doc.AppendCell(theString);
        end;
      end;
      for r := 1 to theTable.RowCount - 1 do
      begin
        doc.NewTuple;
        theString := '';
        for c := startC to theTable.ColCount - 1 do
        begin
          theString := theTable.Cells[c, r];
          Doc.AppendCell(theString);
        end;
      end;

      WriteDIFFile(doc, theFileName, theCode);
      if theCode <> 0 then
        ShowSaveError;
    finally
      doc.Free;
      ReturnCode := theCode;
    end;
  end
  else if theDelimiter <> ' ' then {tab delimited and CSV files}
  begin
    if theDelimiter = 't' then
      theDelimiter := kTAB;
    if theDelimiter = 'c' then
      theDelimiter := kSEMICOLON;
    ReturnCode := 0;
    theContents := TStringList.Create;
    theString := '';
    if colNames then
    begin
      for c := startC to theTable.ColCount - 1 do
        theString := theString + theTable.Cells[c, 0] + theDelimiter;
      theContents.Add(theString);
    end;
    for r := 1 to theTable.RowCount - 1 do
    begin
      theString := '';
      for c := startC to theTable.ColCount - 1 do
        theString := theString + theTable.Cells[c, r] + theDelimiter;
      theContents.Add(theString);
    end;
    try
      try
        theContents.SaveToFile(theFileName);
      except
        on Ex: EFCreateError do
        begin
          ShowMessage(SAVE_ERROR_MESSAGE);
          ReturnCode := -2;
        end;
      end;
    finally
      theContents.Free;
    end;
  end
  else
  begin
    ShowSaveError;
    ReturnCode := -1;
  end;
end;

procedure SetStatusBarPanel0(curr, max: string);
{updates progress indicator in status bar}
begin
  SimThyrLogWindow.StatusBar1.Panels[0].Text := '   ' + curr + ':' + max;
end;

procedure writeaMemoLine(theMemo: TMemo; theString: Str255);
{emulates writeln for a memo}
begin
  {$IFDEF Windows}
  theMemo.Lines.Text := theMemo.Lines.Text + kCRLF + theString;
  {$ELSE}
    {$IFDEF DARWIN}
  theMemo.Lines.Text := theMemo.Lines.Text + kRETURN + theString;
    {$ELSE}
  theMemo.Lines.Text := theMemo.Lines.Text + kLF + theString;
    {$ENDIF}
  {$ENDIF}
end;


{$IFDEF LCLCocoa}
{The following two functions were suggested by Hansaplast at https://forum.lazarus.freepascal.org/index.php/topic,43111.msg304366.html}

// Retrieve key's string value from user preferences. Result is encoded using NSStrToStr's default encoding.
function GetPrefString(const KeyName : string) : string;
begin
  Result := NSStringToString(NSUserDefaults.standardUserDefaults.stringForKey(NSStr(@KeyName[1])));
end;
{$ENDIF}

// IsDarkTheme: Detects if the Dark Theme (true) has been enabled or not (false)
function DarkTheme: boolean;
{$IFDEF Windows}
const
  KEYPATH = '\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize';
  KEYNAME = 'AppsUseLightTheme';
  WindowsDarkModeSupported: boolean = false; // may be set to true in future versions
var
  LightKey: boolean;
  Registry: TRegistry;
{$ENDIF}
begin
  Result := false;
  {$IFDEF Windows}
  if WindowsDarkModeSupported then
  begin
    Registry := TRegistry.Create;
    try
      Registry.RootKey := HKEY_CURRENT_USER;
      if Registry.OpenKeyReadOnly(KEYPATH) then
        begin
          if Registry.ValueExists(KEYNAME) then
            LightKey := Registry.ReadBool(KEYNAME)
          else
            LightKey := true;
        end
      else
        LightKey := true;
        Result := not LightKey
    finally
      Registry.Free;
    end;
  end
  else
  Result := false;
  {$ELSE}
  {$IFDEF LCLCocoa}
  if MojaveOrNewer then
    //Result := pos('DARK',UpperCase(GetPrefString('AppleInterfaceStyle'))) > 0
    Result := pos('Dark', CFStringToStr(CFStringRef(NSApp.effectiveAppearance.name))) > 0
  else
    Result := false;
  {$ELSE}
  Result := false;
  {$ENDIF}
  {$ENDIF}
end;


procedure SetFileName(theForm: TForm; const FileName: string);
{sets the title of a window to file name}
begin
  {$IFNDEF LCLCocoa} // temporary solution for a bug in Cocoa, needs evaluation
  theForm.Caption := ExtractFileName(FileName);
  {$ENDIF}
end;

procedure ShowImplementationMessage;
{generic error message}
begin
  bell;
  ShowMessage(IMPLEMENTATION_MESSAGE + '.');
end;

procedure ShowImplementationMessage(theText: string);
{more flexible error message}
begin
  bell;
  ShowMessage(IMPLEMENTATION_MESSAGE + ': ' + theText);
end;

procedure ShowFormatMessage;
{error message}
begin
  bell;
  ShowMessage(FORMAT_MESSAGE);
end;

procedure ShowFileError;
begin
  bell;
  MessageDlg(FILE_FORMAT_ERROR_MESSAGE, mtError, [mbOK], 0);
end;

procedure ShowVersionError;
begin
  bell;
  MessageDlg(FILE_VERSION_MESSAGE, mtError, [mbOK], 0);
end;

procedure ShowURLStatus(theCode: integer);
begin
  bell;
  MessageDlg(URL_STATUS_MESSAGE + IntToStr(theCode), mtError, [mbOK], 0);
end;

procedure ShowSaveError;
begin
  bell;
  MessageDlg(SAVE_ERROR_MESSAGE, mtError, [mbOK], 0);
end;

procedure ShowMemoryError;
begin
  bell;
  MessageDlg(INSUFFICIENT_MEMORY_MESSAGE, mtError, [mbOK], 0);
end;

procedure ShowPrereleaseWarning;
begin
  MessageDlg(DEBUG_VERSION_MESSAGE, mtWarning, [mbOK], 0);
end;

end.
