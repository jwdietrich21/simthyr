unit SimThyrServices;

 { SimThyr Project }
 { A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

 { (c) J. W. Dietrich, 1994 - 2016 }
 { (c) Ludwig Maximilian University of Munich 1995 - 2002 }
 { (c) Ruhr University of Bochum 2005 - 2016 }

{ This unit provides some global functions }

 { Source code released under the BSD License }
 { See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, Grids, StdCtrls, Dialogs, Forms, SimThyrTypes,
  SimThyrResources, UnitConverter, DIFSupport, DOM, FileUtil, DateUtils
  {$IFDEF WINDOWS}
  , Windows, Win32Proc
  {$ENDIF}
  {$IFDEF LCLCarbon}
  , MacOSAll
  {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF}  ;

const
  iuSystemScript     = -1;
  iuCurrentScript    = -2;
  iuWordSelectTable  = 0;
  iuWordWrapTable    = 1;
  iuNumberPartsTable = 2;
  iuUnTokenTable     = 3;
  iuWhiteSpaceList   = 4;

type
  tSaveMode = (TimeSeries, Plot);

var
  gSaveMode: tSaveMode;

function OSVersion: Str255;
function YosemiteORNewer: boolean;
function SierraOrNewer: boolean;
function XPORNewer: boolean;
function VistaORNewer: boolean;
function Win8OrNewer: boolean;
procedure bell;
function EncodeGreek(theString: string): string;
function DecodeGreek(theString: string): string;
function XMLDateTime2DateTime(const XMLDateTime: string): TDateTime;
function TryXMLDateTime2DateTime(const S: ShortString; out Value: TDateTime): boolean;
function NodeContent(theRoot: TDOMNode; Name: string): string;
procedure VarFromNode(theRoot: TDOMNode; Name: string; var theVar: real);
function SimpleNode(Doc: TXMLDocument; Name, Value: string): TDOMNode;
procedure ClearResultContents(var theContents: tResultContent);
procedure writeaTableCell(theTable: TStringGrid; theCell: TableCell; theString: Str255);
procedure writeTableCells(theTable: TStringGrid; theContents: tResultContent);
procedure SaveGridToFile(theTable: TStringGrid; theFileName: string;
  theDelimiter: char; var ReturnCode: integer);
function AsTime(x: real): TDateTime;
function FormattedTime(x: real): Str255;
procedure SetStatusBarPanel0(curr, max: string);
procedure writeaMemoLine(theMemo: TMemo; theString: Str255);
procedure SetFileName(theForm: TForm; const FileName: string);
procedure ShowImplementationMessage;
procedure ShowImplementationMessage(theText: String);
procedure ShowFormatMessage;
procedure ShowVersionError;
procedure ShowURLStatus(theCode: integer);
procedure ShowSaveError;
procedure ShowMemoryError;
procedure ShowPrereleaseWarning;

implementation

uses SimThyrLog;

function OSVersion: Str255; {returns the major version of the operating system}
begin
  {$IFDEF LCLcarbon}
  if SierraOrNewer then
    OSVersion := 'macOS 10.'
  else
    OSVersion := 'Mac OS X 10.';
  {$ELSE}
  {$IFDEF Linux}
  OSVersion := 'Linux Kernel ';
  {$ELSE}
  {$IFDEF UNIX}
  OSVersion := 'Unix ';
  {$ELSE}
  {$IFDEF WINDOWS}
  if WindowsVersion = wv95 then
    OSVersion := 'Windows 95 / '
  else if WindowsVersion = wvNT4 then
    OSVersion := 'Windows NT v.4 / '
  else if WindowsVersion = wv98 then
    OSVersion := 'Windows 98 / '
  else if WindowsVersion = wvMe then
    OSVersion := 'Windows ME / '
  else if WindowsVersion = wv2000 then
    OSVersion := 'Windows 2000 / '
  else if WindowsVersion = wvXP then
    OSVersion := 'Windows XP / '
  else if WindowsVersion = wvServer2003 then
    OSVersion := 'Windows Server 2003 / '
  else if WindowsVersion = wvVista then
    OSVersion := 'Windows Vista / '
  else if WindowsVersion = wv7 then
    OSVersion := 'Windows 7 / '
  {$if FPC_FULlVERSION >= 30000}{Free Pascal 3.0 or newer}
  else if WindowsVersion = wv8 then
    OSVersion := 'Windows 8 / '
  else if WindowsVersion = wv8_1 then
    OSVersion := 'Windows 8.1 / '
  else if WindowsVersion = wv10 then
    OSVersion := 'Windows 10 / '
  else if WindowsVersion = wvLater then
    OSVersion := 'Windows '
  {$ENDIF}
  else
    OSVersion := 'Windows ';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function YosemiteORNewer: boolean;
  { returns true, if this app runs on Mac OS X 10.10 Yosemite or newer }
  {$IFDEF LCLcarbon}
var
  Major, Minor, Bugfix: SInt32;
  theError: SInt16;
  {$ENDIF}
begin
  Result   := False;
  {$IFDEF LCLcarbon}
  theError := Gestalt(gestaltSystemVersionMinor, Minor);
  if TheError = 0 then
    if Minor >= 10 then
      Result := True;
  {$ENDIF}
end;

function SierraOrNewer: boolean;
{ returns true, if this app runs on macOS X 10.12 Sierra or newer }
{$IFDEF LCLcarbon}
var
Major, Minor, Bugfix: SInt32;
theError: SInt16;
{$ENDIF}
begin
Result   := False;
{$IFDEF LCLcarbon}
theError := Gestalt(gestaltSystemVersionMinor, Minor);
if TheError = 0 then
  if Minor >= 12 then
    Result := True;
{$ENDIF}
end;

function XPORNewer: boolean;
{ returns true, if this app runs on Windows XP or a newer Windows version }
begin
  Result := false;
  {$IFDEF WINDOWS}
  if (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1) then
    result := true;
  {$ENDIF}
end;

function VistaORNewer: boolean;
{ returns true, if this app runs on Windows Vista or a newer Windows version }
begin
  Result := false;
  {$IFDEF WINDOWS}
  if Win32MajorVersion >= 6 then
    result := true;
  {$ENDIF}
end;

function Win8OrNewer: boolean;
{ returns true, if this app runs on Windows 8 or a newer Windows version }
begin
  Result := false;
  {$IFDEF WINDOWS}
  if (Win32MajorVersion > 6) or (Win32MajorVersion = 6) and (Win32MinorVersion >= 2) then
    result := true;
  {$ENDIF}
end;

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
  s := Shell('echo -ne ''\007''');
  {s := fpSystem('echo -ne "\a"');}
  {s := fpSystem('tput bel');}
  {beep;}
    {$ENDIF}
  {$ENDIF}
end;

function EncodeGreek(theString: string): string;
  {encodes greek mu letter as ASCII substitution sequence}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  Result   := StringReplace(theString, #194#181, 'mc', theFlags);
end;

function DecodeGreek(theString: string): string;
  {decodes ASCII substitution sequence for greek mu letter}
begin
  Result := UTF8Decode(StringReplace(theString, 'mc', PrefixLabel[4],
    [rfReplaceAll, rfIgnoreCase]));
end;

function XMLDateTime2DateTime(const XMLDateTime: string): TDateTime;
  { adapted and expanded from a suggestion by Luiz Americo Pereira Camara }
var
  DateOnly, TimeOnly: string;
  theDate, theTime: TDateTime;
  TPos: integer;
begin
  TPos := Pos('T', XMLDateTime);
  if TPos <> 0 then
  begin
    DateOnly := Copy(XMLDateTime, 1, TPos - 1);
    TimeOnly := Copy(XMLDateTime, TPos + 1, Length(XMLDateTime));
    theDate  := ScanDateTime(LeftStr(ISO_8601_DATE_FORMAT, 10), DateOnly);
    theTime  := ScanDateTime(RightStr(ISO_8601_DATE_FORMAT, 8), TimeOnly);
    Result   := theDate + theTime;
  end
  else
  begin
    DateOnly := XMLDateTime;
    Result   := ScanDateTime(LeftStr(ISO_8601_DATE_FORMAT, 10), DateOnly);
  end;
end;

function TryXMLDateTime2DateTime(const S: ShortString; out Value: TDateTime): boolean;
begin
  Result := True;
  try
    Value := XMLDateTime2DateTime(s);
  except
    on EConvertError do
      Result := False
  end;
end;

function NodeContent(theRoot: TDOMNode; Name: string): string;
  {supports XML routines, gets the contents of a node in a file}
var
  theNode: TDOMNode;
  theText: string;
begin
  if assigned(theRoot) then
  begin
    Result := 'NA';
    theNode := theRoot.FindNode(Name);
    if assigned(theNode) then
    begin
      try
        theText := UTF8Encode(theNode.TextContent);
        if theText <> '' then
          Result := theText;
      except
        Result := 'NA';
      end;
      theNode.Destroy;
    end
    else
      Result := 'NA';
  end;
end;

procedure VarFromNode(theRoot: TDOMNode; Name: string; var theVar: real);
{supports XML routines}
var
  oldSep:    char;
  theString: string;
begin
  oldSep    := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := kPERIOD;
  theString := NodeContent(theRoot, Name);
  if theString <> 'NA' then
    theVar := StrToFloat(theString);
  DefaultFormatSettings.DecimalSeparator := oldSep;
end;

function SimpleNode(Doc: TXMLDocument; Name, Value: string): TDOMNode;
  {supports XML routines, creates an XML node from the contents of a string}
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(Name);
  TextNode := Doc.CreateTextNode(UTF8Decode(Value));
  ItemNode.AppendChild(TextNode);
  Result := ItemNode;
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
  theDelimiter: char; var ReturnCode: integer);
 {saves the contents of a string grid}
 {file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theString: string;
  r, c:    integer;
  theContents: TStringList;
  doc:     TDIFDocument;
  theCode: integer;
begin
  if theDelimiter = 'd' then
  begin {DIF file handling}
    theCode := 0;
    try
      doc := TDIFDocument.Create;
      doc.SetHead('SimThyr');

      doc.NewTuple;
      theString := '';
      for c := 1 to theTable.ColCount - 1 do
      begin
        theString := theTable.Cells[c, 0];
        Doc.AppendCell(theString);
      end;
      for r := 1 to theTable.RowCount - 1 do
      begin
        doc.NewTuple;
        theString := '';
        for c := 1 to theTable.ColCount - 1 do
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
    theString   := '';
    for c := 1 to theTable.ColCount - 1 do
      theString := theString + theTable.Cells[c, 0] + theDelimiter;
    theContents.Add(theString);
    for r := 1 to theTable.RowCount - 1 do
    begin
      theString := '';
      for c := 1 to theTable.ColCount - 1 do
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

function AsTime(x: real): TDateTime;
  {Converts second values to TDateTime representation}
var
  r: longint;
  y, m, d, h, n, s, ms, dy: word;
  theTime, theDate: TDateTime;
begin
  y := 1900;                            {Take 1900 as standard year}
  r := trunc(x);
  dy := word(r div 86400);              {day of year}
  if not TryEncodeDateDay(y, dy + 1, theDate) then {error in encoding?}
  begin
    theDate := 0;
    bell;
  end;
  DecodeDateTime(theDate, y, m, d, h, n, s, ms);
  r := r mod 86400;
  h := word(r div 3600);
  r := r mod 3600;
  n := word(r div 60);
  r := r mod 60;
  s := word(r);
  if not TryEncodeDateTime(y, m, d, h, n, s, 0, theTime) then {error in encoding?}
  begin
    theTime := 0;
    bell;
  end;
  AsTime := theTime;
end;

function FormattedTime(x: real): Str255;   {Converts second values to a formatted time}
begin
  FormattedTime := FormatDateTime(gDateTimeFormat, AsTime(x));
end;

procedure SetStatusBarPanel0(curr, max: string);
{updates progress indicator in status bar}
begin
  SimThyrLogWindow.StatusBar1.Panels[0].Text := '   ' + curr + ':' + max;
end;

procedure writeaMemoLine(theMemo: TMemo; theString: Str255);
{emulates writeln for a memo}
begin
  {$IFDEF win32}
  theMemo.Lines.Text := theMemo.Lines.Text + kCRLF + theString;
  {$ELSE}
    {$IFDEF LCLCarbon}
  theMemo.Lines.Text := theMemo.Lines.Text + kRETURN + theString;
    {$ELSE}
  theMemo.Lines.Text := theMemo.Lines.Text + kLF + theString;
    {$ENDIF}
  {$ENDIF}

end;

procedure SetFileName(theForm: TForm; const FileName: string);
{sets the title of a window to file name}
begin
  theForm.Caption := ExtractFileName(FileName);
end;

procedure ShowImplementationMessage;
{generic error message}
begin
  bell;
  ShowMessage(IMPLEMENTATION_MESSAGE + '.');
end;

procedure ShowImplementationMessage(theText: String);
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
