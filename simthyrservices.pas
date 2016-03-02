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
  SimThyrResources, UnitConverter, DOM, XMLRead, FileUtil, DateUtils
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
procedure SetStatusBarPanel0(curr, max: string);
procedure writeaMemoLine(theMemo: TMemo; theString: Str255);
procedure SetFileName(theForm: TForm; const FileName: string);
procedure ShowImplementationMessage;
procedure ShowFormatMessage;
procedure ShowVersionError;
procedure ShowSaveError;
procedure ShowMemoryError;

implementation

uses SimThyrLog;

function OSVersion: Str255; {returns the major version of the operating system}
begin
  {$IFDEF LCLcarbon}
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
    OSVersion := 'Windows 95 '
  else if WindowsVersion = wvNT4 then
    OSVersion := 'Windows NT v.4 '
  else if WindowsVersion = wv98 then
    OSVersion := 'Windows 98 '
  else if WindowsVersion = wvMe then
    OSVersion := 'Windows ME '
  else if WindowsVersion = wv2000 then
    OSVersion := 'Windows 2000 '
  else if WindowsVersion = wvXP then
    OSVersion := 'Windows XP '
  else if WindowsVersion = wvServer2003 then
    OSVersion := 'Windows Server 2003 '
  else if WindowsVersion = wvVista then
    OSVersion := 'Windows Vista '
  else if WindowsVersion = wv7 then
    OSVersion := 'Windows 7 '
  {$if FPC_FULlVERSION >= 30000} {Free Pascal 3.0 or newer}
  else if WindowsVersion = wv8 then
    OSVersion := 'Windows 8 '
  else if WindowsVersion = wv8_1 then
    OSVersion := 'Windows 8.1 '
  else if WindowsVersion = wv10 then
    OSVersion := 'Windows '
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
var
  DateOnly: string;
  TPos:     integer;
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
    theNode := theRoot.FindNode(Name);
    if assigned(theNode) then
    begin
      try
        theText := theNode.TextContent;
        if theText <> '' then
          Result := UTF8Encode(theText);
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
  theTable.Tag := theTable.Tag + 1;
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
{error message}
begin
  bell;
  ShowMessage(IMPLEMENTATION_MESSAGE);
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
  MessageDlg(VERSION_MESSAGE, mtError, [mbOK], 0);
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

end.
