unit SimThyrServices;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.3.2 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

{ This unit provides some global functions }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, Grids, StdCtrls, Dialogs, Forms, SimThyrTypes,
  SimThyrResources, UnitConverter, DOM, XMLRead, FileUtil
      {$IFDEF win32}
  , Windows, Win32Proc
  {$ELSE}
    {$IFDEF LCLCarbon}
  , MacOSAll
    {$ENDIF}
  , Unix
  {$ENDIF}  ;

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

function OSVersion: Str255;
function YosemiteORNewer: Boolean;
procedure bell;
function EncodeGreek(theString: string): string;
function DecodeGreek(theString: string): string;
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
  //else if WindowsVersion = wv8 then   // for future LCL versions
  //  OSVersion := 'Windows 8 '
  else
    OSVersion := 'Windows ';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

function YosemiteORNewer: Boolean;
{ returns true, if this app runs on Mac OS X 10.10 Yosemite or newer }
  {$IFDEF LCLcarbon}
var
  Major, Minor, Bugfix: SInt32;
  theError: SInt16;
  {$ENDIF}
begin
  result := false;
  {$IFDEF LCLcarbon}
  theError := Gestalt(gestaltSystemVersionMinor, Minor);
  if TheError = 0 then
    if Minor >= 10 then
      result := true;
  {$ENDIF}
end;

procedure bell; {platform-independent implementation of acustical warning}
var s:longint;
begin
  {$IFDEF win32}
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
  Result := StringReplace(theString, #194#181, 'mc', theFlags);
end;

function DecodeGreek(theString: string): string;
{decodes ASCII substitution sequence for greek mu letter}
begin
  result := UTF8Decode(StringReplace(theString, 'mc', PrefixLabel[4], [rfReplaceAll, rfIgnoreCase]));
end;

function NodeContent(theRoot: TDOMNode; Name: string): string;
  {supports XML routines, gets the contents of a node in a file}
var
  theNode: TDOMNode;
  theText: String;
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
  oldSep: char;
  theString: string;
begin
  oldSep := DefaultFormatSettings.DecimalSeparator;
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


end.

