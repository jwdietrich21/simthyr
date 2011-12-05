unit SimThyrServices;

{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit provides some global functions }

{ Source code released under the BSD License }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, Grids, StdCtrls, Dialogs, Forms, SimThyrTypes,
  DOM, XMLRead, XMLWrite
      {$IFDEF win32}
  , Windows, Win32Proc
  {$ELSE}
    {$IFDEF LCLCarbon}
      , MacOSAll
    {$ENDIF}
  {$ENDIF}
  ;

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
procedure bell;
function NodeContent(theRoot: TDOMNode; name: String): String;
procedure VarFromNode(theRoot: TDOMNode; name: String; var theVar: real);
function SimpleNode(Doc: TXMLDocument; name, value: String): TDOMNode;
procedure ClearResultContents (var theContents: tResultContent);
procedure writeaTableCell (theTable: TStringGrid; theCell: TableCell; theString: Str255);
procedure writeTableCells (theTable: TStringGrid; theContents: tResultContent);
procedure SetStatusBarPanel0(curr, max: String);
procedure writeaMemoLine(theMemo: TMemo; theString: Str255);
procedure SetFileName(theForm: TForm; const FileName: String);
procedure ShowImplementationMessage;
procedure ShowFormatMessage;


implementation

uses SimThyrLog;

function OSVersion: Str255;
 var
  osErr: integer;
  response: longint;
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
  if WindowsVersion = wv95 then OSVersion := 'Windows 95 '
   else if WindowsVersion = wvNT4 then OSVersion := 'Windows NT v.4 '
   else if WindowsVersion = wv98 then OSVersion := 'Windows 98 '
   else if WindowsVersion = wvMe then OSVersion := 'Windows ME '
   else if WindowsVersion = wv2000 then OSVersion := 'Windows 2000 '
   else if WindowsVersion = wvXP then OSVersion := 'Windows XP '
   else if WindowsVersion = wvServer2003 then OSVersion := 'Windows Server 2003 '
   else if WindowsVersion = wvVista then OSVersion := 'Windows Vista '
   else if WindowsVersion = wv7 then OSVersion := 'Windows 7 '
   else OSVersion:= 'Windows ';
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

procedure bell; {platform-independent implementation of acustical warning}
begin
  {$IFDEF win32}
    MessageBeep(0);
  {$ELSE}
    {$IFDEF LCLCarbon}
      SysBeep(30);
    {$ELSE}
      beep;
    {$ENDIF}
  {$ENDIF}
end;

function NodeContent(theRoot: TDOMNode; name: String): String; {supports XML routines}
var
  theNode: TDOMNode;
begin
  if assigned(theRoot) then
    theNode := theRoot.FindNode(name);
  if assigned(theNode) then
  begin
    Result := theNode.FirstChild.NodeValue;
  end
  else
    Result := 'NA';
end;

procedure VarFromNode(theRoot: TDOMNode; name: String; var theVar: real); {supports XML routines}
var theString: String;
begin
  theString := NodeContent(theRoot, name);
  if theString <> 'NA' then
    theVar := StrToFloat(theString);
end;

function SimpleNode(Doc: TXMLDocument; name, value: String): TDOMNode; {supports XML routines}
var
  ItemNode,TextNode: TDOMNode;
begin
  ItemNode:=Doc.CreateElement(name);
  TextNode:=Doc.CreateTextNode(value);
  ItemNode.AppendChild(TextNode);
  Result := ItemNode;
end;

procedure ClearResultContents (var theContents: tResultContent);
 var
  counter: integer;
begin
 for counter := 1 to RES_MAX_COLS do
  theContents[counter] := ' ';
end;


procedure writeaTableCell (theTable: TStringGrid; theCell: TableCell; theString: Str255);
begin
 theTable.Cells[theCell.x, theCell.y] := theString;
end;


procedure writeTableCells (theTable: TStringGrid; theContents: tResultContent);
{writes a vector of values to the last line of a StringGrid}
 var
  j, ignored: integer;
  cSize: TPoint;
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

procedure SetStatusBarPanel0(curr, max: String);
begin
  SimThyrLogWindow.StatusBar1.Panels[0].Text := '   ' + curr + ':' + max;
end;

procedure writeaMemoLine(theMemo: TMemo; theString: Str255);
begin
   theMemo.Lines.Text := theMemo.Lines.Text + kCRLF + theString;
end;


procedure SetFileName(theForm: TForm; const FileName: String);
begin
  theForm.Caption := ExtractFileName(FileName);
end;

procedure ShowImplementationMessage;
begin
 bell;
 ShowMessage(IMPLEMENTATION_MESSAGE);
end;

procedure ShowFormatMessage;
begin
 bell;
 ShowMessage(FORMAT_MESSAGE);
end;


end.

