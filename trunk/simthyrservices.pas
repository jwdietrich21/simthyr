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
  Classes, SysUtils, Grids, StdCtrls, Dialogs, Forms, SimThyrTypes
      {$IFDEF win32}
  , Windows
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

procedure GetOSVersion;
procedure bell;
function StandardForm (theString: Str255): extended;
function ExtendedForm (theString: Str255): extended;
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

procedure GetOSVersion;
 var
  osErr: integer;
  response: longint;
begin
 {osErr := Gestalt(gestaltSystemVersion, response);
 if (osErr = noErr) then
  gMacOSVersion := response
 else
  gMacOSVersion := 4;    }
end;

procedure bell;
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


function NumberForm (compos: integer; theString: Str255): extended;
       {Wandelt String in Fließkommazahl um}
 var
  num, wholeS, remainderS: Str255;
  whole, remainder: longint;
begin
 wholeS := copy(theString, 0, compos);
 whole := StrToInt(wholeS);
 remainderS := copy(theString, compos + 1, (length(theString) - compos));
 remainder := StrToInt(remainderS);
 NumberForm := whole + remainder / exp(ln(10) * length(remainderS));
end;

function StandardForm (theString: Str255): extended;
 var
  compos: integer;
  num, wholeS, remainderS: Str255;
  whole, remainder: longint;
begin
 compos := pos(DEC_COMMA, theString);
 if compos > 0 then
  StandardForm := NumberForm(compos, theString)
 else
  begin
   compos := pos(DEC_POINT, theString);
   if compos > 0 then
    StandardForm := NumberForm(compos, theString)
   else
    begin
     whole := StrToInt(theString);
     StandardForm := whole;
    end;
  end;
end;

function ExtendedForm (theString: Str255): extended;
 var
  theNumber: extended;
  formatString: str255;
  smResult, dec_places: integer;
  offset, length: longint;
  Decimal, Thousands: Char;
begin
 theNumber := StrToFloat(theString);
 ExtendedForm := theNumber;
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

