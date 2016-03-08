unit SimThyrLog;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2016 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2016 }

{ This unit draws a spreadsheet-like grid with simulation results }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, clipbrd, ComCtrls, ExtCtrls, Menus,
  SimThyrTypes, SimThyrResources, SimThyrServices, DIFSupport;

type

  { TSimThyrLogWindow }

  TSimThyrLogWindow = class(TForm)
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    DeleteItem: TMenuItem;
    Divider1: TMenuItem;
    PasteItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    ProgressBar1: TProgressBar;
    StatusBar1: TStatusBar;
    UndoItem: TMenuItem;
    ValuesGrid: TStringGrid;
    procedure CopyCells;
    procedure CopyItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure InitGrid;
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SaveGrid(theFileName: String; theDelimiter: Char);
    procedure ValuesGridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SimThyrLogWindow: TSimThyrLogWindow;

implementation

uses
  SimThyrMain;

procedure CutorCopyfromGrid(theGrid: TStringGrid; cut: Boolean = False);
{supports cutting or copying cells from the grid}
var
  theSelection: TGridRect;
  r, c: integer;
  textfromSelection: AnsiString;
begin
  theSelection := theGrid.Selection;
  textfromSelection := '';
  for r := theSelection.Top to theSelection.Bottom do
  begin
    for c := theSelection.Left to theSelection.Right do
    begin
      textfromSelection := textfromSelection + theGrid.Cells[c,r];
      if cut then
      begin
        theGrid.Cells[c,r] := '';
      end;
      if c < theSelection.Right then
        textfromSelection := textfromSelection + kTAB;
    end;
    if r < theSelection.Bottom then
      textfromSelection := textfromSelection + kCRLF;
  end;
  ClipBoard.AsText := textfromSelection;
end;

procedure TSimThyrLogWindow.CopyCells;
begin
  CutorCopyfromGrid(valuesGrid, false);
end;

procedure TSimThyrLogWindow.CopyItemClick(Sender: TObject);
begin
  CopyCells;
end;

procedure TSimThyrLogWindow.FormActivate(Sender: TObject);
begin
  if Screen.Width < SimThyrLogWindow.Left + SimThyrLogWindow.Width then
    SimThyrLogWindow.Width := Screen.Width - SimThyrLogWindow.Left - 13;
  SimThyrToolbar.SelectAllMenuItem.Enabled := true;
  gLastActiveCustomForm := SimThyrLogWindow; {stores window as last active form}
end;

procedure TSimThyrLogWindow.InitGrid; {initializes empty table}
begin
  GridRows := RES_BLANK_ROWS;
  ValuesGrid.RowCount := GridRows;
  ValuesGrid.Tag := 0;
end;

procedure TSimThyrLogWindow.PopupMenu1Popup(Sender: TObject);
begin

end;

procedure TSimThyrLogWindow.SaveGrid(theFileName: String; theDelimiter: Char);
{saves the contents of the log window}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theCode: integer;
begin
  SaveGridToFile(SimThyrLogWindow.ValuesGrid, theFileName, theDelimiter, theCode);
  if theCode = 0 then
    SetFileName(SimThyrLogWindow, theFileName);
end;

procedure TSimThyrLogWindow.ValuesGridGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);  { TODO 9 -oJ. W. Dietrich -cbugs : This bug is to be fixed. }
var
  theText: String;
begin
  { Raises a SIGSEGV exception
  theText := 'Simulated behavioural parameters of thyroid homeostasis';
  if ACol > 1 then
    theText := gParameterLabel[ACol - 1];
  HintText := theText;
  }
end;

initialization
  {$I simthyrlog.lrs}

end.

