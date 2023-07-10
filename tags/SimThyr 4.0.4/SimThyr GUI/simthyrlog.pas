unit SimThyrLog;

 { SimThyr Project }
 { A numerical simulator of thyrotropic feedback control }

{ Version 4.0.4 (Merlion) }

 { (c) J. W. Dietrich, 1994 - 2021 }
 { (c) Ludwig Maximilian University of Munich 1995 - 2002 }
 { (c) Ruhr University of Bochum 2005 - 2021 }

{ This unit draws a spreadsheet-like grid with simulation results }

 { Source code released under the BSD License }
 { See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, clipbrd, ComCtrls, ExtCtrls, Menus,
  SimThyrTypes, SimThyrBaseServices, SimThyrServices, DIFSupport;

type

  { TSimThyrLogWindow }

  TSimThyrLogWindow = class(TForm)
    CopyItem:     TMenuItem;
    CutItem:      TMenuItem;
    DeleteItem:   TMenuItem;
    Divider1:     TMenuItem;
    PasteItem:    TMenuItem;
    PopupMenu1:   TPopupMenu;
    ProgressBar1: TProgressBar;
    StatusBar1:   TStatusBar;
    UndoItem:     TMenuItem;
    ValuesGrid:   TStringGrid;
    procedure CopyCells;
    procedure CopyItemClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RedrawGrid(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure InitGrid;
    procedure PopupMenu1Popup(Sender: TObject);
    procedure SaveGrid(theFileName: string; theDelimiter: char);
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

procedure CutorCopyfromGrid(theGrid: TStringGrid; cut: boolean = False);
{supports cutting or copying cells from the grid}
var
  theSelection: TGridRect;
  r, c: integer;
  textfromSelection: ansistring;
begin
  theSelection      := theGrid.Selection;
  textfromSelection := '';
  for r := theSelection.Top to theSelection.Bottom do
  begin
    for c := theSelection.Left to theSelection.Right do
    begin
      textfromSelection := textfromSelection + theGrid.Cells[c, r];
      if cut then
      begin
        theGrid.Cells[c, r] := '';
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
  CutorCopyfromGrid(valuesGrid, False);
end;

procedure TSimThyrLogWindow.CopyItemClick(Sender: TObject);
begin
  CopyCells;
end;

procedure TSimThyrLogWindow.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := BACKCOLOUR;
    ValuesGrid.Color := BACKCOLOUR;
  end
  else
  begin
    Color := clWhite;
    ValuesGrid.Color := clWhite;
  end
end;

procedure TSimThyrLogWindow.FormShow(Sender: TObject);
begin
  FormPaint(Sender);
end;

procedure TSimThyrLogWindow.RedrawGrid(Sender: TObject);
{ (Re)draws grid after activating the form and after change of UOMs }
var
  i, j, k: integer;
begin
  for i := t_pos to cT3_pos do
  begin
    ValuesGrid.Cells[i, 1] := gParameterUnit[i];
  end;
  for j := 1 to length(gResultMatrix) do
  begin
    ValuesGrid.Cells[t_pos, j + 1] := FormattedTime(gResultMatrix[j - 1, t_pos]);
    for k := TRH_pos to cT3_pos do
    begin
      ValuesGrid.Cells[k, j + 1] :=
        FormatFloat(gNumberFormat, gResultMatrix[j - 1, k] * gParameterFactor[k]);
    end;
  end;
end;

procedure TSimThyrLogWindow.FormActivate(Sender: TObject);
begin
  if Screen.Width < SimThyrLogWindow.Left + SimThyrLogWindow.Width then
    SimThyrLogWindow.Width := Screen.Width - SimThyrLogWindow.Left - 13;
  SimThyrToolbar.SelectAllMenuItem.Enabled := True;
  RedrawGrid(Sender);
  gLastActiveCustomForm := SimThyrLogWindow; {stores window as last active form}
end;

procedure TSimThyrLogWindow.InitGrid; {initializes empty table}
begin
  GridRows := RES_BLANK_ROWS;
  ValuesGrid.RowCount := GridRows;
  ValuesGrid.Tag := 1;
end;

procedure TSimThyrLogWindow.PopupMenu1Popup(Sender: TObject);
begin

end;

procedure TSimThyrLogWindow.SaveGrid(theFileName: string; theDelimiter: char);
 {saves the contents of the log window}
 {file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theCode: integer;
begin
  theCode := 0;
  SaveGridToFile(SimThyrLogWindow.ValuesGrid, theFileName, theDelimiter, true, true, theCode);
  if theCode = 0 then
    SetFileName(SimThyrLogWindow, theFileName)
  else
    ShowSaveError;
end;

initialization
  {$I simthyrlog.lrs}

end.
