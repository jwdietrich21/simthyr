unit SimThyrLog;

{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit draws a spreadsheet-like grid with simulation results }

{ Source code released under the BSD License }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, clipbrd, ComCtrls, ExtCtrls, Menus,
  SimThyrTypes, SimThyrServices, DIFSupport;

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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SimThyrLogWindow: TSimThyrLogWindow;

implementation

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
  gLastActiveCustomForm := SimThyrLogWindow; {stores window as last active form}
end;

procedure TSimThyrLogWindow.InitGrid;
begin
  ValuesGrid.RowCount := 1;
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
  theString: String;
  r, c: integer;
  theContents: TStringList;
  doc: TDIFDocument;
begin
  if theDelimiter = 'd' then
  begin {DIF file handling}
    SetFileName(SimThyrLogWindow, theFileName);
    try
      doc := TDIFDocument.Create;
      doc.SetHead('SimThyr');

      doc.NewTuple;
      theString := '';
      for c := 1 to SimThyrLogWindow.ValuesGrid.ColCount - 1 do
      begin
        theString := SimThyrLogWindow.ValuesGrid.Cells[c, 0];
        Doc.AppendCell(theString);
      end;
      for r := 1 to SimThyrLogWindow.ValuesGrid.RowCount - 1 do
        begin
          doc.NewTuple;
          theString := '';
          for c := 1 to SimThyrLogWindow.ValuesGrid.ColCount - 1 do
            begin
              theString := SimThyrLogWindow.ValuesGrid.Cells[c, r];
              Doc.AppendCell(theString);
            end;
        end;

      WriteDIFFile(doc, theFileName);
    finally
      doc.Free;
    end;
  end
  else if theDelimiter <> ' ' then {tab delimited and CSV files}
  begin
    theContents := TStringList.Create;
    SetFileName(SimThyrLogWindow, theFileName);
    theString := '';
    for c := 1 to SimThyrLogWindow.ValuesGrid.ColCount - 1 do
      theString := theString + SimThyrLogWindow.ValuesGrid.Cells[c, 0] + theDelimiter;
    theContents.Add(theString);
    for r := 1 to SimThyrLogWindow.ValuesGrid.RowCount - 1 do
    begin
      theString := '';
      for c := 1 to SimThyrLogWindow.ValuesGrid.ColCount - 1 do
        theString := theString + SimThyrLogWindow.ValuesGrid.Cells[c, r] + theDelimiter;
      theContents.Add(theString);
    end;
    try
      try
        theContents.SaveToFile(theFileName);
      except
        on Ex: EFCreateError do
        ShowMessage(SAVE_ERROR_MESSAGE);
      end;
    finally
    theContents.Free;
    end;
  end
  else
    bell;
end;

initialization
  {$I simthyrlog.lrs}

end.
