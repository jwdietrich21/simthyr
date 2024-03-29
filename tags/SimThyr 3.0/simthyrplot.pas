unit SimThyrPlot;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit plots values }

{ Source code released under the BSD License }

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Spin, ComCtrls, ColorBox, Arrow, Buttons, Menus, TAGraph,
  TASources, TATools, TASeries, TATransformations, TAStyles, TALegendPanel,
  DateUtils, SimThyrTypes, SimThyrServices, HandleNotifier;

type

  { TValuesPlot }

  TValuesPlot = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Chart2: TChart;
    ColorButton1: TColorButton;
    ColorListBox1: TColorListBox;
    ColorListBox2: TColorListBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    DateTimeIntervalChartSource1: TDateTimeIntervalChartSource;
    FullScaleButton2: TSpeedButton;
    Divider1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    DeleteItem: TMenuItem;
    PasteItem: TMenuItem;
    UndoItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    TitleEdit: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    PlotPanel2: TPanel;
    PlotPanel1: TPanel;
    FullScaleButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure Button1Click(Sender: TObject);
    procedure Chart1Click(Sender: TObject);
    procedure Chart2Click(Sender: TObject);
    procedure ColorBox1Change(Sender: TObject);
    procedure ColorBox2Change(Sender: TObject);
    procedure ColorButton1Click(Sender: TObject);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure ColorListBox1Click(Sender: TObject);
    procedure ColorListBox2Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateTimeAxes;
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure PlotPanel2Click(Sender: TObject);
    procedure CopyChart(Sender: TObject);
    procedure PrintChart(Sender: TObject);
    procedure FullScaleButton2Click(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure TitleEditChange(Sender: TObject);
  private
    { private declarations }
    FLine1, Fline2: TLineSeries;
  public
    { public declarations }
  end; 

var
  factor, i0, i1: longint;
  graphready: boolean;
  ValuesPlot: TValuesPlot;
  gr_nummer, antwort, antwort_p: string[4];

function AsTime (x: real): TDateTime;
function FormattedTime (x: real): Str255;

procedure DrawPlot(empty: boolean);

implementation

function AsTime (x: real): TDateTime;  {Converts second values to TDateTime representation}
var
  r: longint;
  y, m, d, h, n, s, ms, dy: word;
  theTime, theDate: TDateTime;
begin
 y := 1900;                            {Take 1900 as standard year}
 r := trunc(x);
 dy := word(r div 86400);              {day of year}
 if not TryEncodeDateDay(y, dy+1, theDate) then {error in encoding?}
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

function FormattedTime (x: real): Str255;   {Converts second values to a formatted time}
begin
  FormattedTime := FormatDateTime(gDateTimeFormat, AsTime(x));
end;

{ TValuesPlot }

procedure DrawDummyPlots;
begin
  with ValuesPlot.Fline1 do
  begin
   ShowLines := true;
   ShowPoints := false;
   Pointer.Brush.Color := ValuesPlot.ColorListBox1.Selected;
   SeriesColor := ValuesPlot.ColorListBox1.Selected;
   ValuesPlot.Chart1.AddSeries(ValuesPlot.Fline1);
   AddXY(0, 0, '', SeriesColor);
   AddXY(10, 0, '', SeriesColor);
  end;
  with ValuesPlot.Fline2 do
  begin
   ShowLines := true;
   ShowPoints := false;
   Pointer.Brush.Color := ValuesPlot.ColorListBox2.Selected;
   SeriesColor := ValuesPlot.ColorListBox2.Selected;
   ValuesPlot.Chart2.AddSeries(ValuesPlot.Fline2);
   AddXY(0, 0, '', SeriesColor);
   AddXY(10, 0, '', SeriesColor);
 end;
 ValuesPlot.Caption := 'Chart View';
end;

procedure DrawPlot(empty: boolean);
var
 j: integer;
 theTime: TDateTime;
 theSecond: real;
begin
 if ValuesPlot.Fline1 <> nil then ValuesPlot.Chart1.ClearSeries;
 if ValuesPlot.Fline2 <> nil then ValuesPlot.Chart2.ClearSeries;
 ValuesPlot.Fline1 := TLineSeries.Create(ValuesPlot.Chart1);
 ValuesPlot.Fline2 := TLineSeries.Create(ValuesPlot.Chart2);
 ValuesPlot.Fline1.BeginUpdate;
 ValuesPlot.Fline2.BeginUpdate;
 if empty then DrawDummyPlots else
  begin
   with ValuesPlot.Fline1 do
   begin
    ShowLines := true;
    ShowPoints := false;
    Pointer.Brush.Color := ValuesPlot.ColorListBox1.Selected;
    SeriesColor := ValuesPlot.ColorListBox1.Selected;
    ValuesPlot.Chart1.AddSeries(ValuesPlot.Fline1);
    for j := 0 to length(gResultMatrix) - 1 do
    begin
      theSecond := gResultMatrix[j, t_pos];
      AddXY(theSecond, gResultMatrix[j, ValuesPlot.ComboBox1.ItemIndex + 2] * gParameterFactor[ValuesPlot.ComboBox1.ItemIndex + 2], '', SeriesColor);
    end;
  end;
  with ValuesPlot.Fline2 do
  begin
    ShowLines := true;
    ShowPoints := false;
    Pointer.Brush.Color := ValuesPlot.ColorListBox2.Selected;
    SeriesColor := ValuesPlot.ColorListBox2.Selected;
    ValuesPlot.Chart2.AddSeries(ValuesPlot.Fline2);
    for j := 0 to length(gResultMatrix) - 1 do
    begin
      theSecond := gResultMatrix[j, t_pos];
      AddXY(theSecond, gResultMatrix[j, ValuesPlot.ComboBox2.ItemIndex + 2] * gParameterFactor[ValuesPlot.ComboBox2.ItemIndex + 2], '', SeriesColor);
    end;
  end;
  graphready := true;
  ValuesPlot.Caption := PLOT_TITLE;
 end;
 ValuesPlot.Fline1.EndUpdate;
 ValuesPlot.Fline2.EndUpdate;
end;

procedure TValuesPlot.PlotPanel2Click(Sender: TObject);
begin
  ;
end;

procedure TValuesPlot.ComboBox1Change(Sender: TObject);
begin
 ValuesPlot.Chart1.LeftAxis.Title.Caption := gParameterLabel[ComboBox1.ItemIndex + 2] + ': ' + gParameterUnit[ComboBox1.ItemIndex + 2];
 ColorListBox1.Selected := gDefaultColors[ComboBox1.ItemIndex + 2];
 DrawPlot(not graphready);
end;

procedure TValuesPlot.ColorBox1Change(Sender: TObject);
begin
 DrawPlot(not graphready);
end;

procedure TValuesPlot.Button1Click(Sender: TObject);
begin
  ValuesPlot.Chart1.Title.Text.SetText(PChar(ValuesPlot.TitleEdit.Text));
  ValuesPlot.Chart1.Title.visible := not(ValuesPlot.Chart1.Title.visible);
  ValuesPlot.Chart1.Title.Font.Color := ValuesPlot.ColorButton1.ButtonColor;
  ValuesPlot.Chart2.Title.Text.SetText(PChar(ValuesPlot.TitleEdit.Text));
  ValuesPlot.Chart2.Title.visible := not(ValuesPlot.Chart2.Title.visible);
  ValuesPlot.Chart2.Title.Font.Color := ValuesPlot.ColorButton1.ButtonColor;
  if ValuesPlot.Chart1.Title.visible then
    begin
      ValuesPlot.Button1.Caption := 'Remove';
      ValuesPlot.TitleEdit.Enabled := false;
    end
  else
    begin
      ValuesPlot.Button1.Caption := 'Add';
      ValuesPlot.TitleEdit.Enabled := true;
    end;
end;

procedure TValuesPlot.Chart1Click(Sender: TObject);
begin
  Chart1.SetFocus;
  gSelectedChart := Chart1;
  PlotPanel1.Color := clHighlight;
  PlotPanel2.Color := clWhite;
end;

procedure TValuesPlot.Chart2Click(Sender: TObject);
begin
  Chart2.SetFocus;
  gSelectedChart := Chart2;
  PlotPanel1.Color := clWhite;
  PlotPanel2.Color := clHighlight;
end;

procedure TValuesPlot.ColorBox2Change(Sender: TObject);
begin
 DrawPlot(not graphready);
end;

procedure TValuesPlot.ColorButton1Click(Sender: TObject);
begin
  ValuesPlot.Chart1.Title.Font.Color := ValuesPlot.ColorButton1.ButtonColor;
  ValuesPlot.Chart2.Title.Font.Color := ValuesPlot.ColorButton1.ButtonColor;
end;

procedure TValuesPlot.ColorButton1ColorChanged(Sender: TObject);
begin
  ValuesPlot.Chart1.Title.Font.Color := ValuesPlot.ColorButton1.ButtonColor;
  ValuesPlot.Chart2.Title.Font.Color := ValuesPlot.ColorButton1.ButtonColor;
end;

procedure TValuesPlot.ColorListBox1Click(Sender: TObject);
begin
 DrawPlot(not graphready);
end;

procedure TValuesPlot.ColorListBox2Click(Sender: TObject);
begin
 DrawPlot(not graphready);
end;

procedure TValuesPlot.ComboBox2Change(Sender: TObject);
begin
  ValuesPlot.Chart2.LeftAxis.Title.Caption := gParameterLabel[ComboBox2.ItemIndex + 2] + ': ' + gParameterUnit[ComboBox2.ItemIndex + 2];
  ColorListBox2.Selected := gDefaultColors[ComboBox2.ItemIndex + 2];
  DrawPlot(not graphready);
end;

procedure TValuesPlot.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := 2;
  ComboBox2.ItemIndex := 4;
  ColorListBox1.Selected := clRed;
  ColorListBox2.Selected := clBlue;
  ValuesPlot.Chart1.Title.Visible := false;
  ValuesPlot.Chart2.Title.Visible := false;
  ValuesPlot.Chart1.LeftAxis.Title.Caption := gParameterLabel[ComboBox1.ItemIndex + 2] + ': ' + gParameterUnit[ComboBox1.ItemIndex + 2];
  ValuesPlot.Chart2.LeftAxis.Title.Caption := gParameterLabel[ComboBox2.ItemIndex + 2] + ': ' + gParameterUnit[ComboBox2.ItemIndex + 2];
  DrawPlot(true);
  gSelectedChart := nil;
  gDefaultColors[0] := clBlack;
  gDefaultColors[1] := clBlack;
  gDefaultColors[2] := clTeal;
  gDefaultColors[3] := clPurple;
  gDefaultColors[4] := clRed;
  gDefaultColors[5] := clNavy;
  gDefaultColors[6] := clBlue;
  gDefaultColors[7] := clOlive;
  gDefaultColors[8] := clGreen;
  gDefaultColors[9] := clMaroon;
end;

procedure TValuesPlot.UpdateTimeAxes;
begin
  DateTimeIntervalChartSource1.DateTimeFormat := String(gDateTimeFormat);
end;

procedure TValuesPlot.FormShow(Sender: TObject);
begin
  UpdateTimeAxes;
end;

procedure TValuesPlot.Panel1Click(Sender: TObject);
begin
  gSelectedChart := nil;
  PlotPanel1.Color := clWhite;
  PlotPanel2.Color := clWhite;
end;

procedure TValuesPlot.Panel2Click(Sender: TObject);
begin
  gSelectedChart := nil;
  PlotPanel1.Color := clWhite;
  PlotPanel2.Color := clWhite;
end;

procedure TValuesPlot.CopyChart(Sender: TObject);
begin
  if gSelectedChart = nil then
    bell
  else
    gSelectedChart.CopyToClipboardBitmap;
end;

procedure TValuesPlot.CopyItemClick(Sender: TObject);
begin
  CopyChart(Sender);
end;

procedure TValuesPlot.PrintChart(Sender: TObject);
begin
  if gSelectedChart = nil then
    bell
  else
    bell;
end;

procedure TValuesPlot.FullScaleButton2Click(Sender: TObject);
begin
  ValuesPlot.Chart2.Extent.UseYMax := false;
  ValuesPlot.Chart2.ZoomFull;
end;

procedure TValuesPlot.FullScaleButton1Click(Sender: TObject);
begin
  ValuesPlot.Chart1.Extent.UseYMax := false;
  ValuesPlot.Chart1.ZoomFull;
end;

procedure TValuesPlot.TitleEditChange(Sender: TObject);
begin

end;

initialization
  {$I simthyrplot.lrs}

end.

