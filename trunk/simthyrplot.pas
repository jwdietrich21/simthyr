unit SimThyrPlot;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.2.5 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit plots values }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ColorBox, Buttons, Menus, TAGraph, TATools,
  TASeries, TATransformations, DateUtils, Math, SimThyrTypes, SimThyrServices,
  HandleNotifier, Clipbrd, TAIntervalSources, TADrawerSVG, TADrawUtils,
  TADrawerCanvas, TAStyles, TANavigation;

type

  { TValuesPlot }

  TValuesPlot = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    Chart2: TChart;
    ChartNavPanel1: TChartNavPanel;
    ChartNavPanel2: TChartNavPanel;
    ColorButton1: TColorButton;
    ColorListBox1: TColorListBox;
    ColorListBox2: TColorListBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    TimeAxisSource: TDateTimeIntervalChartSource;
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
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateTimeAxes;
    procedure FormShow(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure PlotPanel2Click(Sender: TObject);
    procedure CopyChart;
    procedure SaveChart;
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
  graphready, append: boolean;
  ValuesPlot: TValuesPlot;
  gr_nummer, antwort, antwort_p: string[4];

function AsTime(x: real): TDateTime;
function FormattedTime(x: real): Str255;

procedure DrawPlot(empty: boolean);

implementation

uses
  SimThyrMain;

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

{ TValuesPlot }

procedure DrawDummyPlots;
  {Draws empty plots that are displayed before simulation run}
begin
  with ValuesPlot.Fline1 do
  begin
    ShowLines := True;
    ShowPoints := False;
    Pointer.Brush.Color := ValuesPlot.ColorListBox1.Selected;
    SeriesColor := ValuesPlot.ColorListBox1.Selected;
    ValuesPlot.Chart1.AddSeries(ValuesPlot.Fline1);
    AddXY(0, 0, '', SeriesColor);
    AddXY(10, 0, '', SeriesColor);
  end;
  with ValuesPlot.Fline2 do
  begin
    ShowLines := True;
    ShowPoints := False;
    Pointer.Brush.Color := ValuesPlot.ColorListBox2.Selected;
    SeriesColor := ValuesPlot.ColorListBox2.Selected;
    ValuesPlot.Chart2.AddSeries(ValuesPlot.Fline2);
    AddXY(0, 0, '', SeriesColor);
    AddXY(10, 0, '', SeriesColor);
  end;
  ValuesPlot.Caption := 'Chart View';
  append := false;
end;

procedure DrawPlot(empty: boolean);
  {Draws plots from simulated values in gResultMatrix}
var
  j: integer;
  theSecond: real;
begin
  if (empty or not append) then begin
    if ValuesPlot.Fline1 <> nil then
      ValuesPlot.Chart1.Series.Clear;
    if ValuesPlot.Fline2 <> nil then
      ValuesPlot.Chart2.Series.Clear;
    ValuesPlot.Fline1 := TLineSeries.Create(ValuesPlot.Chart1);
    ValuesPlot.Fline2 := TLineSeries.Create(ValuesPlot.Chart2);
  end;
  if (ValuesPlot.FLine1 = nil) or (ValuesPlot.Fline2 = nil) then
  begin
    bell;
    exit;
  end;
  ValuesPlot.Fline1.BeginUpdate;
  ValuesPlot.Fline2.BeginUpdate;
  if empty then
    DrawDummyPlots
  else
  begin
    with ValuesPlot.Fline1 do
    begin
      ShowLines := True;
      ShowPoints := False;
      Pointer.Brush.Color := ValuesPlot.ColorListBox1.Selected;
      SeriesColor := ValuesPlot.ColorListBox1.Selected;
      ValuesPlot.Chart1.AddSeries(ValuesPlot.Fline1);
      for j := nmax_old to length(gResultMatrix) - 1 do
      begin
        if not isNaN(gResultMatrix[j, ValuesPlot.ComboBox1.ItemIndex + 2]) then
        begin
          theSecond := gResultMatrix[j, t_pos];
          AddXY(theSecond, gResultMatrix[j, ValuesPlot.ComboBox1.ItemIndex + 2] *
            gParameterFactor[ValuesPlot.ComboBox1.ItemIndex + 2], '', SeriesColor);
        end;
      end;
    end;
    with ValuesPlot.Fline2 do
    begin
      ShowLines := True;
      ShowPoints := False;
      Pointer.Brush.Color := ValuesPlot.ColorListBox2.Selected;
      SeriesColor := ValuesPlot.ColorListBox2.Selected;
      ValuesPlot.Chart2.AddSeries(ValuesPlot.Fline2);
      for j := nmax_old to length(gResultMatrix) - 1 do
      begin
        if not isNaN(gResultMatrix[j, ValuesPlot.ComboBox2.ItemIndex + 2]) then
        begin
          theSecond := gResultMatrix[j, t_pos];
          AddXY(theSecond, gResultMatrix[j, ValuesPlot.ComboBox2.ItemIndex + 2] *
            gParameterFactor[ValuesPlot.ComboBox2.ItemIndex + 2], '', SeriesColor);
        end;
      end;
    end;
    graphready := True;
    ValuesPlot.Caption := PLOT_TITLE;
    append := true;
  end;
  ValuesPlot.Fline1.EndUpdate;
  ValuesPlot.Fline2.EndUpdate;
  ValuesPlot.Chart1.Invalidate;  {forces redrawing in some operating systems}
  ValuesPlot.Chart2.Invalidate;
end;

procedure TValuesPlot.PlotPanel2Click(Sender: TObject);
begin
  ;
end;

procedure TValuesPlot.ComboBox1Change(Sender: TObject);
begin
  ValuesPlot.Chart1.LeftAxis.Title.Caption :=
    gParameterLabel[ComboBox1.ItemIndex + 2] + ': ' +
    gParameterUnit[ComboBox1.ItemIndex + 2];
  ColorListBox1.Selected := gDefaultColors[ComboBox1.ItemIndex + 2];
  DrawDummyPlots;
  nmax_old := 0;
  DrawPlot(not graphready);
end;

procedure TValuesPlot.ColorBox1Change(Sender: TObject);
begin
  DrawPlot(not graphready);
end;

procedure TValuesPlot.Button1Click(Sender: TObject);
begin
  ValuesPlot.Chart1.Title.Text.SetText(PChar(ValuesPlot.TitleEdit.Text));
  ValuesPlot.Chart1.Title.Visible := not (ValuesPlot.Chart1.Title.Visible);
  ValuesPlot.Chart1.Title.Font.Color := ValuesPlot.ColorButton1.ButtonColor;
  ValuesPlot.Chart2.Title.Text.SetText(PChar(ValuesPlot.TitleEdit.Text));
  ValuesPlot.Chart2.Title.Visible := not (ValuesPlot.Chart2.Title.Visible);
  ValuesPlot.Chart2.Title.Font.Color := ValuesPlot.ColorButton1.ButtonColor;
  if ValuesPlot.Chart1.Title.Visible then
  begin
    ValuesPlot.Button1.Caption := 'Remove';
    ValuesPlot.TitleEdit.Enabled := False;
  end
  else
  begin
    ValuesPlot.Button1.Caption := 'Add';
    ValuesPlot.TitleEdit.Enabled := True;
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
  DrawDummyPlots;
  nmax_old := 0;
  DrawPlot(not graphready);
end;

procedure TValuesPlot.ColorListBox2Click(Sender: TObject);
begin
  DrawDummyPlots;
  nmax_old := 0;
  DrawPlot(not graphready);
end;

procedure TValuesPlot.ComboBox2Change(Sender: TObject);
begin
  ValuesPlot.Chart2.LeftAxis.Title.Caption :=
    gParameterLabel[ComboBox2.ItemIndex + 2] + ': ' +
    gParameterUnit[ComboBox2.ItemIndex + 2];
  ColorListBox2.Selected := gDefaultColors[ComboBox2.ItemIndex + 2];
  DrawDummyPlots;
  nmax_old := 0;
  DrawPlot(not graphready);
end;

procedure TValuesPlot.FormCreate(Sender: TObject);
{sets default values}
begin
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
  ComboBox1.ItemIndex := 2;
  ComboBox2.ItemIndex := 4;
  ColorListBox1.Selected := gDefaultColors[4];
  ColorListBox2.Selected := gDefaultColors[6];
  ValuesPlot.Chart1.Title.Visible := False;
  ValuesPlot.Chart2.Title.Visible := False;
  ValuesPlot.Chart1.LeftAxis.Title.Caption :=
    gParameterLabel[ComboBox1.ItemIndex + 2] + ': ' +
    gParameterUnit[ComboBox1.ItemIndex + 2];
  ValuesPlot.Chart2.LeftAxis.Title.Caption :=
    gParameterLabel[ComboBox2.ItemIndex + 2] + ': ' +
    gParameterUnit[ComboBox2.ItemIndex + 2];
  DrawPlot(True);
  gSelectedChart := nil;
  PlotPanel1.Color := clWhite;
  PlotPanel2.Color := clWhite;
  append := false;
end;

procedure TValuesPlot.UpdateTimeAxes;
begin
  TimeAxisSource.DateTimeFormat := string(gDateTimeFormat);
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

procedure TValuesPlot.CopyChart;
var
  {$IFDEF UNIX} {selects optimal type of clipboard graphic for respective OS}
  theImage: TPortableNetworkGraphic;
  {$ELSE}
  theImage: TBitMap;
  {$ENDIF}
  theWidth, theHeight: integer;
begin
  if gSelectedChart = nil then
    bell
  else
  begin
    {gSelectedChart.CopyToClipboardBitmap doesn't work on Mac OS X}
    {$IFDEF UNIX}
    theImage := TPortableNetworkGraphic.Create;
    try
      theWidth := gSelectedChart.Width;
      theHeight := gSelectedChart.Height;
      theImage.Width := theWidth;
      theImage.Height := theHeight;
      gSelectedChart.DrawOnCanvas(rect(0, 0, theImage.Width, theImage.Height),
        theImage.canvas);
      Clipboard.Assign(theImage);
    finally
      theImage.Free;
    end;
    {$ELSE}
    gSelectedChart.CopyToClipboardBitmap;
    {$ENDIF}
  end;
end;

procedure TValuesPlot.SaveChart;
var
  theFileName:  string;
  theFilterIndex: integer;
  theStream: TFileStream;
  theDrawer: IChartDrawer;
begin
  if gSelectedChart = nil then
    bell
  else
  begin
    theStream := nil;
    SimThyrToolbar.SavePictureDialog1.FilterIndex := 2;
    if SimThyrToolbar.SavePictureDialog1.Execute then
      try
        theFileName    := SimThyrToolbar.SavePictureDialog1.FileName;
        theFilterIndex := SimThyrToolbar.SavePictureDialog1.FilterIndex;
         {$IFDEF LCLcarbon}{compensates for a bug in the carbon widgetset}
        theFilterIndex := theFilterIndex + 1;
         {$ENDIF}{may be removed in future versions}
        case theFilterIndex of
        2: gSelectedChart.SaveToBitmapFile(theFileName);
        3: gSelectedChart.SaveToFile(TPixmap, theFileName);
        4: gSelectedChart.SaveToFile(TPortableNetworkGraphic, theFileName);
        5: gSelectedChart.SaveToFile(TPortableAnyMapGraphic, theFileName);
        6: gSelectedChart.SaveToFile(TJPEGImage, theFileName);
        7: gSelectedChart.SaveToFile(TTIFFImage, theFileName);
        8: begin
             theStream := TFileStream.Create(theFileName, fmCreate);
             theDrawer := TSVGDrawer.Create(theStream, true);
             theDrawer.DoChartColorToFPColor := @ChartColorSysToFPColor;
             with gSelectedChart do
               Draw(theDrawer, Rect(0, 0, Width, Height));
           end;
        otherwise bell;
        end;
      finally
        if theStream <> nil then theStream.Free;
      end;
  end;
end;

procedure TValuesPlot.CopyItemClick(Sender: TObject);
{copy chart to clipboard}
begin
  CopyChart;
end;

procedure TValuesPlot.FormActivate(Sender: TObject);
begin
  {Compensation for very small screens:}
  if Screen.Width < ValuesPlot.Left + ValuesPlot.Width then
    ValuesPlot.Width := Screen.Width - ValuesPlot.Left - 13;
  if ValuesPlot.Top < 0 then ValuesPlot.Top := 26;
  if ValuesPlot.Left < 0 then ValuesPlot.Left := 13;
  SimThyrToolbar.SelectAllMenuItem.Enabled := false;
  gLastActiveCustomForm := ValuesPlot;
end;

procedure TValuesPlot.PrintChart(Sender: TObject);
begin
  if gSelectedChart = nil then
    bell
  else
    bell;  {this function is not yet implemented}
end;

procedure TValuesPlot.FullScaleButton2Click(Sender: TObject);
begin
  ValuesPlot.Chart2.Extent.UseYMax := False;
  ValuesPlot.Chart2.ZoomFull;
end;

procedure TValuesPlot.FullScaleButton1Click(Sender: TObject);
begin
  ValuesPlot.Chart1.Extent.UseYMax := False;
  ValuesPlot.Chart1.ZoomFull;
end;

procedure TValuesPlot.TitleEditChange(Sender: TObject);
begin

end;

initialization
  {$I simthyrplot.lrs}

end.

