unit SimThyrPlot;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.6 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ This unit plots simulated results in time domain }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ColorBox, Buttons, Menus, TAGraph, TATools,
  TASeries, TATransformations, DateUtils, Math, Clipbrd, TAIntervalSources,
  TADrawerSVG, TADrawUtils, TADrawerCanvas, TAStyles, TANavigation, LCLVersion,
  Types, StrUtils,
  SimThyrTypes, SimThyrResources, SimThyrBaseServices, SimThyrGUIServices,
  HandleNotifier, PlotOptions;

type

  { TValuesPlot }

  TValuesPlot = class(TForm)
    OptionsSpeedButton1: TSpeedButton;
    OptionsSpeedButton2: TSpeedButton;
    TSImageList: TImageList;
    Chart1: TChart;
    Chart2: TChart;
    ChartNavPanel1: TChartNavPanel;
    ChartNavPanel2: TChartNavPanel;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    ChartToolset2: TChartToolset;
    ChartToolset2DataPointClickTool1: TDataPointClickTool;
    ChartToolset2PanMouseWheelTool1: TPanMouseWheelTool;
    ChartToolset2ZoomDragTool1: TZoomDragTool;
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
    Panel1: TPanel;
    Panel2: TPanel;
    PlotPanel2: TPanel;
    PlotPanel1: TPanel;
    FullScaleButton1: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure Chart1Click(Sender: TObject);
    procedure Chart2Click(Sender: TObject);
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure ChartToolset2DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
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
    procedure FormPaint(Sender: TObject);
    procedure OptionsSpeedButton1Click(Sender: TObject);
    procedure OptionsSpeedButton2Click(Sender: TObject);
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
    PlotOptions: TPlotOptions;
  end;

var
  factor, i0, i1: longint;
  graphready, append: boolean;
  ValuesPlot: TValuesPlot;
  gr_nummer: string[4];

procedure DrawPlot(empty: boolean);

implementation

uses
  SimThyrMain;

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
  theTime: TDateTime;
  theYear, theMonth: Word;
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
    ShowMemoryError;
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
          theTime := AsTime(gResultMatrix[j, t_pos]);
          AddXY(theTime, gResultMatrix[j, ValuesPlot.ComboBox1.ItemIndex + 2] *
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
          theTime := AsTime(gResultMatrix[j, t_pos]);
          AddXY(theTime, gResultMatrix[j, ValuesPlot.ComboBox2.ItemIndex + 2] *
            gParameterFactor[ValuesPlot.ComboBox2.ItemIndex + 2], '', SeriesColor);
        end;
      end;
    end;
    theYear := YearOf(theTime);
    theMonth := MonthOf(theTime);
    if theYear > 1900 then
      ValuesPlot.TimeAxisSource.DateTimeFormat := '"y"y "m"m "d"D hh:nn:ss'
    else if theMonth > 1 then
      ValuesPlot.TimeAxisSource.DateTimeFormat := '"m"m "d"D hh:nn:ss'
    else
      ValuesPlot.TimeAxisSource.DateTimeFormat := '"d"D hh:nn:ss';
    graphready := True;
    {$IFNDEF LCLCocoa} // temporary solution for a bug in Cocoa, needs evaluation
    ValuesPlot.Caption := PLOT_TITLE;
    {$ENDIF}
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
    gParameterLabel[ComboBox1.ItemIndex + 2] + ' (' +
    gParameterUnit[ComboBox1.ItemIndex + 2] + ')';
  ColorListBox1.Selected := gDefaultColors[ComboBox1.ItemIndex + 2];
  DrawDummyPlots;
  nmax_old := 0;
  DrawPlot(not graphready);
end;

procedure TValuesPlot.ColorBox1Change(Sender: TObject);
begin
  DrawPlot(not graphready);
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

procedure TValuesPlot.ChartToolset1DataPointClickTool1PointClick(
  ATool: TChartTool; APoint: TPoint);
var
  theTitle, theUnit: String;
  x, y: Double;
begin
  theTitle := ExtractDelimited(1, Chart1.LeftAxis.Title.Caption, ['(']);
  theUnit := ExtractDelimited(2, Chart1.LeftAxis.Title.Caption, ['(']);
  with ATool as TDatapointClickTool do
    if (Series is TLineSeries) then
      with TLineSeries(Series) do begin
        x := GetXValue(PointIndex);
        y := GetYValue(PointIndex);
        Statusbar1.SimpleText := Format('%s = %f%s', [theTitle, y, theUnit]);
      end
    else
      Statusbar1.SimpleText := '';
end;

procedure TValuesPlot.ChartToolset2DataPointClickTool1PointClick(
  ATool: TChartTool; APoint: TPoint);
var
  theTitle, theUnit: String;
  x, y: Double;
begin
  theTitle := ExtractDelimited(1, Chart2.LeftAxis.Title.Caption, ['(']);
  theUnit := ExtractDelimited(2, Chart2.LeftAxis.Title.Caption, ['(']);
  with ATool as TDatapointClickTool do
    if (Series is TLineSeries) then
      with TLineSeries(Series) do begin
        x := GetXValue(PointIndex);
        y := GetYValue(PointIndex);
        Statusbar1.SimpleText := Format('%s = %f%s', [theTitle, y, theUnit]);
      end
    else
      Statusbar1.SimpleText := '';
end;

procedure TValuesPlot.ColorBox2Change(Sender: TObject);
begin
  DrawPlot(not graphready);
end;

procedure TValuesPlot.ColorButton1Click(Sender: TObject);
begin
  ValuesPlot.Chart1.Title.Font.Color := PlotOptions.titleColor;
  ValuesPlot.Chart2.Title.Font.Color := PlotOptions.titleColor;
end;

procedure TValuesPlot.ColorButton1ColorChanged(Sender: TObject);
begin
  ValuesPlot.Chart1.Title.Font.Color := PlotOptions.titleColor;
  ValuesPlot.Chart2.Title.Font.Color := PlotOptions.titleColor;
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
    gParameterLabel[ComboBox2.ItemIndex + 2] + ' (' +
    gParameterUnit[ComboBox2.ItemIndex + 2] + ')';
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
  Chart1.Title.Visible := False;
  Chart2.Title.Visible := False;
  Chart1.LeftAxis.Title.Caption :=
    gParameterLabel[ComboBox1.ItemIndex + 2] + ' (' +
    gParameterUnit[ComboBox1.ItemIndex + 2] + ')';
  Chart2.LeftAxis.Title.Caption :=
    gParameterLabel[ComboBox2.ItemIndex + 2] + ' (' +
    gParameterUnit[ComboBox2.ItemIndex + 2] + ')';
  DrawPlot(True);
  gSelectedChart := nil;
  PlotPanel1.Color := clWhite;
  PlotPanel2.Color := clWhite;
  append := false;
end;

procedure TValuesPlot.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := BACKCOLOUR;
    Chart1.Color := BACKCOLOUR;
    Chart1.BackColor := BACKCOLOUR;
    Chart2.Color := BACKCOLOUR;
    Chart2.BackColor := BACKCOLOUR;
    Chart1.AxisList[0].TickColor := clWhite;
    Chart1.AxisList[1].TickColor := clWhite;
    Chart2.AxisList[0].TickColor := clWhite;
    Chart2.AxisList[1].TickColor := clWhite;
    Chart1.Frame.Color := clMedGray;
    Chart2.Frame.Color := clMedGray;
    PlotPanel1.Color := BACKCOLOUR;
    PlotPanel2.Color := BACKCOLOUR;
  end
  else
  begin
    Color := clWhite;
    Chart1.Color := clWhite;
    Chart1.BackColor := clWhite;
    Chart2.Color := clWhite;
    Chart2.BackColor := clWhite;
    Chart1.AxisList[0].TickColor := clBlack;
    Chart1.AxisList[1].TickColor := clBlack;
    Chart2.AxisList[0].TickColor := clBlack;
    Chart2.AxisList[1].TickColor := clBlack;
    Chart1.Frame.Color := clBlack;
    Chart2.Frame.Color := clBlack;
    PlotPanel1.Color := clWhite;
    PlotPanel2.Color := clWhite;
  end
end;

procedure TValuesPlot.OptionsSpeedButton1Click(Sender: TObject);
var
  theTitle: PChar;
begin
  PlotOptions := PlotOptionsForm.GetPlotOptions;
  if PlotOptions.titleString = '' then
  begin
    Chart1.Title.Visible := false;
  end
  else begin
    theTitle := PChar(PlotOptions.titleString);
    Chart1.Title.Text.SetText(theTitle);
    Chart1.Title.Visible := true;
  end;
  Chart1.Font.Name := PlotOptions.fontname;
  {$IF (LCL_MAJOR >= 2)}
    Chart1.LeftAxis.Title.LabelFont.Name := PlotOptions.fontname;
  {$ELSE}
    Chart1.LeftAxis.Title.Font.Name := PlotOptions.fontname;
  {$ENDIF}
  Chart1.LeftAxis.Marks.LabelFont.Name := PlotOptions.fontname;
  {$IF (LCL_MAJOR >= 2)}
    Chart1.BottomAxis.Title.LabelFont.Name := PlotOptions.fontname;
  {$ELSE}
    Chart1.BottomAxis.Title.Font.Name := PlotOptions.fontname;
  {$ENDIF}
  Chart1.BottomAxis.Marks.LabelFont.Name := PlotOptions.fontname;;
  Chart1.Title.Font.Color := PlotOptions.titleColor;
  Chart1.Title.Font.Name := PlotOptions.fontname;
end;

procedure TValuesPlot.OptionsSpeedButton2Click(Sender: TObject);
var
  theTitle: PChar;
begin
  PlotOptions := PlotOptionsForm.GetPlotOptions;
  if PlotOptions.titleString = '' then
  begin
    Chart2.Title.Visible := false;
  end
  else begin
    theTitle := PChar(PlotOptions.titleString);
    Chart2.Title.Text.SetText(theTitle);
    Chart2.Title.Visible := true;
  end;
  Chart2.Font.Name := PlotOptions.fontname;
  {$IF (LCL_MAJOR >= 2)}
    Chart2.LeftAxis.Title.LabelFont.Name := PlotOptions.fontname;
  {$ELSE}
    Chart2.LeftAxis.Title.Font.Name := PlotOptions.fontname;
  {$ENDIF}
  Chart2.LeftAxis.Marks.LabelFont.Name := PlotOptions.fontname;
  {$IF (LCL_MAJOR >= 2)}
    Chart2.BottomAxis.Title.LabelFont.Name := PlotOptions.fontname;
  {$ELSE}
    Chart2.BottomAxis.Title.Font.Name := PlotOptions.fontname;
  {$ENDIF}
  Chart2.BottomAxis.Marks.LabelFont.Name := PlotOptions.fontname;;
  Chart2.Title.Font.Color := PlotOptions.titleColor;
  Chart2.Title.Font.Name := PlotOptions.fontname;
end;

procedure TValuesPlot.UpdateTimeAxes;
begin
  TimeAxisSource.DateTimeFormat := string(gDateTimeFormat);
end;

procedure TValuesPlot.FormShow(Sender: TObject);
begin
  FormPaint(Sender);
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
  {$IFDEF UNIX} {selects optimal type of clipboard graphic for respective OS}
var
  theImage: TPortableNetworkGraphic;
  theWidth, theHeight: integer;
  {$ENDIF}
begin
  if gSelectedChart = nil then
    bell
  else
  begin
    { gSelectedChart.CopyToClipboardBitmap doesn't work on Mac OS X }
    {$IFDEF UNIX}
    theImage := TPortableNetworkGraphic.Create;
    try
      theWidth := gSelectedChart.Width;
      theHeight := gSelectedChart.Height;
      theImage.Width := theWidth;
      theImage.Height := theHeight;
      {$IFDEF VER2}
      if (lcl_major < 2) and (lcl_minor < 4) then
        gSelectedChart.DrawOnCanvas(rect(0, 0, theImage.Width, theImage.Height),
        theImage.canvas)
      else  {$ENDIF}
        gSelectedChart.PaintOnCanvas(theImage.canvas, rect(0, 0, theImage.Width, theImage.Height));
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
         {$IFDEF LCLcarbon} {compensates for a bug in older versions of carbon widgetset}
           if (lcl_major < 2) and (lcl_minor < 2) then
             theFilterIndex := theFilterIndex + 1;
         {$ENDIF}
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
    ShowImplementationMessage;  {this function is not yet implemented}
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

