unit Sensitivityanalysis;

{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ This unit implements sensitivity analysis }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, ExtCtrls, ColorBox, ComCtrls, TAGraph, TASources,
  TATools, TASeries, TATransformations, TAStyles, TALegendPanel, SimThyrTypes,
  SimThyrServices, SimThyrPrediction, Clipbrd, Menus;

const
  MAX_SERIES = 8;
  GD1_FACTOR = 1e9;
  GD2_FACTOR = 1e15;
  GT_FACTOR = 1e12;
  KM1_FACTOR = 1e9;
  KM2_FACTOR = 1e9;
  DT_FACTOR = 1;
  LS_FACTOR = 1e-6;

type

  { TSensitivityAnalysisForm }

  TSensitivityAnalysisForm = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    CheckGroup1: TCheckGroup;
    Divider1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    UndoItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    TSHColorBox: TColorBox;
    FT4ColorBox: TColorBox;
    FT3ColorBox: TColorBox;
    cT3ColorBox: TColorBox;
    FullScaleButton1: TSpeedButton;
    GroupBox1: TGroupBox;
    MinSpinEdit: TFloatSpinEdit;
    MaxSpinEdit: TFloatSpinEdit;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    StrucParCombo: TComboBox;
    procedure CheckGroup1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure CopyItemClick(Sender: TObject);
    procedure cT3ColorBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FT3ColorBoxChange(Sender: TObject);
    procedure FT4ColorBoxChange(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure MaxSpinEditChange(Sender: TObject);
    procedure MinSpinEditChange(Sender: TObject);
    procedure StrucParComboChange(Sender: TObject);
    procedure TSHColorBoxChange(Sender: TObject);
    procedure CopyChart(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TStoredParameters = record
    GT, GD1, GD2, kM1, kM2, dT, LS: real
  end;

var
  OWSensPlotReady: boolean;
  SensitivityAnalysisForm: TSensitivityAnalysisForm;
  StoredParameters: TStoredParameters;
  FLine: array[0..MAX_SERIES] of TLineSeries;
  SeriesCount: integer;
  gMinXPar, gMaxXPar, gSpinFactor: real;

procedure DrawOWSensitivityPlot(empty: boolean);

implementation

procedure SetStrucParBoundaries;
{sets the initial boundaries to useful values}
var
  tempMinX, tempMaxX: real; {necessary to hinder Windows from altering the globals}
begin
  case SensitivityAnalysisForm.StrucParCombo.ItemIndex of
    1:
    begin {GD1}
      gSpinFactor := GD1_FACTOR;
      gMinXPar := GD1 / 3;
      gMaxXPar := GD1 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / GD1_FACTOR;
    end;
    2:
    begin {GD2}
      gSpinFactor := GD2_FACTOR;
      gMinXPar := GD2 / 3;
      gMaxXPar := GD2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / GD2_FACTOR;
    end;
    3:
    begin {GT}
      gSpinFactor := GT_FACTOR;
      gMinXPar := GT / 3;
      gMaxXPar := GT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / GT_FACTOR;
    end;
    4:
    begin {kM1}
      gSpinFactor := KM1_FACTOR;
      gMinXPar := kM1 / 3;
      gMaxXPar := kM1 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / KM1_FACTOR;
    end;
    5:
    begin {kM2}
      gSpinFactor := KM2_FACTOR;
      gMinXPar := kM2 / 3;
      gMaxXPar := kM2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / KM2_FACTOR;
    end;
    6:
    begin {DT}
      gSpinFactor := DT_FACTOR;
      gMinXPar := DT / 3;
      gMaxXPar := DT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    7:
    begin {LS}
      gSpinFactor := LS_FACTOR;
      gMinXPar := LS / 3;
      gMaxXPar := LS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / LS_FACTOR;
    end

  end;
end;

procedure SetBottomAxisCaption;
var
  theCaption: string;
begin
  theCaption := SensitivityAnalysisForm.StrucParCombo.Text;
  if theCaption = 'GD1' then
    theCaption := theCaption + ' (nmol/s)'
  else if theCaption = 'GD2' then
    theCaption := theCaption + ' (fmol/s)'
  else if theCaption = 'GT' then
    theCaption := theCaption + ' (pmol/s)';
  SensitivityAnalysisForm.Chart1.BottomAxis.Title.Caption :=
    theCaption;
end;

procedure DrawDummySensitivityPlot;
{Draws an empty plot}
begin
  SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'Dependent Parameter';
  SetBottomAxisCaption;
  with FLine[0] do
  begin
    AddXY(0, 0, '', clBlack);
    AddXY(100, 0, '', clBlack);
  end;
end;

procedure DrawCurves(xPar: real);
{Draws the curves in dependence from the independent parameter xPar}
begin
  SeriesCount := 0;
  if SensitivityAnalysisForm.CheckGroup1.Checked[0] then
  begin
    {TSH}
    FLine[1].AddXY(xPar, TSH1 * gParameterFactor[TSH_pos], '',
      SensitivityAnalysisForm.TSHColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'TSH' + ': ' + gParameterUnit[TSH_pos];
    Inc(SeriesCount);
    FLine[1].SeriesColor := SensitivityAnalysisForm.TSHColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[1] then
  begin
    {FT3}
    FLine[2].AddXY(xPar, FT41 / UFT4 * gParameterFactor[FT4_pos], '',
      SensitivityAnalysisForm.FT4ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'FT4' + ': ' + gParameterUnit[FT4_pos];
    Inc(SeriesCount);
    FLine[2].SeriesColor := SensitivityAnalysisForm.FT4ColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[2] then
  begin
    {FT3}
    FLine[3].AddXY(xPar, FT31 / UFT3 * gParameterFactor[FT3_pos], '',
      SensitivityAnalysisForm.FT3ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'FT3' + ': ' + gParameterUnit[FT3_pos];
    Inc(SeriesCount);
    FLine[3].SeriesColor := SensitivityAnalysisForm.FT3ColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[3] then
  begin
    {cT3}
    FLine[4].AddXY(xPar, T3z1 / UFT3 * gParameterFactor[cT3_pos], '',
      SensitivityAnalysisForm.cT3ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'cT3' + ': ' + gParameterUnit[cT3_pos];
    Inc(SeriesCount);
    FLine[4].SeriesColor := SensitivityAnalysisForm.cT3ColorBox.Selected;
  end;
  if SeriesCount > 1 then
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'Dependent Parameters';
end;

procedure DrawOWSensitivityPlot(empty: boolean);
{Plots sensitivity analysis results}
const
  max_i = 100;
var
  i, j, k: integer;
  interval: real;
begin
  if FLine[1] <> nil then
    SensitivityAnalysisForm.Chart1.ClearSeries;
  for i := 0 to MAX_SERIES do
  begin
    FLine[i] := TLineSeries.Create(SensitivityAnalysisForm.Chart1);
    with FLine[i] do
    begin
      ShowLines := True;
      ShowPoints := False;
      Pointer.Brush.Color := clBlack;
      SeriesColor := clBlack;
    end;
    SensitivityAnalysisForm.Chart1.AddSeries(FLine[i]);
    FLine[i].BeginUpdate;
  end;
  if empty then
    DrawDummySensitivityPlot
  else
  begin
    {save old structure parameters before systematically modifying them:}
    StoredParameters.GD1 := GD1;
    StoredParameters.GD2 := GD2;
    StoredParameters.GT := GT;
    StoredParameters.kM1 := kM1;
    StoredParameters.kM2 := kM2;
    StoredParameters.dT := dT;
    StoredParameters.LS := LS;;
    interval := (gMaxXPar - gMinXPar) / max_i;
    for i := 0 to max_i do
    begin
      case SensitivityAnalysisForm.StrucParCombo.ItemIndex of
        1:
        begin
          GD1 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(GD1);
        end;
        2:
        begin
          GD2 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(GD2);
        end;
        3:
        begin
          GT := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(GT);
        end;
        4:
        begin
          kM1 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(kM1);
        end;
        5:
        begin
          kM2 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(kM2);
        end;
        6:
        begin
          dT := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(dT);
        end;
        7:
        begin
          LS := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(LS);
        end
      end;
    end;
    SetBottomAxisCaption;
    {restore saved structure parameters:}
    GD1 := StoredParameters.GD1;
    GD2 := StoredParameters.GD2;
    GT := StoredParameters.GT;
    kM1 := StoredParameters.kM1;
    kM2 := StoredParameters.kM2;
    dT := StoredParameters.dT;
    LS := StoredParameters.LS;
    OWSensPlotReady := True;
    PredictEquilibrium;
  end;
  for i := 0 to MAX_SERIES do
    FLine[i].EndUpdate;
end;

{ TSensitivityAnalysisForm }

procedure TSensitivityAnalysisForm.FullScaleButton1Click(Sender: TObject);
{Zooms sensitivity chart to full size}
begin
  SensitivityAnalysisForm.Chart1.Extent.UseYMax := False;
  SensitivityAnalysisForm.Chart1.ZoomFull;
end;

procedure TSensitivityAnalysisForm.FormCreate(Sender: TObject);
{sets default values for UI elements}
begin
  OWSensPlotReady := False;
  SensitivityAnalysisForm.TSHColorBox.Selected := gDefaultColors[4];
  SensitivityAnalysisForm.FT4ColorBox.Selected := gDefaultColors[6];
  SensitivityAnalysisForm.FT3ColorBox.Selected := gDefaultColors[8];
  SensitivityAnalysisForm.cT3ColorBox.Selected := gDefaultColors[9];
  SetStrucParBoundaries;
  DrawOWSensitivityPlot(True);
end;

procedure TSensitivityAnalysisForm.FT3ColorBoxChange(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.FT4ColorBoxChange(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.CheckGroup1Click(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.CheckGroup1ItemClick(Sender: TObject;
  Index: integer);
begin
  SetBottomAxisCaption;
  {ItemIndex is evaluated in the SetStrucParBoundaries and plot routine}
  SetStrucParBoundaries;
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.cT3ColorBoxChange(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.MaxSpinEditChange(Sender: TObject);
begin
  if SensitivityAnalysisForm.MaxSpinEdit.Value <
    SensitivityAnalysisForm.MinSpinEdit.Value then
    {adapts boundaries to avoid negative intervals}
  begin
    bell;
    SensitivityAnalysisForm.MinSpinEdit.Value :=
      SensitivityAnalysisForm.MaxSpinEdit.Value;
  end;
  gMinXPar := SensitivityAnalysisForm.MinSpinEdit.Value / gSpinFactor;
  gMaxXPar := SensitivityAnalysisForm.MaxSpinEdit.Value / gSpinFactor;
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.MinSpinEditChange(Sender: TObject);
begin
  if SensitivityAnalysisForm.MaxSpinEdit.Value <
    SensitivityAnalysisForm.MinSpinEdit.Value then
    {adapts boundaries to avoid negative intervals}
  begin
    bell;
    SensitivityAnalysisForm.MaxSpinEdit.Value :=
      SensitivityAnalysisForm.MinSpinEdit.Value;
  end;
  gMinXPar := SensitivityAnalysisForm.MinSpinEdit.Value / gSpinFactor;
  gMaxXPar := SensitivityAnalysisForm.MaxSpinEdit.Value / gSpinFactor;
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.StrucParComboChange(Sender: TObject);
begin
  SetBottomAxisCaption;
  {ItemIndex is evaluated in the SetStrucParBoundaries and plot routine}
  SetStrucParBoundaries;
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.TSHColorBoxChange(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.CopyChart(Sender: TObject);
var
  {$IFDEF UNIX}
  theImage: TPortableNetworkGraphic;
  {$ELSE}
  theImage: TBitMap;
  {$ENDIF}
  theWidth, theHeight: integer;
begin
  if Chart1 = nil then
    bell
  else
  begin
    {gSelectedChart.CopyToClipboardBitmap doesn't work on Mac OS X}
    {$IFDEF UNIX}
    theImage := TPortableNetworkGraphic.Create;
    {$ELSE}
    Chart1.CopyToClipboardBitmap;
    {$ENDIF}
    try
      theWidth := Chart1.Width;
      theHeight := Chart1.Height;
      theImage.Width := theWidth;
      theImage.Height := theHeight;
      Chart1.DrawOnCanvas(rect(0, 0, theImage.Width, theImage.Height), theImage.canvas);
      Clipboard.Assign(theImage);
    finally
      theImage.Free;
    end;
  end;
end;

procedure TSensitivityAnalysisForm.CopyItemClick(Sender: TObject);
begin
  CopyChart(Sender);
end;


initialization
  {$I sensitivityanalysis.lrs}

end.

