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
  SimThyrServices, SimThyrPrediction;

const
  MAX_SERIES = 8;

type

  { TSensitivityAnalysisForm }

  TSensitivityAnalysisForm = class(TForm)
    Chart1: TChart;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    CheckGroup1: TCheckGroup;
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
    procedure cT3ColorBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FT3ColorBoxChange(Sender: TObject);
    procedure FT4ColorBoxChange(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure MaxSpinEditChange(Sender: TObject);
    procedure MinSpinEditChange(Sender: TObject);
    procedure StrucParComboChange(Sender: TObject);
    procedure TSHColorBoxChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TStoredParameters = record
    GT, GD1, GD2: real
  end;

var
  OWSensPlotReady: boolean;
  SensitivityAnalysisForm: TSensitivityAnalysisForm;
  StoredParameters: TStoredParameters;
  FLine: array[0..MAX_SERIES] of TLineSeries;
  SeriesCount: integer;

procedure DrawOWSensitivityPlot(empty: boolean);

implementation

procedure SetStrucParBoundaries;
{sets the initial boundaries to useful values}
begin
  case SensitivityAnalysisForm.StrucParCombo.ItemIndex of
    0:
    begin {GD1}
      SensitivityAnalysisForm.MinSpinEdit.Value := GD1 / 3;
      SensitivityAnalysisForm.MaxSpinEdit.Value := GD1 * 3;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale := 1e-9;
    end;
    1:
    begin {GD2}
      SensitivityAnalysisForm.MinSpinEdit.Value := GD2 / 3;
      SensitivityAnalysisForm.MaxSpinEdit.Value := GD2 * 3;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale := 1e-15;
    end;
    2:
    begin {GT}
      SensitivityAnalysisForm.MinSpinEdit.Value := GT / 3;
      SensitivityAnalysisForm.MaxSpinEdit.Value := GT * 3;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale := 1e-12;
    end
  end;
end;

procedure SetBottomAxisCaption;
var
  theCaption: String;
begin
  theCaption := SensitivityAnalysisForm.StrucParCombo.Text;
  if theCaption = 'GD1' then theCaption := theCaption + ' (nmol/s)'
  else if theCaption = 'GD2' then theCaption := theCaption + ' (fmol/s)'
  else if theCaption = 'GT' then theCaption := theCaption + ' (pmol/s)';
  SensitivityAnalysisForm.Chart1.BottomAxis.Title.Caption :=
    theCaption;
end;

procedure DrawDummySensitivityPlot;
{Draws an empty plot}
begin
  SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'Dependent Parameter';
  SetBottomAxisCaption;
  with FLine[1] do
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
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'TSH' + ': ' + gParameterUnit[TSH_pos];
    inc(SeriesCount);
    FLine[1].SeriesColor := SensitivityAnalysisForm.TSHColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[1] then
  begin
    {FT3}
    FLine[2].AddXY(xPar, FT41 / UFT4 * gParameterFactor[FT4_pos], '',
      SensitivityAnalysisForm.FT4ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'FT4' + ': ' + gParameterUnit[FT4_pos];
    inc(SeriesCount);
    FLine[2].SeriesColor := SensitivityAnalysisForm.FT4ColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[2] then
  begin
    {FT3}
    FLine[3].AddXY(xPar, FT31 / UFT3 * gParameterFactor[FT3_pos], '',
      SensitivityAnalysisForm.FT3ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'FT3' + ': ' + gParameterUnit[FT3_pos];
    inc(SeriesCount);
    FLine[3].SeriesColor := SensitivityAnalysisForm.FT3ColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[3] then
  begin
    {cT3}
    FLine[4].AddXY(xPar, T3z1 / UFT3 * gParameterFactor[cT3_pos], '',
      SensitivityAnalysisForm.cT3ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'cT3' + ': ' + gParameterUnit[cT3_pos];
    inc(SeriesCount);
    FLine[4].SeriesColor := SensitivityAnalysisForm.cT3ColorBox.Selected;
  end;
  if SeriesCount > 1 then SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'Dependent Parameters';
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
      Pointer.Brush.Color := clRed;
      SeriesColor := clRed;
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
    interval := (SensitivityAnalysisForm.MaxSpinEdit.Value -
      SensitivityAnalysisForm.MinSpinEdit.Value) / max_i;
    for i := 0 to max_i do
    begin
      case SensitivityAnalysisForm.StrucParCombo.ItemIndex of
        0:
        begin
          GD1 := SensitivityAnalysisForm.MinSpinEdit.Value + i * interval;
          PredictEquilibrium;
          DrawCurves(GD1);
        end;
        1:
        begin
          GD2 := SensitivityAnalysisForm.MinSpinEdit.Value + i * interval;
          PredictEquilibrium;
          DrawCurves(GD2);
        end;
        2:
        begin
          GT := SensitivityAnalysisForm.MinSpinEdit.Value + i * interval;
          PredictEquilibrium;
          DrawCurves(GT);
        end
      end;
    end;
    SetBottomAxisCaption;
    {restore saved structure parameters:}
    GD1 := StoredParameters.GD1;
    GD2 := StoredParameters.GD2;
    GT := StoredParameters.GT;
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

initialization
  {$I sensitivityanalysis.lrs}

end.
