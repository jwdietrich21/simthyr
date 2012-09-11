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
  DH_FACTOR = 1e9;
  DR_FACTOR = 1e12;

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
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FT3ColorBoxChange(Sender: TObject);
    procedure FT4ColorBoxChange(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure MaxSpinEditChange(Sender: TObject);
    procedure MinSpinEditChange(Sender: TObject);
    procedure StrucParComboChange(Sender: TObject);
    procedure TSHColorBoxChange(Sender: TObject);
    procedure CopyChart;
  private
    { private declarations }
  public
    { public declarations }
  end;

  TStoredParameters = record
    GT, GD1, GD2, kM1, kM2, dT, LS: real;
    GH, DH, SS, DS, GR, DR: real;
    betaS, betaS2, betaT, beta31, beta32: real;
  end;

var
  OWSensPlotReady: boolean;
  SensitivityAnalysisForm: TSensitivityAnalysisForm;
  StoredParameters: TStoredParameters;
  FLine: array[0..MAX_SERIES] of TLineSeries;
  SeriesCount: integer;
  gMinXPar, gMaxXPar, gSpinFactor: real;

procedure SaveStrucPars;
procedure RestoreStrucPars;
procedure DrawOWSensitivityPlot(empty: boolean);

implementation

procedure SetStandardStrucParBoundaries(factor1, factor2: real);
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
    4:
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
    5:
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
    begin {GH}
      gSpinFactor := DT_FACTOR;
      gMinXPar := GH / 3;
      gMaxXPar := GH * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    8:
    begin {DH}
      gSpinFactor := DH_FACTOR;
      gMinXPar := DH / 3;
      gMaxXPar := DH * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR;
    end;
    9:
    begin {SS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := SS / 3;
      gMaxXPar := SS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    10:
    begin {DS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := DS / 3;
      gMaxXPar := DS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    11:
    begin {GR}
      gSpinFactor := DT_FACTOR;
      gMinXPar := GR / 3;
      gMaxXPar := GR * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    12:
    begin {DR}
      gSpinFactor := DR_FACTOR;
      gMinXPar := DR / 3;
      gMaxXPar := DR * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DR_FACTOR;
    end;
    13:
    begin {betaS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaS / 3;
      gMaxXPar := betaS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    14:
    begin {betaS2}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaS2 / 3;
      gMaxXPar := betaS2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    15:
    begin {betaT}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaT / 3;
      gMaxXPar := betaT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    16:
    begin {beta31}
      gSpinFactor := DT_FACTOR;
      gMinXPar := beta31 / 3;
      gMaxXPar := beta31 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    17:
    begin {beta32}
      gSpinFactor := DT_FACTOR;
      gMinXPar := beta32 / 3;
      gMaxXPar := beta32 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
      SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    18:
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
  if SensitivityAnalysisForm.MinSpinEdit.Value < 10 then SensitivityAnalysisForm.MinSpinEdit.DecimalPlaces := 4
  else if SensitivityAnalysisForm.MinSpinEdit.Value > 100 then SensitivityAnalysisForm.MinSpinEdit.DecimalPlaces := 1
  else SensitivityAnalysisForm.MinSpinEdit.DecimalPlaces := 2;
  if SensitivityAnalysisForm.MaxSpinEdit.Value < 10 then SensitivityAnalysisForm.MaxSpinEdit.DecimalPlaces := 4
  else if SensitivityAnalysisForm.MaxSpinEdit.Value > 100 then SensitivityAnalysisForm.MaxSpinEdit.DecimalPlaces := 1
  else SensitivityAnalysisForm.MaxSpinEdit.DecimalPlaces := 2;
end;

procedure SaveStrucPars;
{save old structure parameters before systematically modifying them:}
begin
  StoredParameters.GD1 := GD1;
  StoredParameters.GD2 := GD2;
  StoredParameters.GT := GT;
  StoredParameters.kM1 := kM1;
  StoredParameters.kM2 := kM2;
  StoredParameters.dT := dT;
  StoredParameters.GH := GH;
  StoredParameters.DH := DH;
  StoredParameters.GR := GR;
  StoredParameters.DR := DR;
  StoredParameters.betaS := betaS;
  StoredParameters.betaS2 := betaS2;
  StoredParameters.betaT := betaT;
  StoredParameters.beta31 := beta31;
  StoredParameters.beta32 := beta32;
  StoredParameters.LS := LS;
end;

procedure RestoreStrucPars;
{restore saved structure parameters:}
begin
  GD1 := StoredParameters.GD1;
  GD2 := StoredParameters.GD2;
  GT := StoredParameters.GT;
  kM1 := StoredParameters.kM1;
  kM2 := StoredParameters.kM2;
  dT := StoredParameters.dT;
  GH := StoredParameters.GH;
  DH := StoredParameters.DH;
  GR := StoredParameters.GR;
  DR := StoredParameters.DR;
  betaS := StoredParameters.betaS;
  betaS2 := StoredParameters.betaS2;
  betaT := StoredParameters.betaT;
  beta31 := StoredParameters.beta31;
  beta32 := StoredParameters.beta32;
  LS := StoredParameters.LS;
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
  {If line series exists it is cleared and recreated to support foundations of redrawing}
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
    SaveStrucPars;
    interval := (gMaxXPar - gMinXPar) / max_i;
    for i := 0 to max_i do
    begin
      case SensitivityAnalysisForm.StrucParCombo.ItemIndex of
        0:
        ;
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
          kM1 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(kM1);
        end;
        4:
        begin
          kM2 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(kM2);
        end;
        5:
        begin
          GT := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(GT);
        end;
        6:
        begin
          dT := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(dT);
        end;
        7:
        begin
          GH := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(GH);
        end;
        8:
        begin
          DH := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(DH);
        end;
        9:
        begin
          SS := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(SS);
        end;
        10:
        begin
          DS := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(DS);
        end;
        11:
        begin
          GR := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(GR);
        end;
        12:
        begin
          DR := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(DR);
        end;
        13:
        begin
          betaS := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(betaS);
        end;
        14:
        begin
          betaS2 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(betaS2);
        end;
        15:
        begin
          betaT := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(betaT);
        end;
        16:
        begin
          beta31 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(beta31);
        end;
        17:
        begin
          beta32 := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(beta32);
        end;
        18:
        begin
          LS := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(LS);
        end
        otherwise
          if not gStartup then bell;
      end;
    end;
    SetBottomAxisCaption;
    RestoreStrucPars;
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
  SetStandardStrucParBoundaries(1 / 3, 3);
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
  {ItemIndex is evaluated in the SetStandardStrucParBoundaries and plot routine}
  SetStandardStrucParBoundaries(1 / 3, 3);
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.cT3ColorBoxChange(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.FormActivate(Sender: TObject);
begin
  gLastActiveCustomForm := SensitivityAnalysisForm;
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
  {ItemIndex is evaluated in the SetStandardStrucParBoundaries and plot routine}
  SetStandardStrucParBoundaries(1 / 3, 3);
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.TSHColorBoxChange(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.CopyChart;
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
    {$ELSE}
    Chart1.CopyToClipboardBitmap;
    {$ENDIF}
  end;
end;

procedure TSensitivityAnalysisForm.CopyItemClick(Sender: TObject);
begin
  CopyChart;
end;


initialization
  {$I sensitivityanalysis.lrs}

end.

