unit Sensitivityanalysis;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2016 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2016 }

{ This unit implements sensitivity analysis }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, Buttons, ExtCtrls, ColorBox, ComCtrls, TAGraph, TASources,
  TATools, TASeries, TATransformations, TAStyles, TALegendPanel, Clipbrd, Menus,
  LCLVersion, Grids, StrUtils, TAIntervalSources, TADrawerSVG, TADrawUtils,
  TADrawerCanvas, TANavigation, SimThyrTypes, SimThyrServices,
  SimThyrPrediction, UnitConverter, Types;

const
  MAX_SERIES = 8;
  GD1_FACTOR = 1e9;  {These factors allow to provide for metric prefixes.}
  GD2_FACTOR = 1e15; {They are used in sensitivity analysis and for nullclines.}
  GT_FACTOR = 1e12;
  KM1_FACTOR = 1e9;
  KM2_FACTOR = 1e9;
  DT_FACTOR = 1;
  LS_FACTOR = 1e-6;
  GH_FACTOR = 1;
  DH_FACTOR = 1e9;
  SS_FACTOR = 1;
  DS_FACTOR = 1;
  GR_FACTOR = 1;
  DR_FACTOR = 1e12;
  BETAT_FACTOR = 1e6;
  TBG_FACTOR = 1e9;
  TBPA_FACTOR = 1e6;
  PAR_COL = 0;
  TSH_COL = 1;
  TT4_COL = 2;
  FT4_COL = 3;
  TT3_COL = 4;
  FT3_COL = 5;
  cT3_COL = 6;

type

  { TSensitivityAnalysisForm }

  TSensitivityAnalysisForm = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    ChartNavPanel1: TChartNavPanel;
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    ChartToolset1ZoomMouseWheelTool1: TZoomMouseWheelTool;
    CheckGrid: TStringGrid;
    CheckGroup1: TCheckGroup;
    ResetButton: TSpeedButton;
    TableButton: TSpeedButton;
    TT4ColorBox: TColorBox;
    Divider1: TMenuItem;
    CutItem: TMenuItem;
    CopyItem: TMenuItem;
    PasteItem: TMenuItem;
    TT3ColorBox: TColorBox;
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
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure CheckGroup1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure CopyItemClick(Sender: TObject);
    procedure cT3ColorBoxChange(Sender: TObject);
    procedure CutItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FT3ColorBoxChange(Sender: TObject);
    procedure FT4ColorBoxChange(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure MaxSpinEditChange(Sender: TObject);
    procedure MinSpinEditChange(Sender: TObject);
    procedure PasteItemClick(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure StrucParComboChange(Sender: TObject);
    procedure TableButtonClick(Sender: TObject);
    procedure TSHColorBoxChange(Sender: TObject);
    procedure CopyChart;
    procedure SaveChart;
    procedure SaveGrid(theFileName: String; theDelimiter: Char);
    procedure TT3ColorBoxChange(Sender: TObject);
    procedure TT4ColorBoxChange(Sender: TObject);
    procedure UndoItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TStoredParameters = record
    GT, GD1, GD2, kM1, kM2, dT, LS: real;
    GH, DH, SS, DS, GR, DR: real;
    betaS, betaS2, betaT, beta31, beta32: real;
    TBG, TBPA: real;
  end;

var
  OWSensPlotReady: boolean;
  SensitivityAnalysisForm: TSensitivityAnalysisForm;
  StoredParameters: TStoredParameters;
  FLine: array[0..MAX_SERIES] of TLineSeries;
  gMinXPar, gMaxXPar, gSpinFactor: real;
  gSensitivityMatrix: tResultMatrix;
  gCurLine: integer;

procedure SaveStrucPars;
procedure RestoreStrucPars;
function composedAxisCaption(theString: String): String;
procedure DrawOWSensitivityPlot(empty: boolean);

implementation

uses
  SimThyrMain;

procedure SetStandardStrucParBoundaries(factor1, factor2: real);
{sets the initial boundaries to useful values}
var
  tempMinX, tempMaxX: real; {necessary to hinder Windows from altering the globals}

  procedure SetSpinEdits;
  begin
    SensitivityAnalysisForm.MinSpinEdit.Value := tempMinX * gSpinFactor;
    SensitivityAnalysisForm.MaxSpinEdit.Value := tempMaxX * gSpinFactor;
    gMinXPar := tempMinX;
    gMaxXPar := tempMaxX;
  end;

begin
  case SensitivityAnalysisForm.StrucParCombo.ItemIndex of
    0:
    begin {Caption}
      gSpinFactor := 0;
      SensitivityAnalysisForm.MinSpinEdit.Value := 0;
      SensitivityAnalysisForm.MaxSpinEdit.Value := 0;
    end;
    1:
    begin {GD1}
      gSpinFactor := GD1_FACTOR;
      gMinXPar := GD1 / 3;
      gMaxXPar := GD1 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
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
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DR_FACTOR;
    end;
    13:
    begin {LS}
      gSpinFactor := LS_FACTOR;
      gMinXPar := LS / 3;
      gMaxXPar := LS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / LS_FACTOR;
    end;
    14:
    begin {betaS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaS / 3;
      gMaxXPar := betaS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    15:
    begin {betaS2}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaS2 / 3;
      gMaxXPar := betaS2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    16:
    begin {betaT}
      gSpinFactor := BETAT_FACTOR;
      gMinXPar := betaT / 3;
      gMaxXPar := betaT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    17:
    begin {beta31}
      gSpinFactor := BETAT_FACTOR;
      gMinXPar := beta31 / 3;
      gMaxXPar := beta31 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    18:
    begin {beta32}
      gSpinFactor := DT_FACTOR;
      gMinXPar := beta32 / 3;
      gMaxXPar := beta32 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;
    end;
    19:
    begin {TBG}
      gSpinFactor := TBG_FACTOR;
      gMinXPar := TBG / 3;
      gMaxXPar := TBG * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / TBG_FACTOR;
    end;
    20:
    begin {TBPA}
      gSpinFactor := TBPA_FACTOR;
      gMinXPar := TBPA / 3;
      gMaxXPar := TBPA * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      SetSpinEdits;
      SensitivityAnalysisForm.ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / TBPA_FACTOR;
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
  StoredParameters.TBG := TBG;
  StoredParameters.TBPA := TBPA;
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
  TBG := StoredParameters.TBG;
  TBPA := StoredParameters.TBPA;
end;

function composedAxisCaption(theString: String): String;
{sets caption and measurement unit for bottom axis according to selected item}
begin
  if theString = 'GD1' then
    theString := theString + ' (nmol/s)'
  else if theString = 'GD2' then
    theString := theString + ' (fmol/s)'
  else if theString = 'GT' then
    theString := theString + ' (pmol/s)';
  result := theString;
end;

procedure SetBottomAxisCaption;
{sets caption and measurement unit for bottom axis according to selected item}
var
  theCaption: string;
begin
  theCaption := composedAxisCaption(SensitivityAnalysisForm.StrucParCombo.Text);
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
var
  SeriesCount: integer;
  FT4conversionFactor, FT3conversionFactor: real;
  TT4conversionFactor, TT3conversionFactor, cT3conversionFactor: real;
begin
  SeriesCount := 0;
  FT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l', gParameterUnit[FT4_pos]);
  TT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l', gParameterUnit[TT4_pos]);
  TT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[TT3_pos]);
  FT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[FT3_pos]);
  cT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[cT3_pos]);
  gSensitivityMatrix[gCurLine, PAR_COL] := xPar;
  if SensitivityAnalysisForm.CheckGroup1.Checked[0] then
  begin
    {TSH}
    gSensitivityMatrix[gCurLine, TSH_COL] :=
      gActiveModel.Equilibrium.TSH1 * gParameterFactor[TSH_pos];
    FLine[1].AddXY(xPar, gSensitivityMatrix[gCurLine, TSH_COL], '',
      SensitivityAnalysisForm.TSHColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'TSH' + ': ' + gParameterUnit[TSH_pos];
    Inc(SeriesCount);
    FLine[1].SeriesColor := SensitivityAnalysisForm.TSHColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[1] then
  begin
    {TT4}
    gSensitivityMatrix[gCurLine, TT4_COL] :=
      gActiveModel.Equilibrium.T41 * TT4conversionFactor;
    FLine[5].AddXY(xPar, gSensitivityMatrix[gCurLine, TT4_COL], '',
      SensitivityAnalysisForm.TT4ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'TT4' + ': ' + gParameterUnit[TT4_pos];
    Inc(SeriesCount);
    FLine[5].SeriesColor := SensitivityAnalysisForm.TT4ColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[2] then
  begin
    {FT4}
    gSensitivityMatrix[gCurLine, FT4_COL] :=
      gActiveModel.Equilibrium.FT41 * FT4conversionFactor;
    FLine[2].AddXY(xPar, gSensitivityMatrix[gCurLine, FT4_COL], '',
      SensitivityAnalysisForm.FT4ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'FT4' + ': ' + gParameterUnit[FT4_pos];
    Inc(SeriesCount);
    FLine[2].SeriesColor := SensitivityAnalysisForm.FT4ColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[3] then
  begin
    {TT3}
    gSensitivityMatrix[gCurLine, TT3_COL] :=
      gActiveModel.Equilibrium.T31 * TT3conversionFactor;
    FLine[6].AddXY(xPar, gSensitivityMatrix[gCurLine, TT3_COL], '',
      SensitivityAnalysisForm.TT3ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'TT3' + ': ' + gParameterUnit[TT3_pos];
    Inc(SeriesCount);
    FLine[6].SeriesColor := SensitivityAnalysisForm.TT3ColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[4] then
  begin
    {FT3}
    gSensitivityMatrix[gCurLine, FT3_COL] :=
      gActiveModel.Equilibrium.FT31 * FT3conversionFactor;
    FLine[3].AddXY(xPar, gSensitivityMatrix[gCurLine, FT3_COL], '',
      SensitivityAnalysisForm.FT3ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'FT3' + ': ' + gParameterUnit[FT3_pos];
    Inc(SeriesCount);
    FLine[3].SeriesColor := SensitivityAnalysisForm.FT3ColorBox.Selected;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[5] then
  begin
    {cT3}
    gSensitivityMatrix[gCurLine, cT3_COL] :=
      gActiveModel.Equilibrium.T3z1 * cT3conversionFactor;
    FLine[4].AddXY(xPar, gSensitivityMatrix[gCurLine, cT3_COL], '',
      SensitivityAnalysisForm.cT3ColorBox.Selected);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption :=
      'cT3' + ': ' + gParameterUnit[cT3_pos];
    Inc(SeriesCount);
    FLine[4].SeriesColor := SensitivityAnalysisForm.cT3ColorBox.Selected;
  end;
  if SeriesCount > 1 then
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'Dependent Parameters';
  inc(gCurLine);
end;

procedure FillCheckGrid;
var
  i, SeriesCount: integer;
begin
  SeriesCount := 0;
  SensitivityAnalysisForm.CheckGrid.Clean;
  GridRows := 2;
  if gCurLine > GridRows then
    SensitivityAnalysisForm.CheckGrid.RowCount := gCurLine + 1
  else
    SensitivityAnalysisForm.CheckGrid.RowCount := GridRows;
  SensitivityAnalysisForm.CheckGrid.Cells[0, 0] := 'n';
  for i := 1 to gCurLine do
  begin
    SensitivityAnalysisForm.CheckGrid.Cells[0, i] := IntToStr(i);
  end;
  if SensitivityAnalysisForm.StrucParCombo.ItemIndex > 0 then
    begin
      SensitivityAnalysisForm.CheckGrid.Cells[1, 0] :=
      SensitivityAnalysisForm.StrucParCombo.Caption;
      for i := 1 to gCurLine do
        SensitivityAnalysisForm.CheckGrid.Cells[1, i] :=
          FloatToStrF(gSensitivityMatrix[i - 1, PAR_COL], ffGeneral, 4, 0);
    end
  else
    SensitivityAnalysisForm.CheckGrid.Cells[1, 0] := '';
  if SensitivityAnalysisForm.CheckGroup1.Checked[0] then
  begin
    {TSH}
    Inc(SeriesCount);
    if SeriesCount > SensitivityAnalysisForm.CheckGrid.ColCount - 2 then
      SensitivityAnalysisForm.CheckGrid.ColCount := SeriesCount + 2;
    SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, 0] := 'TSH';
    for i := 1 to gCurLine do
    begin
      SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, i] :=
        FloatToStrF(gSensitivityMatrix[i - 1, TSH_COL], ffGeneral, 4, 0);
    end;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[1] then
  begin
    {TT4}
    Inc(SeriesCount);
    if SeriesCount > SensitivityAnalysisForm.CheckGrid.ColCount - 2 then
      SensitivityAnalysisForm.CheckGrid.ColCount := SeriesCount + 2;
    SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, 0] := 'TT4';
    for i := 1 to gCurLine do
    begin
      SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, i] :=
        FloatToStrF(gSensitivityMatrix[i - 1, TT4_COL], ffGeneral, 4, 0);
    end;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[2] then
  begin
    {FT4}
    Inc(SeriesCount);
    if SeriesCount > SensitivityAnalysisForm.CheckGrid.ColCount - 2 then
      SensitivityAnalysisForm.CheckGrid.ColCount := SeriesCount + 2;
    SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, 0] := 'FT4';
    for i := 1 to gCurLine do
    begin
      SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, i] :=
        FloatToStrF(gSensitivityMatrix[i - 1, FT4_COL], ffGeneral, 4, 0);
    end;
   end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[3] then
  begin
    {TT3}
    Inc(SeriesCount);
    if SeriesCount > SensitivityAnalysisForm.CheckGrid.ColCount - 2 then
      SensitivityAnalysisForm.CheckGrid.ColCount := SeriesCount + 2;
    SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, 0] := 'TT3';
    for i := 1 to gCurLine do
    begin
      SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, i] :=
        FloatToStrF(gSensitivityMatrix[i - 1, TT3_COL], ffGeneral, 4, 0);
    end;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[4] then
  begin
    {FT3}
    Inc(SeriesCount);
    if SeriesCount > SensitivityAnalysisForm.CheckGrid.ColCount - 2 then
      SensitivityAnalysisForm.CheckGrid.ColCount := SeriesCount + 2;
    SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, 0] := 'FT3';
    for i := 1 to gCurLine do
    begin
      SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, i] :=
        FloatToStrF(gSensitivityMatrix[i - 1, FT3_COL], ffGeneral, 4, 0);
    end;
  end;
  if SensitivityAnalysisForm.CheckGroup1.Checked[5] then
  begin
    {cT3}
    Inc(SeriesCount);
    if SeriesCount > SensitivityAnalysisForm.CheckGrid.ColCount - 2 then
      SensitivityAnalysisForm.CheckGrid.ColCount := SeriesCount + 2;
    SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, 0] := 'cT3';
    for i := 1 to gCurLine do
    begin
      SensitivityAnalysisForm.CheckGrid.Cells[SeriesCount + 1, i] :=
        FloatToStrF(gSensitivityMatrix[i - 1, cT3_COL], ffGeneral, 4, 0);
    end;
  end;
end;

procedure DrawOWSensitivityPlot(empty: boolean);
{Plots sensitivity analysis results}
const
  max_i = 100; // resolution of curves
var
  i: integer;
  interval: real;
begin
  SetLength(gSensitivityMatrix, 0, 7);     // empty matrix
  SetLength(gSensitivityMatrix, max_i + 1, 7); // and create new matrix of correct size
  gCurLine := 0;
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
        0: DrawDummySensitivityPlot;
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
         end;
        19:
        begin
          TBG := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(TBG);
        end;
        20:
        begin
          TBPA := gMinXPar + i * interval;
          PredictEquilibrium;
          DrawCurves(TBPA);
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
  FillCheckGrid;
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
  SensitivityAnalysisForm.TT4ColorBox.Selected := gDefaultColors[5];
  SensitivityAnalysisForm.TT3ColorBox.Selected := gDefaultColors[7];
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

procedure TSensitivityAnalysisForm.ChartToolset1DataPointClickTool1PointClick(
  ATool: TChartTool; APoint: TPoint);
var
  theTitle, theUnit: String;
  x, y: Double;
  begin
    theTitle := ExtractDelimited(1, Chart1.LeftAxis.Title.Caption, [':']);
    theUnit := ExtractDelimited(2, Chart1.LeftAxis.Title.Caption, [':']);
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

procedure TSensitivityAnalysisForm.CutItemClick(Sender: TObject);
begin

end;

procedure TSensitivityAnalysisForm.FormActivate(Sender: TObject);
begin
  if Screen.Width < SensitivityAnalysisForm.Left + SensitivityAnalysisForm.Width then
    SensitivityAnalysisForm.Width := Screen.Width - SensitivityAnalysisForm.Left - 13;
  SimThyrToolbar.SelectAllMenuItem.Enabled := false;
  gLastActiveCustomForm := SensitivityAnalysisForm;
end;

procedure TSensitivityAnalysisForm.MaxSpinEditChange(Sender: TObject);
begin
  if MaxSpinEdit.Value <
    MinSpinEdit.Value then
    {adapts boundaries to avoid negative intervals}
  begin
    bell;
    MinSpinEdit.Value :=
      MaxSpinEdit.Value;
  end;
  gMinXPar := MinSpinEdit.Value / gSpinFactor;
  gMaxXPar := MaxSpinEdit.Value / gSpinFactor;
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.MinSpinEditChange(Sender: TObject);
begin
  if MaxSpinEdit.Value <
    MinSpinEdit.Value then
    {adapts boundaries to avoid negative intervals}
  begin
    { don't ring bell here to avoid sound after change of Structure Parameter }
    MaxSpinEdit.Value :=
      MinSpinEdit.Value;
  end;
  gMinXPar := MinSpinEdit.Value / gSpinFactor;
  gMaxXPar := MaxSpinEdit.Value / gSpinFactor;
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.PasteItemClick(Sender: TObject);
begin

end;

procedure TSensitivityAnalysisForm.ResetButtonClick(Sender: TObject);
begin
  SensitivityAnalysisForm.StrucParCombo.Enabled := false; {fixes error #8}
  SetStandardStrucParBoundaries(1 / 3, 3);
  DrawOWSensitivityPlot(False);
  SensitivityAnalysisForm.StrucParCombo.Enabled := true;
end;

procedure TSensitivityAnalysisForm.StrucParComboChange(Sender: TObject);
begin
  if SensitivityAnalysisForm.StrucParCombo.Text <> '' then
  begin
    SensitivityAnalysisForm.StrucParCombo.Enabled := false; {fixes error #8}
    SetBottomAxisCaption;
    {ItemIndex is evaluated in the SetStandardStrucParBoundaries and plot routine}
    SetStandardStrucParBoundaries(1 / 3, 3);
    DrawOWSensitivityPlot(False);
    SensitivityAnalysisForm.StrucParCombo.Enabled := true;
  end;
end;

procedure TSensitivityAnalysisForm.TableButtonClick(Sender: TObject);
begin
  if not CheckGrid.Visible then
  begin
    CheckGrid.Visible := true;
    Chart1.Visible := false;
  end
  else
  begin
    CheckGrid.Visible := false;
    Chart1.Visible := true;
  end;
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
    {Chart1.CopyToClipboardBitmap doesn't work on Mac OS X}
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

procedure TSensitivityAnalysisForm.SaveChart;
{saves chart as bitmap or SVG file}
var
  theFileName:  string;
  theFilterIndex: integer;
  theStream: TFileStream;
  theDrawer: IChartDrawer;
begin
  if Chart1 = nil then
    bell
  else
  begin
    theStream := nil;
    SimThyrToolbar.SavePictureDialog2.FilterIndex := 2;
    SimThyrToolbar.SavePictureDialog2.FileName := '';
    if SimThyrToolbar.SavePictureDialog2.Execute then
      try
        theFileName    := SimThyrToolbar.SavePictureDialog2.FileName;
        theFilterIndex := SimThyrToolbar.SavePictureDialog2.FilterIndex;
         {$IFDEF LCLcarbon}{compensates for a bug in older versions of carbon widgetset}
           if (lcl_major < 2) and (lcl_minor < 2) then
             theFilterIndex := theFilterIndex + 1;
         {$ENDIF}
        case theFilterIndex of
        2: Chart1.SaveToBitmapFile(theFileName);
        3: Chart1.SaveToFile(TPixmap, theFileName);
        4: Chart1.SaveToFile(TPortableNetworkGraphic, theFileName);
        5: Chart1.SaveToFile(TPortableAnyMapGraphic, theFileName);
        6: Chart1.SaveToFile(TJPEGImage, theFileName);
        7: Chart1.SaveToFile(TTIFFImage, theFileName);
        8: begin
             theStream := TFileStream.Create(theFileName, fmCreate);
             theDrawer := TSVGDrawer.Create(theStream, true);
             theDrawer.DoChartColorToFPColor := @ChartColorSysToFPColor;
             with Chart1 do
               Draw(theDrawer, Rect(0, 0, Width, Height));
           end;
        9: SaveGrid(theFileName, 't');
        10: SaveGrid(theFilename, 'c');
        11: SaveGrid(theFileName, 'd');
        otherwise bell;
        end;
      finally
        if theStream <> nil then theStream.Free;
      end;
  end;
end;

procedure TSensitivityAnalysisForm.SaveGrid(theFileName: String;
  theDelimiter: Char);
{saves the contents of the table of values}
{file type and, where applicable, delimiter are defined by variable theDelimiter}
var
  theCode: integer;
begin
  SaveGridToFile(CheckGrid, theFileName, theDelimiter, theCode);
  if theCode = 0 then
    SetFileName(self, theFileName);
end;

procedure TSensitivityAnalysisForm.TT3ColorBoxChange(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.TT4ColorBoxChange(Sender: TObject);
begin
  DrawOWSensitivityPlot(False);
end;

procedure TSensitivityAnalysisForm.UndoItemClick(Sender: TObject);
begin

end;

initialization
  {$I sensitivityanalysis.lrs}

end.

