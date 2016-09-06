unit Equilibriumdiagram;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2016 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2016 }

{ This unit implements an equilibrium diagram or nullcline plot }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

{ Nomenclature: }
{ BPars: Behavioural parameters (e.g. TSH, T4, T3) }
{ SPars: Structure parameters (e.g. GT, GD, LS etc.) }

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TATransformations,
  TANavigation, TATools, TAStyles, TAChartListbox, TAChartExtentLink,
  TAChartImageList, TADrawerSVG, TADrawUtils, TADrawerCanvas, LResources,
  Forms,Controls, Graphics, Dialogs, Buttons, LCLVersion,
  ExtCtrls, StdCtrls, Spin, ComCtrls, ColorBox, Clipbrd, Menus, {ComboEx,} Math,
  SimThyrTypes, SimThyrResources, Simulator, SimThyrServices, UnitConverter,
  SimThyrPrediction, Sensitivityanalysis, Types;

const
  MAX_SERIES     = 2;    // number of isoclines to draw
  MAX_I          = 100;  // number of samples for tResponseCurve
  MAX_PIT        = 130;  // number of iterations to get pituitary nullcline
  FIRST_VALUE    = 26;   // first value to use as representative for equilibrium
  TRACK_RATIO    = 10;   // ratio to translate from paramter to track bar position
  SPLAY_FACTOR_S = 5;    // splay of trackbars for structure parameters
  SPLAY_FACTOR_B = 3;    // splay of behavioural parameters
  MIN_LOG_VAL    = 1e-2; // minimum value for logarithmic axes

type

  { TEquilibriumDiagramForm }

  TEquilibriumDiagramForm = class(TForm)
    ChartToolset1: TChartToolset;
    ChartToolset1DataPointClickTool1: TDataPointClickTool;
    ChartToolset1ZoomDragTool1: TZoomDragTool;
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    Divider1: TMenuItem;
    PasteItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    SParEdit1: TEdit;
    EquilibriumChart: TChart;
    ChartNavPanel1: TChartNavPanel;
    EquilibriumChartLineSeries1: TLineSeries;
    EquilibriumChartLineSeries2: TLineSeries;
    GroupBox2:     TGroupBox;
    MaxSpinEdit1:  TFloatSpinEdit;
    MaxSpinEdit2:  TFloatSpinEdit;
    MinSpinEdit1:  TFloatSpinEdit;
    MinSpinEdit2:  TFloatSpinEdit;
    SParCombo1:    TComboBox;
    BParCombo1:    TComboBox;
    BParCombo2:    TComboBox;
    SParCombo2:    TComboBox;
    SParCombo3:    TComboBox;
    SParEdit2: TEdit;
    SParEdit3: TEdit;
    SParTrackBar1: TTrackBar;
    SParTrackBar2: TTrackBar;
    SParTrackBar3: TTrackBar;
    UndoItem: TMenuItem;
    xColorBox:     TColorBox;
    FullScaleButton1: TSpeedButton;
    GroupBox1:     TGroupBox;
    MainPanel:     TPanel;
    ResetButton:   TSpeedButton;
    StatusBar1:    TStatusBar;
    yColorBox:     TColorBox;
    procedure ChartToolset1DataPointClickTool1PointClick(ATool: TChartTool;
      APoint: TPoint);
    procedure CopyItemClick(Sender: TObject);
    procedure CopyChart;
    procedure SaveChart;
    procedure FormActivate(Sender: TObject);
    procedure SetStandardStrucParBoundaries;
    procedure GetBParameters;
    procedure SetSpinEditBoundaries;
    procedure SParEdit1Change(Sender: TObject);
    procedure SParEdit2Change(Sender: TObject);
    procedure SParEdit3Change(Sender: TObject);
    procedure UpdateEditsfromTrackBars;
    procedure UpdateTrackBarsfromEdits;
    procedure DrawDummyEquilibriumPlot;
    procedure DrawDiagram(empty: boolean);
    procedure BParCombo1Change(Sender: TObject);
    procedure BParCombo2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure MaxSpinEdit1Change(Sender: TObject);
    procedure MaxSpinEdit2Change(Sender: TObject);
    procedure MinSpinEdit1Change(Sender: TObject);
    procedure MinSpinEdit2Change(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure SParCombo1Change(Sender: TObject);
    procedure SParCombo2Change(Sender: TObject);
    procedure SParCombo3Change(Sender: TObject);
    procedure SParTrackBar1Change(Sender: TObject);
    procedure SParTrackBar2Change(Sender: TObject);
    procedure SParTrackBar3Change(Sender: TObject);
    procedure xColorBoxChange(Sender: TObject);
    procedure yColorBoxChange(Sender: TObject);
  private
    { private declarations }
    FLine: array[0..MAX_SERIES - 1] of TLineSeries;
  public
    { public declarations }
  end;

  tParamVector = array[0..MAX_I] of extended;
  tResponseCurve = record
    input, output: tParamVector;
  end;
  tTSHSamples = array[0..MAX_PIT - 1] of extended;

var
  EquilibriumDiagramForm: TEquilibriumDiagramForm;
  gSelectedBParameter1, gSelectedBParameter2: tBParameter;
  gSelectedSParameter1, gSelectedSParameter2, gSelectedSParameter3: tSParameter;
  gMinSPar1, gMaxSPar1, gMinSPar2, gMaxSPar2, gMinSPar3, gMaxSPar3: real;
  gMidSPar1, gMidSPar2, gMidSPar3: real;
  gSpinFactor, gTrackFactor1, gTrackFactor2, gTrackFactor3: real;
  gResponseCurve1, gResponseCurve2: tResponseCurve;
  gEmptyVector: tParamVector;
  gFT4conversionFactor, gFT3conversionFactor: real;
  gcT3conversionFactor: real;
  TRH1: real; // storage for TRH concentration from previous simulation run
  gUOM1, gUOM2: string;
  automatedSpin: boolean;


implementation

uses
  SimThyrMain;

procedure UpdateStrucPar(theParameter: tSParameter; theValue: real);
{ updates structure parameters after track bars have been changed }
begin
  case theParameter of
    GD1Item: GD1 := theValue;
    GD2Item: GD2 := theValue;
    KM1Item: kM1 := theValue;
    KM2Item: kM2 := theValue;
    GTItem: GT := theValue;
    DTItem: DT := theValue;
    GHItem: GH := theValue;
    DHItem: DH := theValue;
    SSItem: SS := theValue;
    DSItem: DS := theValue;
    GRItem: GR := theValue;
    DRItem: DR := theValue;
    LSItem: LS := theValue;
    betaTItem: betaT := theValue;
    TBGItem: TBG := theValue;
    TBPAItem: TBPA := theValue;
  end;
end;

function SimPituitaryResponse(T3zVector: tParamVector): tParamVector;
{ Simulate response of pituitary subsystem to a vector with cT3 values }
var
  i, j: integer;
  gainOfTSH, usFeedbackGain: real;
  TSHSamples: tTSHSamples;
begin
  gainOfTSH := alphaS / betaS;
  usFeedbackGain := alphaS2 / betaS2;
  for i := 0 to MAX_I do
  begin
    T3z := T3zVector[i];
    { Take multiple samples and ... }
    for j := 0 to MAX_PIT - 1 do
    begin
      SimPituitary(gainOfTSH, usFeedbackGain, false);
      TSHSamples[j] := TSH;
    end;
    { ... calculate mean of samples in equilibrium in order to ... }
    { ... compensate for oscillations resulting from ultrashort feedback }
    Result[i] := mean(TSHSamples[FIRST_VALUE..MAX_PIT-1]);
  end;
end;

function SimThyroidResponse(TSHVector: tParamVector): tParamVector;
{ Simulate response of thyroid subsystem to vector with TSH values }
var
  i: integer;
  gainOfT4: real;
begin
  gainOfT4 := alphaT / betaT;
  for i := 0 to MAX_I do
  begin
    TSH := TSHVector[i];
    SimThyroidGland(gainOfT4, false);
    Result[i] := FT4;
  end;
end;

function SimPDeiodinaseResponse(FT4Vector: tParamVector): tParamVector;
{ Simulate response of peripheral deiodinases to T4 vector }
var
  i: integer;
  gainOfPeripheralT3: real;
begin
  gainOfPeripheralT3 := alpha31 / beta31;
  for i := 0 to MAX_I do
  begin
    FT4 := FT4Vector[i];
    SimPeripheralDeiodination(gainOfPeripheralT3, false);
    Result[i] := FT3;
  end;
end;

function SimCDeiodinaseResponse(FT4Vector: tParamVector): tParamVector;
{ Simulate response of central type 2 deiodinase to T4 vector }
var
  i: integer;
  gainOfCentralT3: real;
begin
  gainOfCentralT3 := alpha32 / beta32;
  for i := 0 to MAX_I do
  begin
    FT4 := FT4Vector[i];
    SimCentralDeiodination(gainOfCentralT3, false);
    Result[i] := T3z;
  end;
end;

procedure RecalculateConversionFactors;
begin
  gFT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT4_pos]);
  gFT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT3_pos]);
  gcT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[cT3_pos]);
end;

function SimSubsystemResponse(bParameter1, bParameter2: tBParameter;
  min, max: real; var conversionFactor1, conversionFactor2: real): tResponseCurve;
{ Prepare and dispatch calls to partial simulators of subsystems of the open loop }
var
  i: integer;
  interval: real;
  inputVector: tParamVector;
begin
  assert(min >= 0, kError101);
  assert(max >= min, kError103);
  assert(max > 0, kError104);
  TRH1 := TRH; // remember TRH concentration from previous simulation run
  TRH := TRH0; // and set TRH concentration to standard value
  interval := (max - min) / MAX_I;
  case bParameter1 of // input (independent parameter)
    TSHItem:
    begin
      conversionFactor1 := 1;
      for i := 0 to MAX_I do
      begin
        TSH := min + i * interval;
        inputVector[i] := TSH;
      end;
    end;
    FT4Item:
    begin
      conversionFactor1 := gFT4conversionFactor;
      for i := 0 to MAX_I do
      begin
        FT4 := min + i * interval;
        inputVector[i] := FT4 / conversionFactor1;
      end;
    end;
    cT3Item:
    begin
      conversionFactor1 := gFT3conversionFactor;
      for i := 0 to MAX_I do
      begin
        T3z := min + i * interval;
        inputVector[i] := T3z / conversionFactor1;
      end;
    end;
    otherwise
    begin
      inputVector := gEmptyVector;
    end;
  end;
  case bParameter2 of // output (dependent parameter)
    IItem:
    begin
      Result.output := gEmptyVector
    end;
    TSHItem:
    begin
      conversionFactor2 := 1;
      case bParameter1 of
        FT4Item:
        begin;
          Result.output := SimPituitaryResponse(SimCDeiodinaseResponse(inputVector));
        end;
        cT3Item:
        begin;
          Result.output := SimPituitaryResponse(inputVector);
        end;
        otherwise
          Result.output := gEmptyVector;
      end;
    end;
    FT4Item:
    begin
      conversionFactor2 := gFT4conversionFactor;
      case bParameter1 of
        TSHItem:
        begin
          Result.output := SimThyroidResponse(inputVector);
        end;
        cT3Item:
        begin
          Result.output := SimThyroidResponse(SimPituitaryResponse(inputVector));
        end;
        otherwise
          Result.output := gEmptyVector;
      end;
    end;
    FT3Item:
    begin
      conversionFactor2 := gFT3conversionFactor;
      case bParameter1 of
        TSHItem:
        begin
          Result.output := SimPDeiodinaseResponse(SimThyroidResponse(inputVector));
        end;
        FT4Item:
        begin
          Result.output := SimPDeiodinaseResponse(inputVector);
        end;
        cT3Item:
        begin
          Result.output := SimPDeiodinaseResponse(SimThyroidResponse(SimPituitaryResponse(inputVector)));
        end;
        otherwise
          Result.output := gEmptyVector;
      end;
    end;
    cT3Item:
    begin
      conversionFactor2 := gFT3conversionFactor;
      case bParameter1 of
        TSHItem:
        begin
          Result.output := SimCDeiodinaseResponse(SimThyroidResponse(inputVector));
        end;
        FT4Item:
        begin
          Result.output := SimCDeiodinaseResponse(inputVector);
        end;
        otherwise
          Result.output := gEmptyVector;
      end;
    end;
    otherwise
      Result.output := gEmptyVector;
  end;
  Result.input := inputVector;
  TRH := TRH1; // restore simulated TRH concentration
end;

{ TEquilibriumDiagramForm }

procedure TEquilibriumDiagramForm.SetStandardStrucParBoundaries;
{ sets boundaries and positions of trackbars according to selected parameter }
var
  tempMinS, tempMaxS, tempMidS: real; {necessary to hinder Windows from altering the globals}
begin
  case gSelectedSParameter1 of
    GD1Item:
    begin
      gTrackFactor1 := GD1_FACTOR;
      gMinSPar1 := GD1 / SPLAY_FACTOR_S;
      gMaxSPar1 := GD1 * SPLAY_FACTOR_S;
      gMidSPar1 := GD1;
    end;
    GD2Item:
    begin
      gTrackFactor1 := GD2_FACTOR;
      gMinSPar1 := GD2 / SPLAY_FACTOR_S;
      gMaxSPar1 := GD2 * SPLAY_FACTOR_S;
      gMidSPar1 := GD2;
    end;
    KM1Item:
    begin
      gTrackFactor1 := kM1_FACTOR;
      gMinSPar1 := kM1 / SPLAY_FACTOR_S;
      gMaxSPar1 := kM1 * SPLAY_FACTOR_S;
      gMidSPar1 := kM1;
    end;
    KM2Item:
    begin
      gTrackFactor1 := KM2_FACTOR;
      gMinSPar1 := kM2 / SPLAY_FACTOR_S;
      gMaxSPar1 := kM2 * SPLAY_FACTOR_S;
      gMidSPar1 := kM2;
    end;
    GTItem:
    begin
      gTrackFactor1 := GT_FACTOR;
      gMinSPar1 := GT / SPLAY_FACTOR_S;
      gMaxSPar1 := GT * SPLAY_FACTOR_S;
      gMidSPar1 := GT;
    end;
    DTItem:
    begin
      gTrackFactor1 := DT_FACTOR;
      gMinSPar1 := DT / SPLAY_FACTOR_S;
      gMaxSPar1 := DT * SPLAY_FACTOR_S;
      gMidSPar1 := DT;
    end;
    GHItem:
    begin
      gTrackFactor1 := GH_FACTOR;
      gMinSPar1 := GH / SPLAY_FACTOR_S;
      gMaxSPar1 := GH * SPLAY_FACTOR_S;
      gMidSPar1 := GH;
    end;
    DHItem:
    begin
      gTrackFactor1 := DH_FACTOR;
      gMinSPar1 := DH / SPLAY_FACTOR_S;
      gMaxSPar1 := DH * SPLAY_FACTOR_S;
      gMidSPar1 := DH;
    end;
    SSItem:
    begin
      gTrackFactor1 := SS_FACTOR;
      gMinSPar1 := SS / SPLAY_FACTOR_S;
      gMaxSPar1 := SS * SPLAY_FACTOR_S;
      gMidSPar1 := SS;
    end;
    DSItem:
    begin
      gTrackFactor1 := DS_FACTOR;
      gMinSPar1 := DS / SPLAY_FACTOR_S;
      gMaxSPar1 := DS * SPLAY_FACTOR_S;
      gMidSPar1 := DS;
    end;
    GRItem:
    begin
      gTrackFactor1 := GR_FACTOR;
      gMinSPar1 := GR / SPLAY_FACTOR_S;
      gMaxSPar1 := GR * SPLAY_FACTOR_S;
      gMidSPar1 := GR;
    end;
    DRItem:
    begin
      gTrackFactor1 := DR_FACTOR;
      gMinSPar1 := DR / SPLAY_FACTOR_S;
      gMaxSPar1 := DR * SPLAY_FACTOR_S;
      gMidSPar1 := DR;
    end;
    LSItem:
    begin
      gTrackFactor1 := LS_FACTOR;
      gMinSPar1 := LS / SPLAY_FACTOR_S;
      gMaxSPar1 := LS * SPLAY_FACTOR_S;
      gMidSPar1 := LS;
    end;
    betaTItem:
    begin
      gTrackFactor1 := betaT_FACTOR;
      gMinSPar1 := betaT / SPLAY_FACTOR_S;
      gMaxSPar1 := betaT * SPLAY_FACTOR_S;
      gMidSPar1 := betaT;
    end;
    TBGItem:
    begin
      gTrackFactor1 := TBG_FACTOR;
      gMinSPar1 := TBG / SPLAY_FACTOR_S;
      gMaxSPar1 := TBG * SPLAY_FACTOR_S;
      gMidSPar1 := TBG;
    end;
    TBPAItem:
    begin
      gTrackFactor1 := TBPA_FACTOR;
      gMinSPar1 := TBPA / SPLAY_FACTOR_S;
      gMaxSPar1 := TBPA * SPLAY_FACTOR_S;
      gMidSPar1 := TBPA;
    end;
    otherwise
    begin
      gTrackFactor1 := 1;
      gMinSPar1 := 0;
      gMaxSPar1 := 50;
      gMidSPar1 := 0;
    end;
  end;
  tempMinS := gMinSPar1;
  tempMaxS := gMaxSPar1;
  tempMidS := gMidSPar1;
  SParTrackBar1.Min := trunc(tempMinS * gTrackFactor1 * TRACK_RATIO);
  SParTrackBar1.Max := trunc(tempMaxS * gTrackFactor1 * TRACK_RATIO);
  SParTrackBar1.Position := trunc(tempMidS * gTrackFactor1 * TRACK_RATIO);
  gMinSPar1 := tempMinS;
  gMaxSPar1 := tempMaxS;
  case gSelectedSParameter2 of
    GD1Item:
    begin
      gTrackFactor2 := GD1_FACTOR;
      gMinSPar2 := GD1 / SPLAY_FACTOR_S;
      gMaxSPar2 := GD1 * SPLAY_FACTOR_S;
      gMidSPar2 := GD1;
    end;
    GD2Item:
    begin
      gTrackFactor2 := GD2_FACTOR;
      gMinSPar2 := GD2 / SPLAY_FACTOR_S;
      gMaxSPar2 := GD2 * SPLAY_FACTOR_S;
      gMidSPar2 := GD2;
    end;
    KM1Item:
    begin
      gTrackFactor2 := kM1_FACTOR;
      gMinSPar2 := kM1 / SPLAY_FACTOR_S;
      gMaxSPar2 := kM1 * SPLAY_FACTOR_S;
      gMidSPar2 := kM1;
    end;
    KM2Item:
    begin
      gTrackFactor2 := KM2_FACTOR;
      gMinSPar2 := KM2 / SPLAY_FACTOR_S;
      gMaxSPar2 := KM2 * SPLAY_FACTOR_S;
      gMidSPar2 := kM2;
    end;
    GTItem:
    begin
      gTrackFactor2 := GT_FACTOR;
      gMinSPar2 := GT / SPLAY_FACTOR_S;
      gMaxSPar2 := GT * SPLAY_FACTOR_S;
      gMidSPar2 := GT;
    end;
    DTItem:
    begin
      gTrackFactor2 := DT_FACTOR;
      gMinSPar2 := DT / SPLAY_FACTOR_S;
      gMaxSPar2 := DT * SPLAY_FACTOR_S;
      gMidSPar2 := DT;
    end;
    GHItem:
    begin
      gTrackFactor2 := GH_FACTOR;
      gMinSPar2 := GH / SPLAY_FACTOR_S;
      gMaxSPar2 := GH * SPLAY_FACTOR_S;
      gMidSPar2 := GH;
    end;
    DHItem:
    begin
      gTrackFactor2 := DH_FACTOR;
      gMinSPar2 := DH / SPLAY_FACTOR_S;
      gMaxSPar2 := DH * SPLAY_FACTOR_S;
      gMidSPar2 := DH;
    end;
    SSItem:
    begin
      gTrackFactor2 := SS_FACTOR;
      gMinSPar2 := SS / SPLAY_FACTOR_S;
      gMaxSPar2 := SS * SPLAY_FACTOR_S;
      gMidSPar2 := SS;
    end;
    DSItem:
    begin
      gTrackFactor2 := DS_FACTOR;
      gMinSPar2 := DS / SPLAY_FACTOR_S;
      gMaxSPar2 := DS * SPLAY_FACTOR_S;
      gMidSPar2 := DS;
    end;
    GRItem:
    begin
      gTrackFactor2 := GR_FACTOR;
      gMinSPar2 := GR / SPLAY_FACTOR_S;
      gMaxSPar2 := GR * SPLAY_FACTOR_S;
      gMidSPar2 := GR;
    end;
    DRItem:
    begin
      gTrackFactor2 := DR_FACTOR;
      gMinSPar2 := DR / SPLAY_FACTOR_S;
      gMaxSPar2 := DR * SPLAY_FACTOR_S;
      gMidSPar2 := DR;
    end;
    LSItem:
    begin
      gTrackFactor2 := LS_FACTOR;
      gMinSPar2 := LS / SPLAY_FACTOR_S;
      gMaxSPar2 := LS * SPLAY_FACTOR_S;
      gMidSPar2 := LS;
    end;
    betaTItem:
    begin
      gTrackFactor2 := betaT_FACTOR;
      gMinSPar2 := betaT / SPLAY_FACTOR_S;
      gMaxSPar2 := betaT * SPLAY_FACTOR_S;
      gMidSPar2 := betaT;
    end;
    TBGItem:
    begin
      gTrackFactor2 := TBG_FACTOR;
      gMinSPar2 := TBG / SPLAY_FACTOR_S;
      gMaxSPar2 := TBG * SPLAY_FACTOR_S;
      gMidSPar2 := TBG;
    end;
    TBPAItem:
    begin
      gTrackFactor2 := TBPA_FACTOR;
      gMinSPar2 := TBPA / SPLAY_FACTOR_S;
      gMaxSPar2 := TBPA * SPLAY_FACTOR_S;
      gMidSPar2 := TBPA;
    end;
    otherwise
    begin
      gTrackFactor2 := 1;
      gMinSPar2 := 0;
      gMaxSPar2 := 50;
      gMidSPar2 := 0;
    end;
  end;
  tempMinS := gMinSPar2;
  tempMaxS := gMaxSPar2;
  tempMidS := gMidSPar2;
  SParTrackBar2.Min := trunc(tempMinS * gTrackFactor2 * TRACK_RATIO);
  SParTrackBar2.Max := trunc(tempMaxS * gTrackFactor2 * TRACK_RATIO);
  SParTrackBar2.Position := trunc(tempMidS * gTrackFactor2 * TRACK_RATIO);
  gMinSPar2 := tempMinS;
  gMaxSPar2 := tempMaxS;
  case gSelectedSParameter3 of
    GD1Item:
    begin
      gTrackFactor3 := GD1_FACTOR;
      gMinSPar3 := GD1 / SPLAY_FACTOR_S;
      gMaxSPar3 := GD1 * SPLAY_FACTOR_S;
      gMidSPar3 := GD1
    end;
    GD2Item:
    begin
      gTrackFactor3 := GD2_FACTOR;
      gMinSPar3 := GD2 / SPLAY_FACTOR_S;
      gMaxSPar3 := GD2 * SPLAY_FACTOR_S;
      gMidSPar3 := GD2
    end;
    KM1Item:
    begin
      gTrackFactor3 := kM1_FACTOR;
      gMinSPar3 := kM1 / SPLAY_FACTOR_S;
      gMaxSPar3 := kM1 * SPLAY_FACTOR_S;
      gMidSPar3 := kM1;
    end;
    KM2Item:
    begin
      gTrackFactor3 := KM2_FACTOR;
      gMinSPar3 := KM2 / SPLAY_FACTOR_S;
      gMaxSPar3 := KM2 * SPLAY_FACTOR_S;
      gMidSPar3 := kM2;
    end;
    GTItem:
    begin
      gTrackFactor3 := GT_FACTOR;
      gMinSPar3 := GT / SPLAY_FACTOR_S;
      gMaxSPar3 := GT * SPLAY_FACTOR_S;
      gMidSPar3 := GT;
    end;
    DTItem:
    begin
      gTrackFactor3 := DT_FACTOR;
      gMinSPar3 := DT / SPLAY_FACTOR_S;
      gMaxSPar3 := DT * SPLAY_FACTOR_S;
      gMidSPar3 := DT;
    end;
    GHItem:
    begin
      gTrackFactor3 := GH_FACTOR;
      gMinSPar3 := GH / SPLAY_FACTOR_S;
      gMaxSPar3 := GH * SPLAY_FACTOR_S;
      gMidSPar3 := GH;
    end;
    DHItem:
    begin
      gTrackFactor3 := DH_FACTOR;
      gMinSPar3 := DH / SPLAY_FACTOR_S;
      gMaxSPar3 := DH * SPLAY_FACTOR_S;
      gMidSPar3 := DH;
    end;
    SSItem:
    begin
      gTrackFactor3 := SS_FACTOR;
      gMinSPar3 := SS / SPLAY_FACTOR_S;
      gMaxSPar3 := SS * SPLAY_FACTOR_S;
      gMidSPar3 := SS;
    end;
    DSItem:
    begin
      gTrackFactor3 := DS_FACTOR;
      gMinSPar3 := DS / SPLAY_FACTOR_S;
      gMaxSPar3 := DS * SPLAY_FACTOR_S;
      gMidSPar3 := DS;
    end;
    GRItem:
    begin
      gTrackFactor3 := GR_FACTOR;
      gMinSPar3 := GR / SPLAY_FACTOR_S;
      gMaxSPar3 := GR * SPLAY_FACTOR_S;
      gMidSPar3 := GR;
    end;
    DRItem:
    begin
      gTrackFactor3 := DR_FACTOR;
      gMinSPar3 := DR / SPLAY_FACTOR_S;
      gMaxSPar3 := DR * SPLAY_FACTOR_S;
      gMidSPar3 := DR;
    end;
    LSItem:
    begin
      gTrackFactor3 := LS_FACTOR;
      gMinSPar3 := LS / SPLAY_FACTOR_S;
      gMaxSPar3 := LS * SPLAY_FACTOR_S;
      gMidSPar3 := LS;
    end;
    betaTItem:
    begin
      gTrackFactor3 := betaT_FACTOR;
      gMinSPar3 := betaT / SPLAY_FACTOR_S;
      gMaxSPar3 := betaT * SPLAY_FACTOR_S;
      gMidSPar3 := betaT;
    end;
    TBGItem:
    begin
      gTrackFactor3 := TBG_FACTOR;
      gMinSPar3 := TBG / SPLAY_FACTOR_S;
      gMaxSPar3 := TBG * SPLAY_FACTOR_S;
      gMidSPar3 := TBG;
    end;
    TBPAItem:
    begin
      gTrackFactor3 := TBPA_FACTOR;
      gMinSPar3 := TBPA / SPLAY_FACTOR_S;
      gMaxSPar3 := TBPA * SPLAY_FACTOR_S;
      gMidSPar3 := TBPA;
    end;
    otherwise
    begin
      gTrackFactor3 := 1;
      gMinSPar3 := 0;
      gMaxSPar3 := 50;
      gMidSPar3 := 0;
    end;
  end;
  tempMinS := gMinSPar3;
  tempMaxS := gMaxSPar3;
  tempMidS := gMidSPar3;
  SParTrackBar3.Min := trunc(tempMinS * gTrackFactor3 * TRACK_RATIO);
  SParTrackBar3.Max := trunc(tempMaxS * gTrackFactor3 * TRACK_RATIO);
  SParTrackBar3.Position := trunc(tempMidS * gTrackFactor3 * TRACK_RATIO);
  gMinSPar3 := tempMinS; // avoids certain Windows-specific problems
  gMaxSPar3 := tempMaxS;
end;

procedure TEquilibriumDiagramForm.GetBParameters;
{ Check, which behavioural parameters are to be inspected }
begin
  { Get parameter for x axis: }
  if pos(LowerCase('TSH'), LowerCase(BParCombo1.Text)) > 0 then
    begin
      gSelectedBParameter1 := TSHItem;
      gUOM1 := gParameterUnit[TSH_pos];
    end
  else if pos(LowerCase('FT4'), LowerCase(BParCombo1.Text)) > 0 then
    begin
      gSelectedBParameter1 := FT4Item;
      gUOM1 := gParameterUnit[FT4_pos];
    end
  else if pos(LowerCase('FT3'), LowerCase(BParCombo1.Text)) > 0 then
    begin
      gSelectedBParameter1 := FT3Item;
      gUOM1 := gParameterUnit[FT3_pos];
    end
  else if pos(LowerCase('cT3'), LowerCase(BParCombo1.Text)) > 0 then
    begin
      gSelectedBParameter1 := cT3Item;
      gUOM1 := gParameterUnit[cT3_pos];
    end
  else
    begin
      gSelectedBParameter1 := IItem;
      gUOM1 := '';
    end;
  { Get parameter for y axis: }
  if pos(LowerCase('TSH'), LowerCase(BParCombo2.Text)) > 0 then
    begin
      gSelectedBParameter2 := TSHItem;
      gUOM2 := gParameterUnit[TSH_pos];
    end
  else if pos(LowerCase('FT4'), LowerCase(BParCombo2.Text)) > 0 then
    begin
      gSelectedBParameter2 := FT4Item;
      gUOM2 := gParameterUnit[FT4_pos];
    end
  else if pos(LowerCase('FT3'), LowerCase(BParCombo2.Text)) > 0 then
    begin
      gSelectedBParameter2 := FT3Item;
      gUOM2 := gParameterUnit[FT3_pos];
    end
  else if pos(LowerCase('cT3'), LowerCase(BParCombo2.Text)) > 0 then
    begin
      gSelectedBParameter2 := cT3Item;
      gUOM2 := gParameterUnit[cT3_pos];
    end
  else
    begin
      gSelectedBParameter2 := IItem;
      gUOM2 := '';
    end;
end;

procedure TEquilibriumDiagramForm.SetSpinEditBoundaries;
begin
  automatedSpin := true; // prevents control of spin boundaries by SpinEditChange handler
  case gSelectedBParameter2 of
    TSHItem:
    begin
      MinSpinEdit2.Value := gActiveModel.Equilibrium.TSH1 / SPLAY_FACTOR_B;
      MaxSpinEdit2.Value := gActiveModel.Equilibrium.TSH1 * SPLAY_FACTOR_B;
    end;
    FT4Item:
    begin
      MinSpinEdit2.Value := gActiveModel.Equilibrium.FT41 / SPLAY_FACTOR_B * gFT4conversionFactor;
      MaxSpinEdit2.Value := gActiveModel.Equilibrium.FT41 * SPLAY_FACTOR_B * gFT4conversionFactor;
    end;
    cT3Item:
    begin
      MinSpinEdit2.Value := gActiveModel.Equilibrium.T3z1 / SPLAY_FACTOR_B * gFT3conversionFactor;
      MaxSpinEdit2.Value := gActiveModel.Equilibrium.T3z1 * SPLAY_FACTOR_B * gFT3conversionFactor;
    end;
  end;
  automatedSpin := false;
end;

procedure TEquilibriumDiagramForm.SParEdit1Change(Sender: TObject);
begin
  { Get numeric value for structure Parameter 1 }
  UpdateTrackBarsfromEdits;
  SaveStrucPars;
  UpdateEditsfromTrackBars;
  UpdateStrucPar(gSelectedSParameter1, SParTrackBar1.Position / gTrackFactor1 / TRACK_RATIO);
  DrawDiagram(false);
end;

procedure TEquilibriumDiagramForm.SParEdit2Change(Sender: TObject);
begin
  { Get numeric value for structure Parameter 2 }
  UpdateTrackBarsfromEdits;
  SaveStrucPars;
  UpdateEditsfromTrackBars;
  UpdateStrucPar(gSelectedSParameter2, SParTrackBar2.Position / gTrackFactor2 / TRACK_RATIO);
  DrawDiagram(false);
end;

procedure TEquilibriumDiagramForm.SParEdit3Change(Sender: TObject);
begin
  { Get numeric value for structure Parameter 3 }
  UpdateTrackBarsfromEdits;
  SaveStrucPars;
  UpdateEditsfromTrackBars;
  UpdateStrucPar(gSelectedSParameter3, SParTrackBar3.Position / gTrackFactor3 / TRACK_RATIO);
  DrawDiagram(false);
end;

procedure TEquilibriumDiagramForm.FormActivate(Sender: TObject);
begin
  //UpdateEditsfromTrackBars;
  gLastActiveCustomForm := EquilibriumDiagramForm;
end;

procedure TEquilibriumDiagramForm.CopyItemClick(Sender: TObject);
{ Copy plot to clipboard }
begin
  CopyChart;
end;

procedure TEquilibriumDiagramForm.ChartToolset1DataPointClickTool1PointClick(
  ATool: TChartTool; APoint: TPoint);
const
  theLegend = 'Selected point on isocline';
var
  x, y: Double;
begin
  with ATool as TDatapointClickTool do
    if (Series is TLineSeries) then
      with TLineSeries(Series) do begin
        x := GetXValue(PointIndex);
        y := GetYValue(PointIndex);
        Statusbar1.SimpleText := Format('%s: x = %f, y = %f', [theLegend, x, y]);
      end
    else
      Statusbar1.SimpleText := '';
end;

procedure TEquilibriumDiagramForm.CopyChart;
{ copy chart to clipboard }
var
  {$IFDEF UNIX} {UNIXoids, i.e. macOS, Linux, Solaris etc.}
  theImage: TPortableNetworkGraphic;
  {$ELSE} {Windows and similar operating systems }
  theImage: TBitMap;
  {$ENDIF}
  theWidth, theHeight: integer;
begin
  if EquilibriumChart = nil then
    bell
  else
  begin
    {Chart1.CopyToClipboardBitmap doesn't work on Mac OS X}
    {$IFDEF UNIX}
    theImage := TPortableNetworkGraphic.Create;
    try
      theWidth := EquilibriumChart.Width;
      theHeight := EquilibriumChart.Height;
      theImage.Width := theWidth;
      theImage.Height := theHeight;
      EquilibriumChart.DrawOnCanvas(rect(0, 0, theImage.Width, theImage.Height), theImage.canvas);
      Clipboard.Assign(theImage);
    finally
      theImage.Free;
    end;
    {$ELSE}
    EquilibriumChart.CopyToClipboardBitmap;
    {$ENDIF}
  end;
end;

procedure TEquilibriumDiagramForm.SaveChart;
{ Save chart to image file }
var
  theFileName:  string;
  theFilterIndex: integer;
  theStream: TFileStream;
  theDrawer: IChartDrawer;
begin
  if EquilibriumChart = nil then
    bell
  else
  begin
    theStream := nil;
    SimThyrToolbar.SavePictureDialog1.FilterIndex := 2;
    if SimThyrToolbar.SavePictureDialog1.Execute then
      try
        theFileName    := SimThyrToolbar.SavePictureDialog1.FileName;
        theFilterIndex := SimThyrToolbar.SavePictureDialog1.FilterIndex;
         {$IFDEF LCLcarbon}{compensates for a bug in older versions of carbon widgetset}
           if (lcl_major < 2) and (lcl_minor < 2) then
             theFilterIndex := theFilterIndex + 1;
         {$ENDIF}
        case theFilterIndex of
        2: EquilibriumChart.SaveToBitmapFile(theFileName);
        3: EquilibriumChart.SaveToFile(TPixmap, theFileName);
        4: EquilibriumChart.SaveToFile(TPortableNetworkGraphic, theFileName);
        5: EquilibriumChart.SaveToFile(TPortableAnyMapGraphic, theFileName);
        6: EquilibriumChart.SaveToFile(TJPEGImage, theFileName);
        7: EquilibriumChart.SaveToFile(TTIFFImage, theFileName);
        8: begin
             theStream := TFileStream.Create(theFileName, fmCreate);
             theDrawer := TSVGDrawer.Create(theStream, true);
             theDrawer.DoChartColorToFPColor := @ChartColorSysToFPColor;
             with EquilibriumChart do
               Draw(theDrawer, Rect(0, 0, Width, Height));
           end;
        otherwise bell;
        end;
      finally
        if theStream <> nil then theStream.Free;
      end;
  end;
end;

procedure TEquilibriumDiagramForm.UpdateEditsfromTrackBars;
begin
  SParEdit1.Text := FloatToStr(SParTrackBar1.Position / TRACK_RATIO);
  SParEdit2.Text := FloatToStr(SParTrackBar2.Position / TRACK_RATIO);
  SParEdit3.Text := FloatToStr(SParTrackBar3.Position / TRACK_RATIO);
end;

procedure TEquilibriumDiagramForm.UpdateTrackBarsfromEdits;
begin
  if (StrToFloatDef(SParEdit1.Text, NaN) * TRACK_RATIO >= SParTrackBar1.Min) and
    (StrToFloatDef(SParEdit1.Text, NaN) * TRACK_RATIO <= SParTrackBar1.Max) then
    SParTrackBar1.Position := trunc(StrToFloatDef(SParEdit1.Text, 0) * TRACK_RATIO)
  else
    SParTrackBar1.Position := SParTrackBar1.Min;
  if (StrToFloatDef(SParEdit2.Text, NaN) * TRACK_RATIO >= SParTrackBar2.Min) and
    (StrToFloatDef(SParEdit2.Text, NaN) * TRACK_RATIO <= SParTrackBar2.Max) then
    SParTrackBar2.Position := trunc(StrToFloatDef(SParEdit2.Text, 0) * TRACK_RATIO)
  else
    SParTrackBar2.Position := SParTrackBar2.Min;
  if (StrToFloatDef(SParEdit3.Text, NaN) * TRACK_RATIO >= SParTrackBar3.Min) and
    (StrToFloatDef(SParEdit3.Text, NaN) * TRACK_RATIO <= SParTrackBar3.Max) then
    SParTrackBar3.Position := trunc(StrToFloatDef(SParEdit3.Text, 0) * TRACK_RATIO)
  else
    SParTrackBar3.Position := SParTrackBar3.Min;
end;

procedure TEquilibriumDiagramForm.DrawDummyEquilibriumPlot;
{Draws an empty plot, e.g. at program start}
var
  i: integer;
begin
  EquilibriumChart.LeftAxis.Title.Caption   := ISOKLINE_2_STRING;
  EquilibriumChart.BottomAxis.Title.Caption := ISOKLINE_1_STRING;
  with FLine[0] do
  begin
    AddXY(0, 0, '', clBlack);
    AddXY(100, 0, '', clBlack);
  end;
  with FLine[1] do
  begin
    AddXY(0, 0, '', clBlack);
    AddXY(0, 100, '', clBlack);
  end;
end;

procedure TEquilibriumDiagramForm.DrawDiagram(empty: boolean);
{ Main draw routine }
{ autoBounds: procedure calculates bounds of plot from data, if TRUE }
{ empty: decides if a dummy plot is drawn (TRUE) or a plot with data (FALSE) }
var
  i, j: integer;
  MinBPar1, MaxBPar1, MinBPar2, MaxBPar2: real;
  ConversionFactor1, ConversionFactor2: real;
  max_x: real;
begin
  ConversionFactor1 := 1;
  ConversionFactor2 := 1;
  GetBParameters; { read, which behavioural parameters have been selected }
  { Recalculate conversion factors, to support change of UOM on the fly: }
  RecalculateConversionFactors;
  {If line series exists it is cleared and recreated to support foundations of redrawing}
  if FLine[1] <> nil then
    EquilibriumChart.ClearSeries;
  for i := 0 to MAX_SERIES - 1 do {{ Prepare line series }}
  begin
    FLine[i] := TLineSeries.Create(EquilibriumChart);
    with FLine[i] do
    begin
      ShowLines   := True;
      ShowPoints  := False;
      Pointer.Brush.Color := clBlack;
      SeriesColor := clBlack;
      LinePen.Mode := pmCopy;
      LinePen.Style := psSolid;
      LinePen.Cosmetic := true;
      LinePen.EndCap := pecRound;
      LinePen.JoinStyle := pjsRound;
      LinePen.Width := 2;
      LineType := ltFromPrevious;
    end;
    EquilibriumChart.AddSeries(FLine[i]);
    FLine[i].BeginUpdate;
  end;
  if empty then
    DrawDummyEquilibriumPlot
  else
  begin
    MinBPar2 := MinSpinEdit2.Value / gSpinFactor;
    MaxBPar2 := MaxSpinEdit2.Value / gSpinFactor;
    EquilibriumChart.LeftAxis.Range.UseMin := true;
    EquilibriumChart.LeftAxis.Range.UseMax := true;
    EquilibriumChart.BottomAxis.Range.UseMin := true;
    EquilibriumChart.BottomAxis.Range.UseMax := true;
    gResponseCurve1 := SimSubsystemResponse(gSelectedBParameter2, gSelectedBParameter1, MinBPar2,
      MaxBPar2, ConversionFactor1, ConversionFactor2);
    for j := 0 to MAX_I do
    begin
      FLine[0].AddXY(gResponseCurve1.input[j] * conversionFactor1,
        gResponseCurve1.output[j] * conversionFactor2, '',
        xColorBox.Selected);
      Fline[0].SeriesColor := xColorBox.Selected;
    end;
    MinBPar1 := 0;
    MaxBPar1 := MaxValue(gResponseCurve1.output);
    if MaxBPar1 <> 0 then
      gResponseCurve2 := SimSubsystemResponse(gSelectedBParameter1, gSelectedBParameter2, MinBPar1,
      MaxBPar1, ConversionFactor1, ConversionFactor2)
    else
      gResponseCurve2.output := gEmptyVector;
    for j := 0 to MAX_I do
    begin
      FLine[1].AddXY(gResponseCurve2.output[j] * conversionFactor2,
        gResponseCurve2.input[j] * conversionFactor1, '',
        yColorBox.Selected);
      Fline[1].SeriesColor := yColorBox.Selected;
    end;
  end;
  for i := 0 to MAX_SERIES - 1 do
    FLine[i].EndUpdate;
  if gSelectedBParameter1 = IItem then
    EquilibriumChart.LeftAxis.Title.Caption := ISOKLINE_2_STRING
  else
    EquilibriumChart.LeftAxis.Title.Caption := BParCombo1.Caption;
  if gSelectedBParameter2 = IItem then
    EquilibriumChart.BottomAxis.Title.Caption := ISOKLINE_1_STRING
  else
    EquilibriumChart.BottomAxis.Title.Caption := BParCombo2.Caption;
end;

procedure TEquilibriumDiagramForm.FormCreate(Sender: TObject);
begin
  gSpinFactor := 1;
  UpdateEditsfromTrackBars;
  DrawDiagram(true);
end;

procedure TEquilibriumDiagramForm.BParCombo1Change(Sender: TObject);
{ Get selected behavioural parameter #1 }
begin
  if BParCombo2.ItemIndex = 0 then
    begin // set selections to useful initial values
      if BParCombo1.ItemIndex = 1 then
        BParCombo2.ItemIndex := 2
      else
        BParCombo2.ItemIndex := 1;
    end;
  GetBParameters;
  RecalculateConversionFactors; { Adapt for change of UOM on the fly }
  SetSpinEditBoundaries;
  xColorBox.Selected := gDefaultColors[integer(gSelectedBParameter1)];
  yColorBox.Selected := gDefaultColors[integer(gSelectedBParameter2)];
  DrawDiagram(false);
end;

procedure TEquilibriumDiagramForm.BParCombo2Change(Sender: TObject);
{ Get selected behavioural parameter #2 }
begin
  if BParCombo1.ItemIndex = 0 then
    begin // set selections to useful initial values
      if BParCombo2.ItemIndex = 1 then
        BParCombo1.ItemIndex := 2
      else
        BParCombo1.ItemIndex := 1;
    end;
  GetBParameters;
  SetSpinEditBoundaries;
  xColorBox.Selected := gDefaultColors[integer(gSelectedBParameter1)];
  yColorBox.Selected := gDefaultColors[integer(gSelectedBParameter2)];
  DrawDiagram(false);
end;

procedure TEquilibriumDiagramForm.FullScaleButton1Click(Sender: TObject);
begin
  EquilibriumChart.Extent.UseYMax := False;
  EquilibriumChart.ZoomFull;
end;

procedure TEquilibriumDiagramForm.MaxSpinEdit1Change(Sender: TObject);
begin
  if not automatedSpin then
    begin
      if MaxSpinEdit1.Value < MinSpinEdit1.Value then
      {adapts boundaries to avoid negative intervals}
      begin
        bell;
        MaxSpinEdit1.Value := MinSpinEdit1.Value;
      end;
      if MaxSpinEdit1.Value = 0 then MaxSpinEdit1.Value := 0.1;
      DrawDiagram(false);
    end;
end;

procedure TEquilibriumDiagramForm.MaxSpinEdit2Change(Sender: TObject);
begin
  if not automatedSpin then
    begin
      if MaxSpinEdit2.Value < MinSpinEdit2.Value then
      {adapts boundaries to avoid negative intervals}
      begin
        bell;
        MaxSpinEdit2.Value := MinSpinEdit2.Value;
      end;
      if MaxSpinEdit2.Value = 0 then MaxSpinEdit2.Value := 0.1;
      DrawDiagram(false);
    end;
end;

procedure TEquilibriumDiagramForm.MinSpinEdit1Change(Sender: TObject);
begin
  if not automatedSpin then
    begin
      if MaxSpinEdit1.Value < MinSpinEdit1.Value then
      {adapts boundaries to avoid negative intervals}
      begin
        bell;
        MinSpinEdit1.Value := MaxSpinEdit1.Value;
      end;
      DrawDiagram(false);
    end;
end;

procedure TEquilibriumDiagramForm.MinSpinEdit2Change(Sender: TObject);
begin
  if not automatedSpin then
    begin
      if MaxSpinEdit2.Value < MinSpinEdit2.Value then
      {adapts boundaries to avoid negative intervals}
      begin
        bell;
        MinSpinEdit2.Value := MaxSpinEdit2.Value;
      end;
      DrawDiagram(false);
    end;
end;

procedure TEquilibriumDiagramForm.ResetButtonClick(Sender: TObject);
{ reset to standard values }
begin
  SParCombo1.ItemIndex := 0;
  SParCombo2.ItemIndex := 0;
  SParCombo3.ItemIndex := 0;
  gSelectedSParameter1 := NullItem;
  gSelectedSParameter2 := NullItem;
  gSelectedSParameter2 := NullItem;
  SParTrackBar1.Position := 0;
  SParTrackBar2.Position := 0;
  SParTrackBar3.Position := 0;
  RestoreStrucPars;
  SetStandardStrucParBoundaries;
  SetSpinEditBoundaries;
  FullScaleButton1Click(Sender);
  DrawDiagram(false);
end;

procedure TEquilibriumDiagramForm.SParCombo1Change(Sender: TObject);
{ read selected structure parameter }
begin
  SaveStrucPars;
  SParTrackBar1.Enabled := false;
  if pos(LowerCase('GD1'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := GD1Item
  else if pos(LowerCase('GD2'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := GD2Item
  else if pos(LowerCase('KM1'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := KM1Item
  else if pos(LowerCase('KM2'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := KM2Item
  else if pos(LowerCase('GT'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := GTItem
  else if pos(LowerCase('DT'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := DTItem
  else if pos(LowerCase('GH'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := GHItem
  else if pos(LowerCase('DH'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := DHItem
  else if pos(LowerCase('SS'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := SSItem
  else if pos(LowerCase('DS'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := DSItem
  else if pos(LowerCase('GR'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := GRItem
  else if pos(LowerCase('DR'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := DRItem
  else if pos(LowerCase('LS'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := LSItem
  else if pos(LowerCase('betaT'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := betaTItem
  else if pos(LowerCase('TBG'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := TBGItem
  else if pos(LowerCase('TBPA'), LowerCase(SParCombo1.Text)) > 0 then
    gSelectedSParameter1 := TBPAItem
  else
    gSelectedSParameter1 := NullItem;
  SetStandardStrucParBoundaries;
  UpdateStrucPar(gSelectedSParameter1, SParTrackBar1.Position / gTrackFactor1 / TRACK_RATIO);
  DrawDiagram(false);
  RestoreStrucPars;
  if gSelectedSParameter1 <> NullItem then
    SParTrackBar1.Enabled := true;
end;

procedure TEquilibriumDiagramForm.SParCombo2Change(Sender: TObject);
{ read selected structure parameter }
begin
  SaveStrucPars;
  SParTrackBar2.Enabled := false;
  if pos(LowerCase('GD1'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := GD1Item
  else if pos(LowerCase('GD2'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := GD2Item
  else if pos(LowerCase('KM1'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := KM1Item
  else if pos(LowerCase('KM2'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := KM2Item
  else if pos(LowerCase('GT'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := GTItem
  else if pos(LowerCase('DT'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := DTItem
  else if pos(LowerCase('GH'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := GHItem
  else if pos(LowerCase('DH'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := DHItem
  else if pos(LowerCase('SS'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := SSItem
  else if pos(LowerCase('DS'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := DSItem
  else if pos(LowerCase('GR'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := GRItem
  else if pos(LowerCase('DR'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := DRItem
  else if pos(LowerCase('LS'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := LSItem
  else if pos(LowerCase('betaT'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := betaTItem
  else if pos(LowerCase('TBG'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := TBGItem
  else if pos(LowerCase('TBPA'), LowerCase(SParCombo2.Text)) > 0 then
    gSelectedSParameter2 := TBPAItem
  else
    gSelectedSParameter2 := NullItem;
  SetStandardStrucParBoundaries;
  UpdateStrucPar(gSelectedSParameter2, SParTrackBar2.Position / gTrackFactor2 / TRACK_RATIO);
  DrawDiagram(false);
  RestoreStrucPars;
  if gSelectedSParameter2 <> NullItem then
    SParTrackBar2.Enabled := true;
end;

procedure TEquilibriumDiagramForm.SParCombo3Change(Sender: TObject);
{ read selected structure parameter }
begin
  SaveStrucPars;
  SParTrackBar3.Enabled := false;
  if pos(LowerCase('GD1'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := GD1Item
  else if pos(LowerCase('GD2'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := GD2Item
  else if pos(LowerCase('KM1'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := KM1Item
  else if pos(LowerCase('KM2'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := KM2Item
  else if pos(LowerCase('GT'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := GTItem
  else if pos(LowerCase('DT'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := DTItem
  else if pos(LowerCase('GH'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := GHItem
  else if pos(LowerCase('DH'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := DHItem
  else if pos(LowerCase('SS'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := SSItem
  else if pos(LowerCase('DS'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := DSItem
  else if pos(LowerCase('GR'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := GRItem
  else if pos(LowerCase('DR'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := DRItem
  else if pos(LowerCase('LS'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := LSItem
  else if pos(LowerCase('betaT'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := betaTItem
  else if pos(LowerCase('TBG'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := TBGItem
  else if pos(LowerCase('TBPA'), LowerCase(SParCombo3.Text)) > 0 then
    gSelectedSParameter3 := TBPAItem
  else
    gSelectedSParameter3 := NullItem;
  SetStandardStrucParBoundaries;
  UpdateStrucPar(gSelectedSParameter3, SParTrackBar3.Position / gTrackFactor3 / TRACK_RATIO);
  DrawDiagram(false);
  RestoreStrucPars;
  if gSelectedSParameter3 <> NullItem then
    SParTrackBar3.Enabled := true;
end;

procedure TEquilibriumDiagramForm.SParTrackBar1Change(Sender: TObject);
begin
  SaveStrucPars;
  UpdateEditsfromTrackBars;
  UpdateStrucPar(gSelectedSParameter1, SParTrackBar1.Position / gTrackFactor1 / TRACK_RATIO);
  DrawDiagram(false);
  RestoreStrucPars;
end;

procedure TEquilibriumDiagramForm.SParTrackBar2Change(Sender: TObject);
begin
  SaveStrucPars;
  UpdateEditsfromTrackBars;
  UpdateStrucPar(gSelectedSParameter2, SParTrackBar2.Position / gTrackFactor2 / TRACK_RATIO);
  DrawDiagram(false);
  RestoreStrucPars;
end;

procedure TEquilibriumDiagramForm.SParTrackBar3Change(Sender: TObject);
begin
  SaveStrucPars;
  UpdateEditsfromTrackBars;
  UpdateStrucPar(gSelectedSParameter3, SParTrackBar3.Position / gTrackFactor3 / TRACK_RATIO);
  DrawDiagram(false);
  RestoreStrucPars;
end;

procedure TEquilibriumDiagramForm.xColorBoxChange(Sender: TObject);
begin
  Fline[0].SeriesColor := xColorBox.Selected;
end;

procedure TEquilibriumDiagramForm.yColorBoxChange(Sender: TObject);
begin
  Fline[1].SeriesColor := yColorBox.Selected;
end;

initialization
  {$I equilibriumdiagram.lrs}
  automatedSpin := false;
  gSelectedSParameter1 := NullItem;
  gSelectedSParameter2 := NullItem;
  gSelectedSParameter2 := NullItem;
  fillchar(gEmptyVector, sizeof(gEmptyVector), 0); // empties vector

end.
