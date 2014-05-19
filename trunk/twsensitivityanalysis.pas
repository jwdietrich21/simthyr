unit TWSensitivityanalysis;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.3.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit implements sensitivity analysis }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAFuncSeries, TASources, LResources,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin, ComCtrls,
  ColorBox, Buttons, TAChartUtils, TANavigation, SimThyrTypes,
  Sensitivityanalysis;

type

  { TTWSensitivityAnalysisForm }

  TTWSensitivityAnalysisForm = class(TForm)
    ChartNavPanel1: TChartNavPanel;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    SensitivityMap: TChart;
    SensitivityMapColorMapSeries1: TColorMapSeries;
    ColorMapLabel: TLabel;
    ColorMapPanel: TPanel;
    ColourSource: TListChartSource;
    FullScaleButton1: TSpeedButton;
    GroupBox1: TGroupBox;
    MaxSpinEdit1: TFloatSpinEdit;
    MaxSpinEdit2: TFloatSpinEdit;
    MinSpinEdit1: TFloatSpinEdit;
    MinSpinEdit2: TFloatSpinEdit;
    PlotPanel: TPanel;
    StatusBar1: TStatusBar;
    StrucParCombo1: TComboBox;
    StrucParCombo2: TComboBox;
    DependentParCombo: TComboBox;
    procedure SetStandardStrucParBoundaries(factor1, factor2: real);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure ColorButton2ColorChanged(Sender: TObject);
    procedure ColorButton3ColorChanged(Sender: TObject);
    procedure SensitivityMapColorMapSeries1Calculate(const AX, AY: Double; out
      AZ: Double);
    procedure DependentParComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure StrucParCombo1Change(Sender: TObject);
    procedure StrucParCombo2Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure PopulateColourSource;
  end;

var
  TWSensitivityAnalysisForm: TTWSensitivityAnalysisForm;
  gMinXPar, gMaxXPar, gSpinFactor: real;

implementation

{ TTWSensitivityAnalysisForm }

procedure TTWSensitivityAnalysisForm.SetStandardStrucParBoundaries(factor1, factor2: real);
{sets the initial boundaries to useful values}
var
  tempMinX, tempMaxX: real; {necessary to hinder Windows from altering the globals}
begin
  case StrucParCombo1.ItemIndex of
    0:
    begin {Caption}
      gSpinFactor := 0;
      MinSpinEdit1.Value := 0;
      MaxSpinEdit1.Value := 0;
    end;
    1:
    begin {GD1}
      gSpinFactor := GD1_FACTOR;
      gMinXPar := GD1 / 3;
      gMaxXPar := GD1 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / GD1_FACTOR;}
    end;
    2:
    begin {GD2}
      gSpinFactor := GD2_FACTOR;
      gMinXPar := GD2 / 3;
      gMaxXPar := GD2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / GD2_FACTOR;  }
    end;
    3:
    begin {kM1}
      gSpinFactor := KM1_FACTOR;
      gMinXPar := kM1 / 3;
      gMaxXPar := kM1 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / KM1_FACTOR;  }
    end;
    4:
    begin {kM2}
      gSpinFactor := KM2_FACTOR;
      gMinXPar := kM2 / 3;
      gMaxXPar := kM2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / KM2_FACTOR;}
    end;
    5:
    begin {GT}
      gSpinFactor := GT_FACTOR;
      gMinXPar := GT / 3;
      gMaxXPar := GT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / GT_FACTOR; }
    end;
    6:
    begin {DT}
      gSpinFactor := DT_FACTOR;
      gMinXPar := DT / 3;
      gMaxXPar := DT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR;  }
    end;
    7:
    begin {GH}
      gSpinFactor := DT_FACTOR;
      gMinXPar := GH / 3;
      gMaxXPar := GH * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DT_FACTOR; }
    end;
    8:
    begin {DH}
      gSpinFactor := DH_FACTOR;
      gMinXPar := DH / 3;
      gMaxXPar := DH * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    9:
    begin {SS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := SS / 3;
      gMaxXPar := SS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    10:
    begin {DS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := DS / 3;
      gMaxXPar := DS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    11:
    begin {GR}
      gSpinFactor := DT_FACTOR;
      gMinXPar := GR / 3;
      gMaxXPar := GR * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    12:
    begin {DR}
      gSpinFactor := DR_FACTOR;
      gMinXPar := DR / 3;
      gMaxXPar := DR * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    13:
    begin {LS}
      gSpinFactor := LS_FACTOR;
      gMinXPar := LS / 3;
      gMaxXPar := LS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    14:
    begin {betaS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaS / 3;
      gMaxXPar := betaS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    15:
    begin {betaS2}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaS2 / 3;
      gMaxXPar := betaS2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    16:
    begin {betaT}
      gSpinFactor := BETAT_FACTOR;
      gMinXPar := betaT / 3;
      gMaxXPar := betaT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    17:
    begin {beta31}
      gSpinFactor := BETAT_FACTOR;
      gMinXPar := beta31 / 3;
      gMaxXPar := beta31 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    18:
    begin {beta32}
      gSpinFactor := DT_FACTOR;
      gMinXPar := beta32 / 3;
      gMaxXPar := beta32 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    19:
    begin {TBG}
      gSpinFactor := TBG_FACTOR;
      gMinXPar := TBG / 3;
      gMaxXPar := TBG * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end;
    20:
    begin {TBPA}
      gSpinFactor := TBPA_FACTOR;
      gMinXPar := TBPA / 3;
      gMaxXPar := TBPA * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit1.Value := tempMinX * gSpinFactor;
      MaxSpinEdit1.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform1.Scale :=
        1 / DH_FACTOR; }
    end
  end;
  if MinSpinEdit1.Value < 10 then MinSpinEdit1.DecimalPlaces := 4
  else if MinSpinEdit1.Value > 100 then MinSpinEdit1.DecimalPlaces := 1
  else MinSpinEdit1.DecimalPlaces := 2;
  if MaxSpinEdit1.Value < 10 then MaxSpinEdit1.DecimalPlaces := 4
  else if MaxSpinEdit1.Value > 100 then MaxSpinEdit1.DecimalPlaces := 1
  else MaxSpinEdit1.DecimalPlaces := 2;

  case StrucParCombo2.ItemIndex of
    0:
    begin {Caption}
      gSpinFactor := 0;
      MinSpinEdit2.Value := 0;
      MaxSpinEdit2.Value := 0;
    end;
    1:
    begin {GD1}
      gSpinFactor := GD1_FACTOR;
      gMinXPar := GD1 / 3;
      gMaxXPar := GD1 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / GD1_FACTOR;}
    end;
    2:
    begin {GD2}
      gSpinFactor := GD2_FACTOR;
      gMinXPar := GD2 / 3;
      gMaxXPar := GD2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / GD2_FACTOR;  }
    end;
    3:
    begin {kM1}
      gSpinFactor := KM1_FACTOR;
      gMinXPar := kM1 / 3;
      gMaxXPar := kM1 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / KM1_FACTOR;  }
    end;
    4:
    begin {kM2}
      gSpinFactor := KM2_FACTOR;
      gMinXPar := kM2 / 3;
      gMaxXPar := kM2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / KM2_FACTOR;}
    end;
    5:
    begin {GT}
      gSpinFactor := GT_FACTOR;
      gMinXPar := GT / 3;
      gMaxXPar := GT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / GT_FACTOR; }
    end;
    6:
    begin {DT}
      gSpinFactor := DT_FACTOR;
      gMinXPar := DT / 3;
      gMaxXPar := DT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DT_FACTOR;  }
    end;
    7:
    begin {GH}
      gSpinFactor := DT_FACTOR;
      gMinXPar := GH / 3;
      gMaxXPar := GH * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DT_FACTOR; }
    end;
    8:
    begin {DH}
      gSpinFactor := DH_FACTOR;
      gMinXPar := DH / 3;
      gMaxXPar := DH * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    9:
    begin {SS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := SS / 3;
      gMaxXPar := SS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    10:
    begin {DS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := DS / 3;
      gMaxXPar := DS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    11:
    begin {GR}
      gSpinFactor := DT_FACTOR;
      gMinXPar := GR / 3;
      gMaxXPar := GR * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    12:
    begin {DR}
      gSpinFactor := DR_FACTOR;
      gMinXPar := DR / 3;
      gMaxXPar := DR * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    13:
    begin {LS}
      gSpinFactor := LS_FACTOR;
      gMinXPar := LS / 3;
      gMaxXPar := LS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    14:
    begin {betaS}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaS / 3;
      gMaxXPar := betaS * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    15:
    begin {betaS2}
      gSpinFactor := DT_FACTOR;
      gMinXPar := betaS2 / 3;
      gMaxXPar := betaS2 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    16:
    begin {betaT}
      gSpinFactor := BETAT_FACTOR;
      gMinXPar := betaT / 3;
      gMaxXPar := betaT * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    17:
    begin {beta31}
      gSpinFactor := BETAT_FACTOR;
      gMinXPar := beta31 / 3;
      gMaxXPar := beta31 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    18:
    begin {beta32}
      gSpinFactor := DT_FACTOR;
      gMinXPar := beta32 / 3;
      gMaxXPar := beta32 * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    19:
    begin {TBG}
      gSpinFactor := TBG_FACTOR;
      gMinXPar := TBG / 3;
      gMaxXPar := TBG * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    20:
    begin {TBPA}
      gSpinFactor := TBPA_FACTOR;
      gMinXPar := TBPA / 3;
      gMaxXPar := TBPA * 3;
      tempMinX := gMinXPar;
      tempMaxX := gMaxXPar;
      MinSpinEdit2.Value := tempMinX * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinXPar := tempMinX;
      gMaxXPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end
  end;
  if MinSpinEdit2.Value < 10 then MinSpinEdit2.DecimalPlaces := 4
  else if MinSpinEdit2.Value > 100 then MinSpinEdit2.DecimalPlaces := 1
  else MinSpinEdit2.DecimalPlaces := 2;
  if MaxSpinEdit2.Value < 10 then MaxSpinEdit2.DecimalPlaces := 4
  else if MaxSpinEdit2.Value > 100 then MaxSpinEdit2.DecimalPlaces := 1
  else MaxSpinEdit2.DecimalPlaces := 2;
  SensitivityMapColorMapSeries1.Extent.XMin := MinSpinEdit1.Value;
  SensitivityMapColorMapSeries1.Extent.XMax := MaxSpinEdit1.Value;
  SensitivityMapColorMapSeries1.Extent.YMin := MinSpinEdit2.Value;
  SensitivityMapColorMapSeries1.Extent.YMax := MaxSpinEdit2.Value;
end;

procedure TTWSensitivityAnalysisForm.SensitivityMapColorMapSeries1Calculate(const AX,
  AY: Double; out AZ: Double);
{ This procedure calculates equilibrium levels of the selected dependent parameter }
{ according to variations in independent structure parameters }
var
  ext: TDoubleRect;
begin
  ext := SensitivityMap.GetFullExtent;
  AZ := (AX - ext.a.x) / (ext.b.x - ext.a.x);
end;

procedure TTWSensitivityAnalysisForm.DependentParComboChange(Sender: TObject);
begin
  SensitivityMap.Title.Text.Text := DependentParCombo.Text;
  PopulateColourSource;
  SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
end;

procedure TTWSensitivityAnalysisForm.ColorButton3ColorChanged(Sender: TObject);
begin
  PopulateColourSource;
  SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
end;

procedure TTWSensitivityAnalysisForm.ColorButton2ColorChanged(Sender: TObject);
begin
  PopulateColourSource;
  SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
end;

procedure TTWSensitivityAnalysisForm.ColorButton1ColorChanged(Sender: TObject);
begin
  PopulateColourSource;
  SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
end;

procedure TTWSensitivityAnalysisForm.FormCreate(Sender: TObject);
begin
  PopulateColourSource;
  SetStandardStrucParBoundaries(1 / 3, 3);
end;

procedure TTWSensitivityAnalysisForm.FullScaleButton1Click(Sender: TObject);
begin
  SensitivityMap.ZoomFull;
end;

procedure SetLeftAxisCaption;
{sets caption and measurement unit for bottom axis according to selected item}
var
  theCaption: string;
begin
  theCaption := composedAxisCaption(TWSensitivityAnalysisForm.StrucParCombo2.Text);
  TWSensitivityAnalysisForm.SensitivityMap.LeftAxis.Title.Caption :=
    theCaption;
end;

procedure SetBottomAxisCaption;
{sets caption and measurement unit for bottom axis according to selected item}
var
  theCaption: string;
begin
  theCaption := composedAxisCaption(TWSensitivityAnalysisForm.StrucParCombo1.Text);
  TWSensitivityAnalysisForm.SensitivityMap.BottomAxis.Title.Caption :=
    theCaption;
end;

procedure TTWSensitivityAnalysisForm.StrucParCombo1Change(Sender: TObject);
begin
  if TWSensitivityAnalysisForm.StrucParCombo1.Text <> '' then
  begin
    TWSensitivityAnalysisForm.StrucParCombo1.Enabled := false; {fixes error #8}
    SetBottomAxisCaption;
    {ItemIndex is evaluated in the SetStandardStrucParBoundaries and plot routine}
    SetStandardStrucParBoundaries(1 / 3, 3);
    //DrawOWSensitivityPlot(False);
    PopulateColourSource;
    SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
    TWSensitivityAnalysisForm.StrucParCombo1.Enabled := true;
  end;
end;

procedure TTWSensitivityAnalysisForm.StrucParCombo2Change(Sender: TObject);
begin
  if TWSensitivityAnalysisForm.StrucParCombo2.Text <> '' then
  begin
    TWSensitivityAnalysisForm.StrucParCombo2.Enabled := false; {fixes error #8}
    SetLeftAxisCaption;
    {ItemIndex is evaluated in the SetStandardStrucParBoundaries and plot routine}
    SetStandardStrucParBoundaries(1 / 3, 3);
    //DrawOWSensitivityPlot(False);
    PopulateColourSource;
    SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
    TWSensitivityAnalysisForm.StrucParCombo2.Enabled := true;
  end;
end;

procedure TTWSensitivityAnalysisForm.PopulateColourSource;
const
  DUMMY = 0.0;
begin
  with ColourSource do
  begin
    Clear;
    Add(0.0, DUMMY, '', ColorButton1.ButtonColor);
    Add(0.5, DUMMY, '', ColorButton2.ButtonColor);
    Add(1.0, DUMMY, '', ColorButton3.ButtonColor);
  end;
end;

initialization
  {$I TWSensitivityanalysis.lrs}

end.

