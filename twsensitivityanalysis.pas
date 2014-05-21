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
  ColorBox, Buttons, Grids, Clipbrd, Menus, TAChartUtils, TANavigation, Math,
  TADrawerSVG, TADrawUtils, TADrawerCanvas, SimThyrTypes, SimThyrServices,
  UnitConverter, SimThyrPrediction, Sensitivityanalysis;

type

  TPartialMatrix = array[1..TWS_RESOLUTION + 1, 1..TWS_RESOLUTION + 1] of real;

  { TSensitivityMatrix }

  TSensitivityMatrix = class(TObject)
  public
    content: array[0..TWS_RESOLUTION + 1, 0..TWS_RESOLUTION + 1] of real;
    constructor create;
    destructor destroy; override;
    procedure ClearContent;
    function GetResults: TPartialMatrix;
    function GetMin: real;
    function GetMax: real;
    function GetMean: real;
  end;

  { TTWSensitivityAnalysisForm }

  TTWSensitivityAnalysisForm = class(TForm)
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    Divider1: TMenuItem;
    LegendMinLabel: TLabel;
    LegendMaxLabel: TLabel;
    LegendMap: TChart;
    LegendColorMapSeries: TColorMapSeries;
    ChartNavPanel1: TChartNavPanel;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ColorButton3: TColorButton;
    GroupBox2: TGroupBox;
    LegendPanel: TPanel;
    PasteItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    SensitivityMap: TChart;
    SensitivityMapColorMapSeries: TColorMapSeries;
    ColourSource: TListChartSource;
    FullScaleButton1: TSpeedButton;
    GroupBox1: TGroupBox;
    MaxSpinEdit1: TFloatSpinEdit;
    MaxSpinEdit2: TFloatSpinEdit;
    MinSpinEdit1: TFloatSpinEdit;
    MinSpinEdit2: TFloatSpinEdit;
    PlotPanel: TPanel;
    LegendFrame: TShape;
    StatusBar1: TStatusBar;
    CheckGrid: TStringGrid;
    StrucParCombo1: TComboBox;
    StrucParCombo2: TComboBox;
    DependentParCombo: TComboBox;
    CheckToggleBox: TToggleBox;
    UndoItem: TMenuItem;
    procedure CalculateSensitivityMatrix(var theMatrix: TSensitivityMatrix;
      const ymax: real; const ymin: real; const xmax: real; const xmin: real);
    procedure CalculateMatrixWithCoordinates(theMatrix: TSensitivityMatrix);
    procedure CheckToggleBoxChange(Sender: TObject);
    procedure CopyChart;
    procedure CopyItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure LegendColorMapSeriesCalculate(const AX, AY: Double; out AZ: Double
      );
    procedure SetStandardStrucParBoundaries(factor1, factor2: real);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure ColorButton2ColorChanged(Sender: TObject);
    procedure ColorButton3ColorChanged(Sender: TObject);
    procedure SensitivityMapColorMapSeriesCalculate(const AX, AY: Double; out
      AZ: Double);
    procedure DependentParComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure StrucParCombo1Change(Sender: TObject);
    procedure StrucParCombo2Change(Sender: TObject);
  private
    { private declarations }
    SensitivityMatrix: TSensitivityMatrix;
    gMinXPar, gMaxXPar, gSpinFactor: real;
    procedure ColouriseLegend;
  public
    { public declarations }
    procedure PopulateColourSource(const minScale, medScale, maxScale: real);
  end;

var
  TWSensitivityAnalysisForm: TTWSensitivityAnalysisForm;
  gMinYPar, gMaxYPar: real;

implementation

uses
  SimThyrMain;

{ TSensitivityMatrix }

function TSensitivityMatrix.GetResults: TPartialMatrix;
var
  i, j: integer;
begin
  for i := 1 to TWS_RESOLUTION + 1 do
  for j := 1 to TWS_RESOLUTION + 1 do
  result[i, j] := content[i, j];
end;

function TSensitivityMatrix.GetMin: real;
var
  i, j: integer;
begin
  result := content[1, 1];
  for i := 1 to TWS_RESOLUTION + 1 do
  for j := 1 to TWS_RESOLUTION + 1 do // beginning with 1 to allow for matrix size of 1
    if content[i, j] < result then result := content[i, j];
end;

function TSensitivityMatrix.GetMax: real;
var
  i, j: integer;
begin
  result := content[1, 1];
  for i := 1 to TWS_RESOLUTION + 1 do
  for j := 1 to TWS_RESOLUTION + 1 do // beginning with 1 to allow for matrix size of 1
    if content[i, j] > result then result := content[i, j];
end;

function TSensitivityMatrix.GetMean: real;
var
  i, j: integer;
begin
  result := 0;
  for i := 1 to TWS_RESOLUTION + 1 do
  for j := 1 to TWS_RESOLUTION + 1 do
    result := result + content[i, j];
  result := result / sqr(TWS_RESOLUTION + 1);
end;

constructor TSensitivityMatrix.create;
begin
  inherited create;
  ClearContent;
end;

destructor TSensitivityMatrix.destroy;
begin
  inherited Destroy;
end;

procedure TSensitivityMatrix.ClearContent;
var
  i, j: integer;
begin
  for i := 0 to TWS_RESOLUTION + 1 do
  for j := 0 to TWS_RESOLUTION + 1 do
  begin
    content[i, j] := 0;
  end;
end;

procedure FillGrid(theGrid: TStringGrid; theMatrix: TSensitivityMatrix;
  const xres, yres: integer);
var
  i, j: integer;
begin
  for i := 0 to xres do
  for j := 0 to yres do
  theGrid.Cells[i, j] := FloatToStr(theMatrix.content[j, i]);
end;

{ TTWSensitivityAnalysisForm }

procedure TTWSensitivityAnalysisForm.CalculateSensitivityMatrix(var theMatrix:
  TSensitivityMatrix; const ymax: real; const ymin: real; const xmax: real;
  const xmin: real);
{ This procedure calculates equilibrium levels of the selected dependent parameter }
{ according to variations in independent structure parameters }
{ results are stored in the 2d array "theMatrix" }
var
  plotPossible: boolean;
  i, j: integer;
  oldCursor: TCursor;
  FT4conversionFactor, FT3conversionFactor: real;
  TT4conversionFactor, TT3conversionFactor, cT3conversionFactor: real;
begin
  plotPossible := false;
  oldCursor := Cursor;
  Cursor := crHourGlass;
  CheckGrid.Clear;
  CheckGrid.ColCount := TWS_RESOLUTION + 2;
  CheckGrid.RowCount := TWS_RESOLUTION + 2;;
  SaveStrucPars;
  FT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l', gParameterUnit[FT4_pos]);
  TT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l', gParameterUnit[TT4_pos]);
  TT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[TT3_pos]);
  FT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[FT3_pos]);
  cT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[cT3_pos]);
  theMatrix.ClearContent;
  for i := 0 to TWS_RESOLUTION do
    theMatrix.content[0, i + 1] := xmin + i / TWS_RESOLUTION * (xmax - xmin);
  for j := 0 to TWS_RESOLUTION do
    theMatrix.content[j + 1, 0] := ymin + j / TWS_RESOLUTION * (ymax - ymin);
  for i := 0 to TWS_RESOLUTION do
  for j := 0 to TWS_RESOLUTION do
  begin
    case StrucParCombo1.ItemIndex of
      0: // title or undefined parameter selected
        ;
      1: // GD1
        GD1 := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / GD1_FACTOR;
      2: // GD2
        GD2 := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / GD2_FACTOR;
      3: // KM1
        KM1 := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / KM1_FACTOR;
      4: // KM2
        KM2 := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / KM2_FACTOR;
      5: // GT
        GT := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / GT_FACTOR;
      6: // DT
        DT := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DT_FACTOR;
      7: // GH
        GH := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DT_FACTOR;
      8: // DH
        DH := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DH_FACTOR;
      9: // SS
        SS := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DT_FACTOR;
      10: // DS
        DS := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DT_FACTOR;
      11: // GR
        GR := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DT_FACTOR;
      12: // DR
        DR := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DR_FACTOR;
      13: // LS
        LS := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / LS_FACTOR;
      14: // betaS
        betaS := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DT_FACTOR;
      15: // betaS2
        betaS2 := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DT_FACTOR;
      16: // betaT
        betaT := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / BETAT_FACTOR;
      17: // beta31
        beta31 := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / BETAT_FACTOR;
      18: // beta32
        beta32 := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / DT_FACTOR;
      19: // TBG
        TBG := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / TBG_FACTOR;
      20: // TBPA
        TBPA := (xmin + i / TWS_RESOLUTION * (xmax - xmin)) / TBPA_FACTOR;
      otherwise
        ;
    end;
    case StrucParCombo2.ItemIndex of
      0: // title or undefined parameter selected
        ;
      1: // GD1
        GD1 := (ymin + j / TWS_RESOLUTION * (ymax - xmin)) / GD1_FACTOR;
      2: // GD2
        GD2 := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / GD2_FACTOR;
      3: // KM1
        KM1 := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / KM1_FACTOR;
      4: // KM2
        KM2 := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / KM2_FACTOR;
      5: // GT
        GT := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / GT_FACTOR;
      6: // DT
        DT := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DT_FACTOR;
      7: // GH
        GH := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DT_FACTOR;
      8: // DH
        DH := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DH_FACTOR;
      9: // SS
        SS := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DT_FACTOR;
      10: // DS
        DS := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DT_FACTOR;
      11: // GR
        GR := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DT_FACTOR;
      12: // DR
        DR := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DR_FACTOR;
      13: // LS
        LS := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / LS_FACTOR;
      14: // betaS
        betaS := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DT_FACTOR;
      15: // betaS2
        betaS2 := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DT_FACTOR;
      16: // betaT
        betaT := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / BETAT_FACTOR;
      17: // beta31
        beta31 := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / BETAT_FACTOR;
      18: // beta32
        beta32 := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / DT_FACTOR;
      19: // TBG
        TBG := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / TBG_FACTOR;
      20: // TBPA
        TBPA := (ymin + j / TWS_RESOLUTION * (ymax - ymin)) / TBPA_FACTOR;
      otherwise
        ;
    end;
    if (StrucParCombo1.ItemIndex > 0) and (StrucParCombo2.ItemIndex > 0) and
       (StrucParCombo1.ItemIndex <> StrucParCombo2.ItemIndex) then
       plotPossible := true;
    if plotPossible then
    begin
      PredictEquilibrium;
      case DependentParCombo.ItemIndex of
        1:
          theMatrix.content[j + 1, i + 1] := TSH1 * gParameterFactor[TSH_pos];
        2:
          theMatrix.content[j + 1, i + 1] := T41 * TT4conversionFactor;
        3:
          theMatrix.content[j + 1, i + 1] := FT41 * FT4conversionFactor;
        4:
          theMatrix.content[j + 1, i + 1] := T31 * TT3conversionFactor;
        5:
          theMatrix.content[j + 1, i + 1] := FT31 * FT3conversionFactor;
        6:
          theMatrix.content[j + 1, i + 1] := T3z1 * cT3conversionFactor;
        otherwise
          theMatrix.content[j + 1, i + 1] := -1; // undefined parameter selected
      end
    end
    else
      theMatrix.content[j + 1, i + 1] := -1;
  end;
  FillGrid(CheckGrid, theMatrix, TWS_RESOLUTION + 1, TWS_RESOLUTION + 1);
  RestoreStrucPars;
  PredictEquilibrium; // restore previous prediction
  Cursor := oldCursor;
end;

procedure TTWSensitivityAnalysisForm.CalculateMatrixWithCoordinates(theMatrix:
  TSensitivityMatrix);
var
  xmin, xmax, ymin, ymax: real;
begin
  xmin := SensitivityMapColorMapSeries.Extent.XMin;
  xmax := SensitivityMapColorMapSeries.Extent.XMax;
  ymin := SensitivityMapColorMapSeries.Extent.YMin;
  ymax := SensitivityMapColorMapSeries.Extent.YMax;
  CalculateSensitivityMatrix(SensitivityMatrix, ymax, ymin, xmax, xmin);
  StatusBar1.Panels[0].Text := ' ' + IntToStr(sqr(TWS_RESOLUTION)) +
    ' data points';
end;

procedure TTWSensitivityAnalysisForm.CheckToggleBoxChange(Sender: TObject);
begin
  if CheckToggleBox.State = cbChecked then
  begin
    CheckGrid.Visible := true;
    SensitivityMap.Visible := false;
  end
  else
  begin
    CheckGrid.Visible := false;
    SensitivityMap.Visible := true;
  end;
end;

procedure TTWSensitivityAnalysisForm.CopyChart;
var
  {$IFDEF UNIX}
  theImage: TPortableNetworkGraphic;
  {$ELSE}
  theImage: TBitMap;
  {$ENDIF}
  theWidth, theHeight: integer;
begin
  if SensitivityMap = nil then
    bell
  else
  begin
    {SensitivityMap.CopyToClipboardBitmap doesn't work on Mac OS X}
    {$IFDEF UNIX}
    theImage := TPortableNetworkGraphic.Create;
    try
      theWidth := SensitivityMap.Width;
      theHeight := SensitivityMap.Height;
      theImage.Width := theWidth;
      theImage.Height := theHeight;
      SensitivityMap.DrawOnCanvas(rect(0, 0, theImage.Width, theImage.Height), theImage.canvas);
      Clipboard.Assign(theImage);
    finally
      theImage.Free;
    end;
    {$ELSE}
    SensitivityMap.CopyToClipboardBitmap;
    {$ENDIF}
  end;
end;

procedure TTWSensitivityAnalysisForm.CopyItemClick(Sender: TObject);
begin
  CopyChart;
end;

procedure TTWSensitivityAnalysisForm.FormActivate(Sender: TObject);
begin
  SimThyrToolbar.SelectAllMenuItem.Enabled := false;
  gLastActiveCustomForm := SensitivityAnalysisForm;
end;

procedure TTWSensitivityAnalysisForm.LegendColorMapSeriesCalculate(const AX,
  AY: Double; out AZ: Double);
var
  subMat: TPartialMatrix;
  ext: TDoubleRect;
  minResult, meanResult, maxResult: real;
begin
  ext := LegendMap.GetFullExtent;
  with SensitivityMatrix do
  begin
    subMat := GetResults;
    minResult := GetMin;
    meanResult := GetMean;
    maxResult := GetMax;
  end;
  if maxResult > minResult then
  begin
    LegendMinLabel.Caption := FloatToStr(minResult);
    LegendMaxLabel.Caption := FloatToStr(maxResult);
  end
  else
  begin
    LegendMinLabel.Caption := '0';
    LegendMaxLabel.Caption := '0';
  end;
  AZ := minResult + (maxResult - minResult) * (AY - ext.a.y) /(ext.b.y - ext.a.y);
end;

procedure TTWSensitivityAnalysisForm.SetStandardStrucParBoundaries(factor1, factor2: real);
{sets the initial boundaries to useful values}
var
  tempMinX, tempMaxX, tempMinY, tempMaxY: real; {necessary to hinder Windows from altering the globals}
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
      gMinYPar := GD1 / 3;
      gMaxYPar := GD1 * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / GD1_FACTOR;}
    end;
    2:
    begin {GD2}
      gSpinFactor := GD2_FACTOR;
      gMinYPar := GD2 / 3;
      gMaxYPar := GD2 * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / GD2_FACTOR;  }
    end;
    3:
    begin {kM1}
      gSpinFactor := KM1_FACTOR;
      gMinYPar := kM1 / 3;
      gMaxYPar := kM1 * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / KM1_FACTOR;  }
    end;
    4:
    begin {kM2}
      gSpinFactor := KM2_FACTOR;
      gMinYPar := kM2 / 3;
      gMaxYPar := kM2 * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / KM2_FACTOR;}
    end;
    5:
    begin {GT}
      gSpinFactor := GT_FACTOR;
      gMinYPar := GT / 3;
      gMaxYPar := GT * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / GT_FACTOR; }
    end;
    6:
    begin {DT}
      gSpinFactor := DT_FACTOR;
      gMinYPar := DT / 3;
      gMaxYPar := DT * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DT_FACTOR;  }
    end;
    7:
    begin {GH}
      gSpinFactor := DT_FACTOR;
      gMinYPar := GH / 3;
      gMaxYPar := GH * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DT_FACTOR; }
    end;
    8:
    begin {DH}
      gSpinFactor := DH_FACTOR;
      gMinYPar := DH / 3;
      gMaxYPar := DH * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    9:
    begin {SS}
      gSpinFactor := DT_FACTOR;
      gMinYPar := SS / 3;
      gMaxYPar := SS * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    10:
    begin {DS}
      gSpinFactor := DT_FACTOR;
      gMinYPar := DS / 3;
      gMaxYPar := DS * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    11:
    begin {GR}
      gSpinFactor := DT_FACTOR;
      gMinYPar := GR / 3;
      gMaxYPar := GR * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    12:
    begin {DR}
      gSpinFactor := DR_FACTOR;
      gMinYPar := DR / 3;
      gMaxYPar := DR * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    13:
    begin {LS}
      gSpinFactor := LS_FACTOR;
      gMinYPar := LS / 3;
      gMaxYPar := LS * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    14:
    begin {betaS}
      gSpinFactor := DT_FACTOR;
      gMinYPar := betaS / 3;
      gMaxYPar := betaS * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    15:
    begin {betaS2}
      gSpinFactor := DT_FACTOR;
      gMinYPar := betaS2 / 3;
      gMaxYPar := betaS2 * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    16:
    begin {betaT}
      gSpinFactor := BETAT_FACTOR;
      gMinYPar := betaT / 3;
      gMaxYPar := betaT * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    17:
    begin {beta31}
      gSpinFactor := BETAT_FACTOR;
      gMinYPar := beta31 / 3;
      gMaxYPar := beta31 * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    18:
    begin {beta32}
      gSpinFactor := DT_FACTOR;
      gMinYPar := beta32 / 3;
      gMaxYPar := beta32 * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    19:
    begin {TBG}
      gSpinFactor := TBG_FACTOR;
      gMinYPar := TBG / 3;
      gMaxYPar := TBG * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxX;
      {ChartAxisTransformations1LinearAxisTransform2.Scale :=
        1 / DH_FACTOR; }
    end;
    20:
    begin {TBPA}
      gSpinFactor := TBPA_FACTOR;
      gMinYPar := TBPA / 3;
      gMaxYPar := TBPA * 3;
      tempMinY := gMinYPar;
      tempMaxX := gMaxYPar;
      MinSpinEdit2.Value := tempMinY * gSpinFactor;
      MaxSpinEdit2.Value := tempMaxX * gSpinFactor;
      gMinYPar := tempMinY;
      gMaxYPar := tempMaxY;
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
  SensitivityMapColorMapSeries.Extent.XMin := MinSpinEdit1.Value;
  SensitivityMapColorMapSeries.Extent.XMax := MaxSpinEdit1.Value;
  SensitivityMapColorMapSeries.Extent.YMin := MinSpinEdit2.Value;
  SensitivityMapColorMapSeries.Extent.YMax := MaxSpinEdit2.Value;
  if SensitivityMatrix <> nil then
    SensitivityMatrix.ClearContent
  else
    SensitivityMatrix := TSensitivityMatrix.create;
end;

procedure TTWSensitivityAnalysisForm.SensitivityMapColorMapSeriesCalculate(const AX,
  AY: Double; out AZ: Double);
{ This procedure reads equilibrium levels from the sensitivity matrix }
{ and delivers point-wise results in the variable AZ }
var
  ext: TDoubleRect;
  i, j: integer;
begin
  SensitivityMap.DisableRedrawing;
  ext := SensitivityMap.GetFullExtent;
  i := trunc((AX - ext.a.x) / (ext.b.x - ext.a.x) * TWS_RESOLUTION);
  j := trunc((AY - ext.a.y) / (ext.b.y - ext.a.y) * TWS_RESOLUTION);
  AZ := SensitivityMatrix.content[j + 1, i + 1];
  SensitivityMap.EnableRedrawing;
end;

procedure TTWSensitivityAnalysisForm.DependentParComboChange(Sender: TObject);
var
  subMat: TPartialMatrix;
begin
  SensitivityMap.Title.Text.Text := DependentParCombo.Text;
  CalculateMatrixWithCoordinates(SensitivityMatrix);
  CalculateMatrixWithCoordinates(SensitivityMatrix);
  with SensitivityMatrix do
  begin
    subMat := GetResults;
    PopulateColourSource(GetMin, GetMean, GetMax);
  end;
  SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
end;

procedure TTWSensitivityAnalysisForm.ColorButton3ColorChanged(Sender: TObject);
var
  subMat: TPartialMatrix;
begin
  CalculateMatrixWithCoordinates(SensitivityMatrix);
  with SensitivityMatrix do
  begin
    subMat := GetResults;
    PopulateColourSource(GetMin, GetMean, GetMax);
  end;
  ColouriseLegend;
  SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
  LegendMap.Invalidate;
end;

procedure TTWSensitivityAnalysisForm.ColorButton2ColorChanged(Sender: TObject);
var
  subMat: TPartialMatrix;
begin
  CalculateMatrixWithCoordinates(SensitivityMatrix);
  with SensitivityMatrix do
  begin
    subMat := GetResults;
    PopulateColourSource(GetMin, GetMean, GetMax);
  end;
  ColouriseLegend;
  SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
  LegendMap.Invalidate;
end;

procedure TTWSensitivityAnalysisForm.ColorButton1ColorChanged(Sender: TObject);
var
  subMat: TPartialMatrix;
begin
  CalculateMatrixWithCoordinates(SensitivityMatrix);
  with SensitivityMatrix do
  begin
    subMat := GetResults;
    PopulateColourSource(GetMin, GetMean, GetMax);
  end;
  ColouriseLegend;
  SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
  LegendMap.Invalidate;
end;

procedure TTWSensitivityAnalysisForm.FormCreate(Sender: TObject);
begin
  {$IFDEF LCLcarbon}
  SensitivityMapColorMapSeries.UseImage := cmuiAlways;
  LegendColorMapSeries.UseImage := cmuiAlways;
  {$ELSE}
  SensitivityMapColorMapSeries.UseImage := cmuiAuto;
  LegendColorMapSeries.UseImage := cmuiAuto;
  {$ENDIF}
  SensitivityMatrix := TSensitivityMatrix.create;
  ColouriseLegend;
  PopulateColourSource(0, 0.5, 1);
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
var
  subMat: TPartialMatrix;
begin
  if TWSensitivityAnalysisForm.StrucParCombo1.Text <> '' then
  begin
    TWSensitivityAnalysisForm.StrucParCombo1.Enabled := false; {fixes error #8}
    SetBottomAxisCaption;
    SetStandardStrucParBoundaries(1 / 3, 3);
    CalculateMatrixWithCoordinates(SensitivityMatrix);
    with SensitivityMatrix do
    begin
      subMat := GetResults;
      PopulateColourSource(GetMin, GetMean, GetMax);
    end;
    SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
    TWSensitivityAnalysisForm.StrucParCombo1.Enabled := true;
  end;
end;

procedure TTWSensitivityAnalysisForm.StrucParCombo2Change(Sender: TObject);
var
  subMat: TPartialMatrix;
begin
  if TWSensitivityAnalysisForm.StrucParCombo2.Text <> '' then
  begin
    TWSensitivityAnalysisForm.StrucParCombo2.Enabled := false; {fixes error #8}
    SetLeftAxisCaption;
    SetStandardStrucParBoundaries(1 / 3, 3);
    CalculateMatrixWithCoordinates(SensitivityMatrix);
    with SensitivityMatrix do
    begin
      subMat := GetResults;
      PopulateColourSource(GetMin, GetMean, GetMax);
    end;
    SensitivityMap.Invalidate;  {forces redrawing in some operating systems}
    TWSensitivityAnalysisForm.StrucParCombo2.Enabled := true;
  end;
end;

procedure TTWSensitivityAnalysisForm.ColouriseLegend;
begin
  LegendMap.Invalidate;
end;

procedure TTWSensitivityAnalysisForm.PopulateColourSource(const minScale,
  medScale, maxScale: real);
const
  DUMMY = 0.0;
begin
  with ColourSource do
  begin
    Clear;
    Add(-1.0, DUMMY, '', clWhite);
    Add(minScale, DUMMY, '', ColorButton1.ButtonColor);
    Add(medScale, DUMMY, '', ColorButton2.ButtonColor);
    Add(maxScale, DUMMY, '', ColorButton3.ButtonColor);
  end;
end;

initialization
  {$I TWSensitivityanalysis.lrs}

end.

