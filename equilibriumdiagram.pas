unit Equilibriumdiagram;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ This unit implements an equilibrium diagram }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TATransformations,
  TANavigation, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Spin, ComCtrls, ColorBox, SimThyrTypes,
  SimThyrResources, Simulator, UnitConverter;

const
  MAX_SERIES = 2;
  MAX_I      = 100;

type

  { TEquilibriumDiagramForm }

  TEquilibriumDiagramForm = class(TForm)
    EquilibriumChart: TChart;
    EquilibriumChartLineSeries1: TLineSeries;
    EquilibriumChartLineSeries2: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    ChartNavPanel1: TChartNavPanel;
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
    SParTrackBar1: TTrackBar;
    SParTrackBar2: TTrackBar;
    SParTrackBar3: TTrackBar;
    xColorBox:     TColorBox;
    FullScaleButton1: TSpeedButton;
    GroupBox1:     TGroupBox;
    MainPanel:     TPanel;
    ResetButton:   TSpeedButton;
    StatusBar1:    TStatusBar;
    yColorBox:     TColorBox;
    procedure GetBParameters;
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

var
  EquilibriumDiagramForm: TEquilibriumDiagramForm;
  gSelectedBParameter1, gSelectedBParameter2: tBParameter;
  gSpinFactor: real;
  gResponseCurve1, gResponseCurve2: tResponseCurve;
  gFT4conversionFactor, gFT3conversionFactor: real;
  gcT3conversionFactor: real;


implementation

function SimThyroidResponse(TSHVector: tParamVector): tParamVector;
{ Simulate response of thyroid subsystem to vector with TSH values }
var
  i: integer;
  interval: real;
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
  interval: real;
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
  interval: real;
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

function SimSubsystemResponse1(bParameter1, bParameter2: tBParameter;
  min, max: real; var conversionFactor1, conversionFactor2: real): tResponseCurve;
{ Simulate response of first subsystem }
var
  i: integer;
  interval: real;
  inputVector: tParamVector;
begin
  assert((min >= 0) and (max >= 0), kError101);
  assert(max > min, kError103);
  assert(max > 0, kError104);
  interval := (max - min) / MAX_I;
  case bParameter1 of // input
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
    otherwise
    begin
      fillchar(InputVector, sizeof(InputVector), 0);
    end;
  end;
  case bParameter2 of // output
    IItem:
    begin
      Result.output := inputVector;
    end;
    TSHItem:
    begin
      Result.output := inputVector; //  to be completed...
    end;
    FT4Item:
    begin
      case bParameter1 of
        TSHItem:
        begin
          conversionFactor2 := gFT4conversionFactor;
          Result.output := SimThyroidResponse(inputVector);
        end;
      end;
    end;
    FT3Item:
    begin
      case bParameter1 of
        TSHItem:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimPDeiodinaseResponse(SimThyroidResponse(inputVector));
        end;
        FT4Item:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimPDeiodinaseResponse(inputVector);
        end;
      end;
    end;
    cT3Item:
    begin
      case bParameter1 of
        TSHItem:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimCDeiodinaseResponse(SimThyroidResponse(inputVector));
        end;
        FT4Item:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimCDeiodinaseResponse(inputVector);
        end;
      end;
    end;
    otherwise
      Result.output := inputVector;
  end;
  Result.input := inputVector;
end;

{ TEquilibriumDiagramForm }

procedure TEquilibriumDiagramForm.GetBParameters;
begin
  if pos('TSH', BParCombo1.Text) > 0 then
    gSelectedBParameter1 := TSHItem
  else if pos('FT4', BParCombo1.Text) > 0 then
    gSelectedBParameter1 := FT4Item
  else if pos('FT3', BParCombo1.Text) > 0 then
    gSelectedBParameter1 := FT3Item
  else if pos('cT3', BParCombo1.Text) > 0 then
    gSelectedBParameter1 := cT3Item
  else
    gSelectedBParameter1 := IItem;
  if pos('TSH', BParCombo2.Text) > 0 then
    gSelectedBParameter2 := TSHItem
  else if pos('FT4', BParCombo2.Text) > 0 then
    gSelectedBParameter2 := FT4Item
  else if pos('FT3', BParCombo2.Text) > 0 then
    gSelectedBParameter2 := FT3Item
  else if pos('cT3', BParCombo2.Text) > 0 then
    gSelectedBParameter2 := cT3Item
  else
    gSelectedBParameter2 := IItem;
end;

procedure TEquilibriumDiagramForm.DrawDummyEquilibriumPlot;
{Draws an empty plot}
begin
  EquilibriumChart.LeftAxis.Title.Caption   := 'Concentration 2';
  EquilibriumChart.BottomAxis.Title.Caption := 'Concentration 1';
  ;
  with FLine[0] do
  begin
    AddXY(0, 0, '', clBlack);
    AddXY(100, 0, '', clBlack);
  end;
end;

procedure TEquilibriumDiagramForm.DrawDiagram(empty: boolean);
var
  i, j: integer;
  MinBPar1, MaxBPar1, MinBPar2, MaxBPar2: real;
  ConversionFactor1, ConversionFactor2: real;
begin
  GetBParameters;
  gFT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT4_pos]);
  gFT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT3_pos]);
  gcT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[cT3_pos]);
  {If line series exists it is cleared and recreated to support foundations of redrawing}
  if FLine[1] <> nil then
    EquilibriumChart.ClearSeries;
  for i := 0 to MAX_SERIES - 1 do
  begin
    FLine[i] := TLineSeries.Create(EquilibriumChart);
    with FLine[i] do
    begin
      ShowLines   := True;
      ShowPoints  := False;
      Pointer.Brush.Color := clBlack;
      SeriesColor := clBlack;
    end;
    EquilibriumChart.AddSeries(FLine[i]);
    FLine[i].BeginUpdate;
  end;
  if empty then
    DrawDummyEquilibriumPlot
  else
  begin
    MinBPar1 := MinSpinEdit1.Value / gSpinFactor;
    MaxBPar1 := MaxSpinEdit1.Value / gSpinFactor;
    MinBPar2 := MinSpinEdit2.Value / gSpinFactor;
    MaxBPar2 := MaxSpinEdit2.Value / gSpinFactor;
    gResponseCurve1 := SimSubsystemResponse1(gSelectedBParameter2, gSelectedBParameter1, MinBPar2,
      MaxBPar2, ConversionFactor1, ConversionFactor2);
    for j := 0 to MAX_I do
    begin
      FLine[1].AddXY(gResponseCurve1.input[j] * conversionFactor1,
        gResponseCurve1.output[j] * conversionFactor2, '',
        xColorBox.Selected);
      Fline[1].SeriesColor := xColorBox.Selected;
    end;
  end;
  for i := 0 to MAX_SERIES - 1 do
    FLine[i].EndUpdate;
end;

procedure TEquilibriumDiagramForm.FormCreate(Sender: TObject);
begin
  gSpinFactor := 1;
  DrawDiagram(True);
end;

procedure TEquilibriumDiagramForm.BParCombo1Change(Sender: TObject);
{ Get selected behavioural parameter #1 }
begin
  GetBParameters;
  xColorBox.Selected := gDefaultColors[integer(gSelectedBParameter1)];
  if BParCombo2.ItemIndex = 0 then
    BParCombo2.ItemIndex := 1;  // set to useful initial value
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.BParCombo2Change(Sender: TObject);
{ Get selected behavioural parameter #2 }
begin
  GetBParameters;
  yColorBox.Selected := gDefaultColors[integer(gSelectedBParameter2)];
  if BParCombo1.ItemIndex = 0 then
    BParCombo1.ItemIndex := 1;  // set to useful initial value
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.FullScaleButton1Click(Sender: TObject);
begin
  EquilibriumChart.Extent.UseYMax := False;
  EquilibriumChart.ZoomFull;
end;

procedure TEquilibriumDiagramForm.MaxSpinEdit1Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.MaxSpinEdit2Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.MinSpinEdit1Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.MinSpinEdit2Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.ResetButtonClick(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.SParCombo1Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParCombo2Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParCombo3Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParTrackBar1Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParTrackBar2Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParTrackBar3Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.xColorBoxChange(Sender: TObject);
begin
  Fline[1].SeriesColor := xColorBox.Selected;
  //EquilibriumChartLineSeries1.SeriesColor := xColorBox.Selected;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.yColorBoxChange(Sender: TObject);
begin
  Fline[1].SeriesColor := xColorBox.Selected;
  //EquilibriumChartLineSeries2.SeriesColor := yColorBox.Selected;
  DrawDiagram(False);
end;

initialization
  {$I equilibriumdiagram.lrs}

end.
