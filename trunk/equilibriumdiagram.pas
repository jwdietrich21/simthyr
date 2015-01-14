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

  tResponseCurve = record
    input, output: array[0..MAX_I] of extended;
  end;

var
  EquilibriumDiagramForm: TEquilibriumDiagramForm;
  gSelectedBParameter1, gSelectedBParameter2: tBParameter;
  gMinBPar1, gMaxBPar1, gMinBPar2, gMaxBPar2, gSpinFactor: real;
  gResponseCurve1, gResponseCurve2: tResponseCurve;

implementation

function SimThyroidResponse(min, max: real): tResponseCurve;
var
  i: integer;
  interval: real;
  T4conversionFactor, gainOfT4: real;
begin
  assert((min >= 0) and (max >= 0), kError101);
  assert(max > min, kError103);
  assert(max > 0, kError104);
  interval := (max - min) / max_i;
  gainOfT4 := alphaT / betaT;
  for i := 0 to MAX_I do
  begin
    TSH := min + i * interval;
    SimThyroidGland(gainOfT4, False);
    Result.input[i]  := TSH;
    Result.output[i] := FT4;
  end;
end;

{ TEquilibriumDiagramForm }

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
  FT4conversionFactor, FT3conversionFactor: real;
  cT3conversionFactor: real;
  ConversionFactor1, ConversionFactor2: real;
begin
  FT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT4_pos]);
  FT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT3_pos]);
  cT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
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
    gMinBPar1 := MinSpinEdit1.Value / gSpinFactor;
    gMaxBPar1 := MaxSpinEdit1.Value / gSpinFactor;
    case gSelectedBParameter1 of
      TSHItem:
      begin

      end;
      FT4Item:
      begin
        gResponseCurve1   := SimThyroidResponse(gMinBPar1, gMaxBPar1);
        conversionFactor1 := 1;
        conversionFactor2 := FT4conversionFactor;
      end;
      FT3Item:
      begin

      end;
      cT3Item:
      begin

      end;
    end;
    for j := 0 to MAX_I do
    begin
      FLine[1].AddXY(gResponseCurve1.input[j] * conversionFactor1,
        gResponseCurve1.output[j] * conversionFactor2, '',
        xColorBox.Selected);
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
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.BParCombo2Change(Sender: TObject);
{ Get selected behavioural parameter #2 }
begin
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
  EquilibriumChartLineSeries1.SeriesColor := xColorBox.Selected;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.yColorBoxChange(Sender: TObject);
begin
  EquilibriumChartLineSeries2.SeriesColor := yColorBox.Selected;
  DrawDiagram(False);
end;

initialization
  {$I equilibriumdiagram.lrs}

end.
