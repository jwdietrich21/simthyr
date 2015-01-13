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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TATransformations,
  TANavigation, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Spin, ComCtrls, ColorBox, SimThyrTypes, Simulator;

type

  { TEquilibriumDiagramForm }

  TEquilibriumDiagramForm = class(TForm)
    EquilibriumChart: TChart;
    EquilibriumChartLineSeries1: TLineSeries;
    EquilibriumChartLineSeries2: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    ChartNavPanel1: TChartNavPanel;
    GroupBox2: TGroupBox;
    MaxSpinEdit1: TFloatSpinEdit;
    MaxSpinEdit2: TFloatSpinEdit;
    MinSpinEdit1: TFloatSpinEdit;
    MinSpinEdit2: TFloatSpinEdit;
    SParCombo1: TComboBox;
    BParCombo1: TComboBox;
    BParCombo2: TComboBox;
    SParCombo2: TComboBox;
    SParCombo3: TComboBox;
    SParTrackBar1: TTrackBar;
    SParTrackBar2: TTrackBar;
    SParTrackBar3: TTrackBar;
    xColorBox: TColorBox;
    FullScaleButton1: TSpeedButton;
    GroupBox1: TGroupBox;
    MainPanel: TPanel;
    ResetButton: TSpeedButton;
    StatusBar1: TStatusBar;
    yColorBox: TColorBox;
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
  public
    { public declarations }
  end;

var
  EquilibriumDiagramForm: TEquilibriumDiagramForm;
  gSelectedBParameter1, gSelectedBParameter2: tBParameter;

implementation

{ TEquilibriumDiagramForm }

procedure TEquilibriumDiagramForm.FormCreate(Sender: TObject);
begin

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
  else gSelectedBParameter1 := IItem;
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
  else gSelectedBParameter2 := IItem;
end;

procedure TEquilibriumDiagramForm.FullScaleButton1Click(Sender: TObject);
begin
  EquilibriumChart.Extent.UseYMax := False;
  EquilibriumChart.ZoomFull;
end;

procedure TEquilibriumDiagramForm.MaxSpinEdit1Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.MaxSpinEdit2Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.MinSpinEdit1Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.MinSpinEdit2Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.ResetButtonClick(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.SParCombo1Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.SParCombo2Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.SParCombo3Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.SParTrackBar1Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.SParTrackBar2Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.SParTrackBar3Change(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.xColorBoxChange(Sender: TObject);
begin
  EquilibriumChartLineSeries1.SeriesColor := xColorBox.Selected;
end;

procedure TEquilibriumDiagramForm.yColorBoxChange(Sender: TObject);
begin
  EquilibriumChartLineSeries2.SeriesColor := yColorBox.Selected;
end;

initialization
  {$I equilibriumdiagram.lrs}

end.

