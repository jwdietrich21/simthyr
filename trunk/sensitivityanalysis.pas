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

type

  { TSensitivityAnalysisForm }

  TSensitivityAnalysisForm = class(TForm)
    Chart1: TChart;
    CheckGroup1: TCheckGroup;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorBox4: TColorBox;
    ColorBox5: TColorBox;
    ColorBox6: TColorBox;
    ColorBox7: TColorBox;
    ColorBox8: TColorBox;
    FullScaleButton1: TSpeedButton;
    GroupBox1: TGroupBox;
    MinSpinEdit: TFloatSpinEdit;
    MaxSpinEdit: TFloatSpinEdit;
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    StrucParCombo: TComboBox;
    procedure CheckGroup1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure MaxSpinEditChange(Sender: TObject);
    procedure MinSpinEditChange(Sender: TObject);
    procedure StrucParComboChange(Sender: TObject);
  private
    { private declarations }
    FLine1, Fline2: TLineSeries;
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


procedure DrawOWSensitivityPlot(empty: boolean);

implementation

procedure DrawDummySensitivityPlot;
{Draws an empty plot}
begin
  with SensitivityAnalysisForm.Fline1 do
  begin
    ShowLines := True;
    ShowPoints := False;
    Pointer.Brush.Color := SensitivityAnalysisForm.ColorBox1.Selected;
    SeriesColor := SensitivityAnalysisForm.ColorBox1.Selected;
    SensitivityAnalysisForm.Chart1.AddSeries(SensitivityAnalysisForm.Fline1);
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'Dependent Parameter';
    SensitivityAnalysisForm.Chart1.BottomAxis.Title.Caption := SensitivityAnalysisForm.StrucParCombo.Text;
    AddXY(0, 0, '', SeriesColor);
    AddXY(100, 0, '', SeriesColor);
  end;
end;

procedure DrawOWSensitivityPlot(empty: boolean);
{Plots sensitivity analysis results}
const
  max_i = 10;
var
  i: integer;
  interval: real;
begin
  if SensitivityAnalysisForm.Fline1 <> nil then
    SensitivityAnalysisForm.Chart1.ClearSeries;
  SensitivityAnalysisForm.Fline1 := TLineSeries.Create(SensitivityAnalysisForm.Chart1);
  SensitivityAnalysisForm.Fline1.BeginUpdate;
  if empty then
    DrawDummySensitivityPlot
  else
  begin
    StoredParameters.GD1 := GD1;
    StoredParameters.GD2 := GD2;
    StoredParameters.GT := GT;

    interval := (SensitivityAnalysisForm.MaxSpinEdit.Value -
      SensitivityAnalysisForm.MinSpinEdit.Value) / max_i;

    with SensitivityAnalysisForm.Fline1 do
    begin
      ShowLines := True;
      ShowPoints := False;
      Pointer.Brush.Color := SensitivityAnalysisForm.ColorBox1.Selected;
      SeriesColor := SensitivityAnalysisForm.ColorBox1.Selected;
      SensitivityAnalysisForm.Chart1.AddSeries(SensitivityAnalysisForm.Fline1);
    end;

    for i := 0 to max_i do
    begin
      GD1 := SensitivityAnalysisForm.MinSpinEdit.Value + i * interval;
      PredictEquilibrium;
      with SensitivityAnalysisForm.Fline1 do
        AddXY(GD1, FT31, '', SeriesColor);
    end;
    SensitivityAnalysisForm.Chart1.LeftAxis.Title.Caption := 'Dependent Parameter';
    SensitivityAnalysisForm.Chart1.BottomAxis.Title.Caption := SensitivityAnalysisForm.StrucParCombo.Text;

    GD1 := StoredParameters.GD1;
    GD2 := StoredParameters.GD2;
    GT := StoredParameters.GT;
    OWSensPlotReady := True;
    PredictEquilibrium;
  end;
  SensitivityAnalysisForm.Fline1.EndUpdate;
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
  SensitivityAnalysisForm.ColorBox1.Selected := gDefaultColors[2];
  SensitivityAnalysisForm.ColorBox2.Selected := gDefaultColors[3];
  SensitivityAnalysisForm.ColorBox3.Selected := gDefaultColors[4];
  SensitivityAnalysisForm.ColorBox4.Selected := gDefaultColors[5];
  SensitivityAnalysisForm.ColorBox5.Selected := gDefaultColors[6];
  SensitivityAnalysisForm.ColorBox6.Selected := gDefaultColors[7];
  SensitivityAnalysisForm.ColorBox7.Selected := gDefaultColors[8];
  SensitivityAnalysisForm.ColorBox8.Selected := gDefaultColors[9];
  SensitivityAnalysisForm.MinSpinEdit.Value := 0;
  SensitivityAnalysisForm.MaxSpinEdit.Value := 100;
  DrawOWSensitivityPlot(True);
end;

procedure TSensitivityAnalysisForm.CheckGroup1Click(Sender: TObject);
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
  SensitivityAnalysisForm.Chart1.BottomAxis.Title.Caption := SensitivityAnalysisForm.StrucParCombo.Text;
  DrawOWSensitivityPlot(False);
end;

initialization
  {$I sensitivityanalysis.lrs}

end.

