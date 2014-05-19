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
  ColorBox, Buttons, TAChartUtils, TANavigation, Sensitivityanalysis;

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

implementation

{ TTWSensitivityAnalysisForm }

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
    //SetStandardStrucParBoundaries(1 / 3, 3);
    //DrawOWSensitivityPlot(False);
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
    //SetStandardStrucParBoundaries(1 / 3, 3);
    //DrawOWSensitivityPlot(False);
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

