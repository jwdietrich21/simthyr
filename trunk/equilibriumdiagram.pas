unit Equilibriumdiagram;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit implements an equilibrium diagram }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TATransformations,
  TANavigation, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, StdCtrls, Spin, ComCtrls, ColorBox;

type

  { TEquilibriumDiagramForm }

  TEquilibriumDiagramForm = class(TForm)
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LinearAxisTransform1: TLinearAxisTransform;
    ChartNavPanel1: TChartNavPanel;
    GroupBox2: TGroupBox;
    MaxSpinEdit1: TFloatSpinEdit;
    MaxSpinEdit2: TFloatSpinEdit;
    MinSpinEdit1: TFloatSpinEdit;
    MinSpinEdit2: TFloatSpinEdit;
    StrucParCombo1: TComboBox;
    BehParCombo1: TComboBox;
    BehParCombo2: TComboBox;
    StrucParCombo2: TComboBox;
    StrucParCombo3: TComboBox;
    StrucParTrackBar1: TTrackBar;
    StrucParTrackBar2: TTrackBar;
    StrucParTrackBar3: TTrackBar;
    xColorBox: TColorBox;
    FullScaleButton1: TSpeedButton;
    GroupBox1: TGroupBox;
    Panel1: TPanel;
    ResetButton: TSpeedButton;
    StatusBar1: TStatusBar;
    yColorBox: TColorBox;
    procedure FormCreate(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  EquilibriumDiagramForm: TEquilibriumDiagramForm;

implementation

{ TEquilibriumDiagramForm }

procedure TEquilibriumDiagramForm.FormCreate(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.FullScaleButton1Click(Sender: TObject);
begin
  EquilibriumDiagramForm.Chart1.Extent.UseYMax := False;
  EquilibriumDiagramForm.Chart1.ZoomFull;
end;

procedure TEquilibriumDiagramForm.ResetButtonClick(Sender: TObject);
begin

end;

initialization
  {$I equilibriumdiagram.lrs}

end.

