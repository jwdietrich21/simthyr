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
  StdCtrls, Spin, Buttons, ExtCtrls, ColorBox, TAGraph, SimThyrTypes,
  SimThyrServices, SimThyrPrediction;

type

  { TSensitivityAnalysisForm }

  TSensitivityAnalysisForm = class(TForm)
    Chart1: TChart;
    CheckGroup1: TCheckGroup;
    ColorListBox1: TColorListBox;
    FullScaleButton1: TSpeedButton;
    MinSpinEdit: TFloatSpinEdit;
    MaxSpinEdit: TFloatSpinEdit;
    Panel1: TPanel;
    StrucParCombo: TComboBox;
    procedure FullScaleButton1Click(Sender: TObject);
    procedure MaxSpinEditChange(Sender: TObject);
    procedure MinSpinEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SensitivityAnalysisForm: TSensitivityAnalysisForm;

implementation

{ TSensitivityAnalysisForm }

procedure TSensitivityAnalysisForm.FullScaleButton1Click(Sender: TObject);
{Zooms sensitivity chart to full size}
begin
  SensitivityAnalysisForm.Chart1.Extent.UseYMax := false;
  SensitivityAnalysisForm.Chart1.ZoomFull;
end;

procedure TSensitivityAnalysisForm.MaxSpinEditChange(Sender: TObject);
begin
  if SensitivityAnalysisForm.MaxSpinEdit.Value < SensitivityAnalysisForm.MinSpinEdit.Value then
    {adapts boundaries to avoid negative intervals}
    begin
      bell;
      SensitivityAnalysisForm.MinSpinEdit.Value := SensitivityAnalysisForm.MaxSpinEdit.Value;
    end;
end;

procedure TSensitivityAnalysisForm.MinSpinEditChange(Sender: TObject);
begin
  if SensitivityAnalysisForm.MaxSpinEdit.Value < SensitivityAnalysisForm.MinSpinEdit.Value then
    {adapts boundaries to avoid negative intervals}
    begin
      bell;
      SensitivityAnalysisForm.MaxSpinEdit.Value := SensitivityAnalysisForm.MinSpinEdit.Value;
    end;
end;

initialization
  {$I sensitivityanalysis.lrs}

end.

