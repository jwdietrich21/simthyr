unit SimOptions;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.5 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ This unit supports a small dialog with additional simulation settings }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, SimThyrTypes, SimThyrServices, EnvironmentInfo;

type

  { TSimOptionsDlg }

  TSimOptionsDlg = class(TForm)
    CancelButton: TButton;
    CircadianCheckBox: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    NeuronImage: TImage;
    TRHEdit: TEdit;
    OKButton: TButton;
    NoiseCheckBox: TCheckBox;
    PreviewCheckBox: TCheckBox;
    procedure CancelButtonClick(Sender: TObject);
    procedure CircadianCheckBoxChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NoiseCheckBoxChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PreviewCheckBoxChange(Sender: TObject);
  private
    { private declarations }
    procedure CheckNoise;
    procedure CheckPreview;
    procedure CheckCircadian;
  public
    { public declarations }
    parentForm: TForm;
  end; 

var
  SimOptionsDlg: TSimOptionsDlg;
  temp_noiseflag, temp_previewflag, temp_circadianflag: boolean;

implementation

uses
  LaunchDialog, StructureParameters;

{ TSimOptionsDlg }

procedure TSimOptionsDlg.CheckNoise;
{flag for simulating noise}
begin
  NoiseCheckBox.Checked := noiseflag;
end;

procedure TSimOptionsDlg.CheckPreview;
{flag for beginning simulation with predicted values}
begin
   PreviewCheckBox.Checked := previewflag;
end;

procedure TSimOptionsDlg.CheckCircadian;
{flag for beginning simulation with predicted values}
begin
  CircadianCheckBox.Checked := circadianflag;
end;

procedure TSimOptionsDlg.FormShow(Sender: TObject);
{code executed on show}
begin
  temp_noiseflag := noiseflag;
  temp_previewflag := previewflag;
  temp_circadianflag := circadianflag;
  TRHEdit.Text := FloatToStrF(gActiveModel.Equilibrium.TRHs, ffGeneral, 5, 2);
  {$IFDEF LCLCocoa}
  parentForm := Screen.ActiveForm;
  if parentForm = SimulationSettings then
    SimulationSettings.Close
  else if parentForm = StructureParametersDlg then
    StructureParametersDlg.Close;
  {$ENDIF}
  FormPaint(Sender);
  ShowOnTop;
  CheckNoise;
  CheckPreview;
  CheckCircadian;
  SetFocus;
end;

procedure TSimOptionsDlg.CancelButtonClick(Sender: TObject);
{closes dialog without reading any data}
begin
  Close;
  {$IFDEF LCLCocoa}
  if parentForm = SimulationSettings then
    SimulationSettings.ShowModal
  else if parentForm = StructureParametersDlg then
    StructureParametersDlg.ShowModal;
  {$ENDIF}
end;

procedure TSimOptionsDlg.FormCreate(Sender: TObject);
begin
  if YosemiteORNewer then
    begin
      OKButton.Height := 22;
      CancelButton.Height := 22;
    end;
end;

procedure TSimOptionsDlg.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := BACKCOLOUR;
  end
  else
  begin
    Color := clWhite;
  end
end;

procedure TSimOptionsDlg.PreviewCheckBoxChange(Sender: TObject);
{flag for beginning simulation with predicted values}
begin
  temp_previewflag := PreviewCheckBox.Checked;
end;

procedure TSimOptionsDlg.NoiseCheckBoxChange(Sender: TObject);
{flag for simulating noise}
begin
  temp_noiseflag := NoiseCheckBox.Checked;
end;

procedure TSimOptionsDlg.CircadianCheckBoxChange(Sender: TObject);
begin
  temp_circadianflag := CircadianCheckBox.Checked;
end;

procedure TSimOptionsDlg.FormActivate(Sender: TObject);
begin
  ShowOnTop;
  SetFocus;
end;

procedure TSimOptionsDlg.OKButtonClick(Sender: TObject);
{closes dialog and accepts entered data}
begin
  noiseflag := temp_noiseflag;
  previewflag := temp_previewflag;
  circadianflag := temp_circadianflag;
  gActiveModel.Equilibrium.TRHs := StrToFloat(TRHEdit.Text);
  Close;
  {$IFDEF LCLCocoa}
  if parentForm = SimulationSettings then
    SimulationSettings.ShowModal
  else if parentForm = StructureParametersDlg then
    StructureParametersDlg.ShowModal;
  {$ENDIF}
end;

initialization
  {$I simoptions.lrs}

end.

