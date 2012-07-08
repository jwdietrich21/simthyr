unit SimOptions;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ This unit supports a small dialog with additional simulation settings }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SimThyrTypes;

type

  { TSimOptionsDlg }

  TSimOptionsDlg = class(TForm)
    CancelButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    TRHEdit: TEdit;
    OKButton: TButton;
    NoiseCheckBox: TCheckBox;
    PreviewCheckBox: TCheckBox;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure NoiseCheckBoxChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PreviewCheckBoxChange(Sender: TObject);
  private
    { private declarations }
    procedure CheckNoise;
    procedure CheckPreview;
  public
    { public declarations }
  end; 

var
  SimOptionsDlg: TSimOptionsDlg;
  temp_noiseflag, temp_previewflag: boolean;

implementation

{ TSimOptionsDlg }

procedure TSimOptionsDlg.CheckNoise;
begin
  if noiseflag then
    NoiseCheckBox.Checked := true
  else
    NoiseCheckBox.Checked := false;
end;

procedure TSimOptionsDlg.CheckPreview;
begin
  if previewflag then
    PreviewCheckBox.Checked := true
  else
    PreviewCheckBox.Checked := false;
end;

procedure TSimOptionsDlg.FormShow(Sender: TObject);
begin
  temp_noiseflag := noiseflag;
  temp_previewflag := previewflag;
  TRHEdit.Text := FloatToStrF(TRHs, ffGeneral, 5, 2);
  CheckNoise;
  CheckPreview;
  ShowOnTop;
  SetFocus;
end;

procedure TSimOptionsDlg.CancelButtonClick(Sender: TObject);
begin
  SimOptionsDlg.Close;
end;

procedure TSimOptionsDlg.NoiseCheckBoxChange(Sender: TObject);
begin
  if NoiseCheckBox.Checked then
    temp_noiseflag := true
  else
    temp_noiseflag := false;
end;

procedure TSimOptionsDlg.OKButtonClick(Sender: TObject);
begin
  noiseflag := temp_noiseflag;
  previewflag := temp_previewflag;
  TRHs := StrToFloat(TRHEdit.Text);
  SimOptionsDlg.Close;
end;

procedure TSimOptionsDlg.PreviewCheckBoxChange(Sender: TObject);
begin
  if PreviewCheckBox.Checked then
    temp_previewflag := true
  else
    temp_previewflag := false;
end;

initialization
  {$I simoptions.lrs}

end.
