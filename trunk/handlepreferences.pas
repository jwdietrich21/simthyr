unit HandlePreferences;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit handles global application preferences }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SimThyrTypes;

type

  { TPreferencesDialog }

  TPreferencesDialog = class(TForm)
    TSHExampleLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    TSHMassPrefixCombo: TComboBox;
    TSHVolumePrefixCombo: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    TSHMassUnitLabel: TLabel;
    TSHVolumeUnitLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure DisplayExamples;
    procedure TSHVolumePrefixComboChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure TSHMassPrefixComboChange(Sender: TObject);
    procedure ShowPreferences;
  private
    { private declarations }
  public
    { public declarations }
  end;


var
  PreferencesDialog: TPreferencesDialog;

procedure InitConversionFactors;

implementation

{ TPreferencesDialog }

procedure TPreferencesDialog.OKButtonClick(Sender: TObject);
begin
  PreferencesDialog.Close;
end;

function TSHUnit:String;
begin
  with PreferencesDialog do
    result := TSHMassPrefixCombo.Caption + TSHMassUnitLabel.Caption + TSHVolumePrefixCombo.Caption + TSHVolumeUnitLabel.Caption;
end;

procedure InitConversionFactors;
begin
  PrefixLables[0] := '';
  PrefixLables[1] := 'm';
  PrefixLables[2] := 'µ';
  PrefixLables[3] := 'n';
  PrefixLables[4] := 'p';
  PrefixLables[5] := 'f';
  PrefixFactors[0] := 1;
  PrefixFactors[1] := 1e-3;
  PrefixFactors[2] := 1e-6;
  PrefixFactors[3] := 1e-9;
  PrefixFactors[4] := 1e-12;
  PrefixFactors[5] := 1e-15;
end;

procedure TPreferencesDialog.DisplayExamples;
begin
  TSHExampleLabel.Caption := EXAMPLE_STRING + FloatToStr(1) + ' ' + TSHUnit;
end;

procedure TPreferencesDialog.TSHVolumePrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.FormShow(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.CancelButtonClick(Sender: TObject);
begin
  PreferencesDialog.Close;
end;

procedure TPreferencesDialog.TSHMassPrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.ShowPreferences;
begin
  PreferencesDialog.ShowModal;
end;

initialization
  {$I handlepreferences.lrs}

end.

