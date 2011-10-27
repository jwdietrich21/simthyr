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
  StdCtrls, SimThyrTypes, SimThyrPlot;

type

  { TPreferencesDialog }

  TPreferencesDialog = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TSHExampleLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    TT4ExampleLabel1: TLabel;
    FT4ExampleLabel2: TLabel;
    TT3ExampleLabel3: TLabel;
    FT3ExampleLabel4: TLabel;
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
procedure SetUnits;

implementation

{ TPreferencesDialog }

function InterimTSHUnit:String;
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

procedure SetUnits;
begin
  gParameterUnit[0] := 'ng/l';
  gParameterUnit[1] := InterimTSHUnit;
  gParameterUnit[2] := InterimTSHUnit;
  gParameterUnit[3] := '';
  gParameterUnit[4] := '';
  gParameterUnit[5] := '';
  gParameterUnit[6] := '';
  gParameterUnit[7] := '';
end;

procedure TPreferencesDialog.DisplayExamples;
begin
  TSHExampleLabel.Caption := EXAMPLE_STRING + FloatToStr(1) + ' ' + InterimTSHUnit;
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

procedure TPreferencesDialog.OKButtonClick(Sender: TObject);
begin
  SetUnits;
  ValuesPlot.ComboBox1Change(Sender);
  ValuesPlot.ComboBox2Change(Sender);
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

