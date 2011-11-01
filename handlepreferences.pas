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
  StdCtrls, SimThyrTypes, SimThyrLog, SimThyrPlot;

type

  { TPreferencesDialog }

  TPreferencesDialog = class(TForm)
    Label6: TLabel;
    TT4VolumeUnitLabel: TLabel;
    TT4VolumePrefixCombo: TComboBox;
    TT4MassUnitCombo: TComboBox;
    TT4MassPrefixCombo: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TSHExampleLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    TT4ExampleLabel: TLabel;
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
    procedure InitMenuItems;
    procedure DisplayExamples;
    procedure TSHVolumePrefixComboChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure TSHMassPrefixComboChange(Sender: TObject);
    procedure ShowPreferences;
    procedure TT4MassPrefixComboChange(Sender: TObject);
    procedure TT4MassUnitComboChange(Sender: TObject);
    procedure TT4VolumePrefixComboChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;


var
  PreferencesDialog: TPreferencesDialog;
  MassPrefixes, VolumePrefixes: array[0..RES_MAX_COLS -1] of array[0..MAXFACTORS - 1] of Str3;
  MassPrefixFactors, T4MassUnitFactors, VolumePrefixFactors: array[0..RES_MAX_COLS -1] of array[0..MAXFACTORS - 1] of real;

procedure InitConversionFactors;
procedure SetUnits;

implementation

{ TPreferencesDialog }

function InterimTSHUnit:String;
begin
  with PreferencesDialog do
    result := TSHMassPrefixCombo.Caption + TSHMassUnitLabel.Caption + TSHVolumePrefixCombo.Caption + TSHVolumeUnitLabel.Caption;
end;

function InterimTT4Unit:String;
begin
  with PreferencesDialog do
    result := TT4MassPrefixCombo.Caption + TT4MassUnitCombo.Caption + '/' + TT4VolumePrefixCombo.Caption + TT4VolumeUnitLabel.Caption;
end;

function InterimTSHFactor:real;
begin
  with PreferencesDialog do
    result := VolumePrefixFactors[TSH_pos, TSHVolumePrefixCombo.ItemIndex] * UTSH / MassPrefixFactors[TSH_pos, TSHMassPrefixCombo.ItemIndex];
end;

function InterimTT4Factor:real;
begin
  with PreferencesDialog do
    result := VolumePrefixFactors[TT4_pos, TT4VolumePrefixCombo.ItemIndex] * 1e-4 * T4MassUnitFactors[TT4_pos, TT4MassUnitCombo.ItemIndex] / MassPrefixFactors[TT4_pos, TT4MassPrefixCombo.ItemIndex];
end;

procedure InitConversionFactors;
begin
  PrefixLabel[0] := '';
  PrefixLabel[1] := 'd';
  PrefixLabel[2] := 'c';
  PrefixLabel[3] := 'm';
  PrefixLabel[4] := 'µ';
  PrefixLabel[5] := 'n';
  PrefixLabel[6] := 'p';
  PrefixLabel[7] := 'f';
  PrefixFactor[0] := 1;
  PrefixFactor[1] := 1e-1;
  PrefixFactor[2] := 1e-2;
  PrefixFactor[3] := 1e-3;
  PrefixFactor[4] := 1e-6;
  PrefixFactor[5] := 1e-9;
  PrefixFactor[6] := 1e-12;
  PrefixFactor[7] := 1e-15;
  T4UnitLabel[0] := 'g';
  T4UnitLabel[1] := 'mol';
  T4UnitFactor[0] := 1;
  T4UnitFactor[1] := UFT4 * 1e8;
end;

procedure SetUnits;
begin
  gParameterUnit[TRH_pos] := 'ng/l';
  gParameterUnit[pTSH_pos] := InterimTSHUnit;
  gParameterUnit[TSH_pos] := gParameterUnit[pTSH_pos];
  gParameterFactor[TRH_pos] := 1;
  gParameterFactor[pTSH_pos] := 1;
  gParameterFactor[TSH_pos] := gParameterFactor[pTSH_pos];
end;

procedure SetMassItemAndFactor(par1, par2: integer; theComboBox: TComboBox; position, theItem: integer);
begin
  theComboBox.Items.Add(PrefixLabel[theItem]);
  MassPrefixFactors[par1, position] := PrefixFactor[theItem];
  if par2 >= 0 then
    MassPrefixFactors[par2, position] := PrefixFactor[theItem];
end;

procedure SetT4MassUnitAndFactor(par1, par2: integer; theComboBox: TComboBox; position, theItem: integer);
begin
  theComboBox.Items.Add(T4UnitLabel[theItem]);
  T4MassUnitFactors[par1, position] := T4UnitFactor[theItem];
  if par2 >= 0 then
    T4MassUnitFactors[par2, position] := T4UnitFactor[theItem];
end;

procedure SetVolumeItemAndFactor(par1, par2: integer; theComboBox: TComboBox; position, theItem: integer);
begin
  theComboBox.Items.Add(PrefixLabel[theItem]);
  VolumePrefixFactors[par1, position] := PrefixFactor[theItem];
  if par2 >= 0 then
    VolumePrefixFactors[par2, position] := PrefixFactor[theItem];
end;

procedure TPreferencesDialog.InitMenuItems;
begin
  with PreferencesDialog.TSHMassPrefixCombo do
  begin
    Items.Clear;
    SetMassItemAndFactor(pTSH_pos, TSH_pos, PreferencesDialog.TSHMassPrefixCombo, 0, 3);
    SetMassItemAndFactor(pTSH_pos, TSH_pos, PreferencesDialog.TSHMassPrefixCombo, 1, 4);
    ItemIndex := 0;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.TSHVolumePrefixCombo do
  begin
    Items.Clear;
    SetVolumeItemAndFactor(pTSH_pos, TSH_pos, PreferencesDialog.TSHVolumePrefixCombo, 0, 0);
    SetVolumeItemAndFactor(pTSH_pos, TSH_pos, PreferencesDialog.TSHVolumePrefixCombo, 1, 3);
    ItemIndex := 0;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.TT4MassPrefixCombo do
  begin
    Items.Clear;
    SetMassItemAndFactor(TT4_pos, -1, PreferencesDialog.TT4MassPrefixCombo, 0, 4);
    SetMassItemAndFactor(TT4_pos, -1, PreferencesDialog.TT4MassPrefixCombo, 1, 5);
    ItemIndex := 0;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.TT4MassUnitCombo do
  begin
    Items.Clear;
    SetT4MassUnitAndFactor(TT4_pos, -1, PreferencesDialog.TT4MassUnitCombo, 0, 0);
    SetT4MassUnitAndFactor(TT4_pos, -1, PreferencesDialog.TT4MassUnitCombo, 1, 1);
    ItemIndex := 0;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.TT4VolumePrefixCombo do
  begin
    Items.Clear;
    SetVolumeItemAndFactor(TT4_pos, -1, PreferencesDialog.TT4VolumePrefixCombo, 0, 0);
    SetVolumeItemAndFactor(TT4_pos, -1, PreferencesDialog.TT4VolumePrefixCombo, 1, 1);
    ItemIndex := 1;
    Text := Items[ItemIndex];
  end;
end;

procedure TPreferencesDialog.DisplayExamples;
begin
  TSHExampleLabel.Caption := EXAMPLE_STRING + FloatToStr(InterimTSHFactor) + ' ' + InterimTSHUnit;
  TT4ExampleLabel.Caption := EXAMPLE_STRING + FloatToStr(InterimTT4Factor) + ' ' + InterimTT4Unit;
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

procedure RescaleParameters;
var j, k: integer;
begin
  for j := 1 to length(gResultMatrix) do
    for k := TRH_pos to cT3_pos do
    begin
      SimThyrLogWindow.ValuesGrid.Cells[k, j] := FloatToStr(gResultMatrix[j-1, k] * gParameterFactor[k]);
    end;
end;

procedure TPreferencesDialog.OKButtonClick(Sender: TObject);
begin
  SetUnits;
  gParameterFactor[pTSH_pos] := InterimTSHFactor;
  gParameterFactor[TSH_pos] := gParameterFactor[pTSH_pos];
  gParameterFactor[TT4_pos] := InterimTT4Factor;
  RescaleParameters;
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

procedure TPreferencesDialog.TT4MassPrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.TT4MassUnitComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.TT4VolumePrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

initialization
  {$I handlepreferences.lrs}

end.

