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
  StdCtrls, SimThyrTypes, SimThyrLog, SimThyrPlot, SimThyrPrediction,
  SimThyrServices, DOM, XMLRead, XMLWrite
  {$IFDEF win32}
  , Windows
  {$ELSE}
    {$IFDEF LCLCarbon}
      , MacOSAll
    {$ENDIF}
  {$ENDIF};

type

  { TPreferencesDialog }

  TPreferencesDialog = class(TForm)
    DateTimeFormatEdit: TEdit;
    Label12: TLabel;
    NumberFormatEdit: TEdit;
    FT3MassPrefixCombo: TComboBox;
    FT3MassUnitCombo: TComboBox;
    FT3VolumePrefixCombo: TComboBox;
    GroupBox2: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    NumberFormatExampleLabel: TLabel;
    TT3MassPrefixCombo: TComboBox;
    TT3MassUnitCombo: TComboBox;
    TT3VolumePrefixCombo: TComboBox;
    FT4MassPrefixCombo: TComboBox;
    FT4MassUnitCombo: TComboBox;
    FT4VolumePrefixCombo: TComboBox;
    TT3VolumeUnitLabel: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    FT3VolumeUnitLabel: TLabel;
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
    FT4ExampleLabel: TLabel;
    TT3ExampleLabel: TLabel;
    FT3ExampleLabel: TLabel;
    TSHMassPrefixCombo: TComboBox;
    TSHVolumePrefixCombo: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    TSHMassUnitLabel: TLabel;
    TSHVolumeUnitLabel: TLabel;
    FT4VolumeUnitLabel: TLabel;
    procedure NumberFormatEditChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FT3MassPrefixComboChange(Sender: TObject);
    procedure FT3MassUnitComboChange(Sender: TObject);
    procedure FT3VolumePrefixComboChange(Sender: TObject);
    procedure FT4MassPrefixComboChange(Sender: TObject);
    procedure FT4MassUnitComboChange(Sender: TObject);
    procedure FT4VolumePrefixComboChange(Sender: TObject);
    procedure InitMenuItems;
    procedure DisplayExamples;
    procedure TSHVolumePrefixComboChange(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure TSHMassPrefixComboChange(Sender: TObject);
    procedure ShowPreferences;
    procedure TT3MassPrefixComboChange(Sender: TObject);
    procedure TT3MassUnitComboChange(Sender: TObject);
    procedure TT3VolumePrefixComboChange(Sender: TObject);
    procedure TT4MassPrefixComboChange(Sender: TObject);
    procedure TT4MassUnitComboChange(Sender: TObject);
    procedure TT4VolumePrefixComboChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TUnitElements = record
    MassPrefix, MassUnit, VolumePrefix, VolumeUnit: String;
  end;


var
  PreferencesDialog: TPreferencesDialog;
  MassPrefixes, VolumePrefixes: array[0..RES_MAX_COLS -1] of array[0..MAXFACTORS - 1] of Str3;
  MassPrefixFactors, T4MassUnitFactors, T3MassUnitFactors, VolumePrefixFactors: array[0..RES_MAX_COLS -1] of array[0..MAXFACTORS - 1] of real;

procedure InitConversionFactors;
procedure SetUnits;
function GetPreferencesFolder: String;
function GetPreferencesFile: String;
procedure ReadPreferences;

implementation

function GetPreferencesFolder: String;
  { platform-independend method to search for the location of preferences folder}
const
  kMaxPath = 1024;
var
  {$IFDEF LCLCarbon}
  theError: OSErr;
  theRef: FSRef;
  {$ENDIF}
  pathBuffer: PChar;
begin
  {$IFDEF LCLCarbon}
    try
      pathBuffer := Allocmem(kMaxPath);
    except on exception do exit;
    end;
    try
      Fillchar(pathBuffer^, kMaxPath, #0);
      Fillchar(theRef, Sizeof(theRef), #0);
      theError := FSFindFolder(kOnAppropriateDisk, kPreferencesFolderType, kDontCreateFolder, theRef);
      if (pathBuffer <> nil) and (theError = noErr) then
      begin
        theError := FSRefMakePath(theRef, pathBuffer, kMaxPath);
        if theError = noErr then GetPreferencesFolder := UTF8ToAnsi(StrPas(pathBuffer)) + '/';
      end;
    finally
      Freemem(pathBuffer);
    end
  {$ELSE}
    GetPreferencesFolder := GetAppConfigDir(false);
  {$ENDIF}
end;

function GetPreferencesFile: String;
begin
  {$IFDEF LCLCarbon}
    GetPreferencesFile := GetPreferencesFolder + SIMTHYR_GLOBAL_ID + '.xml';
  {$ELSE}
    GetPreferencesFile := GetAppConfigFile(false);
  {$ENDIF}
end;

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

function InterimFT4Unit:String;
begin
  with PreferencesDialog do
    result := FT4MassPrefixCombo.Caption + FT4MassUnitCombo.Caption + '/' + FT4VolumePrefixCombo.Caption + FT4VolumeUnitLabel.Caption;
end;

function InterimTT3Unit:String;
begin
  with PreferencesDialog do
    result := TT3MassPrefixCombo.Caption + TT3MassUnitCombo.Caption + '/' + TT3VolumePrefixCombo.Caption + TT3VolumeUnitLabel.Caption;
end;

function InterimFT3Unit:String;
begin
  with PreferencesDialog do
    result := FT3MassPrefixCombo.Caption + FT3MassUnitCombo.Caption + '/' + FT3VolumePrefixCombo.Caption + FT3VolumeUnitLabel.Caption;
end;

function InterimTSHFactor:real;
begin
  with PreferencesDialog do
    result := VolumePrefixFactors[TSH_pos, TSHVolumePrefixCombo.ItemIndex] * UTSH / MassPrefixFactors[TSH_pos, TSHMassPrefixCombo.ItemIndex];
end;

function InterimTT4Factor:real;
begin
  with PreferencesDialog do
    result := VolumePrefixFactors[TT4_pos, TT4VolumePrefixCombo.ItemIndex] * 1e-5 * T4MassUnitFactors[TT4_pos, TT4MassUnitCombo.ItemIndex] / MassPrefixFactors[TT4_pos, TT4MassPrefixCombo.ItemIndex];
end;

function InterimFT4Factor:real;
begin
  with PreferencesDialog do
    result := VolumePrefixFactors[FT4_pos, FT4VolumePrefixCombo.ItemIndex] * 1e-5 * T4MassUnitFactors[FT4_pos, FT4MassUnitCombo.ItemIndex] / MassPrefixFactors[FT4_pos, FT4MassPrefixCombo.ItemIndex];
end;

function InterimTT3Factor:real;
begin
  with PreferencesDialog do
    result := VolumePrefixFactors[TT3_pos, TT3VolumePrefixCombo.ItemIndex] * 1e-5 * T3MassUnitFactors[TT3_pos, TT3MassUnitCombo.ItemIndex] / MassPrefixFactors[TT3_pos, TT3MassPrefixCombo.ItemIndex];
end;

function InterimFT3Factor:real;
begin
  with PreferencesDialog do
    result := VolumePrefixFactors[FT3_pos, FT3VolumePrefixCombo.ItemIndex] * 1e-5 * T3MassUnitFactors[FT3_pos, FT3MassUnitCombo.ItemIndex] / MassPrefixFactors[FT3_pos, FT3MassPrefixCombo.ItemIndex];
end;

procedure InitConversionFactors;
{sets labels and factors for the elements of measurement units}
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
  T4UnitFactor[0] := 1e-3;
  T4UnitFactor[1] := UFT4 * 1e5;
  T3UnitLabel[0] := 'g';
  T3UnitLabel[1] := 'mol';
  T3UnitFactor[0] := 1e-4;
  T3UnitFactor[1] := UFT3 * 1e5;
end;

procedure SetUnits;
{sets definitive units to the values of the interim units}
begin
  gParameterUnit[TRH_pos] := 'ng/l';
  gParameterUnit[pTSH_pos] := InterimTSHUnit;
  gParameterUnit[TSH_pos] := gParameterUnit[pTSH_pos];
  gParameterUnit[TT4_pos] := InterimTT4Unit;
  gParameterUnit[FT4_pos] := InterimFT4Unit;
  gParameterUnit[TT3_pos] := InterimTT3Unit;
  gParameterUnit[FT3_pos] := InterimFT3Unit;
  gParameterUnit[cT3_pos] := InterimFT3Unit;
  gParameterFactor[TRH_pos] := 1;
  gParameterFactor[pTSH_pos] := 1;
  gParameterFactor[TSH_pos] := gParameterFactor[pTSH_pos];
  gParameterFactor[pTSH_pos] := InterimTSHFactor;
  gParameterFactor[TSH_pos] := gParameterFactor[pTSH_pos];
  gParameterFactor[TT4_pos] := InterimTT4Factor;
  gParameterFactor[FT4_pos] := InterimFT4Factor;
  gParameterFactor[TT3_pos] := InterimTT3Factor;
  gParameterFactor[FT3_pos] := InterimFT3Factor;
  gParameterFactor[cT3_pos] := InterimFT3Factor;
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

procedure SetT3MassUnitAndFactor(par1, par2: integer; theComboBox: TComboBox; position, theItem: integer);
begin
  theComboBox.Items.Add(T3UnitLabel[theItem]);
  T3MassUnitFactors[par1, position] := T3UnitFactor[theItem];
  if par2 >= 0 then
    T3MassUnitFactors[par2, position] := T3UnitFactor[theItem];
end;

procedure SetVolumeItemAndFactor(par1, par2: integer; theComboBox: TComboBox; position, theItem: integer);
begin
  theComboBox.Items.Add(PrefixLabel[theItem]);
  VolumePrefixFactors[par1, position] := PrefixFactor[theItem];
  if par2 >= 0 then
    VolumePrefixFactors[par2, position] := PrefixFactor[theItem];
end;

{ TPreferencesDialog }

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
    ItemIndex := 1;
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
  with PreferencesDialog.FT4MassPrefixCombo do
  begin
    Items.Clear;
    SetMassItemAndFactor(FT4_pos, -1, PreferencesDialog.FT4MassPrefixCombo, 0, 5);
    SetMassItemAndFactor(FT4_pos, -1, PreferencesDialog.FT4MassPrefixCombo, 1, 6);
    ItemIndex := 0;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.FT4MassUnitCombo do
  begin
    Items.Clear;
    SetT4MassUnitAndFactor(FT4_pos, -1, PreferencesDialog.FT4MassUnitCombo, 0, 0);
    SetT4MassUnitAndFactor(FT4_pos, -1, PreferencesDialog.FT4MassUnitCombo, 1, 1);
    ItemIndex := 0;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.FT4VolumePrefixCombo do
  begin
    Items.Clear;
    SetVolumeItemAndFactor(FT4_pos, -1, PreferencesDialog.FT4VolumePrefixCombo, 0, 0);
    SetVolumeItemAndFactor(FT4_pos, -1, PreferencesDialog.FT4VolumePrefixCombo, 1, 1);
    ItemIndex := 1;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.TT3MassPrefixCombo do
  begin
    Items.Clear;
    SetMassItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3MassPrefixCombo, 0, 4);
    SetMassItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3MassPrefixCombo, 1, 5);
    SetMassItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3MassPrefixCombo, 2, 6);
    ItemIndex := 2;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.TT3MassUnitCombo do
  begin
    Items.Clear;
    SetT3MassUnitAndFactor(TT3_pos, -1, PreferencesDialog.TT3MassUnitCombo, 0, 0);
    SetT3MassUnitAndFactor(TT3_pos, -1, PreferencesDialog.TT3MassUnitCombo, 1, 1);
    ItemIndex := 0;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.TT3VolumePrefixCombo do
  begin
    Items.Clear;
    SetVolumeItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3VolumePrefixCombo, 0, 0);
    SetVolumeItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3VolumePrefixCombo, 1, 1);
    SetVolumeItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3VolumePrefixCombo, 2, 3);
    ItemIndex := 2;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.FT3MassPrefixCombo do
  begin
    Items.Clear;
    SetMassItemAndFactor(FT3_pos, -1, PreferencesDialog.FT3MassPrefixCombo, 0, 5);
    SetMassItemAndFactor(FT3_pos, -1, PreferencesDialog.FT3MassPrefixCombo, 1, 6);
    ItemIndex := 1;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.FT3MassUnitCombo do
  begin
    Items.Clear;
    SetT3MassUnitAndFactor(FT3_pos, -1, PreferencesDialog.FT3MassUnitCombo, 0, 0);
    SetT3MassUnitAndFactor(FT3_pos, -1, PreferencesDialog.FT3MassUnitCombo, 1, 1);
    ItemIndex := 0;
    Text := Items[ItemIndex];
  end;
  with PreferencesDialog.FT3VolumePrefixCombo do
  begin
    Items.Clear;
    SetVolumeItemAndFactor(FT3_pos, -1, PreferencesDialog.FT3VolumePrefixCombo, 0, 0);
    SetVolumeItemAndFactor(FT3_pos, -1, PreferencesDialog.FT3VolumePrefixCombo, 1, 1);
    SetVolumeItemAndFactor(FT3_pos, -1, PreferencesDialog.FT3VolumePrefixCombo, 2, 3);
    ItemIndex := 2;
    Text := Items[ItemIndex];
  end;
end;

procedure TPreferencesDialog.DisplayExamples;
{Shows an example text in the preferences window}
var theString: String;
begin
  theString := EXAMPLE_STRING + FloatToStr(InterimTSHFactor) + ' ' + InterimTSHUnit;
  TSHExampleLabel.Caption := theString;
  TT4ExampleLabel.Caption := EXAMPLE_STRING + FloatToStr(InterimTT4Factor * 1e4) + ' ' + InterimTT4Unit;
  FT4ExampleLabel.Caption := EXAMPLE_STRING + FloatToStr(InterimFT4Factor * 1) + ' ' + InterimFT4Unit;
  TT3ExampleLabel.Caption := EXAMPLE_STRING + FloatToStr(InterimTT3Factor * 1e3) + ' ' + InterimTT3Unit;
  FT3ExampleLabel.Caption := EXAMPLE_STRING + FloatToStr(InterimFT3Factor * 3) + ' ' + InterimFT3Unit;
  NumberFormatExampleLabel.Caption := FormatFloat(NumberFormatEdit.Text, 123456.789);
end;

procedure TPreferencesDialog.FormShow(Sender: TObject);
begin
  if not gStartup then
    begin
      DisplayExamples;
      NumberFormatEdit.Text := gNumberFormat;
      DateTimeFormatEdit.Text := gDateTimeFormat;
    end;
end;

procedure TPreferencesDialog.NumberFormatEditChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.CancelButtonClick(Sender: TObject);
begin
  NumberFormatEdit.Text := gNumberFormat;
  DateTimeFormatEdit.Text := gDateTimeFormat;
  PreferencesDialog.Close;
end;

procedure RescaleParameters;
var j, k: integer;
begin
  for j := 1 to length(gResultMatrix) do
    begin
      SimThyrLogWindow.ValuesGrid.Cells[t_pos, j] := FormattedTime(gResultMatrix[j-1, t_pos]);
      for k := TRH_pos to cT3_pos do
      begin
        SimThyrLogWindow.ValuesGrid.Cells[k, j] := FormatFloat(gNumberFormat, gResultMatrix[j-1, k] * gParameterFactor[k]);
      end;
    end;
end;

function ParsedUnitString(theString: String): TUnitElements;
  { parses a string for measurement unit and breaks it up in single parts }
var
  theElements: TUnitElements;
begin
  if theString <> 'NA' then
    begin
      with theElements do
      begin
        if copy(theString, 1, 1) = 'm' then
          begin
            if copy(theString, 2, 1) = 'c' then
              MassPrefix := PrefixLabel[4]; {mc -> µ}
            end
          else
            MassPrefix := copy(theString, 1, 1);
        MassUnit := copy(theString, 2, pos('/', theString) - 2);
        VolumePrefix := copy(theString, pos('/', theString) + 1, 1);
        VolumeUnit := 'l';
        if VolumePrefix = VolumeUnit then VolumePrefix := '';  {no prefix set}
      end;
    end
  else
  begin
    with theElements do
    begin
      MassPrefix := 'NA';
      MassUnit := 'NA';
      VolumePrefix := 'NA';
      VolumeUnit := 'NA';
    end;
  end;
  ParsedUnitString := theElements;
end;

procedure SetTSHUnitCombo;
var
  theElements: TUnitElements;
  i, max: integer;
begin
  theElements := ParsedUnitString(gParameterUnit[pTSH_pos]);
  with PreferencesDialog do
    begin
    max := TSHMassPrefixCombo.Items.Count;
    for i := 0 to max -1 do
    begin
      if TSHMassPrefixCombo.Items[i] = theElements.MassPrefix then
        TSHMassPrefixCombo.ItemIndex := i;
    end;
    max := TSHVolumePrefixCombo.Items.Count;
    for i := 0 to max -1 do
    begin
      if TSHVolumePrefixCombo.Items[i] = theElements.VolumePrefix then
        TSHVolumePrefixCombo.ItemIndex := i;
    end;
  end;
end;

procedure SetCombo(MassPrefixCombo, MassUnitCombo, VolumePrefixCombo: TComboBox; UnitString: String);
var
  theElements: TUnitElements;
  i, max: integer;
begin
  theElements := ParsedUnitString(UnitString);
  max := MassPrefixCombo.Items.Count;
  for i := 0 to max -1 do
  begin
    if MassPrefixCombo.Items[i] = theElements.MassPrefix then
      MassPrefixCombo.ItemIndex := i;
  end;
  max := MassUnitCombo.Items.Count;
  for i := 0 to max -1 do
  begin
    if MassUnitCombo.Items[i] = theElements.MassUnit then
      MassUnitCombo.ItemIndex := i;
  end;
  max := VolumePrefixCombo.Items.Count;
  for i := 0 to max -1 do
  begin
    if VolumePrefixCombo.Items[i] = theElements.VolumePrefix then
      VolumePrefixCombo.ItemIndex := i;
  end;
end;

procedure ReadPreferences; {should not be called before PreferencesDialog has been created}
var
  Doc: TXMLDocument;
  RootNode, theNode: TDOMNode;
  theFileName, theString: String;
begin
  theFileName := GetPreferencesFile;
  if FileExists(theFileName) then
  try
    Doc := TXMLDocument.Create();
    ReadXMLFile(Doc, theFileName);
    with PreferencesDialog do
    begin
      RootNode := Doc.DocumentElement.FindNode('units');
      gParameterUnit[pTSH_pos] := NodeContent(RootNode, 'TSH');
      gParameterUnit[TSH_pos] := gParameterUnit[pTSH_pos];
      SetTSHUnitCombo;
      gParameterUnit[TT4_pos] := NodeContent(RootNode, 'TT4');
      SetCombo(TT4MassPrefixCombo, TT4MassUnitCombo, TT4VolumePrefixCombo, gParameterUnit[TT4_pos]);
      gParameterUnit[FT4_pos] := NodeContent(RootNode, 'FT4');
      SetCombo(FT4MassPrefixCombo, FT4MassUnitCombo, FT4VolumePrefixCombo, gParameterUnit[FT4_pos]);
      gParameterUnit[TT3_pos] := NodeContent(RootNode, 'TT3');
      SetCombo(TT3MassPrefixCombo, TT3MassUnitCombo, TT3VolumePrefixCombo, gParameterUnit[TT3_pos]);
      gParameterUnit[FT3_pos] := NodeContent(RootNode, 'FT3');
      SetCombo(FT3MassPrefixCombo, FT3MassUnitCombo, FT3VolumePrefixCombo, gParameterUnit[FT3_pos]);
      gParameterUnit[cT3_pos] := gParameterUnit[FT3_pos];
    end;
    RootNode := Doc.DocumentElement.FindNode('formats');
    gNumberFormat := NodeContent(RootNode, 'numbers');
    gDateTimeFormat := NodeContent(RootNode, 'time');
  finally
    Doc.Free;
  end
  else  {Standards from dialog, if preference file does not exist}
  if PreferencesDialog <> nil then begin
    gNumberFormat := PreferencesDialog.NumberFormatEdit.Text;
    gDateTimeFormat := PreferencesDialog.DateTimeFormatEdit.Text;
  end
  else
  begin  {fall-back solution, if neither file nor dialog exist}
    gNumberFormat := '###,###.00##';
    gDateTimeFormat := '"d"D hh:nn:ss';
  end;
end;

procedure SavePreferences;
var
  theFileName, PreferencesFolder: String;
  Doc: TXMLDocument;
  StartComment: TDOMComment;
  RootNode, ElementNode, ItemNode, TextNode: TDOMNode;
begin
  theFileName := GetPreferencesFile;
  PreferencesFolder := GetPreferencesFolder;
  try
    Doc := TXMLDocument.Create;

    {StartComment := Doc.CreateComment('SimThyr Preferences');
    RootNode.AppendChild(StartComment);}

    RootNode := Doc.CreateElement('preferences');
    Doc.Appendchild(RootNode);
    RootNode:= Doc.DocumentElement;

    ElementNode:=Doc.CreateElement('units');

    ElementNode.AppendChild(SimpleNode(Doc, 'TSH', EncodeGreek(gParameterUnit[pTSH_pos])));
    ElementNode.AppendChild(SimpleNode(Doc, 'TT4', EncodeGreek(gParameterUnit[TT4_pos])));
    ElementNode.AppendChild(SimpleNode(Doc, 'FT4', EncodeGreek(gParameterUnit[FT4_pos])));
    ElementNode.AppendChild(SimpleNode(Doc, 'TT3', EncodeGreek(gParameterUnit[TT3_pos])));
    ElementNode.AppendChild(SimpleNode(Doc, 'FT3', EncodeGreek(gParameterUnit[FT3_pos])));

    RootNode.AppendChild(ElementNode);

    ElementNode:=Doc.CreateElement('formats');

    ElementNode.AppendChild(SimpleNode(Doc, 'numbers', gNumberFormat));
    ElementNode.AppendChild(SimpleNode(Doc, 'time', gDateTimeFormat));

    RootNode.AppendChild(ElementNode);

    if not DirectoryExists(PreferencesFolder) then
      if not CreateDir(PreferencesFolder) then
        ShowMessage(PREFERENCES_SAVE_ERROR_MESSAGE);
    if DirectoryExists(PreferencesFolder) then
        begin
          if FileExists(theFileName) then
            SysUtils.DeleteFile(theFileName);
          WriteXMLFile(Doc,theFileName);
        end;
  finally
    Doc.Free;
  end;
end;

procedure TPreferencesDialog.OKButtonClick(Sender: TObject);
begin
  SetUnits;
  gNumberFormat := NumberFormatEdit.Text;
  gDateTimeFormat := DateTimeFormatEdit.Text;
  RescaleParameters;
  ValuesPlot.UpdateTimeAxes;
  ValuesPlot.ComboBox1Change(Sender);
  ValuesPlot.ComboBox2Change(Sender);
  ShowPredictedValues;
  SavePreferences;
  PreferencesDialog.Close;
end;

procedure TPreferencesDialog.ShowPreferences;
begin
  PreferencesDialog.ShowModal;
end;

procedure TPreferencesDialog.TSHMassPrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.TSHVolumePrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.FT3MassPrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.FT3MassUnitComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.FT3VolumePrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.TT3MassPrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.TT3MassUnitComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.TT3VolumePrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
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

procedure TPreferencesDialog.FT4MassPrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.FT4MassUnitComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

procedure TPreferencesDialog.FT4VolumePrefixComboChange(Sender: TObject);
begin
  DisplayExamples;
end;

initialization
  {$I handlepreferences.lrs}

end.

