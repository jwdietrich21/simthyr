unit HandlePreferences;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 5.0.0 (Mirage) }

{ (c) J. W. Dietrich, 1994 - 2023 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2023 }

{ This unit handles global application preferences }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SimThyrTypes, SimThyrResources, SimThyrBaseServices,
  SimThyrGUIServices, EnvironmentInfo, UnitConverter, SimThyrLog, SimThyrPlot,
  Sensitivityanalysis, TWSensitivityanalysis, Equilibriumdiagram, SimThyrPrediction,
  DOM, XMLRead, XMLWrite
  {$IFDEF Windows}
  , Windows
  {$ELSE}
    {$IFDEF LCLCarbon}
      , MacOSAll
    {$ELSE}
      {$IFDEF LCLCocoa}
        , CocoaAll, MacOSAll
      {$ENDIF}
    {$ENDIF}
  , Unix
  {$ENDIF}
;

const
  STANDARD_NUM_FORMAT = '###,###.00##';
  STANDARD_TIME_FORMAT = '"d"D hh:nn:ss';

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

var
  PreferencesDialog: TPreferencesDialog;
  MassPrefixes, VolumePrefixes: array[0..RES_MAX_COLS -1] of array[0..MAXFACTORS - 1] of Str3;
  MassPrefixFactors, T4MassUnitFactors, T3MassUnitFactors, VolumePrefixFactors: array[0..RES_MAX_COLS -1] of array[0..MAXFACTORS - 1] of real;

procedure InitHormoneConversionFactors;
procedure SetUnits;
function GetPreferencesFolder: String;
function GetPreferencesFile: String;
procedure ReadPreferences;

implementation

function GetPreferencesFolder: String;
  { platform-independend method to search for the location of preferences folder}
const
  kMaxPath = 1024;
{$IFDEF DARWIN}
var
  theError: OSErr;
  theRef: FSRef;
  pathBuffer: PChar;
{$ENDIF}
begin
{$IFDEF DARWIN}
    try
      pathBuffer := Allocmem(kMaxPath);
    except on exception do
      begin
        ShowMessage(PREFERENCES_READ_ERROR_MESSAGE);
        exit;
      end;
    end;
    try
      Fillchar(pathBuffer^, kMaxPath, #0);
      Fillchar(theRef, Sizeof(theRef), #0);
      theError := FSFindFolder(kOnAppropriateDisk, kPreferencesFolderType, kDontCreateFolder, theRef);
      if (pathBuffer <> nil) and (theError = noErr) then
      begin
        theError := FSRefMakePath(theRef, pathBuffer, kMaxPath);
        if theError = noErr then
          GetPreferencesFolder := UTF8ToAnsi(StrPas(pathBuffer)) + '/'
        else
          ShowMessage(PREFERENCES_SAVE_ERROR_MESSAGE);
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
  {$IFDEF DARWIN}
    GetPreferencesFile := GetPreferencesFolder + SIMTHYR_GLOBAL_ID + '.xml';
  {$ELSE}
    GetPreferencesFile := GetAppConfigFile(false);
  {$ENDIF}
end;

function InterimTSHUnit:String;
{Composes a preliminary unit string}
begin
  with PreferencesDialog do
    result := TSHMassPrefixCombo.Caption + TSHMassUnitLabel.Caption + TSHVolumePrefixCombo.Caption + TSHVolumeUnitLabel.Caption;
end;

function InterimTT4Unit:String;
{Composes a preliminary unit string}
begin
  with PreferencesDialog do
    result := TT4MassPrefixCombo.Caption + TT4MassUnitCombo.Caption + '/' + TT4VolumePrefixCombo.Caption + TT4VolumeUnitLabel.Caption;
end;

function InterimFT4Unit:String;
{Composes a preliminary unit string}
begin
  with PreferencesDialog do
    result := FT4MassPrefixCombo.Caption + FT4MassUnitCombo.Caption + '/' + FT4VolumePrefixCombo.Caption + FT4VolumeUnitLabel.Caption;
end;

function InterimTT3Unit:String;
{Composes a preliminary unit string}
begin
  with PreferencesDialog do
    result := TT3MassPrefixCombo.Caption + TT3MassUnitCombo.Caption + '/' + TT3VolumePrefixCombo.Caption + TT3VolumeUnitLabel.Caption;
end;

function InterimFT3Unit:String;
{Composes a preliminary unit string}
begin
  with PreferencesDialog do
    result := FT3MassPrefixCombo.Caption + FT3MassUnitCombo.Caption + '/' + FT3VolumePrefixCombo.Caption + FT3VolumeUnitLabel.Caption;
end;

function InterimTSHFactor:real;
{Calculates a preliminary conversion factor}
begin
  with PreferencesDialog do
    result := VolumePrefixFactors[TSH_pos, TSHVolumePrefixCombo.ItemIndex] * UTSH / MassPrefixFactors[TSH_pos, TSHMassPrefixCombo.ItemIndex];
end;

function InterimTT4Factor:real;
{Calculates a preliminary conversion factor}
begin
  with PreferencesDialog do
    begin
      result := 1 / ConvertedValue(1, T4_MOLAR_MASS, TT4MassPrefixCombo.Text + TT4MassUnitCombo.Text + '/' + TT4VolumePrefixCombo.Text + 'l', 'ng/dl');
    end;
end;

function InterimFT4Factor:real;
{Calculates a preliminary conversion factor}
begin
  with PreferencesDialog do
    begin
      result := 1 / ConvertedValue(1, T4_MOLAR_MASS, FT4MassPrefixCombo.Text + FT4MassUnitCombo.Text + '/' + FT4VolumePrefixCombo.Text + 'l', 'ng/dl');
    end;
end;

function InterimTT3Factor:real;
{Calculates a preliminary conversion factor}
begin
  with PreferencesDialog do
    begin
      result := 1 / ConvertedValue(1, T3_MOLAR_MASS, TT3MassPrefixCombo.Text + TT3MassUnitCombo.Text + '/' + TT3VolumePrefixCombo.Text + 'l', 'pg/ml');
    end;
end;

function InterimFT3Factor:real;
{Calculates a preliminary conversion factor}
begin
  with PreferencesDialog do
    begin
      result := 1 / ConvertedValue(1, T3_MOLAR_MASS, FT3MassPrefixCombo.Text + FT3MassUnitCombo.Text + '/' + FT3VolumePrefixCombo.Text + 'l', 'pg/ml');
    end;
end;

procedure InitHormoneConversionFactors;
{sets labels and factors for the elements of measurement units}
begin
  InitConversionFactors;
  T4UnitFactor[0] := 1e-3;
  T4UnitFactor[1] := 1 / T4_MOLAR_MASS * 1e-3;
  T3UnitFactor[0] := 1e-4;
  T3UnitFactor[1] := 1 / T3_MOLAR_MASS * 1e-3;
end;

procedure SetUnits;
{ sets definitive units of measurement and conversion factors for behavioural }
{ parameters, depending on constants and values of the interim units }
begin
  gParameterUnit[t_pos] := 'day h:m:s';
  gParameterUnit[TRH_pos] := 'ng/l';
  gParameterUnit[pTSH_pos] := InterimTSHUnit;
  gParameterUnit[TSH_pos] := gParameterUnit[pTSH_pos];
  gParameterUnit[TT4_pos] := InterimTT4Unit;
  gParameterUnit[FT4_pos] := InterimFT4Unit;
  gParameterUnit[TT3_pos] := InterimTT3Unit;
  gParameterUnit[FT3_pos] := InterimFT3Unit;
  gParameterUnit[cT3_pos] := InterimFT3Unit;
  gParameterFactor[TRH_pos] := 1;
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
  theComboBox.Items.Add(UnitLabel[theItem]);
  T4MassUnitFactors[par1, position] := T4UnitFactor[theItem];
  if par2 >= 0 then
    T4MassUnitFactors[par2, position] := T4UnitFactor[theItem];
end;

procedure SetT3MassUnitAndFactor(par1, par2: integer; theComboBox: TComboBox; position, theItem: integer);
begin
  theComboBox.Items.Add(UnitLabel[theItem]);
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
{sets menu items to initial values}
begin
  with PreferencesDialog.TSHMassPrefixCombo do
  begin
    Items.Clear;
    SetMassItemAndFactor(pTSH_pos, TSH_pos, PreferencesDialog.TSHMassPrefixCombo, 0, 3);
    SetMassItemAndFactor(pTSH_pos, TSH_pos, PreferencesDialog.TSHMassPrefixCombo, 1, 5);
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
    SetMassItemAndFactor(TT4_pos, -1, PreferencesDialog.TT4MassPrefixCombo, 0, 5);
    SetMassItemAndFactor(TT4_pos, -1, PreferencesDialog.TT4MassPrefixCombo, 1, 6);
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
    SetMassItemAndFactor(FT4_pos, -1, PreferencesDialog.FT4MassPrefixCombo, 0, 6);
    SetMassItemAndFactor(FT4_pos, -1, PreferencesDialog.FT4MassPrefixCombo, 1, 7);
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
    SetMassItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3MassPrefixCombo, 0, 5);
    SetMassItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3MassPrefixCombo, 1, 6);
    SetMassItemAndFactor(TT3_pos, -1, PreferencesDialog.TT3MassPrefixCombo, 2, 7);
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
    SetMassItemAndFactor(FT3_pos, -1, PreferencesDialog.FT3MassPrefixCombo, 0, 6);
    SetMassItemAndFactor(FT3_pos, -1, PreferencesDialog.FT3MassPrefixCombo, 1, 7);
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

procedure TPreferencesDialog.FormDestroy(Sender: TObject);
{Deallocates items before destroying the preferences dialog}
begin
  TSHMassPrefixCombo.Items.Clear;
  TSHMassPrefixCombo.ItemIndex := 0;
  TSHVolumePrefixCombo.Items.Clear;
  TSHVolumePrefixCombo.ItemIndex := 0;

  TT4MassPrefixCombo.Items.Clear;
  TT4MassPrefixCombo.ItemIndex := 0;
  TT4MassUnitCombo.Items.Clear;
  TT4MassUnitCombo.ItemIndex := 0;
  TT4VolumePrefixCombo.Items.Clear;
  TT4VolumePrefixCombo.ItemIndex := 0;

  FT4MassPrefixCombo.Items.Clear;
  FT4MassPrefixCombo.ItemIndex := 0;
  FT4MassUnitCombo.Items.Clear;
  FT4MassUnitCombo.ItemIndex := 0;
  FT4VolumePrefixCombo.Items.Clear;
  FT4VolumePrefixCombo.ItemIndex := 0;

  TT3MassPrefixCombo.Items.Clear;
  TT3MassPrefixCombo.ItemIndex := 0;
  TT3MassUnitCombo.Items.Clear;
  TT3MassUnitCombo.ItemIndex := 0;
  TT3VolumePrefixCombo.Items.Clear;
  TT3VolumePrefixCombo.ItemIndex := 0;

  FT3MassPrefixCombo.Items.Clear;
  FT3MassPrefixCombo.ItemIndex := 0;
  FT3MassUnitCombo.Items.Clear;
  FT3MassUnitCombo.ItemIndex := 0;
  FT3VolumePrefixCombo.Items.Clear;
  FT3VolumePrefixCombo.ItemIndex := 0;
end;

procedure TPreferencesDialog.FormCreate(Sender: TObject);
begin
  NumberFormatEdit.Text := STANDARD_NUM_FORMAT;
  DateTimeFormatEdit.Text := STANDARD_TIME_FORMAT;
  if YosemiteORNewer then
    begin
      OKButton.Height := 22;
      CancelButton.Height := 22;
    end;
end;

procedure TPreferencesDialog.CancelButtonClick(Sender: TObject);
{dismisses all entries}
begin
  NumberFormatEdit.Text := gNumberFormat;
  DateTimeFormatEdit.Text := gDateTimeFormat;
  PreferencesDialog.Close;
end;

procedure RescaleParameters;
{change scaling of simulated parameters after change of units of measurement}
begin
  SimThyrLogWindow.RedrawGrid(nil);
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

procedure SubstitutePreferences;
{get standard values for preferences if preferences file nonexistent or corrupt}
begin
  begin
    if PreferencesDialog <> nil then
    begin
      gNumberFormat := PreferencesDialog.NumberFormatEdit.Text;
      gDateTimeFormat := PreferencesDialog.DateTimeFormatEdit.Text;
    end
    else
    begin  {fall-back solution if neither file nor dialog exist}
      gNumberFormat := STANDARD_NUM_FORMAT;
      gDateTimeFormat := STANDARD_TIME_FORMAT;
    end;
  end
end;

procedure ReadPreferences;
{reads preferences from file}
{should not be called before PreferencesDialog has been created}
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  theFileName: String;
begin
  theFileName := GetPreferencesFile;
  if FileExists(theFileName) then
  try
    //Doc := TXMLDocument.Create();  Doc automatically created by ReadXMLFile
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
      if assigned(RootNode) then
        RootNode.Destroy;
    end;
    RootNode := Doc.DocumentElement.FindNode('formats');
    if not assigned(RootNode) then
      SubstitutePreferences
    else
    begin
      gNumberFormat := NodeContent(RootNode, 'numbers');
      gDateTimeFormat := NodeContent(RootNode, 'time');
    end;
  finally
    if assigned(RootNode) then
      RootNode.Destroy;
    if assigned(Doc) then
      Doc.Destroy;
  end
  else  {Standards from dialog, if preference file does not exist}
    SubstitutePreferences;
end;

procedure SavePreferences;
{saves preferences to file}
var
  theFileName, PreferencesFolder: String;
  Doc: TXMLDocument;
  StartComment: TDOMComment;
  RootNode, ElementNode: TDOMNode;
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
    ElementNode.Destroy;
    RootNode.Destroy;
    Doc.Destroy;
  end;
end;

procedure TPreferencesDialog.OKButtonClick(Sender: TObject);
{applies and saves preferences}
begin
  SetUnits;
  gNumberFormat := NumberFormatEdit.Text;
  gDateTimeFormat := DateTimeFormatEdit.Text;
  RescaleParameters; // adapts contents of log window to reflect changes in UOM
  ValuesPlot.UpdateTimeAxes; // adapts time-series plot
  ValuesPlot.ComboBox1Change(Sender);
  ValuesPlot.ComboBox2Change(Sender);
  ShowPredictedValues(false); // adapts prediction window
  DrawOWSensitivityPlot(false); // adapts one-way sensitivity analysis
  {DrawTornadoPlot; // tornado plot not affected since it displays percentages}
  TWSensitivityAnalysisForm.Rescale2DMap; // rescales two-way sensitivity analysis
  EquilibriumDiagramForm.DrawDiagram(false); // adapts equilibrium diagram
  EquilibriumDiagramForm.ResetButtonClick(Sender); // "double drawing" necessary on Mac
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

