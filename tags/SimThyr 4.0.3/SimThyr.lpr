program SimThyr;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.3 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

{ This is the main project file }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}{$R+}
{$define UseCThreads}

{$undef debug} // Additional debugging information recorded if defined

uses
{$IFDEF UNIX}
{$IFDEF UseCThreads}
  cthreads,
{$ENDIF}
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SimThyrMain,
  Controls,
  TAChartLazarusPkg,
  Math,
  SimThyrPlot,
  LaunchDialog,
  simthyrlog,
  SimThyrTypes,
  SimThyrServices,
  ShowIPS,
  Simulator,
  Splash,
  AboutDialog,
  ShowAboutModel,
  SimThyrPrediction,
  StructureParameters,
  SimOptions,
  ScenarioHandler,
  HandlePreferences,
  HandleNotifier,
  Sensitivityanalysis,
  tornado,
  DIFSupport,
  help,
  SimThyrResources,
  unitconverter,
  TWSensitivityanalysis,
  equilibriumdiagram,
  MIRIAMForm,
  plotoptions,
  EnvironmentInfo
 {$IFDEF debug}  ,
  SysUtils // heaptrc unit is to be included with Lazarus project options
 {$ENDIF} ;

{$R *.res}

begin
  // ReturnNilIfGrowHeapFails := true;
  {$IFDEF debug}// Diagnostic code for developmental versions
  if FileExists(kHeapTraceFile) then
    DeleteFile(kHeapTraceFile);
  SetHeapTraceOutput(kHeapTraceFile);
  {$ENDIF}
  Application.Initialize;
  splashflag := True; {splash screen can be switched off for debugging}
  gPigMode := False;
  showSettingsAtStartup := True;
  if splashflag then
  begin
    SplashScreen := TSplashScreen.Create(nil);
    SplashScreen.ShowOnTop;
    SplashScreen.FormStyle := fsSplash;
    SplashScreen.AlphaBlendValue := 200;
    Application.ProcessMessages;
  end;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
    exOverflow, exUnderflow, exPrecision]);
  gStartup := True;
  runcommand := False;
  simready := True;
  randomize;
  gActiveModel := NewScenario;
  InitSimulationControl;
  SetBaseVariables;
  StandardValues;
  InitHormoneConversionFactors;
  testflag := False;
  tbgflag := False;
  previewflag := True;
  noiseflag := True;
  circadianflag := True;
  haltsim := False;
  Application.CreateForm(TSimThyrToolbar, SimThyrToolbar);
  SimThyrToolbar.SetPosition;
  if splashflag then
    SplashScreen.Update;
  Application.BringToFront;
  Application.CreateForm(TAboutWindow, AboutWindow);
  AboutWindow.Hide;
  AboutWindow.AlphaBlend := False;
  Application.CreateForm(TAboutModelForm, AboutModelForm);
  AboutModelForm.Hide;
  AboutModelForm.AlphaBlend := False;
  Application.CreateForm(TPreferencesDialog, PreferencesDialog);
  PreferencesDialog.Hide;
  PreferencesDialog.AlphaBlend := False;
  PreferencesDialog.InitMenuItems;
  ReadPreferences;
  SetUnits;
  Application.CreateForm(TStructureParameters, StructureParametersDlg);
  StructureParametersDlg.Hide;
  StructureParametersDlg.AlphaBlend := False;
  Application.CreateForm(TIPSForm, IPSForm);
  with IPSForm do
  begin
    Hide;
    AlphaBlend := False;
  end;
  IPSForm.Show;
  Application.CreateForm(TSimThyrLogWindow, SimThyrLogWindow);
  with SimThyrLogWindow do
  begin
    Hide;
    InitGrid;
    Top := SimThyrToolbar.Top + SimThyrToolbar.Height + 52;
    Left := 26;
    if Screen.Width > 1024 then
      Width := 2 * Screen.Width div 3;
    if Screen.Height > 800 then
      Height := Screen.Height div 2 - Top - 52;
    AlphaBlend := False;
  end;
  Application.CreateForm(TValuesPlot, ValuesPlot);
  with ValuesPlot do
  begin
    Hide;
    top := Screen.Height - Height - 92;
    left := Screen.Width - Width - 52;
    AlphaBlend := False;
  end;
  Application.CreateForm(TPrediction, Prediction);
  Prediction.Hide;
  Prediction.AlphaBlend := False;
  Prediction.Left := Screen.DesktopWidth - Prediction.Width - 13;
  if Screen.MonitorCount > 1 then
    if Screen.Monitors[0].Primary then
      Prediction.MakeFullyVisible(Screen.Monitors[1])
    else
      Prediction.MakeFullyVisible(Screen.Monitors[0]);
  Prediction.Show;
  Application.CreateForm(TSimOptionsDlg, SimOptionsDlg);
  SimOptionsDlg.parentForm := nil;
  SimOptionsDlg.Hide;
  SimOptionsDlg.AlphaBlend := False;
  Notice := TNotice.Create(SimThyrLogWindow);
  Notice.Hide;
  Notice.NoticeLabel.Caption := WAIT_TITLE;
  Application.CreateForm(TAnnotationForm, AnnotationForm);
  AnnotationForm.Hide;
  Application.CreateForm(TSimulationSettings, SimulationSettings);
  SimThyrToolbar.Show;
  ValuesPlot.Show;
  if ValuesPlot.Left + ValuesPLot.Width >= Screen.Width then
    ValuesPlot.Left := Screen.Width - ValuesPLot.Width - 26;
  SimThyrLogWindow.Show;
  Application.CreateForm(TSensitivityAnalysisForm, SensitivityAnalysisForm);
  SensitivityAnalysisForm.Hide;
  SensitivityAnalysisForm.AlphaBlend := False;
  Application.CreateForm(TTWSensitivityAnalysisForm, TWSensitivityAnalysisForm);
  TWSensitivityAnalysisForm.Hide;
  TWSensitivityAnalysisForm.AlphaBlend := False;
  Application.CreateForm(TEquilibriumDiagramForm, EquilibriumDiagramForm);
  EquilibriumDiagramForm.Hide;
  EquilibriumDiagramForm.AlphaBlend := False;
  Application.CreateForm(TTornadoPlotForm, TornadoPlotForm);
  TornadoPlotForm.Hide;
  TornadoPlotForm.AlphaBlend := False;
  Application.CreateForm(THelpWindow, HelpWindow);
  {$IFDEF debug}
  ShowPrereleaseWarning;
  {$ENDIF}
  if showSettingsAtStartup then
  begin
    SimThyrToolbar.SendToBack;
    {$IFDEF LCLcarbon}
    SimulationSettings.ShowOnTop; // fixes a bug in the Carbon widgetset
    SimulationSettings.Hide;
    {$ENDIF}
    SimulationSettings.ShowModal;
  end
  else
    SimulationSettings.hide;
  Application.CreateForm(TPlotOptionsForm, PlotOptionsForm);
  PlotOptionsForm.Visible := false;
  gStartup := False;
  SimThyrToolbar.SelectAllMenuItem.Enabled := True;
  Application.Run;
  if assigned(SimThread) then
    SimThread.SafeFree;
  SimThread := nil;
end.
