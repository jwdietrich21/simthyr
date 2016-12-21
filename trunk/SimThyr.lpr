program SimThyr;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2016 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2016 }

{ This is the main project file }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}{$R+}
{$define UseCThreads}

{$DEFINE debug}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SimThyrMain, Controls, TAChartLazarusPkg, fpvectorialpkg, math,
  SimThyrPlot, LaunchDialog, simthyrlog, SimThyrTypes, SimThyrServices, ShowIPS,
  Simulator, Splash, AboutDialog, ShowAboutModel, SimThyrPrediction,
  StructureParameters, SimOptions, VersionSupport, ScenarioHandler,
  HandlePreferences, HandleNotifier, Sensitivityanalysis, tornado, DIFSupport,
  help, SimThyrResources, unitconverter, TWSensitivityanalysis,
  equilibriumdiagram, MIRIAMForm
  {$IFDEF debug}
  , SysUtils // include heaptrc with Lazarus project options
  {$ENDIF}
  ;

{{$IFDEF WINDOWS}{$R SimThyr.rc}{$ENDIF}}

{$R *.res}

var
  Monitor0, Monitor1: TMonitor;

begin
  // ReturnNilIfGrowHeapFails := true;
  {$IFDEF debug} // Diagnostic code for developmental versions
  if FileExists(kHeapTraceFile) then
    DeleteFile(kHeapTraceFile);
  SetHeapTraceOutput(kHeapTraceFile);
  {$ENDIF}
  Application.Initialize;
  splashflag := true; {for debugging}
  gPigMode := false;
  showSettingsAtStartup := true;
  if splashflag then begin
    SplashScreen := TSplashScreen.Create(nil);
    SplashScreen.ShowOnTop;
    SplashScreen.FormStyle := fsSplash;
    SplashScreen.AlphaBlendValue := 200;
    Application.ProcessMessages;
  end;
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                   exOverflow, exUnderflow, exPrecision]);
  gStartup := true;
  runcommand := false;
  simready := true;
  randomize;
  InitSimulationControl;
  SetBaseVariables;
  StandardValues;
  InitHormoneConversionFactors;
  testflag := false;
  tbgflag := false;
  previewflag := true;
  noiseflag := true;
  circadianflag := true;
  haltsim := false;
  Application.CreateForm(TSimThyrToolbar, SimThyrToolbar);
  SimThyrToolbar.SetPosition;
  if splashflag then SplashScreen.Update;
  Application.BringToFront;
  Application.CreateForm(TAboutWindow, AboutWindow);
  AboutWindow.Hide;
  AboutWindow.AlphaBlend := false;
  Application.CreateForm(TAboutModelForm, AboutModelForm);
  AboutModelForm.Hide;
  AboutModelForm.AlphaBlend := false;
  Application.CreateForm(TPreferencesDialog, PreferencesDialog);
  PreferencesDialog.Hide;
  PreferencesDialog.AlphaBlend := false;
  PreferencesDialog.InitMenuItems;
  ReadPreferences;
  SetUnits;
  Application.CreateForm(TIPSForm, IPSForm);
  with IPSForm do
  begin
    Hide;
    AlphaBlend := false;
  end;
  Application.CreateForm(TSimThyrLogWindow, SimThyrLogWindow);
  with SimThyrLogWindow do
  begin
    Hide;
    InitGrid;
    Top := SimThyrToolbar.Top + SimThyrToolbar.Height + 52;
    Left := 26;
    if Screen.Width > 1024 then Width := 2 * Screen.Width div 3;
    if Screen.Height > 800 then Height := Screen.Height div 2 - Top - 52;
    AlphaBlend := false;
  end;
  Application.CreateForm(TValuesPlot, ValuesPlot);
  with ValuesPlot do
  begin
    Hide;
    top := Screen.Height - Height - 80;
    AlphaBlend := false;
  end;
  Application.CreateForm(TPrediction, Prediction);
  Prediction.Hide;
  Prediction.AlphaBlend := false;
  Prediction.Left := Screen.DesktopWidth - Prediction.Width - 13;
  if Screen.MonitorCount > 1 then
  begin
    Monitor0 := Screen.Monitors[0];
    Monitor1 := Screen.Monitors[1];
    if
      Monitor1.Primary = TRUE then
      begin
        Prediction.Left := Monitor0.Left + Monitor0.Width - Prediction.Width - 13;
      end
    else if
      Monitor0.Primary = TRUE then
      begin
        Prediction.Left := Monitor1.Left + MOnitor1.Width - Prediction.Width - 13;
      end;
  end;
  Application.CreateForm(TStructureParameters, StructureParametersDlg);
  StructureParametersDlg.Hide;
  StructureParametersDlg.AlphaBlend := false;
  Application.CreateForm(TSimOptionsDlg, SimOptionsDlg);
  SimOptionsDlg.Hide;
  SimOptionsDlg.AlphaBlend := false;
  Notice := TNotice.Create(SimThyrLogWindow);
  Notice.Hide;
  Notice.NoticeLabel.Caption := WAIT_TITLE;
  Application.CreateForm(TAnnotationForm, AnnotationForm);
  AnnotationForm.Hide;
  Application.CreateForm(TSimulationSettings, SimulationSettings);
  SimThyrToolbar.Show;
  IPSForm.Show;
  ValuesPlot.show;
  if ValuesPlot.Left + ValuesPLot.Width >= Screen.Width then
    ValuesPlot.Left := Screen.Width - ValuesPLot.Width - 26;
  SimThyrLogWindow.show;
  Prediction.MakeFullyVisible();
  Prediction.Show;
  Application.CreateForm(TSensitivityAnalysisForm, SensitivityAnalysisForm);
  SensitivityAnalysisForm.Hide;
  SensitivityAnalysisForm.AlphaBlend := false;
  Application.CreateForm(TTWSensitivityAnalysisForm, TWSensitivityAnalysisForm);
  TWSensitivityAnalysisForm.Hide;
  TWSensitivityAnalysisForm.AlphaBlend := false;
  Application.CreateForm(TEquilibriumDiagramForm, EquilibriumDiagramForm);
  EquilibriumDiagramForm.Hide;
  EquilibriumDiagramForm.AlphaBlend := false;
  Application.CreateForm(TTornadoPlotForm, TornadoPlotForm);
  TornadoPlotForm.Hide;
  TornadoPlotForm.AlphaBlend := false;
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
  else SimulationSettings.hide;
  gStartup := false;
  SimThyrToolbar.SelectAllMenuItem.Enabled := true;
  Application.Run;
  if assigned(SimThread) then
    SimThread.SafeFree;
  SimThread := nil;
end.

