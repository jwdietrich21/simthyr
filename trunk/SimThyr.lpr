program SimThyr;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.2.3 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This is the main project file }

{ Source code released under the BSD License }

{$mode objfpc}{$H+}{$R+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SimThyrMain, Controls, TAChartLazarusPkg, SimThyrPlot, LaunchDialog,
  simthyrlog, SimThyrTypes, SimThyrServices, ShowIPS, Simulator, Splash,
  AboutDialog, ShowAboutModel, SimThyrPrediction, StructureParameters,
  SimOptions, VersionSupport, ScenarioHandler, HandlePreferences,
  HandleNotifier, Sensitivityanalysis, tornado, DIFSupport, help;

{{$IFDEF WINDOWS}{$R SimThyr.rc}{$ENDIF}}

{$R *.res}

begin
  Application.Initialize;
  splashflag := true; {for debugging}
  showSettingsAtStartup := true;
  if splashflag then begin
    SplashScreen := TSplashScreen.Create(nil);
    SplashScreen.ShowOnTop;
    SplashScreen.FormStyle := fsSplash;
    SplashScreen.AlphaBlendValue := 200;
    Application.ProcessMessages;
  end;
  gStartup := true;
  runcommand := false;
  simready := true;
  randomize;
  InitSimulationControl;
  SetBaseVariables;
  StandardValues;
  InitConversionFactors;
  testflag := false;
  tbgflag := false;
  previewflag := true;
  noiseflag := true;
  haltsim := false;
  Application.CreateForm(TSimThyrToolbar, SimThyrToolbar);
  SimThyrToolbar.SetPosition;
  if splashflag then SplashScreen.Update;
  {Application.MainFormOnTaskbar := True;}
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
    Top := SimThyrToolbar.Top + SimThyrToolbar.Height + 32;
    Height := Screen.Height - Top - 100;
    width := trunc(1.3 * Height * Image1.Width / Image1.Height);
    AlphaBlend := false;
  end;
  Application.CreateForm(TSimThyrLogWindow, SimThyrLogWindow);
  with SimThyrLogWindow do
  begin
    Hide;
    InitGrid;
    Top := SimThyrToolbar.Top + SimThyrToolbar.Height + 52;
    Left := 32;
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
  Prediction.Left := Screen.Width - Prediction.Width - 13;
  Application.CreateForm(TStructureParameters, StructureParametersDlg);
  StructureParametersDlg.Hide;
  StructureParametersDlg.AlphaBlend := false;
  Application.CreateForm(TSimOptionsDlg, SimOptionsDlg);
  SimOptionsDlg.Hide;
  SimOptionsDlg.AlphaBlend := false;
  Notice := TNotice.Create(SimThyrLogWindow);
  Notice.Hide;
  Notice.Label1.Caption := WAIT_TITLE;
  Application.CreateForm(TSimulationSettings, SimulationSettings);
  SimThyrToolbar.Show;
  ValuesPlot.show;
  if ValuesPlot.Left + ValuesPLot.Width >= Screen.Width then
    ValuesPlot.Left := Screen.Width - ValuesPLot.Width - 26;
  SimThyrLogWindow.show;
  Prediction.MakeFullyVisible();
  Prediction.Show;
  Application.CreateForm(TSensitivityAnalysisForm, SensitivityAnalysisForm);
  SensitivityAnalysisForm.Hide;
  SensitivityAnalysisForm.AlphaBlend := false;
  Application.CreateForm(TTornadoPlotForm, TornadoPlotForm);
  TornadoPlotForm.Hide;
  TornadoPlotForm.AlphaBlend := false;
  if showSettingsAtStartup then
  begin
    SimThyrToolbar.SendToBack;
    SimulationSettings.ShowOnTop;
    SimulationSettings.SetFocus;
    {SimulationSettings.FormStyle := fsSystemStayOnTop;}
  end
  else SimulationSettings.hide;
  gStartup := false;
  SimThyrToolbar.SelectAllMenuItem.Enabled := true;
  Application.CreateForm(THelpWindow, HelpWindow);
  Application.Run;
  if assigned(SimThread) then
    SimThread.SafeFree;
end.

