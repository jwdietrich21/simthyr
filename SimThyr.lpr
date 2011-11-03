program SimThyr;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This is the main project file }

{ Source code released under the BSD License }

{$mode objfpc}{$H+}{$R+}
{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SimThyrMain, Controls,
  TAChartLazarusPkg, SimThyrPlot, LaunchDialog, simthyrlog, SimThyrTypes,
  SimThyrServices, ShowIPS, Simulator, Splash, AboutDialog, ShowAboutModel,
  SimThyrPrediction, StructureParameters, SimOptions, VersionSupport,
  ScenarioHandler, HandlePreferences, HandleNotifier;

{{$IFDEF WINDOWS}{$R SimThyr.rc}{$ENDIF}}

{$R *.res}

begin
  Application.Initialize;
  splashflag := true;
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
  StandardValues;
  InitConversionFactors;
  testflag := false;
  tbgflag := false;
  previewflag := true;
  noiseflag := true;
  haltsim := false;
  Application.CreateForm(TSimThyrToolbar, SimThyrToolbar);
  with SimThyrToolbar do
  begin
    hide;
  {$IFDEF LCLcarbon}
    left := 0;
    top := 20;
    width := Screen.Width;
  {$ELSE}
    left := 1;
    top := 0;
    width := Screen.Width - 3;
  {$ENDIF}
    height := Toolbar1.Height + 3;
    WindowState := wsNormal;
    AlphaBlend := false;
  end;
  if splashflag then SplashScreen.Update;
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
  SetUnits;
  Application.CreateForm(TIPSForm, IPSForm);
  with IPSForm do
  begin
    Hide;
    Top := SimThyrToolbar.Top + SimThyrToolbar.Height + 32;
    Height := Screen.Height - Top - 48;
    width := trunc(Height * Image1.Width / Image1.Height);
    AlphaBlend := false;
  end;
  Application.CreateForm(TSimThyrLogWindow, SimThyrLogWindow);
  with SimThyrLogWindow do
  begin
    Hide;
    InitGrid;
    Top := SimThyrToolbar.Top + SimThyrToolbar.Height + 45;
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
  SimThyrLogWindow.show;
  Prediction.MakeFullyVisible();
  Prediction.Show;
  if showSettingsAtStartup then
  begin
    SimulationSettings.ShowOnTop;
    SimulationSettings.SetFocus;
    {SimulationSettings.FormStyle := fsStayOnTop;}
  end
  else SimulationSettings.hide;
  Application.Run;
end.

