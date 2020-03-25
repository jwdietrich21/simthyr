unit LaunchDialog;

 { SimThyr Project }
 { A numerical simulator of thyrotropic feedback control }

{ Version 4.0.2 (Merlion) }

 { (c) J. W. Dietrich, 1994 - 2020 }
 { (c) Ludwig Maximilian University of Munich 1995 - 2002 }
 { (c) Ruhr University of Bochum 2005 - 2020 }

{ This unit delivers the launch dialog with simulation settings }

 { Source code released under the BSD License }
 { See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, types, LCLType, SimThyrTypes, SimThyrServices,
  EnvironmentInfo, SimThyrPlot, SimThyrLog, StructureParameters, Simulator,
  SimOptions;

type

  { TSimulationSettings }

  TSimulationSettings = class(TForm)
    CyclesIconDark: TImage;
    CyclesIconLight: TImage;
    DurationLabel: TLabel;
    HeavisideIconDark: TImage;
    HeavisideIconLight: TImage;
    StartLabel: TLabel;
    SimThyrLabel: TLabel;
    TestTimeUnit: TComboBox;
    RunTimeUnit: TComboBox;
    ParsButton:  TButton;
    TestTimeEdit: TEdit;
    OptButton:   TButton;
    StartButton: TButton;
    CancelButton: TButton;
    ThyroidIcon:      TImage;
    CyclesIcon:      TImage;
    HeavisideIcon:      TImage;
    Label2:      TLabel;
    Label3:      TLabel;
    RunTimeEdit: TEdit;
    GroupBox1:   TGroupBox;
    GroupBox2:   TGroupBox;
    TitleLabel:      TLabel;
    TestOffRadio: TRadioButton;
    TestTRHRadio: TRadioButton;
    TestTBGRadio: TRadioButton;
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure OptButtonClick(Sender: TObject);
    procedure ParsButtonClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure EnableTestUIElements(enable: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure FormShow(Sender: TObject);
    procedure ThyroidIconClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure RunDaysRadioChange(Sender: TObject);
    procedure RunHoursRadioChange(Sender: TObject);
    procedure RunMinutesRadioChange(Sender: TObject);
    procedure RunSecondsRadioChange(Sender: TObject);
    procedure TestTBGRadioChange(Sender: TObject);
    procedure TestTRHRadioChange(Sender: TObject);
    procedure TestSignalTimesButtonClick(Sender: TObject);
    procedure CheckGroup1Click(Sender: TObject);
    procedure TestTRHRadioKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure RunTimeEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SimulationSettings: TSimulationSettings;

implementation

{ TSimulationSettings }

procedure TSimulationSettings.StartButtonClick(Sender: TObject);
begin
  {$IFDEF LCLCocoa}
  SimOptionsDlg.Close;
  {$ENDIF}
  if i > 1 then
    nmax_old := nmax  {continue halted simulation}
  else
    nmax_old := 0;    {new simulation}
  tmax_text := RunTimeEdit.Text;  {duration of simulation}
  i1_text := TestTimeEdit.Text;   {begin of load}
  try
    tmax := StrToInt(tmax_text);
    tt   := StrToInt(i1_text);
  except
    on E: EConvertError do
    begin
      ShowFormatMessage;
      RunTimeEdit.SetFocus;
      tmax := 0;
      tt   := 0;
    end
    else
    begin
      bell;
      RunTimeEdit.SetFocus;
      tmax := 0;
      tt   := 0;
    end;
  end;
  if (tmax > 0) then
  begin
    if RunTimeUnit.Caption = 'Hours' then
      tmax_unit := 'h'
    else if RunTimeUnit.Caption = 'Days' then
      tmax_unit := 'd'
    else if RunTimeUnit.Caption = 'Weeks' then
      tmax_unit := 'w'
    else
      tmax_unit := '';
    if TestTimeUnit.Caption = 'Hours' then
      i1_unit := 'h'
    else if TestTimeUnit.Caption = 'Days' then
      i1_unit := 'd'
    else if TestTimeUnit.Caption = 'Weeks' then
      i1_unit := 'w'
    else
      i1_unit := '';
    graphready := False;
    runcommand := True;
    Close;
    nmax := nmax_old + cycles(tmax, tmax_unit);
    i1   := nmax_old + cycles(tt, i1_unit);
    SetStatusBarPanel0('   ' + IntToStr(nmax_old) + ':', IntToStr(nmax));
    SimThyrLogWindow.StatusBar1.Panels[1].Text := '   DeltaT:' + FloatToStr(delt) + ' s';
    application.ProcessMessages;
    InitSimulation;
  end;
end;

procedure TSimulationSettings.OptButtonClick(Sender: TObject);
begin
  SimOptionsDlg.ShowOnTop;
end;


procedure TSimulationSettings.FormKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if (Key = VK_D) and ((ssCtrl in Shift) or (ssMeta in Shift)) then
  begin
    ;
  end;
end;

procedure TSimulationSettings.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := BACKCOLOUR;
    GroupBox1.Color := BACKCOLOUR;
    GroupBox2.Color := BACKCOLOUR;
    {$IFDEF LCLCocoa}
    CyclesIcon.Picture := CyclesIconDark.Picture;
    HeavisideIcon.Picture := HeavisideIconDark.Picture;
    {$ENDIF}
  end
  else
  begin
    Color := clWhite;
    GroupBox1.Color := clWhite;
    GroupBox2.Color := clWhite;
    {$IFDEF LCLCocoa}
    CyclesIcon.Picture := CyclesIconLight.Picture;
    HeavisideIcon.Picture := HeavisideIconLight.Picture;
    {$ENDIF}
  end
end;

procedure TSimulationSettings.ParsButtonClick(Sender: TObject);
begin
  StructureParametersDlg.HandleStrucPars;
end;

procedure TSimulationSettings.RadioGroup1Click(Sender: TObject);
begin

end;

procedure TSimulationSettings.CancelButtonClick(Sender: TObject);
begin
  {$IFDEF LCLCocoa}
  SimOptionsDlg.Close;
  {$ENDIF}
  runcommand := False;
  SimulationSettings.Close;
end;

procedure TSimulationSettings.EnableTestUIElements(enable: boolean);
begin
  TestTimeEdit.Enabled   := enable;
  TestTimeUnit.Enabled := enable;
end;

procedure TSimulationSettings.FormCreate(Sender: TObject);
begin
  FormPaint(Sender);
  SimThyrLabel.Caption := 'SimThyr ' + FileVersion;
  BringToFront;
  if (testflag or tbgflag) then
  begin
    EnableTestUIElements(True);
  end
  else
  begin
    EnableTestUIElements(False);
  end;
  if RunTimeUnit.Caption = 'Hours' then
    tmax_unit := 'h'
  else if RunTimeUnit.Caption = 'Days' then
    tmax_unit := 'd'
  else if RunTimeUnit.Caption = 'Weeks' then
    tmax_unit := 'w'
  else
    tmax_unit := '';
  if YosemiteORNewer then
  begin
    StartButton.Height  := 22;
    CancelButton.Height := 22;
    OptButton.Height    := 22;
    ParsButton.Height   := 22;
  end;
end;

procedure TSimulationSettings.FormDeactivate(Sender: TObject);
begin

end;

procedure TSimulationSettings.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin

end;

procedure TSimulationSettings.FormShow(Sender: TObject);
begin
  FormPaint(Sender);
  ShowOnTop;
  SetFocus;
end;

procedure TSimulationSettings.ThyroidIconClick(Sender: TObject);
begin

end;

procedure TSimulationSettings.Label2Click(Sender: TObject);
begin

end;

procedure TSimulationSettings.RunDaysRadioChange(Sender: TObject);
begin
  tmax_unit := 'd';
end;

procedure TSimulationSettings.RunHoursRadioChange(Sender: TObject);
begin
  tmax_unit := 'h';
end;

procedure TSimulationSettings.RunMinutesRadioChange(Sender: TObject);
begin
  tmax_unit := 'm';
end;

procedure TSimulationSettings.RunSecondsRadioChange(Sender: TObject);
begin
  tmax_unit := 's';
end;

procedure TSimulationSettings.TestTBGRadioChange(Sender: TObject);
begin
  if TestTBGRadio.Checked then
  begin
    testflag := False;
    tbgflag  := True;
    EnableTestUIElements(True);
  end
  else
  begin
    tbgflag := False;
    if TestTRHRadio.Checked then
      EnableTestUIElements(True)
    else
      EnableTestUIElements(False);
  end;
end;

procedure TSimulationSettings.TestTRHRadioChange(Sender: TObject);
begin
  if TestTRHRadio.Checked then
  begin
    testflag := True;
    tbgflag  := False;
    EnableTestUIElements(True);
  end
  else
  begin
    testflag := False;
    if TestTBGRadio.Checked then
      EnableTestUIElements(True)
    else
      EnableTestUIElements(False);
  end;
end;

procedure TSimulationSettings.TestSignalTimesButtonClick(Sender: TObject);
begin
  ShowImplementationMessage;
end;

procedure TSimulationSettings.CheckGroup1Click(Sender: TObject);
begin

end;

procedure TSimulationSettings.TestTRHRadioKeyUp(Sender: TObject;
  var Key: word; Shift: TShiftState);
begin

end;

procedure TSimulationSettings.RunTimeEditChange(Sender: TObject);
begin

end;

initialization
  {$I launchdialog.lrs}
end.

