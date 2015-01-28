unit LaunchDialog;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ This unit delivers the launch dialog with simulation settings }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ActnList, types, LCLType, SimThyrTypes, SimThyrServices,
  SimThyrPlot, SimThyrLog, StructureParameters, Simulator, SimOptions;

type

  { TSimulationSettings }

  TSimulationSettings = class(TForm)
    ParsButton: TButton;
    TestRadioGroup: TRadioGroup;
    TestTimeEdit: TEdit;
    OptButton: TButton;
    StartButton: TButton;
    CancelButton: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label2: TLabel;
    Label3: TLabel;
    RunTimeEdit: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    RunSecondsRadio: TRadioButton;
    RunMinutesRadio: TRadioButton;
    RunHoursRadio: TRadioButton;
    RunDaysRadio: TRadioButton;
    TestOffRadio: TRadioButton;
    TestTRHRadio: TRadioButton;
    TestTBGRadio: TRadioButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OptButtonClick(Sender: TObject);
    procedure OptButtonKeyPress(Sender: TObject; var Key: char);
    procedure OptButtonKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ParsButtonClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure EnableTestUIElements(enable: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure RunDaysRadioChange(Sender: TObject);
    procedure RunHoursRadioChange(Sender: TObject);
    procedure RunMinutesRadioChange(Sender: TObject);
    procedure RunSecondsRadioChange(Sender: TObject);
    procedure TestTBGRadioChange(Sender: TObject);
    procedure TestTRHRadioChange(Sender: TObject);
    procedure TestSignalTimesButtonClick(Sender: TObject);
    procedure CheckGroup1Click(Sender: TObject);
    procedure TestTRHRadioKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
  if i > 1 then
    nmax_old := nmax  {continue halted simulation}
  else
    nmax_old := 0;    {new simulation}
  tmax_text := RunTimeEdit.Text;  {duration of simulation}
  i1_text := TestTimeEdit.Text;   {begin of load}
  try
    tmax := StrToInt(tmax_text);
    tt := StrToInt(i1_text);
  except
    on E: EConvertError do
    begin
      ShowFormatMessage;
      RunTimeEdit.SetFocus;
      tmax := 0;
      tt := 0;
    end
    else
    begin
      bell;
      RunTimeEdit.SetFocus;
      tmax := 0;
      tt := 0;
    end;
  end;
  if (tmax > 0) then
  begin
    if RunDaysRadio.Checked then
      tmax_unit := 'd'
    else if RunHoursRadio.Checked then
      tmax_unit := 'h'
    else if RunMinutesRadio.Checked then
      tmax_unit := 'm'
    else if RunSecondsRadio.Checked then
        tmax_unit := 's'
    else tmax_unit := '';
    if TestRadioGroup.ItemIndex = 0 then
      i1_unit := 's'
    else if TestRadioGroup.ItemIndex = 1 then
      i1_unit := 'm'
    else if TestRadioGroup.ItemIndex = 2 then
      i1_unit := 'h'
    else if TestRadioGroup.ItemIndex = 3 then
        i1_unit := 'd'
    else i1_unit := '';
    graphready := false;
    runcommand := true;
    SimulationSettings.Close;
    nmax := nmax_old + cycles(tmax, tmax_unit);
    i1 := nmax_old + cycles(tt, i1_unit);
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

procedure TSimulationSettings.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if (Key = VK_D) and ((ssCtrl in Shift) or (ssMeta in Shift)) then
   begin
     ;
   end;
end;

procedure TSimulationSettings.OptButtonKeyPress(Sender: TObject; var Key: char);
begin
   ;
end;

procedure TSimulationSettings.OptButtonKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

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
  runcommand := false;
  SimulationSettings.Close;
end;

procedure TSimulationSettings.EnableTestUIElements(enable: boolean);
begin
  TestTimeEdit.Enabled := enable;
  TestRadioGroup.Enabled := enable;
end;

procedure TSimulationSettings.FormCreate(Sender: TObject);
begin
  BringToFront;
  RunDaysRadio.left := RunMinutesRadio.left;
  RunHoursRadio.left := RunSecondsRadio.left;
  TestOffRadio.left := RunSecondsRadio.left; {Adapts to oddities on different platforms}
  if (testflag or tbgflag) then
  begin
    EnableTestUIElements(true);
  end
  else
  begin
    EnableTestUIElements(false);
  end;
  if RunDaysRadio.Checked then
    tmax_unit := 'd'
  else if RunHoursRadio.Checked then
    tmax_unit := 'h'
  else if RunMinutesRadio.Checked then
    tmax_unit := 'm'
  else if RunSecondsRadio.Checked then
      tmax_unit := 's'
  else tmax_unit := '';
  if YosemiteORNewer then
    begin
      StartButton.Height := 22;
      CancelButton.Height := 22;
      OptButton.Height := 22;
      ParsButton.Height := 22;
    end;
end;

procedure TSimulationSettings.FormDeactivate(Sender: TObject);
begin

end;

procedure TSimulationSettings.FormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TSimulationSettings.FormShow(Sender: TObject);
begin
  ShowOnTop;
  SetFocus;
end;

procedure TSimulationSettings.Image1Click(Sender: TObject);
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
  if TestTBGRadio.checked then
  begin
    testflag := false;
    tbgflag := true;
    EnableTestUIElements(true);
  end
  else
  begin
    tbgflag := false;
    if TestTRHRadio.checked then
      EnableTestUIElements(true)
    else
      EnableTestUIElements(false);
  end;
end;

procedure TSimulationSettings.TestTRHRadioChange(Sender: TObject);
begin
  if TestTRHRadio.checked then
  begin
    testflag := true;
    tbgflag := false;
    EnableTestUIElements(true);
  end
  else
  begin
    testflag := false;
    if TestTBGRadio.checked then
      EnableTestUIElements(true)
    else
      EnableTestUIElements(false);
  end;
end;

procedure TSimulationSettings.TestSignalTimesButtonClick(Sender: TObject);
begin
  ShowImplementationMessage;
end;

procedure TSimulationSettings.CheckGroup1Click(Sender: TObject);
begin

end;

procedure TSimulationSettings.TestTRHRadioKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TSimulationSettings.RunTimeEditChange(Sender: TObject);
begin

end;

initialization
  {$I launchdialog.lrs}
end.

