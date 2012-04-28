unit SimThyrMain;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ This unit provides global GUI functions, toolbar and menubar handling }

{ Source code released under the BSD License }

{$mode objfpc}{$H+}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, ExtCtrls, StdActns, StdCtrls, LCLType, Menus, ActnList, SimThyrTypes,
  SimThyrServices, LaunchDialog, ShowIPS, Simulator, Printers, ComCtrls,
  LCLIntf, ExtDlgs, SimThyrLog, SimThyrPlot, AboutDialog, ShowAboutModel,
  StructureParameters, SimThyrPrediction, Sensitivityanalysis, ScenarioHandler, HandlePreferences,
  HandleNotifier, LCLProc, LazHelpHTML;

type

  { TSimThyrToolbar }

  TSimThyrToolbar = class(TForm)
    ActionList1: TActionList;
    ApplicationProperties1: TApplicationProperties;
    CloseMenuItem: TMenuItem;
    CopyMenuItem: TMenuItem;
    CutMenuItem: TMenuItem;
    Divider_1_1: TMenuItem;
    Divider_2_1: TMenuItem;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditMenu: TMenuItem;
    EditPaste1: TEditPaste;
    EditUndo1: TEditUndo;
    FileMenu: TMenuItem;
    IdleTimer1: TIdleTimer;
    ImageList1: TImageList;
    MacAboutItem: TMenuItem;
    AppleMenu: TMenuItem;
    MacPreferencesItem: TMenuItem;
    MainMenu1: TMainMenu;
    HelpMenu: TMenuItem;
    HelpItem: TMenuItem;
    Divider_5_1: TMenuItem;
    Divider_1_3: TMenuItem;
    Divider_1_2: TMenuItem;
    AboutModelItem: TMenuItem;
    Divider_3_3: TMenuItem;
    ChangeParItem: TMenuItem;
    IPSItem: TMenuItem;
    LogItem: TMenuItem;
    IPSItem2: TMenuItem;
    Divider_3_2: TMenuItem;
    OWSensitivityAnalysisItem: TMenuItem;
    TornadoPlotItem: TMenuItem;
    TornadoPlotItem2: TMenuItem;
    OWSensitivityAnalysisItem2: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    WinPreferencesItem: TMenuItem;
    Divider_2_2: TMenuItem;
    PredictionItem: TMenuItem;
    PlotItem: TMenuItem;
    WindowMenu: TMenuItem;
    PauseItem: TMenuItem;
    StopItem: TMenuItem;
    Divider_3_1: TMenuItem;
    RunItem: TMenuItem;
    SimulationMenu: TMenuItem;
    PrintItem: TMenuItem;
    PageSetupItem: TMenuItem;
    SaveItem: TMenuItem;
    QuitMenuItem: TMenuItem;
    ToolBar1: TToolBar;
    NewToolButton: TToolButton;
    OpenToolButton: TToolButton;
    SaveToolButton: TToolButton;
    SaveAsToolButton: TToolButton;
    ToolButton1: TToolButton;
    PrefsToolButton: TToolButton;
    ParametersToolButton: TToolButton;
    RunToolButton: TToolButton;
    PauseToolButton: TToolButton;
    StopToolButton: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    UndoToolButton: TToolButton;
    CutToolButton: TToolButton;
    CopyToolButton: TToolButton;
    PasteToolButton: TToolButton;
    PrintToolButton: TToolButton;
    ToolButton9: TToolButton;
    WinAboutItem: TMenuItem;
    OnlineInfo: TMenuItem;
    Divider_0_1: TMenuItem;
    Divider_0_2: TMenuItem;
    DeleteMenuItem: TMenuItem;
    NewMenuItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    SaveAsItem: TMenuItem;
    SaveDialog1: TSaveDialog;
    UndoMenuItem: TMenuItem;
    procedure AboutModelItemClick(Sender: TObject);
    procedure CloseMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HandleIdle(Sender: TObject; var Done: Boolean);
    procedure HelpItemClick(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure IPSItemClick(Sender: TObject);
    procedure LogItemClick(Sender: TObject);
    procedure OnlineInfoClick(Sender: TObject);
    procedure OpenToolButtonClick(Sender: TObject);
    procedure OWSensitivityAnalysisItem2Click(Sender: TObject);
    procedure PageSetupItemClick(Sender: TObject);
    procedure ParametersToolButtonClick(Sender: TObject);
    procedure PauseToolButtonClick(Sender: TObject);
    procedure PlotItemClick(Sender: TObject);
    procedure PredictionItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
    procedure RunItemClick(Sender: TObject);
    procedure SaveAsToolButtonClick(Sender: TObject);
    procedure SaveToolButtonClick(Sender: TObject);
    procedure OWSensitivityAnalysisItemClick(Sender: TObject);
    procedure StopToolButtonClick(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
    procedure NewToolButtonClick(Sender: TObject);
    procedure PrefsToolButtonClick(Sender: TObject);
    procedure PrintToolButtonClick(Sender: TObject);
    procedure TornadoPlotItemClick(Sender: TObject);
    procedure UndoMenuItemClick(Sender: TObject);
    procedure AboutItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;
  tInterfaceLanguage = (English, German);

var
  SimThyrToolbar: TSimThyrToolbar;
  gInterfaceLanguage: tInterfaceLanguage;
  j, gIdleCounter: integer;

implementation

{ TSimThyrToolbar }

procedure AdaptMenus;
var
  modifierKey: TShiftState;
begin
  {$IFDEF LCLcarbon}
    modifierKey := [ssMeta];
    SimThyrToolbar.WinAboutItem.Visible := false;
    SimThyrToolbar.Divider_5_1.Visible := false;
    SimThyrToolbar.Divider_2_2.Visible := false;
    SimThyrToolbar.Divider_2_2.Visible := false;
    SimThyrToolbar.WinPreferencesItem.Visible := false;
    SimThyrToolbar.AppleMenu.Visible := true;
  {$ELSE}
    modifierKey := [ssCtrl];
    SimThyrToolbar.WinAboutItem.Visible := true;
    SimThyrToolbar.Divider_5_1.Visible := true;
    SimThyrToolbar.Divider_2_2.Visible := true;
    SimThyrToolbar.WinPreferencesItem.Visible := true;
    SimThyrToolbar.AppleMenu.Visible := false;
  {$ENDIF}
  SimThyrToolbar.NewMenuItem.ShortCut:=ShortCut(VK_N, modifierKey);
  SimThyrToolbar.OpenMenuItem.ShortCut:=ShortCut(VK_O, modifierKey);
  SimThyrToolbar.CloseMenuItem.ShortCut:=ShortCut(VK_W, modifierKey);
  SimThyrToolbar.SaveItem.ShortCut:=ShortCut(VK_S, modifierKey);
  SimThyrToolbar.PrintItem.ShortCut:=ShortCut(VK_P, modifierKey);
  SimThyrToolbar.QuitMenuItem.ShortCut:=ShortCut(VK_Q, modifierKey);
  SimThyrToolbar.UndoMenuItem.ShortCut:=ShortCut(VK_Z, modifierKey);
  SimThyrToolbar.CutMenuItem.ShortCut:=ShortCut(VK_X, modifierKey);
  SimThyrToolbar.CopyMenuItem.ShortCut:=ShortCut(VK_C, modifierKey);
  SimThyrToolbar.PasteMenuItem.ShortCut:=ShortCut(VK_V, modifierKey);
  SimThyrToolbar.RunItem.ShortCut:=ShortCut(VK_R, modifierKey);
end;

procedure AdaptLanguages;
begin
   if gInterfaceLanguage = English then
   begin
     ;
   end
   else
   begin
     ;
   end;
   AdaptMenus;
end;

procedure TSimThyrToolbar.FormCreate(Sender: TObject);
begin
  AdaptLanguages;
  gIdleCounter := 0;
end;

procedure TSimThyrToolbar.FormShow(Sender: TObject);
begin
  if (SimulationSettings <> nil) and showSettingsAtStartup then
  begin
    SimulationSettings.ShowOnTop;
    SimulationSettings.SetFocus;
  end;
end;

procedure TSimThyrToolbar.HandleIdle(Sender: TObject; var Done: Boolean);
begin
  if simready then
  begin
  {$IFDEF LCLcarbon}
    SimThyrLogWindow.ValuesGrid.BeginUpdate;
    SimThyrLogWindow.ValuesGrid.EndUpdate(true);
  {$ENDIF}
    if gIdleCounter < 2 then
    begin
     SimThyrToolbar.SendToBack;
     SimulationSettings.ShowOnTop;
     SimulationSettings.SetFocus;
     gIdleCounter := gIdleCounter + 1;
    end;
  application.ProcessMessages;
  end
  else;
end;

procedure TSimThyrToolbar.HelpItemClick(Sender: TObject);
begin
  OpenURL(HELP_URL);
end;

procedure TSimThyrToolbar.IdleTimer1Timer(Sender: TObject);
begin
  if simready then
  begin
  {$IFDEF LCLcarbon}
    SimThyrLogWindow.ValuesGrid.BeginUpdate;
    SimThyrLogWindow.ValuesGrid.EndUpdate(true);
  {$ENDIF}
    application.ProcessMessages;
    Notice.Hide;
  end;
end;

procedure TSimThyrToolbar.FormActivate(Sender: TObject);
begin
  if (SimulationSettings <> nil) and gStartup and showSettingsAtStartup then
  begin
    SimulationSettings.ShowOnTop;
    SimulationSettings.SetFocus;
  end;
end;

procedure TSimThyrToolbar.CloseMenuItemClick(Sender: TObject);
var theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  {if (simready and theForm = SimThyrLogWindow) then SimThread.free;}
  theForm.Close;
end;

procedure TSimThyrToolbar.AboutModelItemClick(Sender: TObject);
begin
  AboutModelForm.Show;
end;

procedure TSimThyrToolbar.CopyMenuItemClick(Sender: TObject);
var theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  if theForm = SimThyrLogWindow then SimThyrLogWindow.CopyCells
  else if theForm = IPSForm then IPSForm.CopyImage
  else if theForm = ValuesPlot then ValuesPlot.CopyChart(Sender)
  else ActionList1.Actions[2].Execute;
end;

procedure TSimThyrToolbar.IPSItemClick(Sender: TObject);
begin
  IPSForm.Show;
end;

procedure TSimThyrToolbar.LogItemClick(Sender: TObject);
begin
  SimThyrLogWindow.Show;
end;

procedure TSimThyrToolbar.OnlineInfoClick(Sender: TObject);
begin
 OpenURL(BASE_URL);
end;

procedure TSimThyrToolbar.OpenToolButtonClick(Sender: TObject);
var
  theFile: File of Byte;
  fileData: Array of Byte;
  theString, theFileName: String;
  theSize: Int64;
  i: integer;
begin
  OpenDialog1.FilterIndex := 2;
  if OpenDialog1.Execute then
  begin
    theFileName := OpenDialog1.FileName;
    case OpenDialog1.FilterIndex of
      1: bell;
      2: ReadScenario(theFileName);
      end;
    {AssignFile(theFile, theFileName);
    FileMode := 0;
    {$i-}
    Reset(theFile);
    {$i+}
    if IOResult <> 0 then
      begin
        bell;
        exit ;
      end
    else
      begin }
        {StatusBar1.SimpleText := StatusStringReading;
        theSize := FileSize(theFileName);
        SetLength(fileData, theSize) ;
        BlockRead(theFile, fileData[0], theSize) ;}
        {CloseFile(theFile) ;}
        {FileContent := AnsiString(fileData);
        PreviewPanel.text := FileContent;
        StatusBar1.SimpleText := StatusStringDataRead;    }
      {end;}
  end;
end;

procedure TSimThyrToolbar.OWSensitivityAnalysisItem2Click(Sender: TObject);
begin
  SensitivityAnalysisForm.Show;
end;

procedure TSimThyrToolbar.PageSetupItemClick(Sender: TObject);
begin
  ShowImplementationMessage;
end;

procedure TSimThyrToolbar.ParametersToolButtonClick(Sender: TObject);
begin
  StructureParametersDlg.HandleStrucPars;
end;

procedure TSimThyrToolbar.PauseToolButtonClick(Sender: TObject);
begin
  if haltsim then
    SimThread.Restart
  else
    SimThread.Pause;
end;

procedure TSimThyrToolbar.PlotItemClick(Sender: TObject);
begin
  ValuesPlot.Show;
end;

procedure TSimThyrToolbar.PredictionItemClick(Sender: TObject);
begin
  Prediction.Show;
end;

procedure TSimThyrToolbar.QuitMenuItemClick(Sender: TObject);
begin
  {SavePreferences;}
  if SimThread <> nil then
    SimThread.Terminate;
  application.Terminate;
end;

procedure TSimThyrToolbar.RunItemClick(Sender: TObject);
begin
  if simready = true then
    SimulationSettings.ShowOnTop
  else if haltsim then
    SimThread.Restart
  else
    bell;
end;

procedure TSimThyrToolbar.SaveAsToolButtonClick(Sender: TObject);
begin
  ShowImplementationMessage;
end;

procedure TSimThyrToolbar.SaveToolButtonClick(Sender: TObject);
var
  theForm: TForm;
  theDelimiter: Char;
  theFileName: String;
begin
  theForm := Screen.ActiveForm;
  if theForm = IPSForm then
    begin
      if SavePictureDialog1.Execute then
      try
        theFileName := SavePictureDialog1.FileName;
        IPSForm.Image1.Picture.SaveToFile(theFileName);
      finally
        ;
      end;
    end
  else
  begin
    if theForm = SimThyrToolbar then
      SaveDialog1.FilterIndex := 4
    else if theForm = SimThyrLogWindow then
      SaveDialog1.FilterIndex := 1;
    if SaveDialog1.Execute then
      begin
        theFileName := SaveDialog1.FileName;
        case SaveDialog1.FilterIndex of
          1: theDelimiter := kTab;
          2: if DecimalSeparator = ',' then
               theDelimiter := ';'
             else theDelimiter := ',';
          3: theDelimiter := 'd';
          4: theDelimiter := ' ';
        end;
        case SaveDialog1.FilterIndex of
          1..3: SimThyrLogWindow.SaveGrid(theFileName, theDelimiter);
          4: SaveScenario(theFilename);
        end;
      end;
    end;
end;

procedure TSimThyrToolbar.OWSensitivityAnalysisItemClick(Sender: TObject);
begin
  SensitivityAnalysisForm.Show;
end;

procedure TSimThyrToolbar.StopToolButtonClick(Sender: TObject);
begin
  haltsim := true;
  graphready := false;
  nmax := 0;
  if SimThread <> nil then
    begin
      SimThread.WaitFor;  {ensure that simulation thread has completed last cycle}
      SimThread.Terminate;
    end;
  SetBaseVariables;
  SetLength(gResultMatrix, 0, 9);
  SimThyrLogWindow.InitGrid;
  ClearPrediction;
  DrawPlot(true);
end;

procedure TSimThyrToolbar.ToolBar1Click(Sender: TObject);
begin

end;

procedure TSimThyrToolbar.NewToolButtonClick(Sender: TObject);
begin
  ShowImplementationMessage;
end;

procedure TSimThyrToolbar.PrefsToolButtonClick(Sender: TObject);
begin
  PreferencesDialog.ShowPreferences;
end;

procedure TSimThyrToolbar.PrintToolButtonClick(Sender: TObject);
var theForm: TForm;
begin
  theForm := Screen.ActiveForm;
  if theForm = ValuesPlot then
    ValuesPlot.PrintChart(Sender)
  else
    ShowImplementationMessage;
end;

procedure TSimThyrToolbar.TornadoPlotItemClick(Sender: TObject);
begin
  bell;
end;

procedure TSimThyrToolbar.UndoMenuItemClick(Sender: TObject);
begin

end;

procedure TSimThyrToolbar.AboutItemClick(Sender: TObject);
begin
  AboutWindow.ShowAbout;
end;

initialization
  {$I simthyrmain.lrs}
  gParameterLabel[i_pos] := 'i';
  gParameterLabel[t_pos] := 'Time';
  gParameterLabel[TRH_pos] := 'Portal TRH';
  gParameterLabel[pTSH_pos] := 'Pituitary TSH';
  gParameterLabel[TSH_pos] := 'Serum TSH';
  gParameterLabel[TT4_pos] := 'Serum total T4';
  gParameterLabel[FT4_pos] := 'Serum free T4';
  gParameterLabel[TT3_pos] := 'Serum total T3';
  gParameterLabel[FT3_pos] := 'Serum free T3';
  gParameterLabel[cT3_pos] := 'Central T3';
  for j := i_pos to cT3_pos do gParameterFactor[j] := 1;
end.

