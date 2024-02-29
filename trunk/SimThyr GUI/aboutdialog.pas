unit AboutDialog;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 5.0.0 (Mirage) }

{ (c) J. W. Dietrich, 1994 - 2024 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2024 }

{ This unit implements an about-box }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, LCLIntf, ComCtrls, SimThyrTypes, SimThyrResources,
  SimThyrGUIServices, EnvironmentInfo, DOS, HandlePreferences, Sensitivityanalysis,
  TWSensitivityanalysis, Equilibriumdiagram
  {$IFDEF WINDOWS}
  , Windows, Win32Proc
  {$ENDIF}
    {$IFDEF DARWIN}
  , MacOSAll
    {$ENDIF}
  {$IFDEF UNIX}
  , Unix
  {$ENDIF}
  , types;

type

  { TAboutWindow }

  TAboutWindow = class(TForm)
    CopyrightLabel1: TLabel;
    CopyrightLabel2: TLabel;
    CopyrightLabel3: TLabel;
    CopyrightLabel4: TLabel;
    CopyrightLabel5: TLabel;
    CopyrightLabel6: TLabel;
    CopyrightLabel7: TLabel;
    CopyrightLabel8: TLabel;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Label12: TLabel;
    Label13: TLabel;
    PumaImageLight: TImage;
    BMHImageDark: TImage;
    PumaImageDark: TImage;
    RubImageDark: TImage;
    SimThyrLabel: TImage;
    Image2: TImage;
    KKBImage: TImage;
    UlmImageLight: TImage;
    RubImageLight: TImage;
    BMHImageLight: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    SciCrunchLabel: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Memo1: TMemo;
    OKButton: TButton;
    BigLogo: TImage;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    Tabs: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    UlmImageDark: TImage;
    URL1: TLabel;
    URL2: TLabel;
    VersionLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image10Click(Sender: TObject);
    procedure Image11Click(Sender: TObject);
    procedure Image12Click(Sender: TObject);
    procedure KKBImageClick(Sender: TObject);
    procedure PumaImageLightClick(Sender: TObject);
    procedure BigLogoClick(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure UlmImageLightClick(Sender: TObject);
    procedure RubImageLightClick(Sender: TObject);
    procedure BMHImageLightClick(Sender: TObject);
    procedure Image6Click(Sender: TObject);
    procedure Image7Click(Sender: TObject);
    procedure Image8Click(Sender: TObject);
    procedure Image9Click(Sender: TObject);
    procedure Label10Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure SciCrunchLabelClick(Sender: TObject);
    procedure ShowAbout;
    procedure URL1Click(Sender: TObject);
    procedure URL2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutWindow: TAboutWindow;
  gExtendedInfo: boolean;

implementation

{ TAboutWindow }

procedure TAboutWindow.OKButtonClick(Sender: TObject);
begin
  AboutWindow.Close;
end;

procedure TAboutWindow.SciCrunchLabelClick(Sender: TObject);
begin
  OpenURL('http://scicrunch.org/browse/resources/SCR_014351');
end;

procedure TAboutWindow.Label2Click(Sender: TObject);
begin

end;

procedure TAboutWindow.Label5Click(Sender: TObject);
begin
  OpenURL('http://www.famfamfam.com/lab/icons/silk/');
end;

procedure TAboutWindow.Label7Click(Sender: TObject);
begin
  OpenURL('http://tango.freedesktop.org/Tango_Icon_Library');
end;

procedure TAboutWindow.Label9Click(Sender: TObject);
begin
  OpenURL('http://www.lazarus.freepascal.org/index.php/topic,13957.0.html');
end;

procedure TAboutWindow.Memo1Change(Sender: TObject);
begin

end;

procedure TAboutWindow.BigLogoClick(Sender: TObject);
begin
  OpenURL(BASE_URL);
end;

procedure TAboutWindow.Image10Click(Sender: TObject);
begin
  OpenURL('http://versionsapp.com/');
end;

procedure TAboutWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
{provide additional information, if option or alt key is pressed}
begin
  if (key = 18) and not gExtendedInfo then
  begin
    AboutWindow.Memo1.Lines.Add('');
    AboutWindow.Memo1.Lines.Add('Preferences file: ' + GetPreferencesFile);
    gExtendedInfo := true;
    if not gPigMode and
      (MessageDlg('Start developer mode?', mtConfirmation, [mbCancel, mbYes], 0)
      = mrYes) then
    begin
      gPigMode := true;
      SensitivityAnalysisForm.StartPigMode;
      TWSensitivityAnalysisForm.StartPigMode;
      EquilibriumDiagramForm.StartPigMode;
    end;
  end
  else if (key = 87) and ((ssMeta in Shift) or (ssCtrl in Shift)) then
    self.Close;
end;

procedure TAboutWindow.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := BACKCOLOUR;
    Memo1.Font.Color := clWhite;
    URL1.Font.Color := clSkyBlue;
    URL2.Font.Color := clSkyBlue;
    SciCrunchLabel.Font.Color := clSkyBlue;
    Label5.Font.Color := clSkyBlue;
    Label7.Font.Color := clSkyBlue;
    Label15.Font.Color := clSkyBlue;
    BMHImageDark.Visible := true;
    BMHImageLight.Visible := false;
    RUBImageDark.Visible := true;
    RUBImageLight.Visible := false;
    UlmImageDark.Visible := true;
    UlmImageLight.Visible := false;
  end
  else
  begin
    Color := clWhite;
    Memo1.Font.Color := clDefault;
    URL1.Font.Color := clNavy;
    URL2.Font.Color := clNavy;
    SciCrunchLabel.Font.Color := clNavy;
    Label5.Font.Color := clNavy;
    Label7.Font.Color := clNavy;
    Label15.Font.Color := clNavy;
    BMHImageDark.Visible := false;
    BMHImageLight.Visible := true;
    RUBImageDark.Visible := false;
    RUBImageLight.Visible := true;
    UlmImageDark.Visible := false;
    UlmImageLight.Visible := true;
  end
end;

procedure TAboutWindow.FormShow(Sender: TObject);
begin
  FormPaint(Sender);
end;

procedure TAboutWindow.FormCreate(Sender: TObject);
begin
  VersionLabel.Caption := 'Version ' + FileVersion;
  if YosemiteORNewer then
    OKButton.Height := 22;
end;

procedure TAboutWindow.Image11Click(Sender: TObject);
begin
  OpenURL('https://cornerstone.assembla.com');
end;

procedure TAboutWindow.Image12Click(Sender: TObject);
begin
  OpenURL('http://developer.apple.com/');
end;

procedure TAboutWindow.KKBImageClick(Sender: TObject);
begin
  OpenURL('http://www.klinikum-bochum.de');
end;

procedure TAboutWindow.PumaImageLightClick(Sender: TObject);
begin
  OpenURL('http://puma-repository.sf.net');
end;

procedure TAboutWindow.Image2Click(Sender: TObject);
begin
  OpenURL('http://link.medinn.med.uni-muenchen.de');
end;

procedure TAboutWindow.UlmImageLightClick(Sender: TObject);
begin
  OpenURL('http://www.uniklinik-ulm.de');
end;

procedure TAboutWindow.RubImageLightClick(Sender: TObject);
begin
  OpenURL('http://www.ruhr-uni-bochum.de');
end;

procedure TAboutWindow.BMHImageLightClick(Sender: TObject);
begin
  OpenURL('http://www.bergmannsheil.de');
end;

procedure TAboutWindow.Image6Click(Sender: TObject);
begin
  OpenURL('http://www.instruct.de');
end;

procedure TAboutWindow.Image7Click(Sender: TObject);
begin
  OpenURL('http://www.freepascal.org');
end;

procedure TAboutWindow.Image8Click(Sender: TObject);
begin
  OpenURL('http://lazarus.freepascal.org');
end;

procedure TAboutWindow.Image9Click(Sender: TObject);
begin
  OpenURL('http://www.barebones.com/products/textwrangler/');
end;

procedure TAboutWindow.Label10Click(Sender: TObject);
begin

end;

procedure TAboutWindow.ShowAbout;
begin
  gExtendedInfo := false;
  AboutWindow.FormStyle := fsStayOnTop;
  AboutWindow.AlphaBlend := false;
  {The following lines provide additional information}
  {on the software installation}
  AboutWindow.Memo1.Lines.Clear;
  AboutWindow.Memo1.Lines.Add('SimThyr ' + FileVersion);
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('License: BSD');
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('File version: ' + FileVersion);
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('Build Date: ' + {$I %DATE%} + ', ' + {$I %TIME%});
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('Developed with Lazarus / Free Pascal');
  AboutWindow.Memo1.Lines.Add('Built for '+ PlatformInfo.OS + ' (' + PlatformInfo.CPU + ')');
  AboutWindow.Memo1.Lines.Add('with '+ CompilerVersion + ' on '+ DateOfCompilingAsString);
  AboutWindow.Memo1.Lines.Add('and using '+ EnvironmentInfo.LCLVersion + ' with ' + CurrentWidgetSet);
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('Operating system: ' + PlatformInfo.OS + ' (' + SystemVersion + ')');
  AboutWindow.ShowModal;
end;

procedure TAboutWindow.URL1Click(Sender: TObject);
begin
  OpenURL('http://simthyr.medical-cybernetics.de')
end;

procedure TAboutWindow.URL2Click(Sender: TObject);
begin
  OpenURL('http://simthyr.sf.net')
end;

initialization
  {$I aboutdialog.lrs}

end.

