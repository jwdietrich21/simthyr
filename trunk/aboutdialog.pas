unit AboutDialog;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.2.4 }

{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ This unit implements an about-box }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, LCLIntf, ComCtrls, SimThyrTypes,
  SimThyrServices, VersionSupport, DOS, HandlePreferences;

type

  { TAboutWindow }

  TAboutWindow = class(TForm)
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    OKButton: TButton;
    Image1: TImage;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    Tabs: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Image10Click(Sender: TObject);
    procedure Image11Click(Sender: TObject);
    procedure Image12Click(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
    procedure Image3Click(Sender: TObject);
    procedure Image4Click(Sender: TObject);
    procedure Image5Click(Sender: TObject);
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
    procedure ShowAbout;
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

procedure TAboutWindow.Image1Click(Sender: TObject);
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
  end;
end;

procedure TAboutWindow.Image11Click(Sender: TObject);
begin
  OpenURL('http://www.zennaware.com');
end;

procedure TAboutWindow.Image12Click(Sender: TObject);
begin
  OpenURL('http://developer.apple.com/');
end;

procedure TAboutWindow.Image2Click(Sender: TObject);
begin
  OpenURL('http://link.medinn.med.uni-muenchen.de');
end;

procedure TAboutWindow.Image3Click(Sender: TObject);
begin
  OpenURL('http://www.uniklinik-ulm.de');
end;

procedure TAboutWindow.Image4Click(Sender: TObject);
begin
  OpenURL('http://www.ruhr-uni-bochum.de');
end;

procedure TAboutWindow.Image5Click(Sender: TObject);
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
var
  SystemStem, MajVer, MinVer: Str255;
begin
  gExtendedInfo := false;
  SystemStem := OSVersion;
  AboutWindow.FormStyle := fsStayOnTop;
  AboutWindow.AlphaBlend := false;
  {The following lines provide additional information}
  {on the software installation}
  AboutWindow.Memo1.Lines.Clear;
  AboutWindow.Memo1.Lines.Add('SimThyr ' + GetFileVersion);
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('Licence: BSD');
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('File version: ' + GetFileVersion);
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('Build Date: ' + {$I %DATE%} + ', ' + {$I %TIME%});
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('Developed with Lazarus / Free Pascal');
  AboutWindow.Memo1.Lines.Add('Built for '+ GetTargetInfo);
  AboutWindow.Memo1.Lines.Add('with '+ GetCompilerInfo + ' on '+ GetCompiledDate);
  AboutWindow.Memo1.Lines.Add('and using '+ GetLCLVersion + ' and ' + GetWidgetset);
  AboutWindow.Memo1.Lines.Add('');
  {$IFDEF WINDOWS}
  MajVer := IntToStr(Win32MajorVersion);
  MinVer := IntToStr(Win32MinorVersion);
  {$ELSE}
  MajVer := IntToStr(Lo(DosVersion) - 4);
  MinVer := IntToStr(Hi(DosVersion));
  {$ENDIF}
  AboutWindow.Memo1.Lines.Add('Operating system: ' + GetOS + ' (' + SystemStem + MajVer + '.' + MinVer + ')');
  AboutWindow.ShowModal;
end;

initialization
  {$I aboutdialog.lrs}

end.

