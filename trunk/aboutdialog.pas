unit AboutDialog;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit implements an about-box }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, types, LCLIntf, ComCtrls, SimThyrTypes,
  SimThyrServices, VersionSupport, DOS;

type

  { TAboutWindow }

  TAboutWindow = class(TForm)
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
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Image1Click(Sender: TObject);
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

procedure TAboutWindow.ShowAbout;
var
  SystemStem, MajVer, MinVer: Str255;
begin
  SystemStem := OSVersion;
  AboutWindow.FormStyle := fsStayOnTop;
  AboutWindow.AlphaBlend := false;
  AboutWindow.Memo1.Lines.Clear;
  AboutWindow.Memo1.Lines.Add('SimThyr 3.0');
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
  AboutWindow.Memo1.Lines.Add('');
  AboutWindow.Memo1.Lines.Add('Preferences file: ' + gPreferencesFile);
  AboutWindow.ShowModal;
end;

initialization
  {$I aboutdialog.lrs}

end.

