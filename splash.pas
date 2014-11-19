unit Splash;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.4.0 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit implements a splash screen, while other windows are loaded }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, LaunchDialog, VersionSupport;

type

  { TSplashScreen }

  TSplashScreen = class(TForm)
    CopyrightLabel1: TLabel;
    CopyrightLabel2: TLabel;
    CopyrightLabel3: TLabel;
    CopyrightLabel4: TLabel;
    CopyrightLabel5: TLabel;
    CopyrightLabel6: TLabel;
    CopyrightLabel7: TLabel;
    Image1: TImage;
    Timer1: TTimer;
    VersionLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SplashScreen: TSplashScreen;

implementation

{ TSplashScreen }

procedure TSplashScreen.Timer1Timer(Sender: TObject);
begin
  Timer1.Free;
  Close;
  Free;
end;

procedure TSplashScreen.Image1Click(Sender: TObject);
begin

end;

procedure TSplashScreen.FormCreate(Sender: TObject);
begin
  VersionLabel.Caption := 'Version ' + GetFileVersion;
end;

initialization
  {$I splash.lrs}

end.

