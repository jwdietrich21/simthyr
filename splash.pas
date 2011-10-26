unit Splash;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit implements a splash screen, while other windows are loaded }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, LaunchDialog;

type

  { TSplashScreen }

  TSplashScreen = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
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
  Close;
end;

procedure TSplashScreen.Image1Click(Sender: TObject);
begin

end;

initialization
  {$I splash.lrs}

end.

