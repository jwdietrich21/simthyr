unit help;

{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit draws a help window with clickable options to get additional information }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLIntf, ExtCtrls, SimThyrTypes, SimThyrServices, ShowAboutModel,
  AboutDialog;

type

  { THelpWindow }

  THelpWindow = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  HelpWindow: THelpWindow;

implementation

{ THelpWindow }

procedure THelpWindow.Label4Click(Sender: TObject);
begin
  OpenURL(HELP_URL);
end;

procedure THelpWindow.Label6Click(Sender: TObject);
begin
  HelpWindow.Close;
  AboutWindow.ShowAbout;
end;

procedure THelpWindow.Label3Click(Sender: TObject);
begin
  AboutModelForm.ShowOnTop;
  HelpWindow.Close;
end;

initialization
  {$I help.lrs}

end.

