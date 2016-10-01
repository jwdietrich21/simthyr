unit help;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2016 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2016 }

{ This unit draws a help window with clickable options to get additional information }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LCLIntf, ExtCtrls, SimThyrTypes, SimThyrResources,
  ShowAboutModel, AboutDialog;

type

  { THelpWindow }

  THelpWindow = class(TForm)
    SimThyrIconImage: TImage;
    MainTitle: TLabel;
    OnlineHelpSubTitle: TLabel;
    ModelInfoTitle: TLabel;
    OnlineHelpTitle: TLabel;
    ModelInfoSubTitle: TLabel;
    VersionInfoTitle: TLabel;
    VersionInfoSubTitle: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ModelInfoTitleClick(Sender: TObject);
    procedure OnlineHelpTitleClick(Sender: TObject);
    procedure VersionInfoTitleClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  HelpWindow: THelpWindow;

implementation

{ THelpWindow }

procedure THelpWindow.OnlineHelpTitleClick(Sender: TObject);
begin
  OpenURL(HELP_URL);
end;

procedure THelpWindow.VersionInfoTitleClick(Sender: TObject);
begin
  HelpWindow.Close;
  AboutWindow.ShowAbout;
end;

procedure THelpWindow.ModelInfoTitleClick(Sender: TObject);
begin
  AboutModelForm.ShowOnTop;
  HelpWindow.Close;
end;

procedure THelpWindow.FormCreate(Sender: TObject);
begin
  {$IFDEF win32}
    {adapt font sizes to be readable also under Windows}
    OnlineHelpTitle.Font.Height := 15;
    OnlineHelpSubTitle.Font.Height := 13;
    ModelInfoTitle.Font.Height := 15;
    ModelInfoSubTitle.Font.Height := 13;
    VersionInfoTitle.Font.Height := 15;
    VersionInfoSubTitle.Font.Height := 13;
  {$ENDIF}
end;

initialization
  {$I help.lrs}

end.

