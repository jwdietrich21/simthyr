unit HandleNotifier;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.6 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ This unit draws a small notifier window that informs about simulation running }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SimThyrLog;

type

  { TNotice }

  TNotice = class(TForm)
    NoticeLabel: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Notice: TNotice;

implementation

{ TNotice }

procedure TNotice.FormCreate(Sender: TObject);
begin
  Left := SimThyrLogWindow.Left + SimThyrLogWindow.Width div 2 - Width div 2;
  Top := SimThyrLogWindow.Top + SimThyrLogWindow.Height div 3;
end;

procedure TNotice.FormActivate(Sender: TObject);
begin
  BringToFront;
end;

initialization
  {$I handlenotifier.lrs}

end.

