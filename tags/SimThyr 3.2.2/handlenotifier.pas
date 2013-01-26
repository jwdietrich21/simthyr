unit HandleNotifier;

{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit draws a small notifier window that informs about simulation running }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TNotice }

  TNotice = class(TForm)
    Label1: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Notice: TNotice;

implementation

initialization
  {$I handlenotifier.lrs}

end.

