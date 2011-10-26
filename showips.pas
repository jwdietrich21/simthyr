unit ShowIPS;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit draws the information processing structure }

{ Source code released under the BSD License }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, clipbrd, SimThyrServices;

type

  { TIPSForm }

  TIPSForm = class(TForm)
    Image1: TImage;
    procedure CopyImage;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  IPSForm: TIPSForm;

implementation

procedure TIPSForm.CopyImage;
begin
  clipboard.Assign(IPSForm.Image1.Picture);
end;

initialization
  {$I showips.lrs}

end.

