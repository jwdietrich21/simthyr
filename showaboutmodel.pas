unit ShowAboutModel;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2016 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2016 }

{ This unit draws a window with additional information and references }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LCLIntf, StdCtrls;

type

  { TAboutModelForm }

  TAboutModelForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    StaticText2: TStaticText;
    procedure Label1Click(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure StaticText1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  AboutModelForm: TAboutModelForm;

implementation

{ TAboutModelForm }

procedure TAboutModelForm.Label1Click(Sender: TObject);
begin

end;

procedure TAboutModelForm.Label2Click(Sender: TObject);
begin
  OpenURL('http://tfc.medical-cybernetics.de');
end;

procedure TAboutModelForm.Memo1Change(Sender: TObject);
begin

end;

procedure TAboutModelForm.StaticText1Click(Sender: TObject);
begin

end;

initialization
  {$I showaboutmodel.lrs}

end.

