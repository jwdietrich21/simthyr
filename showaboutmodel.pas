unit ShowAboutModel;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.2 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

{ This unit draws a window with additional information and references }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LCLIntf, StdCtrls, ExtCtrls, SimThyrServices, MIRIAMForm;

type

  { TAboutModelForm }

  TAboutModelForm = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    URLLabel: TLabel;
    Memo1: TMemo;
    StaticText2: TStaticText;
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure URLLabelClick(Sender: TObject);
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

procedure TAboutModelForm.Image1Click(Sender: TObject);
begin
  AnnotationForm.Show;
end;

procedure TAboutModelForm.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := clDefault;
    Memo1.Color := clDefault;
    Memo1.Font.Color := clWhite;
    URLLabel.Font.Color := clSkyBlue;
  end
  else
  begin
    Color := clWhite;
    Memo1.Color := clWhite;
    Memo1.Font.Color := clDefault;
    URLLabel.Font.Color := clNavy;
  end
end;

procedure TAboutModelForm.FormShow(Sender: TObject);
begin
  FormPaint(Sender);
end;

procedure TAboutModelForm.URLLabelClick(Sender: TObject);
begin
  OpenURL('http://tfc.medizinische-kybernetik.de');
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

