unit ShowIPS;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.2.5 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit draws the information processing structure }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, clipbrd, Menus, SimThyrTypes, SimThyrServices, StructureParameters;

type

  { TIPSForm }

  TIPSForm = class(TForm)
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    DeleteItem: TMenuItem;
    Divider1: TMenuItem;
    Image1: TImage;
    PasteItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    UndoItem: TMenuItem;
    procedure CopyImage;
    procedure CopyItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure SaveFigure;
    procedure FormCreate(Sender: TObject);
    private
    { private declarations }
  public
    { public declarations }
  end; 

var
  IPSForm: TIPSForm;

implementation

uses
  SimThyrMain;

procedure TIPSForm.CopyImage;
begin
  clipboard.Assign(IPSForm.Image1.Picture);
end;

procedure TIPSForm.CopyItemClick(Sender: TObject);
begin
  CopyImage;
end;

procedure TIPSForm.FormActivate(Sender: TObject);
begin
  gLastActiveCustomForm := IPSForm;
end;

procedure TIPSForm.Image1Click(Sender: TObject);
begin
  StructureParametersDlg.HandleStrucPars;
end;

procedure TIPSForm.SaveFigure;
{saves IPS as a graphics file}
var
  theFileName:  string;
  theFilterIndex: integer;
begin
  SimThyrToolbar.SavePictureDialog1.FilterIndex := 2;
  if SimThyrToolbar.SavePictureDialog1.Execute then
    try
      theFileName    := SimThyrToolbar.SavePictureDialog1.FileName;
      theFilterIndex := SimThyrToolbar.SavePictureDialog1.FilterIndex;
        {$IFDEF LCLcarbon}{compensates for a bug in the carbon widgetset}
      theFilterIndex := theFilterIndex + 1;
       {$ENDIF}{may be removed in future versions}
      if theFilterIndex = 8 then
        bell
      else
        IPSForm.Image1.Picture.SaveToFile(theFileName);
    except
      bell;
    end;
end;


procedure TIPSForm.FormCreate(Sender: TObject);
begin
  {scaffold for possible future extension}
end;

initialization
  {$I showips.lrs}

end.

