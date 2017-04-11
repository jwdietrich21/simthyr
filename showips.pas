unit ShowIPS;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2017 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2017 }

{ This unit draws the information processing structure }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, clipbrd, Menus, LCLVersion,
  SimThyrTypes, SimThyrServices, StructureParameters, ShowAboutModel;

type

  { TIPSForm }

  TIPSForm = class(TForm)
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    DeleteItem: TMenuItem;
    Divider1: TMenuItem;
    Image1: TImage;
    Image2: TImage;
    PasteItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    UndoItem: TMenuItem;
    procedure CopyImage;
    procedure CopyItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure Image2Click(Sender: TObject);
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

procedure TIPSForm.Image2Click(Sender: TObject);
begin
  AboutModelForm.Show;
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
        {$IFDEF LCLcarbon}{compensates for a bug in older version of carbon widgetset}
          if (lcl_major < 2) and (lcl_minor < 2) then
            theFilterIndex := theFilterIndex + 1;
        {$ENDIF}
      if theFilterIndex = 8 then
        bell
      else
        IPSForm.Image1.Picture.SaveToFile(theFileName);
    except
      ShowSaveError;
    end;
end;


procedure TIPSForm.FormCreate(Sender: TObject);
begin
  { adapt extent of this rather large window to GUI style of different platforms }
    {$IFDEF LCLcarbon}
      Top := SimThyrToolbar.Top + SimThyrToolbar.Height + 32;
    {$ELSE}
      Top := SimThyrToolbar.Top + SimThyrToolbar.Height + 38;
    if XPOrNewer then
      Top := Top + 10;
    if VistaORNewer then
      Top := Top + 4;
    if Win8OrNewer then
      Top := Top + 10;
    {$ENDIF}
    Height := Screen.Height - Top - 100;
    width := trunc(Height * Image1.Width / Image1.Height);
end;

initialization
  {$I showips.lrs}

end.

