unit PlotOptions;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.2 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

{ This unit implements a dialog box for plot options }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SimThyrTypes, SimThyrServices, EnvironmentInfo;

type

  { TPlotOptionsForm }

  TPlotOptionsForm = class(TForm)
    FontsCombobox: TComboBox;
    TitleLabel: TLabel;
    OKButton: TButton;
    ColorButton1: TColorButton;
    TitleEdit: TEdit;
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function GetPlotOptions: TPlotOptions;
  end;

var
  PlotOptionsForm: TPlotOptionsForm;

implementation

{$R *.lfm}

{ TPlotOptionsForm }

procedure TPlotOptionsForm.FormCreate(Sender: TObject);
begin
  if YosemiteORNewer then
  begin
    OKButton.Height := 22;
  end;
end;

function TPlotOptionsForm.GetPlotOptions: TPlotOptions;
begin
  ShowModal;
  GetPlotOptions.titleString := AnsiString(TitleEdit.Text);
  GetPlotOptions.titleColor := ColorButton1.ButtonColor;
  if FontsCombobox.ItemIndex < 1 then
    GetPlotOptions.fontname := 'default'
  else
    GetPlotOptions.fontname := FontsCombobox.Items[FontsCombobox.ItemIndex];
end;

procedure TPlotOptionsForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TPlotOptionsForm.FormShow(Sender: TObject);
begin
  FontsCombobox.Items.Assign(Screen.Fonts);
  ShowOnTop;
  SetFocus;
end;

end.

