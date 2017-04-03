unit PlotOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SimThyrTypes, SimThyrServices;

type

  { TPlotOptionsForm }

  TPlotOptionsForm = class(TForm)
    TitleLabel: TLabel;
    OKButton: TButton;
    ColorButton1: TColorButton;
    TitleEdit: TEdit;
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
  GetPlotOptions.titleString := String(TitleEdit.Text);
  GetPlotOptions.titleColor := ColorButton1.ButtonColor;     ;
end;

procedure TPlotOptionsForm.OKButtonClick(Sender: TObject);
begin
  Close;
end;

end.

