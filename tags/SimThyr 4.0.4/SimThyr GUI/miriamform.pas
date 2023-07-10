unit MIRIAMForm;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.4 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2021 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2021 }

{ This unit provides an editor for MIRIAM-compliant model annotation }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, LCLIntf, DateUtils, StrUtils, EnvironmentInfo,
  SimThyrTypes, SimThyrResources, SimThyrServices, ScenarioHandler;

type

  { TAnnotationForm }

  TAnnotationForm = class(TForm)
    StandardButton: TButton;
    CreatedTimeEdit: TEdit;
    CommentsMemo: TMemo;
    Image1: TImage;
    mibbiLogo: TImage;
    MIASELogo: TImage;
    ModelCommentLabel: TLabel;
    ModifiedTimeEdit: TEdit;
    ModelTermsCombo: TComboBox;
    MIRIAMLogo: TImage;
    ModelTermsLabel: TLabel;
    BottomPanel: TPanel;
    ReferenceLabel: TLabel;
    ReferenceEdit: TEdit;
    ModelNameLabel: TLabel;
    ModelNameEdit: TEdit;
    ModifiedLabel: TLabel;
    CreatorsMemo: TMemo;
    CreatorsLabel: TLabel;
    CreatedDateEdit: TDateEdit;
    CreatedLabel: TLabel;
    ModifiedDateEdit: TDateEdit;
    SpeciesLabel: TLabel;
    SpeciesCombo: TComboBox;
    OKButton: TButton;
    ScrollBox1: TScrollBox;
    TitleLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
    procedure mibbiLogoClick(Sender: TObject);
    procedure MIASELogoClick(Sender: TObject);
    procedure MIRIAMLogoClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure StandardButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure ShowDatesAndTimes(theModel: TModel);
    procedure ShowAnnotation(theModel: TModel);
  end;

var
  AnnotationForm: TAnnotationForm;

implementation

uses
  ShowAboutModel;

{$R *.lfm}

{ TAnnotationForm }

procedure TAnnotationForm.OKButtonClick(Sender: TObject);
var
  TimeCreated, TimeModified: TDateTime;
  theHour, theMinute, theSecond: integer;
  ConvError: boolean;
begin
  ConvError := false;
  gActiveModel.Name := ModelNameEdit.Text;
  gActiveModel.Reference := ReferenceEdit.Text;
  gActiveModel.Species := SpeciesCombo.Text;
  gActiveModel.Creators := CreatorsMemo.Lines.Text;
  gActiveModel.Created := CreatedDateEdit.Date;
  if not tryStrToInt(ExtractDelimited(1, CreatedTimeEdit.Text, [':', '.']), theHour) then
  begin
    bell;
    ConvError := true;
    theHour := 0;
  end;
  if not tryStrToInt(ExtractDelimited(2, CreatedTimeEdit.Text, [':', '.']), theMinute) then
  begin
    bell;
    ConvError := true;
    theMinute := 0;
  end;
  if not tryStrToInt(ExtractDelimited(3, CreatedTimeEdit.Text, [':', '.']), theSecond) then
  begin
    bell;
    ConvError := true;
    theSecond := 0;
  end;
  TimeCreated := EncodeDateTime(1900, 1, 1, theHour, theMinute, theSecond, 0);
  ReplaceTime(gActiveModel.Created, TimeCreated);
  gActiveModel.LastModified := ModifiedDateEdit.Date;
  if not tryStrToInt(ExtractDelimited(1, ModifiedTimeEdit.Text, [':', '.']), theHour) then
  begin
    bell;
    ConvError := true;
    theHour := 0;
  end;
  if not tryStrToInt(ExtractDelimited(2, ModifiedTimeEdit.Text, [':', '.']), theMinute) then
  begin
    bell;
    ConvError := true;
    theMinute := 0;
  end;
  if not tryStrToInt(ExtractDelimited(3, ModifiedTimeEdit.Text, [':', '.']), theSecond) then
  begin
    bell;
    ConvError := true;
    theSecond := 0;
  end;
  TimeModified := EncodeDateTime(1900, 1, 1, theHour, theMinute, theSecond, 0);
  ReplaceTime(gActiveModel.LastModified, TimeModified);
  gActiveModel.Terms := ModelTermsCombo.Text;
  gActiveModel.Comments := CommentsMemo.Lines.Text;
  if ConvError then
    ShowDatesAndTimes(gActiveModel)
  else
    Close;
end;

procedure TAnnotationForm.StandardButtonClick(Sender: TObject);
var
  tempModel: TModel;
begin
  tempModel := emptyModel;
  ShowAnnotation(tempModel);
end;

procedure TAnnotationForm.ShowDatesAndTimes(theModel: TModel);
begin
  CreatedDateEdit.Date := theModel.Created;
  CreatedTimeEdit.Text := TimeToStr(theModel.Created);
  ModifiedDateEdit.Date := theModel.LastModified;
  ModifiedTimeEdit.Text := TimeToStr(theModel.LastModified);
end;

procedure TAnnotationForm.ShowAnnotation(theModel: TModel);
begin
  ModelNameEdit.Text := theModel.Name;
  ReferenceEdit.Text := theModel.Reference;
  SpeciesCombo.Text := theModel.Species;
  CreatorsMemo.Lines.Text := theModel.Creators;
  ShowDatesAndTimes(theModel);
  ModelTermsCombo.Text := theModel.Terms;
  CommentsMemo.Lines.Text := theModel.Comments;
end;

procedure TAnnotationForm.FormShow(Sender: TObject);
begin
  FormPaint(Sender);
  ShowAnnotation(gActiveModel);
end;

procedure TAnnotationForm.Image1Click(Sender: TObject);
begin
  AboutModelForm.Show;
end;

procedure TAnnotationForm.mibbiLogoClick(Sender: TObject);
begin
  OpenURL(MIBBI_URL);
end;

procedure TAnnotationForm.MIASELogoClick(Sender: TObject);
begin
  OpenURL(MIASE_URL);
end;

procedure TAnnotationForm.FormCreate(Sender: TObject);
begin
  if YosemiteORNewer then
  begin
    StandardButton.Height := 22;
    OKButton.Height  := 22;
  end;
end;

procedure TAnnotationForm.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := BACKCOLOUR;
    BottomPanel.Color := BACKCOLOUR;
  end
  else
  begin
    Color := clWhite;
    BottomPanel.Color := clWhite;
  end
end;

procedure TAnnotationForm.MIRIAMLogoClick(Sender: TObject);
begin
  OpenURL(MIRIAM_URL);
end;

end.

