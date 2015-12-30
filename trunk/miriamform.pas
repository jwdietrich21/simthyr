unit MIRIAMForm;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ This unit provides an editor for MIRIAM-compliant model annotation }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, LCLIntf, Spin, MaskEdit, DateUtils, StrUtils,
  SimThyrTypes, SimThyrServices;

type

  { TAnnotationForm }

  TAnnotationForm = class(TForm)
    CreatedTimeEdit: TEdit;
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
    procedure FormShow(Sender: TObject);
    procedure MIRIAMLogoClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    procedure ShowDatesAndTimes;
    procedure ShowAnnotation;
    { public declarations }
  end;

var
  AnnotationForm: TAnnotationForm;

implementation

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
  if ConvError then
    ShowDatesAndTimes
  else
    Close;
end;

procedure TAnnotationForm.ShowDatesAndTimes;
begin
  CreatedDateEdit.Date := gActiveModel.Created;
  CreatedTimeEdit.Text := TimeToStr(gActiveModel.Created);
  ModifiedDateEdit.Date := gActiveModel.LastModified;
  ModifiedTimeEdit.Text := TimeToStr(gActiveModel.LastModified);
end;

procedure TAnnotationForm.ShowAnnotation;
begin
  ModelNameEdit.Text := gActiveModel.Name;
  ReferenceEdit.Text := gActiveModel.Reference;
  SpeciesCombo.Text := gActiveModel.Species;
  CreatorsMemo.Lines.Text := gActiveModel.Creators;
  ShowDatesAndTimes;
  ModelTermsCombo.Text := gActiveModel.Terms;
end;

procedure TAnnotationForm.FormShow(Sender: TObject);
begin
  ShowAnnotation;
end;

procedure TAnnotationForm.FormCreate(Sender: TObject);
begin
  if YosemiteORNewer then
  begin
    OKButton.Height  := 22;
  end;
end;

procedure TAnnotationForm.MIRIAMLogoClick(Sender: TObject);
begin
  OpenURL('http://www.ebi.ac.uk/miriam/main/');
end;

end.

