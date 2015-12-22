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
  ExtCtrls, EditBtn, LCLIntf, Spin, DateUtils, SimThyrTypes;

type

  { TAnnotationForm }

  TAnnotationForm = class(TForm)
    CreatedColonHM: TLabel;
    CreatedColonMS: TLabel;
    ModifiedColonMS: TLabel;
    ModifiedColonHM: TLabel;
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
    CreatedHourSpinEdit: TSpinEdit;
    CreatedMinuteSpinEdit: TSpinEdit;
    CreatedSecondSpinEdit: TSpinEdit;
    ModifiedHourSpinEdit: TSpinEdit;
    ModifiedMinuteSpinEdit: TSpinEdit;
    ModifiedSecondSpinEdit: TSpinEdit;
    TitleLabel: TLabel;
    procedure FormShow(Sender: TObject);
    procedure MIRIAMLogoClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
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
begin
  gActiveModel.Name := ModelNameEdit.Text;
  gActiveModel.Reference := ReferenceEdit.Text;
  gActiveModel.Species := SpeciesCombo.Text;
  gActiveModel.Creators := CreatorsMemo.Lines.Text;
  gActiveModel.Created := CreatedDateEdit.Date;
  TimeCreated := EncodeDateTime(1900, 1, 1, CreatedHourSpinEdit.Value, CreatedMinuteSpinEdit.Value, CreatedSecondSpinEdit.Value, 0);
  ReplaceTime(gActiveModel.Created, TimeCreated);
  gActiveModel.LastModified := ModifiedDateEdit.Date;
  TimeModified := EncodeDateTime(1900, 1, 1, ModifiedHourSpinEdit.Value, ModifiedMinuteSpinEdit.Value, ModifiedSecondSpinEdit.Value, 0);
  ReplaceTime(gActiveModel.LastModified, TimeModified);
  gActiveModel.Terms := ModelTermsCombo.Text;
  Close;
end;

procedure TAnnotationForm.FormShow(Sender: TObject);
begin
  ModelNameEdit.Text := gActiveModel.Name;
  ReferenceEdit.Text := gActiveModel.Reference;
  SpeciesCombo.Text := gActiveModel.Species;
  CreatorsMemo.Lines.Text := gActiveModel.Creators;
  CreatedDateEdit.Date := gActiveModel.Created;
  CreatedHourSpinEdit.Value := HourOf(gActiveModel.Created);
  CreatedMinuteSpinEdit.Value := MinuteOf(gActiveModel.Created);
  CreatedSecondSpinEdit.Value := SecondOf(gActiveModel.Created);
  ModifiedDateEdit.Date := gActiveModel.LastModified;
  ModifiedHourSpinEdit.Value := HourOf(gActiveModel.LastModified);
  ModifiedMinuteSpinEdit.Value := MinuteOf(gActiveModel.LastModified);
  ModifiedSecondSpinEdit.Value := SecondOf(gActiveModel.LastModified);
  ModelTermsCombo.Text := gActiveModel.Terms;
end;

procedure TAnnotationForm.MIRIAMLogoClick(Sender: TObject);
begin
  OpenURL('http://www.ebi.ac.uk/miriam/main/');
end;

end.

