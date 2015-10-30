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
  ExtCtrls, EditBtn, SimThyrTypes;

type

  { TAnnotationForm }

  TAnnotationForm = class(TForm)
    ModelTermsEdit: TLabeledEdit;
    ModifiedTimeEdit: TEdit;
    ModifiedLabel: TLabel;
    CreatorsMemo: TMemo;
    CreatorsLabel: TLabel;
    CreatedDateEdit: TDateEdit;
    CreatedLabel: TLabel;
    CreatedTimeEdit: TEdit;
    ModifiedDateEdit: TDateEdit;
    SpeciesLabel: TLabel;
    SpeciesCombo: TComboBox;
    ReferenceEdit: TLabeledEdit;
    ModelNameEdit: TLabeledEdit;
    OKButton: TButton;
    ScrollBox1: TScrollBox;
    procedure FormShow(Sender: TObject);
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
  gActiveModel.Creators := CreatorsMemo.Lines.Text;
  gActiveModel.Created := CreatedDateEdit.Date; // + CreatedTimeEdit.Text;
  gActiveModel.LastModified := ModifiedDateEdit.Date; // + ModifiedTimeEdit.Text;
  gActiveModel.Terms := ModelTermsEdit.Text;
  Close;
end;

procedure TAnnotationForm.FormShow(Sender: TObject);
begin

end;

end.

