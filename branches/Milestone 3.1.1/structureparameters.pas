unit StructureParameters;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit iplements an editor for structure parameters }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, SimThyrTypes, SimThyrServices, Simulator;

type

  { TStructureParameters }

  TStructureParameters = class(TForm)
    AlphaREdit: TEdit;
    BetaREdit: TEdit;
    StandardButton: TButton;
    CancelButton: TButton;
    DREdit: TEdit;
    AlphaSEdit: TEdit;
    BetaSEdit: TEdit;
    AlphaS2Edit: TEdit;
    BetaS2Edit: TEdit;
    AlphaTEdit: TEdit;
    BetaTEdit: TEdit;
    Alpha31Edit: TEdit;
    Beta31Edit: TEdit;
    Alpha32Edit: TEdit;
    Beta32Edit: TEdit;
    Image6: TImage;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Tau03zEdit: TEdit;
    Tau0TEdit: TEdit;
    Tau0S2Edit: TEdit;
    Tau0SEdit: TEdit;
    Tau0REdit: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    k30Edit: TEdit;
    k31Edit: TEdit;
    k41Edit: TEdit;
    k42Edit: TEdit;
    GD2Edit: TEdit;
    KM2Edit: TEdit;
    GD1Edit: TEdit;
    KM1Edit: TEdit;
    GTEdit: TEdit;
    DTEdit: TEdit;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SSEdit: TEdit;
    DSEdit: TEdit;
    GHEdit: TEdit;
    DHEdit: TEdit;
    LSEdit: TEdit;
    GREdit: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    OKButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure StandardButtonClick(Sender: TObject);
    procedure FillInParameters;
    function ParametersCorrect: boolean;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HandleStrucPars;
    procedure Image1Click(Sender: TObject);
    procedure Label31Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  StructureParametersDlg: TStructureParameters;

implementation

uses
  SimThyrPrediction, Sensitivityanalysis, Tornado;

procedure TStructureParameters.FillInParameters;
begin
  AlphaREdit.Text := FloatToStrF(AlphaR, ffGeneral, 5, 2);;
  BetaREdit.Text := FloatToStrF(BetaR, ffGeneral, 5, 2);;
  GREdit.Text := FloatToStrF(GR, ffGeneral, 5, 2);;
  DREdit.Text := FloatToStrF(DR, ffGeneral, 5, 2);;
  AlphaSEdit.Text := FloatToStrF(AlphaS, ffGeneral, 5, 2);;
  BetaSEdit.Text := FloatToStrF(BetaS, ffGeneral, 5, 2);;
  AlphaS2Edit.Text := FloatToStrF(AlphaS2, ffGeneral, 5, 2);;
  BetaS2Edit.Text := FloatToStrF(BetaS2, ffGeneral, 5, 2);;
  GHEdit.Text := FloatToStrF(GH, ffGeneral, 5, 2);;
  DHEdit.Text := FloatToStrF(DH, ffGeneral, 5, 2);;
  LSEdit.Text := FloatToStrF(LS, ffGeneral, 5, 2);;
  SSEdit.Text := FloatToStrF(SS, ffGeneral, 5, 2);;
  DSEdit.Text := FloatToStrF(DS, ffGeneral, 5, 2);;
  AlphaTEdit.Text := FloatToStrF(AlphaT, ffGeneral, 5, 2);;
  BetaTEdit.Text := FloatToStrF(BetaT, ffGeneral, 5, 2);;
  GTEdit.Text := FloatToStrF(GT, ffGeneral, 5, 2);;
  DTEdit.Text := FloatToStrF(DT, ffGeneral, 5, 2);;
  Alpha31Edit.Text := FloatToStrF(Alpha31, ffGeneral, 5, 2);;
  Beta31Edit.Text := FloatToStrF(Beta31, ffGeneral, 5, 2);;
  GD1Edit.Text := FloatToStrF(GD1, ffGeneral, 5, 2);;
  KM1Edit.Text := FloatToStrF(KM1, ffGeneral, 5, 2);;
  Alpha32Edit.Text := FloatToStrF(Alpha32, ffGeneral, 5, 2);;
  Beta32Edit.Text := FloatToStrF(Beta32, ffGeneral, 5, 2);;
  GD2Edit.Text := FloatToStrF(GD2, ffGeneral, 5, 2);;
  KM2Edit.Text := FloatToStrF(KM2, ffGeneral, 5, 2);;
  K30Edit.Text := FloatToStrF(K30, ffGeneral, 5, 2);;
  K31Edit.Text := FloatToStrF(K31, ffGeneral, 5, 2);;
  K41Edit.Text := FloatToStrF(K41, ffGeneral, 5, 2);;
  K42Edit.Text := FloatToStrF(K42, ffGeneral, 5, 2);;
  Tau0REdit.Text := FloatToStrF(TT1, ffGeneral, 5, 2);;
  Tau0SEdit.Text := FloatToStrF(TT2, ffGeneral, 5, 2);;
  Tau0S2Edit.Text := FloatToStrF(TT22, ffGeneral, 5, 2);;
  Tau0TEdit.Text := FloatToStrF(TT3, ffGeneral, 5, 2);;
  Tau03zEdit.Text := FloatToStrF(TT4, ffGeneral, 5, 2);;
end;

function TStructureParameters.ParametersCorrect: boolean;
  {reads structure parameters and checks for syntactical correctness}
var
  wellFormated: boolean;
begin
  wellFormated := false;
  try
    AlphaR := StrToFloat(AlphaREdit.Text);
    BetaR := StrToFloat(BetaREdit.Text);
    GR := StrToFloat(GREdit.Text);
    DR := StrToFloat(DREdit.Text);
    AlphaS := StrToFloat(AlphaSEdit.Text);
    BetaS := StrToFloat(BetaSEdit.Text);
    AlphaS2 := StrToFloat(AlphaS2Edit.Text);
    BetaS2 := StrToFloat(BetaS2Edit.Text);
    GH := StrToFloat(GHEdit.Text);
    DH := StrToFloat(DHEdit.Text);
    LS := StrToFloat(LSEdit.Text);
    SS := StrToFloat(SSEdit.Text);
    DS := StrToFloat(DSEdit.Text);
    AlphaT := StrToFloat(AlphaTEdit.Text);
    BetaT := StrToFloat(BetaTEdit.Text);
    GT := StrToFloat(GTEdit.Text);
    DT := StrToFloat(DTEdit.Text);
    Alpha31 := StrToFloat(Alpha31Edit.Text);
    Beta31 := StrToFloat(Beta31Edit.Text);
    GD1 := StrToFloat(GD1Edit.Text);
    KM1 := StrToFloat(KM1Edit.Text);
    Alpha32 := StrToFloat(Alpha32Edit.Text);
    Beta32 := StrToFloat(Beta32Edit.Text);
    GD2 := StrToFloat(GD2Edit.Text);
    KM2 := StrToFloat(KM2Edit.Text);
    K30 := StrToFloat(K30Edit.Text);
    K31 := StrToFloat(K31Edit.Text);
    K41 := StrToFloat(K41Edit.Text);
    K42 := StrToFloat(K42Edit.Text);
    Tt1 := StrToFloat(Tau0REdit.Text);
    Tt2 := StrToFloat(Tau0SEdit.Text);
    Tt22 := StrToFloat(Tau0S2Edit.Text);
    Tt3 := StrToFloat(Tau0TEdit.Text);
    Tt4 := StrToFloat(Tau03zEdit.Text);
    wellFormated := true;
  except
    on E: EConvertError do
      begin
        ShowFormatMessage;
        wellFormated := false;
      end
      else
      begin
        ShowFormatMessage;
        wellFormated := false;
      end;
  end;
  result := wellFormated;
end;

procedure TStructureParameters.StandardButtonClick(Sender: TObject);
begin
  StandardValues;
  FillInParameters;
end;

procedure TStructureParameters.HandleStrucPars;
begin
  StructureParametersDlg.ShowModal;
end;

procedure TStructureParameters.Image1Click(Sender: TObject);
begin

end;

procedure TStructureParameters.Label31Click(Sender: TObject);
begin

end;

procedure TStructureParameters.Label6Click(Sender: TObject);
begin

end;

procedure TStructureParameters.OKButtonClick(Sender: TObject);
begin
  if ParametersCorrect then
    StructureParametersDlg.Close;
  if TornadoPlotForm.Visible = true then
    begin
      PredictEquilibrium;
      DrawTornadoPlot;
    end;
  if (SensitivityAnalysisForm.Visible = true) and OWSensPlotReady then
    begin
      PredictEquilibrium;
      DrawOWSensitivityPlot(false);
    end;
end;

procedure TStructureParameters.CancelButtonClick(Sender: TObject);
begin
  StructureParametersDlg.Close;
end;

procedure TStructureParameters.FormShow(Sender: TObject);
begin
  FillInParameters;
end;

initialization
  {$I structureparameters.lrs}

end.

