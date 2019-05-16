unit StructureParameters;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.1 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2019 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2019 }

{ This unit implements an editor for structure parameters }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, SimThyrTypes, SimThyrServices, EnvironmentInfo,
  Simulator, SimOptions;

type

  { TStructureParameters }

  TStructureParameters = class(TForm)
    AlphaREdit: TEdit;
    BetaREdit: TEdit;
    LiverImage: TImage;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    Label66: TLabel;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    Label70: TLabel;
    Label71: TLabel;
    Label72: TLabel;
    OptionsButton: TButton;
    ScrollBox1: TScrollBox;
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
    TFCLogo: TImage;
    ThyroidImage: TImage;
    NeuronImage: TImage;
    PituitaryImage: TImage;
    NeuronImage2: TImage;
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
    TitleLabel: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure StandardButtonClick(Sender: TObject);
    procedure FillInParameters;
    function ParametersCorrect: boolean;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HandleStrucPars;
    procedure TFCLogoClick(Sender: TObject);
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
{fills TEdit boxes of the dialog box with existing values of structure parameters}
begin
  AlphaREdit.Text := FloatToStrF(AlphaR, ffGeneral, 5, 2);
  BetaREdit.Text := FloatToStrF(BetaR, ffGeneral, 5, 2);
  GREdit.Text := FloatToStrF(GR, ffGeneral, 5, 2);
  DREdit.Text := FloatToStrF(DR, ffGeneral, 5, 2);
  AlphaSEdit.Text := FloatToStrF(AlphaS, ffGeneral, 5, 2);
  BetaSEdit.Text := FloatToStrF(BetaS, ffGeneral, 6, 2);
  AlphaS2Edit.Text := FloatToStrF(AlphaS2, ffGeneral, 5, 2);
  BetaS2Edit.Text := FloatToStrF(BetaS2, ffGeneral, 5, 2);
  GHEdit.Text := FloatToStrF(GH, ffGeneral, 5, 2);
  DHEdit.Text := FloatToStrF(DH, ffGeneral, 5, 2);
  LSEdit.Text := FloatToStrF(LS, ffGeneral, 5, 2);
  SSEdit.Text := FloatToStrF(SS, ffGeneral, 5, 2);
  DSEdit.Text := FloatToStrF(DS, ffGeneral, 5, 2);
  AlphaTEdit.Text := FloatToStrF(AlphaT, ffGeneral, 5, 2);
  BetaTEdit.Text := FloatToStrF(BetaT, ffGeneral, 5, 2);
  GTEdit.Text := FloatToStrF(GT, ffGeneral, 5, 2);
  DTEdit.Text := FloatToStrF(DT, ffGeneral, 5, 2);
  Alpha31Edit.Text := FloatToStrF(Alpha31, ffGeneral, 5, 2);
  Beta31Edit.Text := FloatToStrF(Beta31, ffGeneral, 5, 2);
  GD1Edit.Text := FloatToStrF(GD1, ffGeneral, 5, 2);
  KM1Edit.Text := FloatToStrF(KM1, ffGeneral, 5, 2);
  Alpha32Edit.Text := FloatToStrF(Alpha32, ffGeneral, 5, 2);
  Beta32Edit.Text := FloatToStrF(Beta32, ffGeneral, 6, 2);
  GD2Edit.Text := FloatToStrF(GD2, ffGeneral, 5, 2);
  KM2Edit.Text := FloatToStrF(KM2, ffGeneral, 5, 2);
  K30Edit.Text := FloatToStrF(K30, ffGeneral, 5, 2);
  K31Edit.Text := FloatToStrF(K31, ffGeneral, 5, 2);
  K41Edit.Text := FloatToStrF(K41, ffGeneral, 5, 2);
  K42Edit.Text := FloatToStrF(K42, ffGeneral, 5, 2);
  Tau0REdit.Text := FloatToStrF(TT1, ffGeneral, 5, 2);
  Tau0SEdit.Text := FloatToStrF(TT2, ffGeneral, 5, 2);
  Tau0S2Edit.Text := FloatToStrF(TT22, ffGeneral, 5, 2);
  Tau0TEdit.Text := FloatToStrF(TT3, ffGeneral, 5, 2);
  Tau03zEdit.Text := FloatToStrF(TT4, ffGeneral, 5, 2);
end;

function TStructureParameters.ParametersCorrect: boolean;
{reads structure parameters from dialog and checks for syntactical correctness}
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

procedure TStructureParameters.OptionsButtonClick(Sender: TObject);
begin
  SimOptionsDlg.ShowOnTop;
end;

procedure TStructureParameters.FormActivate(Sender: TObject);
begin
  {Adaptations for small screens:}
  if Top < 0 then Top := 26;
  if Left < 0 then Left := 7;
  if Screen.Width < Left + Width then
    Width := Screen.Width - Left;
  if Screen.Height < Top + Height then
    Height := Screen.Height - Top;
  MakeFullyVisible;
end;

procedure TStructureParameters.FormCreate(Sender: TObject);
begin
  if YosemiteORNewer then
    begin
      OKButton.Height := 22;
      CancelButton.Height := 22;
      OptionsButton.Height := 22;
      StandardButton.Height := 22;
    end;
end;

procedure TStructureParameters.HandleStrucPars;
{opens the window and displays it in front similar to a dialog box}
begin
  ShowModal;
end;

procedure TStructureParameters.TFCLogoClick(Sender: TObject);
begin
  {for future extensions}
end;

procedure TStructureParameters.Label31Click(Sender: TObject);
begin
  {for future extensions}
end;

procedure TStructureParameters.Label6Click(Sender: TObject);
begin
  {for future extensions}
end;

procedure TStructureParameters.OKButtonClick(Sender: TObject);
{reads entered values and closes window}
begin
  if ParametersCorrect then
    Close;
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
{closes window without reading parameter values}
begin
  Close;
end;

procedure TStructureParameters.FormShow(Sender: TObject);
{standard actions on show}
begin
  FillInParameters;
  FormActivate(Sender);
end;

initialization
  {$I structureparameters.lrs}

end.

