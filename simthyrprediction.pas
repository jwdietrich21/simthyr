unit SimThyrPrediction;

 { SimThyr Project }
 { A numerical simulator of thyrotropic feedback control }

{ Version 4.0.2 (Merlion) }

 { (c) J. W. Dietrich, 1994 - 2020 }
 { (c) Ludwig Maximilian University of Munich 1995 - 2002 }
 { (c) Ruhr University of Bochum 2005 - 2020 }

{ This unit implements a window showing predicted equilibrium values }

 { Source code released under the BSD License }
 { See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SimThyrTypes, SimThyrResources, SimThyrServices, UnitConverter, Math;

type

  TRoots = record
    x1, x2, x3: extended;
  end;

  { TPrediction }

  TPrediction = class(TForm)
    PredictionMemo: TMemo;
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Prediction: TPrediction;

function TRH0: real;
procedure PredictEquilibrium;
procedure ClearPrediction;
procedure ShowPredictedValues(activate: boolean);

implementation

function arc(chi: real): real;
  {rechnet Winkel von Grad nach Bogenmaß um}
  {converts an angle from degree to radian}
begin
  arc := 2 * pi * (chi / 360);
end;

function arccosinus(cosphi: real): real;
  {errechnet den Arcus-Cosinus einer Zahl zwischen -1 und 1}
  {calculates the arcus cosine of a number between -1 and 1}
var
  arcsin: real;
begin
  arcsin     := arctan(cosphi / sqrt(1 - sqr(cosphi)));
  arccosinus := arc(90) - arcsin;
end;

function cbrt(x: real): real;
  {calculates cubic root of x}
begin
  result := sign(x) * power(abs(x), 1/3);
end;

function SolveCubic(a, b, c, d: extended): TRoots;
var
  r, s, p, q, u, v, Det, phi, y1, y2, y3: extended;
begin
  r  := c / a - 1 / 3 * sqr(b / a);
  s  := 2 / 27 * power((b / a), 3) - 1 / 3 * c * b / sqr(a) + d / a;
  p  := r / 3;
  q  := s / 2;

  Det := p * p * p + q * q;

  if Det >= 0 then
  begin {Cardano's formula, one real solution}
    u  := cbrt(-q + sqrt(Det));
    v  := cbrt(-q - sqrt(Det));
    y1 := u + v;        {real solution of Cardano's equation}
    y2 := -(u + v) / 2; {Real part of the first complex solution}
    y3 := y2;           {Real part of the second complex solution (=y2)}
  end
  else
  begin {Casus irreducibilis, three real solutions}
    u   := -q / (sqrt(-p * sqr(-p))); {cos phi}
    phi := arccosinus(u);            {angle as radian}
    y1  := 2 * sqrt(-p) * cos(phi / 3);
    y2  := -2 * sqrt(-p) * cos(phi / 3 + arc(60));
    y3  := -2 * sqrt(-p) * cos(phi / 3 - arc(60));
  end;

  result.x1 := y1 - b / (3 * a);
  result.x2 := y2 - b / (3 * a);
  result.x3 := y3 - b / (3 * a);
end;

function TRH0: real;
begin
  TRHe := 0;                             {mol/l    exogeniously applied TRH}
  TRHi := gActiveModel.Equilibrium.TRHs;
  {ng/l    endogenious TRH, according to Rondeel et al. 1988}
  TRHi := TRHi * UTRH;                   {mol/l}
  TRH0 := TRHi + TRHe;                   {mol/l    portal total TRH concentration}
end;

procedure PredictEquilibrium;
{predicts equilibrium values of behavioural parameters like TSH, free T4 or free T3}
var
  CubicRoots: TRoots;
begin
  gActiveModel.Equilibrium.TRH1 := TRH0;
  k1  := gH * alphaS / betaS;
  k11 := alphaS / betaS;
  k2  := GT * alphaT / betaT;
  k21 := k2;
  k22 := k21 / (1 + k41 * TBG + k42 * TBPA);
  k3  := GD2 * alpha32 / (beta32 * (1 + k31 * IBS));
  k5  := GD1 * alpha31 / beta31;
  k51 := k5 / (1 + k30 * TBG);
  k6  := 1 / (2 * (1 + SS) * alphaS2);
  k61 := k6 * k11;
  k7  := dH + gActiveModel.Equilibrium.TRH1;
  k8  := alphaS2 * gH * gActiveModel.Equilibrium.TRH1 /
    (dH + gActiveModel.Equilibrium.TRH1);
  k9  := DS * betaS2;
  G3  := k3 * GR / (k3 + dR);
  D3  := kM2 * dR / (k3 + dR);
  a1  := (D3 + k22 + LS * G3 * k22) / k61;
  b1  := D3 * dT / k61 + 2 * D3 * k9 + 2 * k9 * k22 + 2 * LS * G3 *
    k9 * k22 - 2 * D3 * k8 - 2 * k8 * k22;
  c1  := 2 * (D3 * dT * k9 - D3 * dT * k8 - 2 * (1 + SS) * D3 * k8 *
    k9 * k61 - 2 * (1 + SS) * k8 * k9 * k22 * k61);
  d1  := -4 * (1 + SS) * D3 * dT * k8 * k9 * k61;
  CubicRoots := SolveCubic(a1, b1, c1, d1);
  with gActiveModel.Equilibrium do
  begin
    TSH1 := CubicRoots.x1;
    TSH2 := CubicRoots.x2;
    TSH3 := CubicRoots.x3;
    FT41 := alphaT * GT * TSH1 / (betaT * (dT + TSH1) * (1 + k41 * TBG + k42 * TBPA));
    FT42 := alphaT * GT * TSH2 / (betaT * (dT + TSH2) * (1 + k41 * TBG + k42 * TBPA));
    FT43 := alphaT * GT * TSH3 / (betaT * (dT + TSH3) * (1 + k41 * TBG + k42 * TBPA));
    T41  := alphaT * GT * TSH1 / (betaT * (dT + TSH1));
    T42  := alphaT * GT * TSH2 / (betaT * (dT + TSH2));
    T43  := alphaT * GT * TSH3 / (betaT * (dT + TSH3));
    T3z1 := alpha32 * GD2 * FT41 / (beta32 * (kM2 + FT41));
    T3z2 := alpha32 * GD2 * FT42 / (beta32 * (kM2 + FT42));
    T3z3 := alpha32 * GD2 * FT43 / (beta32 * (kM2 + FT43));
    T3n1 := T3z1 / (1 + k31 * IBS);
    T3n2 := T3z2 / (1 + k31 * IBS);
    T3n3 := T3z3 / (1 + k31 * IBS);
    T3R1 := GR * T3n1 / (DR + T3n1);
    T3R2 := GR * T3n2 / (DR + T3n2);
    T3R3 := GR * T3n3 / (DR + T3n3);
    FT31 := k51 * FT41 / (kM1 + FT41);
    FT32 := k51 * FT42 / (kM1 + FT42);
    FT33 := k51 * FT43 / (kM1 + FT43);
    T31  := k5 * FT41 / (kM1 + FT41);
    T32  := k5 * FT42 / (kM1 + FT42);
    T33  := k5 * FT43 / (kM1 + FT43);
  end;
end;

procedure ClearPrediction;
{clears the contents of the prediction window}
var
  theCaption: string;
begin
  theCaption := Prediction.PredictionMemo.Lines[0];
  Prediction.PredictionMemo.Clear;
  Prediction.PredictionMemo.Lines.Text := theCaption;
end;

procedure ShowPredictedValues(activate: boolean);
{writes predicted values into the memo field of the prediction window}
var
  FF: TFloatFormat;
  FT4conversionFactor, FT3conversionFactor: real;
  TT4conversionFactor, TT3conversionFactor, cT3conversionFactor: real;
begin
  FT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT4_pos]);
  TT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l',
    gParameterUnit[TT4_pos]);
  TT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[TT3_pos]);
  FT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT3_pos]);
  cT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[cT3_pos]);
  ClearPrediction;
  if activate then
    Prediction.Show;
  FF := ffFixed;
  writeaMemoLine(Prediction.PredictionMemo, '');
  with gActiveModel.Equilibrium do
  begin
    writeaMemoLine(Prediction.PredictionMemo, 'TRH: ' +
      FloatToStrF(TRH1 / UTRH, FF, 0, 4) + ' ' + gParameterUnit[TRH_pos]);
    writeaMemoLine(Prediction.PredictionMemo, 'TSH: ' +
      FloatToStrF(TSH1 * gParameterFactor[pTSH_pos], FF, 0, 4) + ', ' +
      FloatToStrF(TSH2 * gParameterFactor[pTSH_pos], FF, 0, 4) + ' and ' +
      FloatToStrF(TSH3 * gParameterFactor[pTSH_pos], FF, 0, 4) + ' ' +
      gParameterUnit[TSH_pos]);
    writeaMemoLine(Prediction.PredictionMemo, 'TT4: ' +
      FloatToStrF(T41 * TT4conversionFactor, FF, 0, 4) + ', ' +
      FloatToStrF(T42 * TT4conversionFactor, FF, 0, 4) + ' and ' +
      FloatToStrF(T43 * TT4conversionFactor, FF, 0, 4) + ' ' + gParameterUnit[TT4_pos]);
    writeaMemoLine(Prediction.PredictionMemo, 'FT4: ' +
      FloatToStrF(FT41 * FT4conversionFactor, FF, 0, 4) + ', ' +
      FloatToStrF(FT42 * FT4conversionFactor, FF, 0, 4) + ' and ' +
      FloatToStrF(FT43 * FT4conversionFactor, FF, 0, 4) + ' ' + gParameterUnit[FT4_pos]);
    writeaMemoLine(Prediction.PredictionMemo, 'TT3: ' +
      FloatToStrF(T31 * TT3conversionFactor, FF, 0, 4) + ', ' +
      FloatToStrF(T32 * TT3conversionFactor, FF, 0, 4) + ' and ' +
      FloatToStrF(T33 * TT3conversionFactor, FF, 0, 4) + ' ' + gParameterUnit[TT3_pos]);
    writeaMemoLine(Prediction.PredictionMemo, 'FT3: ' +
      FloatToStrF(FT31 * FT3conversionFactor, FF, 0, 4) + ', ' +
      FloatToStrF(FT32 * FT3conversionFactor, FF, 0, 4) + ' and ' +
      FloatToStrF(FT33 * FT3conversionFactor, FF, 0, 4) + ' ' + gParameterUnit[FT3_pos]);
    writeaMemoLine(Prediction.PredictionMemo, 'cT3: ' +
      FloatToStrF(T3z1 * cT3conversionFactor, FF, 0, 4) + ', ' +
      FloatToStrF(T3z2 * cT3conversionFactor, FF, 0, 4) + ' and ' +
      FloatToStrF(T3z2 * cT3conversionFactor, FF, 0, 4) + ' ' + gParameterUnit[FT3_pos]);
    writeaMemoLine(Prediction.PredictionMemo, '');
    writeaMemoLine(Prediction.PredictionMemo, NEGATIVE_VALUES_HINT);
    writeaMemoLine(Prediction.PredictionMemo, DEVIATION_STRING);
  end;
  Prediction.PredictionMemo.SelStart  := 0;
  Prediction.PredictionMemo.SelLength := 0;
end;

{ TPrediction }

procedure TPrediction.FormPaint(Sender: TObject);
begin
  if DarkTheme then
  begin
    Color := BACKCOLOUR;
  end
  else
  begin
    Color := clWhite;
  end
end;

procedure TPrediction.FormShow(Sender: TObject);
begin
  FormPaint(Sender);
end;

initialization
  {$I simthyrprediction.lrs}

end.
