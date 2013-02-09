unit SimThyrPrediction;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.2.3 }

{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit implements a window showing predicted equilibrium values }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SimThyrTypes, SimThyrServices;

type

  { TPrediction }

  TPrediction = class(TForm)
    Memo1: TMemo;
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
procedure ShowPredictedValues;

implementation

function arc (chi: real): real;
{rechnet Winkel von Grad nach Bogenmaß um}
begin
 arc := 2 * pi * (chi / 360);
end;

function arccosinus (cosphi: real): real;
{errechnet den Arcus-Cosinus einer Zahl zwischen -1 und 1}
 var
  arcsin: real;
begin
 arcsin := arctan(cosphi / sqrt(1 - sqr(cosphi)));
 arccosinus := arc(90) - arcsin;
end;

function TRH0: real;
begin
  TRHe := 0;                   {mol/l		exogeniously applied TRH}
  TRHi := TRHs;                {ng/l		endogenious TRH, according to Rondeel et al. 1988}
  TRHi := TRHi * UTRH;         {mol/l}
  TRH0 := TRHi + TRHe;  {mol/l		portal total TRH concentration}
end;

procedure PredictEquilibrium;
{predicts equilibrium values of behavioural parameters like TSH, free T4 or free T3}
begin
  TRH1 := TRH0;
  k1 := gH * alphaS / betaS;
  k11 := alphaS / betaS;
  k2 := GT * alphaT / betaT;
  k21 := k2;
  k22 := k21 / (1 + k41 * TBG + k42 * TBPA);
  k3 := GD2 * alpha32 / (beta32 * (1 + k31 * IBS));
  k5 := GD1 * alpha31 / beta31;
  k51 := k5 / (1 + k30 * TBG);
  k6 := 1 / (2 * (1 + SS) * alphaS2);
  k61 := k6 * k11;
  k7 := dH + TRH1;
  k8 := alphaS2 * gH * TRH1 / (dH + TRH1);
  k9 := DS * betaS2;
  G3 := k3 * GR / (k3 + dR);
  D3 := kM2 * dR / (k3 + dR);
  a1 := (D3 + k22 + LS * G3 * k22) / k61;
  b1 := D3 * dT / k61 + 2 * D3 * k9 + 2 * k9 * k22 + 2 * LS * G3 * k9 * k22 - 2 * D3 * k8 - 2 * k8 * k22;
  c1 := 2 * (D3 * dT * k9 - D3 * dT * k8 - 2 * (1 + SS) * D3 * k8 * k9 * k61 - 2 * (1 + SS) * k8 * k9 * k22 * k61);
  d1 := -4 * (1 + SS) * D3 * dT * k8 * k9 * k61;
  r1 := c1 / a1 - 1 / 3 * sqr(b1 / a1);
  s1 := 2 / 27 * sqr(b1 / a1) * (b1 / a1) - 1 / 3 * c1 * b1 / sqr(a1) + d1 / a1;
  p1 := r1 / 3;
  q1 := s1 / 2;
  Det := p1 * p1 * p1 + q1 * q1;
  if Det > 0 then
   begin {Cardano-Formel, eine reale Lösung}
    u := exp(ln(-q1 + sqrt(Det)) * (1 / 3));
    v := exp(ln(-q1 - sqrt(Det)) * (1 / 3));
    y1 := u + v;	{reale Lösung nach der Cardano-Fomel}
    y2 := -(u + v) / 2; {Realteil der 1. komplexen Lösung}
    y3 := y2; {Realteil der 2. komplexen Lösung, identisch mit y2}
   end
  else
   begin {Casus irreducibilis, drei reale Lösungen}
    u := -q1 / (sqrt(-p1 * sqr(-p1))); {cos phi}
    phi := arccosinus(u); {Winkel im Bogenmaß}
    y1 := 2 * sqrt(-p1) * cos(phi / 3);
    y2 := -2 * sqrt(-p1) * cos(phi / 3 + arc(60));
    y3 := -2 * sqrt(-p1) * cos(phi / 3 - arc(60));
   end;
  TSH1 := y1 - b1 / (3 * a1);
  TSH2 := y2 - b1 / (3 * a1);
  TSH3 := y3 - b1 / (3 * a1);
  FT41 := alphaT * GT * TSH1 / (betaT * (dT + TSH1) * (1 + k41 * TBG + k42 * TBPA));
  FT42 := alphaT * GT * TSH2 / (betaT * (dT + TSH2) * (1 + k41 * TBG + k42 * TBPA));
  FT43 := alphaT * GT * TSH3 / (betaT * (dT + TSH3) * (1 + k41 * TBG + k42 * TBPA));
  T41 := alphaT * GT * TSH1 / (betaT * (dT + TSH1));
  T42 := alphaT * GT * TSH2 / (betaT * (dT + TSH2));
  T43 := alphaT * GT * TSH3 / (betaT * (dT + TSH3));
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
  T31 := k5 * FT41 / (kM1 + FT41);
  T32 := k5 * FT42 / (kM1 + FT42);
  T33 := k5 * FT43 / (kM1 + FT43);
end;

procedure ClearPrediction;
{clears the contents of the prediction window}
var
  theCaption: String;
begin
  theCaption := Prediction.Memo1.Lines[0];
  Prediction.Memo1.Clear;
  Prediction.Memo1.Lines.Text := theCaption;
end;

procedure ShowPredictedValues;
{writes predicted values into the memo field of the prediction window}
var
  FF: TFloatFormat;
begin
  ClearPrediction;
  Prediction.Show;
  FF := ffFixed;
  writeaMemoLine(Prediction.Memo1, '');
  writeaMemoLine(Prediction.Memo1, 'TRH: ' + FloatToStrF(TRH1 / UTRH, FF, 0, 4) + ' ' + gParameterUnit[TRH_pos]);
  writeaMemoLine(Prediction.Memo1, 'TSH: ' + FloatToStrF(TSH1 * gParameterFactor[pTSH_pos], FF, 0, 4) + ', ' + FloatToStrF(TSH2 * gParameterFactor[pTSH_pos], FF, 0, 4) + ' and ' + FloatToStrF(TSH3 * gParameterFactor[pTSH_pos], FF, 0, 4) + ' ' + gParameterUnit[TSH_pos]);
  writeaMemoLine(Prediction.Memo1, 'TT4: ' + FloatToStrF(T41 / UFT4 * gParameterFactor[TT4_pos], FF, 0, 4) + ', ' + FloatToStrF(T42 / UFT4 * gParameterFactor[TT4_pos], FF, 0, 4) + ' and ' + FloatToStrF(T43 / UFT4 * gParameterFactor[TT4_pos], FF, 0, 4) + ' ' + gParameterUnit[TT4_pos]);
  writeaMemoLine(Prediction.Memo1, 'FT4: ' + FloatToStrF(FT41 / UFT4 * gParameterFactor[FT4_pos], FF, 0, 4) + ', ' + FloatToStrF(FT42 / UFT4 * gParameterFactor[FT4_pos], FF, 0, 4) + ' and ' + FloatToStrF(FT43 / UFT4 * gParameterFactor[FT4_pos], FF, 0, 4) + ' ' + gParameterUnit[FT4_pos]);
  writeaMemoLine(Prediction.Memo1, 'TT3: ' + FloatToStrF(T31 / UFT3 * gParameterFactor[TT3_pos], FF, 0, 4) + ', ' + FloatToStrF(T32 / UFT3 * gParameterFactor[TT3_pos], FF, 0, 4) + ' and ' + FloatToStrF(T33 / UFT3 * gParameterFactor[TT3_pos], FF, 0, 4) + ' ' + gParameterUnit[TT3_pos]);
  writeaMemoLine(Prediction.Memo1, 'FT3: ' + FloatToStrF(FT31 / UFT3 * gParameterFactor[FT3_pos], FF, 0, 4) + ', ' + FloatToStrF(FT32 / UFT3 * gParameterFactor[FT3_pos], FF, 0, 4) + ' and ' + FloatToStrF(FT33 / UFT3 * gParameterFactor[FT3_pos], FF, 0, 4) + ' ' + gParameterUnit[FT3_pos]);
  writeaMemoLine(Prediction.Memo1, 'cT3: ' + FloatToStrF(T3z1 / UFT3, FF, 0, 4) + ', ' + FloatToStrF(T3z2 / UFT3, FF, 0, 4) + ' and ' + FloatToStrF(T3z2 / UFT3, FF, 0, 4) + ' ' + gParameterUnit[FT3_pos]);
  writeaMemoLine(Prediction.Memo1, '');
  writeaMemoLine(Prediction.Memo1, NEGATIVE_VALUES_HINT);
  writeaMemoLine(Prediction.Memo1, DEVIATION_STRING);
end;

initialization
  {$I simthyrprediction.lrs}

end.

