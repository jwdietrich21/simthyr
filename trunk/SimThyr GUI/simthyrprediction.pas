unit SimThyrPrediction;

 { SimThyr Project }
 { A numerical simulator of thyrotropic feedback control }

{ Version 5.0.0 (Mirage) }

 { (c) J. W. Dietrich, 1994 - 2023 }
 { (c) Ludwig Maximilian University of Munich 1995 - 2002 }
 { (c) Ruhr University of Bochum 2005 - 2023 }

{ This unit implements a window showing predicted equilibrium values }

 { Source code released under the BSD License }
 { See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, SimThyrTypes, SimThyrResources, SimThyrGUIServices, UnitConverter, Math,
  Predictor;

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

procedure ClearPrediction;
procedure ShowPredictedValues(activate: boolean);

implementation

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
