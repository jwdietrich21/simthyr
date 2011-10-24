unit SimThyrPrediction;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit implements a window showing predicted equilibrium values }

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

procedure ClearPrediction;
procedure ShowPredictedValues;

implementation

procedure ClearPrediction;
var
  theCaption: String;
begin
  theCaption := Prediction.Memo1.Lines[0];
  Prediction.Memo1.Clear;
  Prediction.Memo1.Lines.Text := theCaption;
end;

procedure ShowPredictedValues;
var
  FF: TFloatFormat;
begin
  ClearPrediction;
  Prediction.Show;
  FF := ffFixed;
  writeaMemoLine(Prediction.Memo1, '');
  writeaMemoLine(Prediction.Memo1, 'TRH: ' + FloatToStrF(TRH1 / UTRH, FF, 0, 4));
  writeaMemoLine(Prediction.Memo1, 'TSH: ' + FloatToStrF(TSH1, FF, 0, 4) + ', ' + FloatToStrF(TSH2, FF, 0, 4) + ' and ' + FloatToStrF(TSH3, FF, 0, 4));
  writeaMemoLine(Prediction.Memo1, 'FT4: ' + FloatToStrF(FT41 / UFT4, FF, 0, 4) + ', ' + FloatToStrF(FT42 / UFT4, FF, 0, 4) + ' and ' + FloatToStrF(FT43 / UFT4, FF, 0, 4));
  writeaMemoLine(Prediction.Memo1, 'FT3: ' + FloatToStrF(FT31 / UFT3, FF, 0, 4) + ', ' + FloatToStrF(FT32 / UFT3, FF, 0, 4) + ' and ' + FloatToStrF(FT33 / UFT3, FF, 0, 4));
  writeaMemoLine(Prediction.Memo1, 'cT3: ' + FloatToStrF(T3z1 / UFT3, FF, 0, 4) + ', ' + FloatToStrF(T3z2 / UFT3, FF, 0, 4) + ' and ' + FloatToStrF(T3z2 / UFT3, FF, 0, 4));
end;

initialization
  {$I simthyrprediction.lrs}

end.

