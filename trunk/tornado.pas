unit tornado;

{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ This unit implements a tornado plot for sensitivity analysis }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  SimThyrTypes, Classes, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, TAGraph, TAStyles, TASeries,
  TASources, TATools, TATransformations, SimThyrServices, Sensitivityanalysis,
  SimThyrPrediction;

type

  { TTornadoPlotForm }

  TTornadoPlotForm = class(TForm)
    Chart1: TChart;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
    FBar: TBarSeries;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TNumberTriplet = record
    l, o, u: real;
  end;

var
  TornadoPlotForm: TTornadoPlotForm;
  gStrucPar, gDepPar: TNumberTriplet;

implementation

uses TACustomSeries;

{ TTornadoPlotForm }

procedure Rotate(ASeries: TBasicPointSeries);
{rotates bars}
var
  t: integer;
begin
  with ASeries do
  begin
    t := AxisIndexX;
    AxisIndexX := AxisIndexY;
    AxisIndexY := t;
  end;
end;

function TestRecord(thePar, amount: real): TNumberTriplet;
{creates a pair of test values from the given structure parameter and amount}
var
  tempResult: TNumberTriplet;
begin
  tempResult.o := thePar;
  tempResult.l := (1 - amount) * thePar;
  tempResult.u := (1 + amount) * thePar;
  TestRecord := tempResult;
end;

procedure DrawTornadoPlot;
var
  i, j: integer;
begin
  {If bar series exists it is cleared and recreated to support foundations of redrawing}
  if TornadoPlotForm.FBar <> nil then
    TornadoPlotForm.Chart1.Series.Clear;
  TornadoPlotForm.FBar := TBarSeries.Create(TornadoPlotForm.Chart1);
  TornadoPlotForm.Chart1.AddSeries(TornadoPlotForm.FBar);
  with TornadoPlotForm.FBar do
  begin
    AxisIndexX := 1;
    AxisIndexY := 0;
    BarWidthPercent := 20;
  end;
  Rotate(TornadoPlotForm.FBar);
  SaveStrucPars;
  gstrucPar := TestRecord(GD1, 0.2);
  gDepPar.o := TSH1;
  GD1 := gStrucPar.l;
  PredictEquilibrium;
  gDepPar.l := (TSH1 - gDepPar.o) / gDepPar.o;
  GD1 := gStrucPar.u;
  PredictEquilibrium;
  gDepPar.u := (TSH1 - gDepPar.o) / gDepPar.o;
  TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
  TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
  RestoreStrucPars;
  gstrucPar := TestRecord(GD2, 0.2);
  gDepPar.o := TSH1;
  GD2 := gStrucPar.l;
  PredictEquilibrium;
  gDepPar.l := (TSH1 - gDepPar.o) / gDepPar.o;
  GD2 := gStrucPar.u;
  PredictEquilibrium;
  gDepPar.u := (TSH1 - gDepPar.o) / gDepPar.o;
  TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
  TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
  RestoreStrucPars;
  PredictEquilibrium;
end;

procedure TTornadoPlotForm.FormCreate(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.FormShow(Sender: TObject);
begin
  DrawTornadoPlot;
end;

initialization
  {$I tornado.lrs}

end.

