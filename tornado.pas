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
  TASources, TATools, TATransformations, TALegendPanel, SimThyrServices,
  Sensitivityanalysis, SimThyrPrediction;

type

  { TTornadoPlotForm }

  TTornadoPlotForm = class(TForm)
    Chart1: TChart;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
    FBar: TBarSeries;
    ListChartSource1: TListChartSource;
    StatusBar1: TStatusBar;
    procedure CheckGroup1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure ComboBox1Change(Sender: TObject);
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

function ResponseVariable: real;
  {delivers the variable that has been selected in the combo box}
begin
  case TornadoPlotForm.ComboBox1.ItemIndex of
    0:
      ResponseVariable := 1;
    1:
      ResponseVariable := TSH1;
    2:
      ResponseVariable := FT41;
    3:
      ResponseVariable := FT31;
    4:
      ResponseVariable := T3z1; {cT3}
  end;
end;

procedure DrawTornadoPlot;
var
  i: integer;
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
    BarWidthPercent := 70;
  end;
  Rotate(TornadoPlotForm.FBar);
  SaveStrucPars;
  i := 0;

  if TornadoPlotForm.CheckGroup1.Checked[0] then
  begin
    {GD1}
    gstrucPar := TestRecord(GD1, 0.2);
    gDepPar.o := ResponseVariable;
    GD1 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o;
    GD1 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    {TornadoPlotForm.ListChartSource1.DataPoints.Add('GD1');}
  end;

  if TornadoPlotForm.CheckGroup1.Checked[1] then
  begin
    {GD2}
    gstrucPar := TestRecord(GD2, 0.2);
    gDepPar.o := ResponseVariable;
    GD2 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o;
    GD2 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[2] then
  begin
    {GT}
    gstrucPar := TestRecord(GT, 0.2);
    gDepPar.o := ResponseVariable;
    GT := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o;
    GT := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[3] then
  begin
    {kM1}
    gstrucPar := TestRecord(kM1, 0.2);
    gDepPar.o := ResponseVariable;
    kM1 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o;
    kM1 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[4] then
  begin
    {kM2}
    gstrucPar := TestRecord(kM2, 0.2);
    gDepPar.o := ResponseVariable;
    kM2 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o;
    kM2 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[5] then
  begin
    {DT}
    gstrucPar := TestRecord(DT, 0.2);
    gDepPar.o := ResponseVariable;
    DT := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o;
    DT := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[6] then
  begin
    {LS}
    gstrucPar := TestRecord(LS, 0.2);
    gDepPar.o := ResponseVariable;
    LS := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o;
    LS := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
  end;

  PredictEquilibrium;     {restore previous predictions}

end;

procedure TTornadoPlotForm.FormCreate(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.CheckGroup1Click(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.CheckGroup1ItemClick(Sender: TObject; Index: integer);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.ComboBox1Change(Sender: TObject);
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

