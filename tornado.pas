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
  Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls, Menus, TAGraph, TAStyles,
  TASeries, TASources, TATools, TATransformations, TALegend, TALegendPanel,
  SimThyrServices, Sensitivityanalysis, SimThyrPrediction, Clipbrd, Buttons,
  ColorBox, TADrawerSVG, TADrawUtils, TADrawerCanvas;

type

  { TTornadoPlotForm }

  TTornadoPlotForm = class(TForm)
    Chart1: TChart;
    DecreaseColorBox: TColorBox;
    IncreaseColorBox: TColorBox;
    LegendPosCombo: TComboBox;
    DummySeries: TBarSeries;
    CheckGroup1: TCheckGroup;
    DepParameterCombo: TComboBox;
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    Divider1: TMenuItem;
    FBar: TBarSeries;
    ListChartSource1: TListChartSource;
    Panel1: TPanel;
    PasteItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    RadioGroup1: TRadioGroup;
    StatusBar1: TStatusBar;
    UndoItem: TMenuItem;
    {function GetFileName(const AExt: String): String;  // for a future extension }
    {procedure SaveAsSVG(Sender: TObject);  // for a future extension }
    procedure CheckGroup1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure DecreaseColorBoxChange(Sender: TObject);
    procedure DepParameterComboChange(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CopyTornado;
    procedure IncreaseColorBoxChange(Sender: TObject);
    procedure LegendPosComboChange(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure SaveChart;
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
  gFracFactor: real;
  gDecreaseTitle, gIncreaseTitle: string;

procedure DrawTornadoPlot;

implementation

uses
  SimThyrMain, TACustomSeries;

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
var
  scaleIndicator: string;
begin
  if gFracFactor = 100 then
    scaleIndicator := ' (%)'
  else
    scaleIndicator := '';
  case TornadoPlotForm.DepParameterCombo.ItemIndex of
    0:
    begin
      TornadoPlotForm.Chart1.BottomAxis.Title.Caption :=
        CHANGE_IN_STRING + DEPENDEND_VAR_STRING + scaleIndicator;
      ResponseVariable := 1;
    end;
    1:
    begin
      TornadoPlotForm.Chart1.BottomAxis.Title.Caption :=
        CHANGE_IN_STRING + 'TSH' + scaleIndicator;
      ResponseVariable := TSH1;
    end;
    2:
    begin
      TornadoPlotForm.Chart1.BottomAxis.Title.Caption :=
        CHANGE_IN_STRING + 'TT4' + scaleIndicator;
      ResponseVariable := T41;
    end;
    3:
    begin
      TornadoPlotForm.Chart1.BottomAxis.Title.Caption :=
        CHANGE_IN_STRING + 'FT4' + scaleIndicator;
      ResponseVariable := FT41;
    end;
    4:
    begin
      TornadoPlotForm.Chart1.BottomAxis.Title.Caption :=
        CHANGE_IN_STRING + 'TT3' + scaleIndicator;
      ResponseVariable := T31;
    end;
    5:
    begin
      TornadoPlotForm.Chart1.BottomAxis.Title.Caption :=
        CHANGE_IN_STRING + 'FT3' + scaleIndicator;
      ResponseVariable := FT31;
    end;
    6:
    begin
      TornadoPlotForm.Chart1.BottomAxis.Title.Caption :=
        CHANGE_IN_STRING + 'cT3' + scaleIndicator;
      ResponseVariable := T3z1; {cT3}
    end;
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
    SeriesColor := TornadoPlotForm.DecreaseColorBox.Selected;
    Title := gDecreaseTitle;
  end;
  Rotate(TornadoPlotForm.FBar);
  TornadoPlotForm.DummySeries := TBarSeries.Create(TornadoPlotForm.Chart1);
  TornadoPlotForm.Chart1.AddSeries(TornadoPlotForm.DummySeries);
  with TornadoPlotForm.DummySeries do
  begin
    AxisIndexX := 1;
    AxisIndexY := 0;
    BarWidthPercent := 70;
    SeriesColor := TornadoPlotForm.IncreaseColorBox.Selected;
    Title := gIncreaseTitle;
  end;

  case TornadoPlotForm.LegendPosCombo.ItemIndex of
    1:
      with TornadoPlotForm.Chart1.Legend do
      begin
        Alignment := laTopLeft;
        MarginX := 63;
        MarginY := 4;
      end;
    2:
      with TornadoPlotForm.Chart1.Legend do
      begin
        TornadoPlotForm.Chart1.Legend.Alignment := laTopRight;
        MarginX := 4;
        MarginY := 4;
      end;
    3:
      with TornadoPlotForm.Chart1.Legend do
      begin
        TornadoPlotForm.Chart1.Legend.Alignment := laBottomLeft;
        MarginX := 63;
        MarginY := 39;
      end;
    4:
      with TornadoPlotForm.Chart1.Legend do
      begin
        TornadoPlotForm.Chart1.Legend.Alignment := laBottomRight;
        MarginX := 4;
        MarginY := 39;
      end;
  end;

  SaveStrucPars;
  i := 1;
  TornadoPlotForm.ListChartSource1.DataPoints.Clear;

  if TornadoPlotForm.RadioGroup1.ItemIndex = 0 then
    gFracFactor := 1
  else
    gFracFactor := 100;

  if TornadoPlotForm.CheckGroup1.Checked[0] then
  begin
    {GD1}
    gstrucPar := TestRecord(GD1, 0.2);
    gDepPar.o := ResponseVariable;
    GD1 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    GD1 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'GD1');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[1] then
  begin
    {GD2}
    gstrucPar := TestRecord(GD2, 0.2);
    gDepPar.o := ResponseVariable;
    GD2 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    GD2 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'GD2');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[2] then
  begin
    {kM1}
    gstrucPar := TestRecord(kM1, 0.2);
    gDepPar.o := ResponseVariable;
    kM1 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    kM1 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'kM1');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[3] then
  begin
    {kM2}
    gstrucPar := TestRecord(kM2, 0.2);
    gDepPar.o := ResponseVariable;
    kM2 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    kM2 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'kM2');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[4] then
  begin
    {GT}
    gstrucPar := TestRecord(GT, 0.2);
    gDepPar.o := ResponseVariable;
    GT := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    GT := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'GT');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[5] then
  begin
    {DT}
    gstrucPar := TestRecord(DT, 0.2);
    gDepPar.o := ResponseVariable;
    DT := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    DT := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'DT');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[6] then
  begin
    {GH}
    gstrucPar := TestRecord(GH, 0.2);
    gDepPar.o := ResponseVariable;
    GH := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    GH := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'GH');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[7] then
  begin
    {DH}
    gstrucPar := TestRecord(DH, 0.2);
    gDepPar.o := ResponseVariable;
    DH := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    DH := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'DH');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[8] then
  begin
    {SS}
    gstrucPar := TestRecord(SS, 0.2);
    gDepPar.o := ResponseVariable;
    SS := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    SS := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'SS');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[9] then
  begin
    {DS}
    gstrucPar := TestRecord(DS, 0.2);
    gDepPar.o := ResponseVariable;
    DS := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    DS := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'DS');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[10] then
  begin
    {GR}
    gstrucPar := TestRecord(GR, 0.2);
    gDepPar.o := ResponseVariable;
    GR := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    GR := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'GR');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[11] then
  begin
    {DR}
    gstrucPar := TestRecord(DR, 0.2);
    gDepPar.o := ResponseVariable;
    DR := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    DR := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'DR');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[12] then
  begin
    {betaS}
    gstrucPar := TestRecord(betaS, 0.2);
    gDepPar.o := ResponseVariable;
    betaS := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    betaS := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'betaS');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[13] then
  begin
    {betaS2}
    gstrucPar := TestRecord(betaS2, 0.2);
    gDepPar.o := ResponseVariable;
    betaS2 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    betaS2 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'betaS2');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[14] then
  begin
    {betaT}
    gstrucPar := TestRecord(betaT, 0.2);
    gDepPar.o := ResponseVariable;
    betaT := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    betaT := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'betaT');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[15] then
  begin
    {beta31}
    gstrucPar := TestRecord(beta31, 0.2);
    gDepPar.o := ResponseVariable;
    beta31 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    beta31 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'beta31');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[16] then
  begin
    {beta32}
    gstrucPar := TestRecord(beta32, 0.2);
    gDepPar.o := ResponseVariable;
    beta32 := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    beta32 := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'beta32');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[17] then
  begin
    {LS}
    gstrucPar := TestRecord(LS, 0.2);
    gDepPar.o := ResponseVariable;
    LS := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    LS := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'LS');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[18] then
  begin
    {TBG}
    gstrucPar := TestRecord(TBG, 0.2);
    gDepPar.o := ResponseVariable;
    TBG := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TBG := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'TBG');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[19] then
  begin
    {TBPA}
    gstrucPar := TestRecord(TBPA, 0.2);
    gDepPar.o := ResponseVariable;
    TBPA := gStrucPar.l;
    PredictEquilibrium;
    gDepPar.l := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TBPA := gStrucPar.u;
    PredictEquilibrium;
    gDepPar.u := (ResponseVariable - gDepPar.o) / gDepPar.o * gFracFactor;
    TornadoPlotForm.FBar.Add(gDepPar.l, '', TornadoPlotForm.DecreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', TornadoPlotForm.IncreaseColorBox.Selected);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    PredictEquilibrium;     {restore previous predictions}
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'TBPA');
    i := i + 3;
  end;

  if i = 1 then
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + '');
  PredictEquilibrium;     {restore previous predictions}

end;

procedure TTornadoPlotForm.FormCreate(Sender: TObject);
begin
  gDecreaseTitle := '20% ' + DECREASE_STRING;
  gINcreaseTitle := '20% ' + INCREASE_STRING;
  RadioGroup1.ItemIndex := 0;
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

procedure TTornadoPlotForm.DecreaseColorBoxChange(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.DepParameterComboChange(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.CopyTornado;
var
  {$IFDEF UNIX}
  theImage: TPortableNetworkGraphic;
  {$ELSE}
  theImage: TBitMap;
  {$ENDIF}
  theWidth, theHeight: integer;
begin
  if Chart1 = nil then
    bell
  else
  begin
    {gSelectedChart.CopyToClipboardBitmap doesn't work on Mac OS X}
    {$IFDEF UNIX}
    theImage := TPortableNetworkGraphic.Create;
    try
      theWidth := Chart1.Width;
      theHeight := Chart1.Height;
      theImage.Width := theWidth;
      theImage.Height := theHeight;
      Chart1.DrawOnCanvas(rect(0, 0, theImage.Width, theImage.Height), theImage.canvas);
      Clipboard.Assign(theImage);
    finally
      theImage.Free;
    end;
    {$ELSE}
    Chart1.CopyToClipboardBitmap;
    {$ENDIF}
  end;
end;

procedure TTornadoPlotForm.IncreaseColorBoxChange(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.LegendPosComboChange(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.RadioGroup1Click(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.CopyItemClick(Sender: TObject);
begin
  CopyTornado;
end;

procedure TTornadoPlotForm.SaveChart;
var
  theFileName:  string;
  theFilterIndex: integer;
  theStream: TFileStream;
  theDrawer: IChartDrawer;
  theWidth, theHeight: integer;
begin
  if Chart1 = nil then
    bell
  else
  begin
    theStream := nil;
    SimThyrToolbar.SavePictureDialog1.FilterIndex := 2;
    if SimThyrToolbar.SavePictureDialog1.Execute then
      try
        theFileName    := SimThyrToolbar.SavePictureDialog1.FileName;
        theFilterIndex := SimThyrToolbar.SavePictureDialog1.FilterIndex;
         {$IFDEF LCLcarbon}{compensates for a bug in the carbon widgetset}
        theFilterIndex := theFilterIndex + 1;
         {$ENDIF}{may be removed in future versions}
        case theFilterIndex of
        2: Chart1.SaveToBitmapFile(theFileName);
        3: Chart1.SaveToFile(TPixmap, theFileName);
        4: Chart1.SaveToFile(TPortableNetworkGraphic, theFileName);
        5: Chart1.SaveToFile(TPortableAnyMapGraphic, theFileName);
        6: Chart1.SaveToFile(TJPEGImage, theFileName);
        7: Chart1.SaveToFile(TTIFFImage, theFileName);
        8: begin
             theStream := TFileStream.Create(theFileName, fmCreate);
             theDrawer := TSVGDrawer.Create(theStream, true);
             theDrawer.DoChartColorToFPColor := @ChartColorSysToFPColor;
             with Chart1 do
               Draw(theDrawer, Rect(0, 0, Width, Height));
           end;
        otherwise bell;
        end;
      finally
        if theStream <> nil then theStream.Free;
      end;
  end;
end;

procedure TTornadoPlotForm.FormActivate(Sender: TObject);
begin
  SimThyrToolbar.SelectAllMenuItem.Enabled := false;
  gLastActiveCustomForm := TornadoPlotForm;
end;

procedure TTornadoPlotForm.FormShow(Sender: TObject);
begin
  DrawTornadoPlot;
end;

initialization
  {$I tornado.lrs}

end.

