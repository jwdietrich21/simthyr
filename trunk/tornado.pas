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
  TASeries, TASources, TATools, TATransformations, TALegendPanel,
  SimThyrServices, Sensitivityanalysis, SimThyrPrediction, Clipbrd;

type

  { TTornadoPlotForm }

  TTornadoPlotForm = class(TForm)
    Chart1: TChart;
    DummySeries: TBarSeries;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
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
    procedure CheckGroup1Click(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure ComboBox1Change(Sender: TObject);
    procedure CopyItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CopyTornado(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
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
  gDecreaseTitle, gIncreaseTitle: String;

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
var
  scaleIndicator: string;
begin
  if gFracFactor = 100 then
    scaleIndicator := ' (%)'
  else
    scaleIndicator := '';
  case TornadoPlotForm.ComboBox1.ItemIndex of
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
        CHANGE_IN_STRING + 'FT4' + scaleIndicator;
      ResponseVariable := FT41;
    end;
    3:
    begin
      TornadoPlotForm.Chart1.BottomAxis.Title.Caption :=
        CHANGE_IN_STRING + 'FT3' + scaleIndicator;
      ResponseVariable := FT31;
    end;
    4:
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
    SeriesColor := clGray;
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
    SeriesColor := clBlack;
    Title := gIncreaseTitle;
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
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
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
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'GD2');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[2] then
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
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'GT');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[3] then
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
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'kM1');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[4] then
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
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
     TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'kM2');
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
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'DT');
    i := i + 3;
  end;

  if TornadoPlotForm.CheckGroup1.Checked[6] then
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
    TornadoPlotForm.FBar.Add(gDepPar.l, '', clGray);
    TornadoPlotForm.FBar.Add(gDepPar.u, '', clBlack);
    TornadoPlotForm.FBar.Add(0, '', clDkGray);
    RestoreStrucPars;
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) +
      '|' + IntToStr(i) + '|?|' + 'LS');
    i := i + 3;
  end;

  if i = 1 then
    TornadoPlotForm.ListChartSource1.DataPoints.Add(IntToStr(i) + '|' +
      IntToStr(i) + '|?|' + '');
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

procedure TTornadoPlotForm.ComboBox1Change(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.CopyTornado(Sender: TObject);
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

procedure TTornadoPlotForm.RadioGroup1Click(Sender: TObject);
begin
  DrawTornadoPlot;
end;

procedure TTornadoPlotForm.CopyItemClick(Sender: TObject);
begin
  CopyTornado(Sender);
end;

procedure TTornadoPlotForm.FormShow(Sender: TObject);
begin
  DrawTornadoPlot;
end;

initialization
  {$I tornado.lrs}

end.

