unit Equilibriumdiagram;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2015 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2015 }

{ This unit implements an equilibrium diagram }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}
{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASources, TASeries, TATransformations,
  TANavigation, TATools, TAStyles, LResources, Forms, Controls, Graphics,
  Dialogs, Buttons, ExtCtrls, StdCtrls, Spin, ComCtrls, ColorBox, Clipbrd, Menus,
  Math, SimThyrTypes, SimThyrResources, Simulator, SimThyrServices,
  UnitConverter;

const
  MAX_SERIES = 2;
  MAX_I      = 100;
  MAX_PIT    = 13;

type

  { TEquilibriumDiagramForm }

  TEquilibriumDiagramForm = class(TForm)
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    Divider1: TMenuItem;
    PasteItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    SParEdit1: TEdit;
    EquilibriumChart: TChart;
    ChartNavPanel1: TChartNavPanel;
    EquilibriumChartLineSeries1: TLineSeries;
    EquilibriumChartLineSeries2: TLineSeries;
    GroupBox2:     TGroupBox;
    MaxSpinEdit1:  TFloatSpinEdit;
    MaxSpinEdit2:  TFloatSpinEdit;
    MinSpinEdit1:  TFloatSpinEdit;
    MinSpinEdit2:  TFloatSpinEdit;
    SParCombo1:    TComboBox;
    BParCombo1:    TComboBox;
    BParCombo2:    TComboBox;
    SParCombo2:    TComboBox;
    SParCombo3:    TComboBox;
    SParEdit2: TEdit;
    SParEdit3: TEdit;
    SParTrackBar1: TTrackBar;
    SParTrackBar2: TTrackBar;
    SParTrackBar3: TTrackBar;
    UndoItem: TMenuItem;
    xColorBox:     TColorBox;
    FullScaleButton1: TSpeedButton;
    GroupBox1:     TGroupBox;
    MainPanel:     TPanel;
    ResetButton:   TSpeedButton;
    StatusBar1:    TStatusBar;
    yColorBox:     TColorBox;
    procedure CopyItemClick(Sender: TObject);
    procedure CopyChart;
    procedure FormActivate(Sender: TObject);
    procedure GetBParameters;
    procedure SParEdit1Change(Sender: TObject);
    procedure SParEdit2Change(Sender: TObject);
    procedure SParEdit3Change(Sender: TObject);
    procedure UpdateEditsfromTrackBars;
    procedure UpdateTrackBarsfromEdits;
    procedure DrawDummyEquilibriumPlot;
    procedure DrawDiagram(empty: boolean);
    procedure BParCombo1Change(Sender: TObject);
    procedure BParCombo2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FullScaleButton1Click(Sender: TObject);
    procedure MaxSpinEdit1Change(Sender: TObject);
    procedure MaxSpinEdit2Change(Sender: TObject);
    procedure MinSpinEdit1Change(Sender: TObject);
    procedure MinSpinEdit2Change(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
    procedure SParCombo1Change(Sender: TObject);
    procedure SParCombo2Change(Sender: TObject);
    procedure SParCombo3Change(Sender: TObject);
    procedure SParTrackBar1Change(Sender: TObject);
    procedure SParTrackBar2Change(Sender: TObject);
    procedure SParTrackBar3Change(Sender: TObject);
    procedure xColorBoxChange(Sender: TObject);
    procedure yColorBoxChange(Sender: TObject);
  private
    { private declarations }
    FLine: array[0..MAX_SERIES - 1] of TLineSeries;
  public
    { public declarations }
  end;

  tParamVector = array[0..MAX_I] of extended;
  tResponseCurve = record
    input, output: tParamVector;
  end;
  tTSHSamples = array[0..MAX_PIT - 1] of extended;

var
  EquilibriumDiagramForm: TEquilibriumDiagramForm;
  gSelectedBParameter1, gSelectedBParameter2: tBParameter;
  gSelectedSParameter1, gSelectedSParameter2, gSelectedSParameter3: tSParameter;
  gSpinFactor: real;
  gResponseCurve1, gResponseCurve2: tResponseCurve;
  gFT4conversionFactor, gFT3conversionFactor: real;
  gcT3conversionFactor: real;


implementation

function SimPituitaryResponse(T3zVector: tParamVector): tParamVector;
{ Simulate response of thyroid subsystem to vector with TSH values }
var
  i, j: integer;
  interval: real;
  gainOfTSH, usFeedbackGain: real;
  TSHSamples: tTSHSamples;
begin
  gainOfTSH := alphaS / betaS;
  usFeedbackGain := alphaS2 / betaS2;
  for i := 0 to MAX_I do
  begin
    T3z := T3zVector[i];
    { Take multiple samples and ... }
    for j := 0 to MAX_PIT - 1 do
    begin
      SimPituitary(gainOfTSH, usFeedbackGain, false);
      TSHSamples[j] := TSH;
    end;
    { calculate mean of samples in equilibrium without transient results }
    { to compensate for oscillations resulting from ultrashort feedback }
    Result[i] := mean(TSHSamples[3..MAX_PIT-1]);
  end;
end;

function SimThyroidResponse(TSHVector: tParamVector): tParamVector;
{ Simulate response of thyroid subsystem to vector with TSH values }
var
  i: integer;
  interval: real;
  gainOfT4: real;
begin
  gainOfT4 := alphaT / betaT;
  for i := 0 to MAX_I do
  begin
    TSH := TSHVector[i];
    SimThyroidGland(gainOfT4, false);
    Result[i] := FT4;
  end;
end;

function SimPDeiodinaseResponse(FT4Vector: tParamVector): tParamVector;
{ Simulate response of peripheral deiodinases to T4 vector }
var
  i: integer;
  interval: real;
  gainOfPeripheralT3: real;
begin
  gainOfPeripheralT3 := alpha31 / beta31;
  for i := 0 to MAX_I do
  begin
    FT4 := FT4Vector[i];
    SimPeripheralDeiodination(gainOfPeripheralT3, false);
    Result[i] := FT3;
  end;
end;

function SimCDeiodinaseResponse(FT4Vector: tParamVector): tParamVector;
{ Simulate response of central type 2 deiodinase to T4 vector }
var
  i: integer;
  interval: real;
  gainOfCentralT3: real;
begin
  gainOfCentralT3 := alpha32 / beta32;
  for i := 0 to MAX_I do
  begin
    FT4 := FT4Vector[i];
    SimCentralDeiodination(gainOfCentralT3, false);
    Result[i] := T3z;
  end;
end;

function SimSubsystemResponse(bParameter1, bParameter2: tBParameter;
  min, max: real; var conversionFactor1, conversionFactor2: real): tResponseCurve;
{ Simulate response of first subsystem }
var
  i: integer;
  interval: real;
  inputVector, emptyVector: tParamVector;
begin
  assert((min >= 0) and (max >= 0), kError101);
  assert(max >= min, kError103);
  assert(max > 0, kError104);
  fillchar(emptyVector, sizeof(emptyVector), 0);
  interval := (max - min) / MAX_I;
  case bParameter1 of // input (independent parameter)
    TSHItem:
    begin
      conversionFactor1 := 1;
      for i := 0 to MAX_I do
      begin
        TSH := min + i * interval;
        inputVector[i] := TSH;
      end;
    end;
    FT4Item:
    begin
      conversionFactor1 := gFT4conversionFactor;
      for i := 0 to MAX_I do
      begin
        FT4 := min + i * interval;
        inputVector[i] := FT4 / conversionFactor1;
      end;
    end;
    cT3Item:
    begin
      conversionFactor1 := gFT3conversionFactor;
      for i := 0 to MAX_I do
      begin
        T3z := min + i * interval;
        inputVector[i] := T3z / conversionFactor1;
      end;
    end;
    otherwise
    begin
      inputVector := emptyVector;
    end;
  end;
  case bParameter2 of // output (dependent parameter)
    IItem:
    begin
      Result.output := emptyVector
    end;
    TSHItem:
    begin
      case bParameter1 of
        FT4Item:
        begin;
          conversionFactor2 := 1;
          Result.output := SimPituitaryResponse(SimCDeiodinaseResponse(inputVector));
        end;
        cT3Item:
        begin;
          conversionFactor2 := 1;
          Result.output := SimPituitaryResponse(inputVector);
        end;
        otherwise
          Result.output := emptyVector;
      end;
    end;
    FT4Item:
    begin
      case bParameter1 of
        TSHItem:
        begin
          conversionFactor2 := gFT4conversionFactor;
          Result.output := SimThyroidResponse(inputVector);
        end;
        cT3Item:
        begin
          conversionFactor2 := gFT4conversionFactor;
          Result.output := SimThyroidResponse(SimPituitaryResponse(inputVector));
        end;
        otherwise
          Result.output := emptyVector;
      end;
    end;
    FT3Item:
    begin
      case bParameter1 of
        TSHItem:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimPDeiodinaseResponse(SimThyroidResponse(inputVector));
        end;
        FT4Item:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimPDeiodinaseResponse(inputVector);
        end;
        cT3Item:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimPDeiodinaseResponse(SimThyroidResponse(SimPituitaryResponse(inputVector)));
        end;
        otherwise
          Result.output := emptyVector;
      end;
    end;
    cT3Item:
    begin
      case bParameter1 of
        TSHItem:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimCDeiodinaseResponse(SimThyroidResponse(inputVector));
        end;
        FT4Item:
        begin
          conversionFactor2 := gFT3conversionFactor;
          Result.output := SimCDeiodinaseResponse(inputVector);
        end;
        otherwise
          Result.output := emptyVector;
      end;
    end;
    otherwise
      Result.output := emptyVector;
  end;
  Result.input := inputVector;
end;

{ TEquilibriumDiagramForm }

procedure TEquilibriumDiagramForm.GetBParameters;
begin
  if pos('TSH', BParCombo1.Text) > 0 then
    gSelectedBParameter1 := TSHItem
  else if pos('FT4', BParCombo1.Text) > 0 then
    gSelectedBParameter1 := FT4Item
  else if pos('FT3', BParCombo1.Text) > 0 then
    gSelectedBParameter1 := FT3Item
  else if pos('cT3', BParCombo1.Text) > 0 then
    gSelectedBParameter1 := cT3Item
  else
    gSelectedBParameter1 := IItem;
  if pos('TSH', BParCombo2.Text) > 0 then
    gSelectedBParameter2 := TSHItem
  else if pos('FT4', BParCombo2.Text) > 0 then
    gSelectedBParameter2 := FT4Item
  else if pos('FT3', BParCombo2.Text) > 0 then
    gSelectedBParameter2 := FT3Item
  else if pos('cT3', BParCombo2.Text) > 0 then
    gSelectedBParameter2 := cT3Item
  else
    gSelectedBParameter2 := IItem;
end;

procedure TEquilibriumDiagramForm.SParEdit1Change(Sender: TObject);
begin
  UpdateTrackBarsfromEdits;
end;

procedure TEquilibriumDiagramForm.SParEdit2Change(Sender: TObject);
begin
  UpdateTrackBarsfromEdits;
end;

procedure TEquilibriumDiagramForm.SParEdit3Change(Sender: TObject);
begin
  UpdateTrackBarsfromEdits;
end;

procedure TEquilibriumDiagramForm.FormActivate(Sender: TObject);
begin
  UpdateEditsfromTrackBars;
end;

procedure TEquilibriumDiagramForm.CopyItemClick(Sender: TObject);
begin
  CopyChart;
end;

procedure TEquilibriumDiagramForm.CopyChart;
var
  {$IFDEF UNIX}
  theImage: TPortableNetworkGraphic;
  {$ELSE}
  theImage: TBitMap;
  {$ENDIF}
  theWidth, theHeight: integer;
begin
  if EquilibriumChart = nil then
    bell
  else
  begin
    {Chart1.CopyToClipboardBitmap doesn't work on Mac OS X}
    {$IFDEF UNIX}
    theImage := TPortableNetworkGraphic.Create;
    try
      theWidth := EquilibriumChart.Width;
      theHeight := EquilibriumChart.Height;
      theImage.Width := theWidth;
      theImage.Height := theHeight;
      EquilibriumChart.DrawOnCanvas(rect(0, 0, theImage.Width, theImage.Height), theImage.canvas);
      Clipboard.Assign(theImage);
    finally
      theImage.Free;
    end;
    {$ELSE}
    EquilibriumChart.CopyToClipboardBitmap;
    {$ENDIF}
  end;
end;

procedure TEquilibriumDiagramForm.UpdateEditsfromTrackBars;
begin
  SParEdit1.Text := IntToStr(SParTrackBar1.Position);
  SParEdit2.Text := IntToStr(SParTrackBar2.Position);
  SParEdit3.Text := IntToStr(SParTrackBar3.Position);
end;

procedure TEquilibriumDiagramForm.UpdateTrackBarsfromEdits;
begin
  if (StrToFloatDef(SParEdit1.Text, NaN) >= SParTrackBar1.Min) and
    (StrToFloatDef(SParEdit1.Text, NaN) <= SParTrackBar1.Max) then
    SParTrackBar1.Position := trunc(StrToFloatDef(SParEdit1.Text, 0))
  else
    SParTrackBar1.Position := SParTrackBar1.Min;
  if (StrToFloatDef(SParEdit2.Text, NaN) >= SParTrackBar2.Min) and
    (StrToFloatDef(SParEdit2.Text, NaN) <= SParTrackBar2.Max) then
    SParTrackBar2.Position := trunc(StrToFloatDef(SParEdit2.Text, 0))
  else
    SParTrackBar2.Position := SParTrackBar2.Min;
  if (StrToFloatDef(SParEdit3.Text, NaN) >= SParTrackBar3.Min) and
    (StrToFloatDef(SParEdit3.Text, NaN) <= SParTrackBar3.Max) then
    SParTrackBar3.Position := trunc(StrToFloatDef(SParEdit3.Text, 0))
  else
    SParTrackBar3.Position := SParTrackBar3.Min;
end;

procedure TEquilibriumDiagramForm.DrawDummyEquilibriumPlot;
{Draws an empty plot}
var
  i: integer;
begin
  EquilibriumChart.LeftAxis.Title.Caption   := 'Concentration 2';
  EquilibriumChart.BottomAxis.Title.Caption := 'Concentration 1';
  with FLine[0] do
  begin
    AddXY(0, 0, '', clBlack);
    AddXY(100, 0, '', clBlack);
  end;
  with FLine[1] do
  begin
    AddXY(0, 0, '', clBlack);
    AddXY(0, 100, '', clBlack);
  end;
end;

procedure TEquilibriumDiagramForm.DrawDiagram(empty: boolean);
var
  i, j: integer;
  MinBPar1, MaxBPar1, MinBPar2, MaxBPar2: real;
  ConversionFactor1, ConversionFactor2: real;
  max_x: real;
begin
  GetBParameters;
  gFT4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT4_pos]);
  gFT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[FT3_pos]);
  gcT3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l',
    gParameterUnit[cT3_pos]);
  {If line series exists it is cleared and recreated to support foundations of redrawing}
  if FLine[1] <> nil then
    EquilibriumChart.ClearSeries;
  for i := 0 to MAX_SERIES - 1 do
  begin
    FLine[i] := TLineSeries.Create(EquilibriumChart);
    with FLine[i] do
    begin
      ShowLines   := True;
      ShowPoints  := False;
      Pointer.Brush.Color := clBlack;
      SeriesColor := clBlack;
      LinePen.Mode := pmCopy;
      LineType := ltFromPrevious;
    end;
    EquilibriumChart.AddSeries(FLine[i]);
    FLine[i].BeginUpdate;
  end;
  if empty then
    DrawDummyEquilibriumPlot
  else
  begin
    MinBPar1 := MinSpinEdit1.Value / gSpinFactor;
    MaxBPar1 := MaxSpinEdit1.Value / gSpinFactor;
    MinBPar2 := MinSpinEdit2.Value / gSpinFactor;
    MaxBPar2 := MaxSpinEdit2.Value / gSpinFactor;
    EquilibriumChart.LeftAxis.Range.UseMin := false;
    EquilibriumChart.LeftAxis.Range.UseMax := false;
    EquilibriumChart.BottomAxis.Range.UseMin := false;
    EquilibriumChart.BottomAxis.Range.UseMax := false;
    gResponseCurve1 := SimSubsystemResponse(gSelectedBParameter2, gSelectedBParameter1, MinBPar2,
      MaxBPar2, ConversionFactor1, ConversionFactor2);
    for j := 0 to MAX_I do
    begin
      FLine[0].AddXY(gResponseCurve1.input[j] * conversionFactor1,
        gResponseCurve1.output[j] * conversionFactor2, '',
        xColorBox.Selected);
      Fline[0].SeriesColor := xColorBox.Selected;
    end;
    gResponseCurve2 := SimSubsystemResponse(gSelectedBParameter1, gSelectedBParameter2, MinBPar1,
    MaxBPar1, ConversionFactor1, ConversionFactor2);
    for j := 0 to MAX_I do
    begin
      FLine[1].AddXY(gResponseCurve2.output[j] * conversionFactor2,
        gResponseCurve2.input[j] * conversionFactor1, '',
        yColorBox.Selected);
      Fline[1].SeriesColor := yColorBox.Selected;
    end;
  end;
  for i := 0 to MAX_SERIES - 1 do
    FLine[i].EndUpdate;
  if isNaN(conversionFactor1) or isNaN(conversionFactor2) then
    max_x := NaN
  else
    begin
      max_x := max(MaxValue(gResponseCurve1.input) * conversionFactor1,
      MaxValue(gResponseCurve2.output) * conversionFactor2);
      if MaxSpinEdit2.Value < max_x then MaxSpinEdit2.Value := max_x;
    end;
  if gSelectedBParameter1 <> IItem then
    EquilibriumChart.LeftAxis.Title.Caption := BParCombo1.Caption;
  if gSelectedBParameter2 <> IItem then
    EquilibriumChart.BottomAxis.Title.Caption := BParCombo2.Caption;
end;

procedure TEquilibriumDiagramForm.FormCreate(Sender: TObject);
begin
  gSpinFactor := 1;
  UpdateEditsfromTrackBars;
  DrawDiagram(True);
end;

procedure TEquilibriumDiagramForm.BParCombo1Change(Sender: TObject);
{ Get selected behavioural parameter #1 }
begin
  GetBParameters;
  xColorBox.Selected := gDefaultColors[integer(gSelectedBParameter1)];
  if BParCombo2.ItemIndex = 0 then
    BParCombo2.ItemIndex := 1;  // set to useful initial value
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.BParCombo2Change(Sender: TObject);
{ Get selected behavioural parameter #2 }
begin
  GetBParameters;
  yColorBox.Selected := gDefaultColors[integer(gSelectedBParameter2)];
  if BParCombo1.ItemIndex = 0 then
    BParCombo1.ItemIndex := 1;  // set to useful initial value
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.FullScaleButton1Click(Sender: TObject);
begin
  EquilibriumChart.Extent.UseYMax := False;
  EquilibriumChart.ZoomFull;
end;

procedure TEquilibriumDiagramForm.MaxSpinEdit1Change(Sender: TObject);
begin
  if MaxSpinEdit1.Value < MinSpinEdit1.Value then
  {adapts boundaries to avoid negative intervals}
  begin
    bell;
    MaxSpinEdit1.Value := MinSpinEdit1.Value;
  end;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.MaxSpinEdit2Change(Sender: TObject);
begin
  if MaxSpinEdit2.Value < MinSpinEdit2.Value then
  {adapts boundaries to avoid negative intervals}
  begin
    bell;
    MaxSpinEdit2.Value := MinSpinEdit2.Value;
  end;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.MinSpinEdit1Change(Sender: TObject);
begin
  if MaxSpinEdit1.Value < MinSpinEdit1.Value then
  {adapts boundaries to avoid negative intervals}
  begin
    bell;
    MinSpinEdit1.Value := MaxSpinEdit1.Value;
  end;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.MinSpinEdit2Change(Sender: TObject);
begin
  if MaxSpinEdit2.Value < MinSpinEdit2.Value then
  {adapts boundaries to avoid negative intervals}
  begin
    bell;
    MinSpinEdit2.Value := MaxSpinEdit2.Value;
  end;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.ResetButtonClick(Sender: TObject);
begin

end;

procedure TEquilibriumDiagramForm.SParCombo1Change(Sender: TObject);
begin
  DrawDiagram(False);
  if pos('GD1', SParCombo1.Text) > 0 then
    gSelectedSParameter1 := GD1Item  // to be completed...
  else
    gSelectedSParameter1 := NullItem;
end;

procedure TEquilibriumDiagramForm.SParCombo2Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParCombo3Change(Sender: TObject);
begin
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParTrackBar1Change(Sender: TObject);
begin
  UpdateEditsfromTrackBars;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParTrackBar2Change(Sender: TObject);
begin
  UpdateEditsfromTrackBars;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.SParTrackBar3Change(Sender: TObject);
begin
  UpdateEditsfromTrackBars;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.xColorBoxChange(Sender: TObject);
begin
  Fline[1].SeriesColor := xColorBox.Selected;
  //EquilibriumChartLineSeries1.SeriesColor := xColorBox.Selected;
  DrawDiagram(False);
end;

procedure TEquilibriumDiagramForm.yColorBoxChange(Sender: TObject);
begin
  Fline[1].SeriesColor := xColorBox.Selected;
  //EquilibriumChartLineSeries2.SeriesColor := yColorBox.Selected;
  DrawDiagram(False);
end;

initialization
  {$I equilibriumdiagram.lrs}
  gSelectedSParameter1 := NullItem;
  gSelectedSParameter2 := NullItem;
  gSelectedSParameter2 := NullItem;

end.
