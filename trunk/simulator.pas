unit Simulator;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.2 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

{ This unit implements the main simulation methods }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}
{$R+}
{$define UseCThreads}
{$ASSERTIONS ON}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Forms, SyncObjs, SysUtils, SimThyrTypes, SimThyrResources, SimThyrServices, SimThyrLog,
  SimThyrPlot, SimThyrPrediction, HandleNotifier, UnitConverter;

const
  THREE_FIFTH = 3 / 5; { optimization for speed }
  MAX_DEAD = 300;      { maximum queue length for dead-time interval element }

type
  TQueue_xt = array[0..MAX_DEAD] of real;
  TSimulationThread = class(TThread)
  public
    procedure Pause;
    procedure Restart;
    procedure Execute; override;
    procedure SafeFree;
  end;

var
  xt1, xt2, xt22, xt3, xt4: TQueue_xt;
  nt1, nt2, nt22, nt3, nt4: integer;
  i2, rastergap_text, rastergap_werte: integer;
  i: longint;
  SimCS: TCriticalSection;
  SimThread: TSimulationThread;

function cycles (var n: longint; n_text: str255): integer;
procedure InitSimulationControl;
procedure SimHypothalamus(const inv_hpd: real);
procedure SimPituitary(const gainOfTSH: real; const ultrashortFeedbackGain: real; memory: boolean);
procedure SimThyroidGland(const gainOfT4: real; memory: boolean);
procedure SimCentralDeiodination(const gainOfCentralT3: real; memory: boolean);
procedure SimPeripheralDeiodination(const gainOfPeripheralT3: real; memory: boolean);
procedure SetBaseVariables;
procedure StandardValues;
procedure InitSimulation;

implementation

{-----------------> Blocks of feedback control <--}

 procedure pt1 (var vpt, tpt, x0, xe, ya: real);
 {First order delay element, according to Neuber}
  var
   f: real;
 begin
  assert((vpt >= 0) and (tpt >=0), kError101);
  f := exp(-delt / tpt);
  ya := f * x0 + vpt * (1 - f) * xe;
  x0 := ya;
 end;


 function pt0 (var xt: TQueue_xt; nt: integer; xe: real): real;
 {dead-time element, according to Neuber, improved}
  var
   i: integer;
 begin
  assert((nt >= 0) and (nt <= MAX_DEAD), kError101);
  pt0 := xt[nt];
  if nt > 1 then
   begin
    for i := nt downto 1 do
     begin
      xt[i] := xt[i - 1]
     end;
   end;
  xt[1] := xe;
  xt[0] := xe;
 end;


{-----------------> Help routines <--}

 function InitialValues (var xt: TQueue_xt; nt: integer; xe: real): TQueue_xt;
 {fills virtual signal memory ("anfangswert" in old SimThyr versions)}
  var
   i: integer;
 begin
  assert((nt >= 0) and (nt <= MAX_DEAD), kError101);
  for i := 0 to nt do
   begin
    xt[i] := xe;
   end;
  InitialValues := xt;
 end;

 function cycles (var n: longint; n_text: str255): integer;
 {determines number of loops from given simulation time ("cycles" in old SimThyr versions)}
 begin
  assert(n >= 0, kError101);
  if pos('w', n_text) > 0 then
   factor := SECONDS_PER_WEEK
  else if ((pos('d', n_text) > 0) or (pos('T', n_text) > 0)) then
   factor := SECONDS_PER_DAY
  else if ((pos('h', n_text) > 0) or (pos('St', n_text) > 0)) then
   factor := SECONDS_PER_HOUR
  else if pos('m', n_text) > 0 then
   factor := SECONDS_PER_MINUTE
  else if pos('s', n_text) > 0 then
   factor := 1
  else
   factor := round(delt);
  cycles := round(n * factor / delt) + 1;
 end;

function rnorm (mean, sd: real): real;
{Calculates Gaußian random numbers according to the Box-Müller approach}
 var
  u1, u2: real;
begin
  assert(sd >= 0, kError101);
  u1 := random;
  u2 := random;
  rnorm := mean * abs(1 + sqrt(-2 * (ln(u1))) * cos(2 * pi * u2) * sd);
 end;

 function getgauss (sigma: real): real;
 {controllable function to calculate random numbers of a normal distribution}
 begin
  assert(sigma >= 0, kError101);
  if noiseflag then
   begin
    getgauss := rnorm(1, sigma);
   end
  else
   getgauss := 1;
 end;

 procedure InitSimulationControl;
 { sets standard initial values for time and parameters that control the simulation process }
 begin
  i := 1;
  t := 0;
  delt := 100;                 {calculation steps in seconds; e.g. 1/3 of smallest Halflife}
  signalflag := false;         {Is test signal set?}
  i0 := 0;
 end;

 procedure SetBaseVariables;
 { set initial values for hormone levels and other behavioural parameters  }
 begin
  dTSH := 0.001;               {mU/s		Inhibited production rate [calculated according to D'Angelo 1976, Okuno 1979 and Greenspan 1997]}
{InitialValues:}
  gActiveModel.Equilibrium.TRHs := 2500;                {ng/l, endogenious TRH, according to Rondeel et al. 1988}
  TRH := TRH0;
  TSH := 2;                    {mU/l		from reference value}
  TSHz := 4;                   {Mittlerer Wert}
  T4 := ConvertedValue(7, T4_MOLAR_MASS, 'mcg/dl', 'mol/l');    {from reference value}
  FT4 := ConvertedValue(1.3, T4_MOLAR_MASS, 'ng/dl', 'mol/l');  {from reference value}
  T3p := ConvertedValue(130, T3_MOLAR_MASS, 'ng/dl', 'mol/l');  {from reference value}
  FT3 := ConvertedValue(3.2, T3_MOLAR_MASS, 'pg/ml', 'mol/l');  {from reference value}
  T3z := ConvertedValue(8200, T3_MOLAR_MASS, 'pg/ml', 'mol/l'); {from reference value}
  TBG := 3e-7;                 {mol/l		from reference value}
  TBPA := 4.5e-6;              {mol/l		from reference value}
  IBS := 8e-6;                 {mol/l		estimated from TBG level, corrected for intracellular accumulation [Hays et al. 1988]}
                               {                CRYM, Mu-crystallin homolog also known as NADP-regulated thyroid-hormone-binding     }
                               {                protein (THBP) could be a biochemical representation of IBS}
 end;

 procedure SetDerivedVariables;
 { calculate initial values for variables that have been derived from others }
 begin
  T3n := T3z / (1 + k31 * IBS);
  T3R := GR * T3n / (DR + T3n);	{Calculated from physiological initial values for T3z}
  gH := dTSH * (DH + TRH0) * (1 + LS * T3R) * (1 + Ss * alphaS2 * dTSH / (betaS2 * DS + alphaS2 * dTSH)) / TRH0;   {Max. ungebremste Sekretionsleitung der Hypophyse; Näherungswert: 817 mU/s}
 end;

 procedure SimHypothalamus(const inv_hpd: real);
 { Simulator module for hypothalamic function }
 { invoked by TSimulationThread }
 var
   circadianControl: real;
 begin
   assert(inv_hpd >= 0, kError101);
   omega := 2 * pi * f; {Angular frequency}
   chi := 2 * pi * inv_hpd * 5;
   if circadianflag then
     circadianControl := cos(omega * t - chi)
   else
     circadianControl := 0;
   TRHi := gActiveModel.Equilibrium.TRH1 + THREE_FIFTH * gActiveModel.Equilibrium.TRHs * circadianControl * UTRH;
   TRH := TRHi + TRHe; {Total TRH is sum of internal and external TRH}
   TRH := TRH * getgauss(0.5); {Noise}
 end;

 procedure SimPituitary(const gainOfTSH: real; const ultrashortFeedbackGain: real; memory: boolean);
 { Simulator module for pituitary gland }
 { invoked by TSimulationThread and from Equilibriumdiagram unit }
 begin
   assert((gainOfTSH >= 0) and (ultrashortFeedbackGain >= 0), kError101);
   T3n := T3z / (1 + k31 * IBS);
   T3R := GR * T3n / (dR + T3n);
   dTSH := gH * TRH / ((dH + TRH) * (1 + LS * T3R) * (1 + SS * TSHz / (DS + TSHz)));
   {differential quotient of secretory rate only, no degradation}
   if memory then
     begin
       {Simulation variant with first order memory elements}
       {used for normal simulation run}
       {Equifinal approximation: TSHz := (alphaS2 / betaS2) * dTSH;}
       vpt10 := ultrashortFeedbackGain;
       pt1(vpt10, tpt16, x6, dTSH, TSHz);
       TSHz := pt0(xt22, nt22, TSHz); {pituitary TSH for Brokken-Wiersinga-Prummel loop}
       {optional: TSHz := TSHz * getgauss(0.2); {additional Noise}}
       vpt10 := gainOfTSH;
       pt1(vpt10, tpt12, x2, dTSH, TSH);
       {Equifinal approximation: TSH := alphaS * dTSH / betaS;}
       TSH := pt0(xt2, nt2, TSH);
     end
   else
     begin
       {Equifinal approximation for equilibrium diagram}
       TSHz := ultrashortFeedbackGain * dTSH;
       TSH := gainOfTSH * dTSH;
     end;
 end;

 procedure SimThyroidGland(const gainOfT4: real; memory: boolean);
 { Simulator module for thyroid gland }
 { invoked by TSimulationThread and from Equilibriumdiagram unit  }
 begin
   assert(gainOfT4 >= 0, kError101);
   dT4 := GT * TSH / (dT + TSH);
   if memory then begin
     vpt10 := gainOfT4;
     pt1(vpt10, tpt13, x3, dT4, T4);
     {Equifinal approximation: T4 := alphaT * dT4 / betaT;}
     T4 := pt0(xt3, nt3, T4);
   end
   else
     {Equifinal approximation for equilibrium diagram}
     T4 := gainOfT4 * dT4;
   FT4 := T4 / (1 + k41 * TBG + k42 * TBPA);
 end;

 procedure SimCentralDeiodination(const gainOfCentralT3: real; memory: boolean);
 { Simulator module for central deiodination }
 { invoked by TSimulationThread and from Equilibriumdiagram unit  }
 begin
   assert(gainOfCentralT3 >= 0, kError101);
   dT3z := GD2 * FT4 / (kM2 + FT4);
   if memory then begin
     vpt10 := gainOfCentralT3;
     pt1(vpt10, tpt14, x4, dT3z, T3z);
     T3z := pt0(xt4, nt4, T3z);
   end
   else
     {Equifinal approximation: T3z := alpha32 * dT3z / beta32;}
     T3z := gainOfCentralT3 * dT3z;
 end;

 procedure SimPeripheralDeiodination(const gainOfPeripheralT3: real; memory: boolean);
 { Simulator module for peripheral deiodination }
 { invoked by TSimulationThread and from Equilibriumdiagram unit  }
 begin
   assert(gainOfPeripheralT3 >= 0, kError101);
   dT3p := GD1 * FT4 / (kM1 + FT4);
   if memory then begin
     vpt10 := gainOfPeripheralT3;
     pt1(vpt10, tpt15, x5, dT3p, T3p);
   end
   else
     {Equifinal approximation: T3p := alpha31 * dT3p / beta31;}
     T3p := gainOfPeripheralT3 * dT3p;
   FT3 := T3p / (1 + k30 * TBG);
 end;

 procedure StandardValues;
 begin
{ Set initial values for structure parameters and time constants }

{Reference values:}
{TRH: 5 — 6 ng/l in peripheral plasma, ca. 2000 ng/l in portal plasma}
{TSH: 0,4 — 4 mU/l or 9 — 29 µmol/l}
{TT4: 4,5 — 10 µg/dl or 57,9 — 128,7 nmol/l}
{FT4: 0,8 — 1,8 ng/dl or 10 — 23 pmol/l}
{TT3: 80 — 180 ng/dl or 1,23 — 2,77 nmol/l}
{FT3: 2,3 — 4,2 pg/ml or 3,5 — 6,5 µmol/l}

{Transfer parameters:}
  alphaR := 0.4;               {/l: Dilution factor for TRH in TRH-Test, as alphaS}
  alphaS := 0.4;               {/l: from V0 = 2.5l (peptide hormone disbributing in peripheral plasma only)}
  betaS := 2.3e-4;             {/s: from halflife of about 50 Min. [Li et al. 1995, Odell et al. 1967]}
  alphaS2 := 2.6e5;            {/l: from V0 = 3,8µl (calculated according to Edelman 1959 and other sources)}
  betaS2 := 140;               {/s: primary unknown kinetics, conclusions by analogy from typical receptor potentials, corresponding to halflife of 5 ms}
  dH := 47e-9;                 {mol/l: [Le Dafniet et al. 1994]: 47 nmol/l}
  dR := 0.1e-9;                {mol/l: [Vybok et al. 1994, Lazar et al., 1990]}
  GR := 1;                     {mol/s: scaled to 1, as amplification of feedback path quantified by LS}
  SS := 100;                   {l/mU: calculated from results of [Kakita et al. 1984] with Lineweaver-Burk regression}
  DS := 50;                    {mU/l: calculated from results of [Kakita et al. 1984] with Lineweaver-Burk regression}
  GT := 1.5e-12 * 2.25;        {mol/s: Mean production rate times 2.25 [Li et al. 1995]}
  alphaT := 0.1;               {/l: from V0 = 10 l [Greenspan 1997]}
  betaT := 1.1e-6;             {/s: from halflife of seven days [Greenspan 1997]}
  dT := 2.75;                  {mU/l: averaged from[Dumont & Vassart 1995]}
  GD1 := 2.8e-8;               {mol/sec: determined from reference values from SPINA I trial}
  GD2 := 4.3e-15;              {mol/sec: calculated from pituitary T3 content}
  alpha31 := 2.6e-2;           {/l: from V0 = 38 l [Greenspan 1997]}
  beta31 := 8e-6;              {/s: from halflife of 24h [Greenspan 1997]}
  alpha32 := 1.3e5;            {/l: calculated from V0 = 7.6 µl}
  beta32 := 8.3e-4;            {/s: from halflife of 15 minutes [Oppenheimer et al. 1967}
  kM1 := 0.5e-6;               {mol/l: [Greenspan 1997]}
  kM2 := 1e-9;                 {mol/l: [Visser et al. 1983]}
  k30 := 2e9;                  {l/mol: [Li et al. 1995]}
  k31 := 2e9;                  {l/mol: estimated}
  k41 := 2e10;                 {l/mol: [Li et al. 1995]}
  k42 := 2e8;                  {l/mol: [Li et al. 1995]}
  k3 := GD2 * alpha32 / (beta32 * (1 + k31 * IBS)); {for calculating LS from clinical data}
  G3 := k3 * GR / (k3 + dR);   {dto.: Amplification factor for T3z }
  AC1 := 500;                  {dto.: typical non-brakes TSH concentration}
  MI := AC1 / 0.001;           {dto.: strength of inhibition}
  LS := MI / G3;               {Compensation for GR; LS therefore depending from empirical data only; approximation: 1.68 e6 l/mol}

  SetDerivedVariables;
  if gStartup then
    gH0 := gH                  {stores standard value for gH}
  else
    gH := gH0;                 {restores gH to native value in order to avoid interference as gH is infered from other parameters}

  x1 := TRH0;
  x2 := TSH;
  x3 := T4;
  x4 := T3z;
  x5 := T3p;
  x6 := TSHz;

{Time constants:}
  t121 := 300;                 {TRH: Halflife in seconds}
  betaR := ln(2) / t121;       {Clearance-Exponent für TRH}
  t122 := ln(2) / betaS;       {TSH: Halflife in seconds}
  t123 := ln(2) / betaT;       {T4: Halflife in seconds}
  t124 := ln(2) / beta32;      {T3z: Halflife in seconds}
  t125 := ln(2) / beta31;      {T3p: Halflife in seconds}
  t126 := ln(2) / betaS2;      {TSHz: Halflife in seconds}
  tpt11 := t121 / ln(2);       {1st order delay constant for TRH}
  tpt12 := t122 / ln(2);       {1st order delay constant for TSH}
  tpt13 := t123 / ln(2);       {1st order delay constant for T4}
  tpt14 := t124 / ln(2);       {1st order delay constant for T3z}
  tpt15 := t125 / ln(2);       {1st order delay constant for T3p}
  tpt16 := t126 / ln(2);       {1st order delay constant for TSHz}
  Tt1 := 1800;                 {dead time for TRH: Tau 0 R}
  Tt2 := 120;                  {dead time for TSH: Tau 0 S}
  Tt22 := 3240;                {dead time for Ultrashort feedback effect [derived from Greenspan 1997]: Tau 0 S2}
  Tt3 := 300;                  {dead time for T4: Tau 0 T}
  Tt4 := 3600;                 {dead time for T3z: Tau 0 3z}
  nt2 := trunc(Tt2 / delt);
  nt22 := trunc(Tt22 / delt);
  nt3 := trunc(Tt3 / delt);
  nt4 := trunc(Tt4 / delt);

 end;

procedure TSimulationThread.Pause; {Implements a suspend function even on Unix systems}
begin
 Notice.Hide;
 haltsim := true;
end;

procedure TSimulationThread.Restart; {implements a resume function even on Unix systems}
begin
 if not simready then Notice.ShowOnTop;
 if not assigned(SimCS) then
   SimCS := TCriticalSection.Create;
 haltsim := false;
end;

procedure TSimulationThread.Execute; {Main simulation thread}
var
  theContents: tResultContent;
  nmaxString, iString: string;
  T4conversionFactor, T3conversionFactor: real;
  inv_hpd, inv_nmax, inv_UTRH: real;
  ultrashortFeebackGain: real;
  gainOfTSH, gainOfT4, gainOfCentralT3, gainOfPeripheralT3: real;
begin
  inv_hpd := 1 / HoursPerDay; { optimization for speed: }
  inv_nmax := 1 / nmax;         { on most processors multiplications are }
  inv_UTRH := 1 / UTRH;         { faster than divisions }
  ultrashortFeebackGain := alphaS2 / betaS2;
  gainOfTSH := alphaS / betaS;
  gainOfT4 := alphaT / betaT;
  gainOfCentralT3 := alpha32 / beta32;
  gainOfPeripheralT3 := alpha31 / beta31;
  SimCS.Enter;
  try
    simready := false;
    nmaxString := IntToStr(nmax);
    { Precompute conversion factors in order to speed up simulation:}
    T4conversionFactor := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l', 'ng/dl');
    T3conversionFactor := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', 'pg/ml');
    while i <= nmax do
      begin
        iString := IntToStr(i);
        if not haltsim then
          begin
            gResultMatrix[i-1, i_pos] := i;
            gResultMatrix[i-1, t_pos] := AsTime(t);
            gResultMatrix[i-1, TRH_pos] := TRH * inv_UTRH;
            gResultMatrix[i-1, pTSH_pos] := TSHz;
            gResultMatrix[i-1, TSH_pos] := TSH;
            gResultMatrix[i-1, TT4_pos] := T4 * T4conversionFactor;
            gResultMatrix[i-1, FT4_pos] := FT4 * T4conversionFactor;
            gResultMatrix[i-1, TT3_pos] := T3p * T3conversionFactor;
            gResultMatrix[i-1, FT3_pos] := FT3 * T3conversionFactor;
            gResultMatrix[i-1, cT3_pos] := T3z * T3conversionFactor;
            theContents[i_pos] := iString;
            theContents[t_pos] := FormattedTime(t);
            theContents[TRH_pos] := FormatFloat(gNumberFormat, gResultMatrix[i-1, TRH_pos] * gParameterFactor[TRH_pos]); {TRH}
            theContents[pTSH_pos] := FormatFloat(gNumberFormat, gResultMatrix[i-1, pTSH_pos] * gParameterFactor[pTSH_pos]); {portal TSH}
            theContents[TSH_pos] := FormatFloat(gNumberFormat, gResultMatrix[i-1, TSH_pos] * gParameterFactor[TSH_pos]); {serum TSH}
            theContents[TT4_pos] := FormatFloat(gNumberFormat, gResultMatrix[i-1, TT4_pos] * gParameterFactor[TT4_pos]); {T4}
            theContents[FT4_pos] := FormatFloat(gNumberFormat, gResultMatrix[i-1, FT4_pos] * gParameterFactor[FT4_pos]); {FT4}
            theContents[TT3_pos] := FormatFloat(gNumberFormat, gResultMatrix[i-1, TT3_pos] * gParameterFactor[TT3_pos]); {T3p}
            theContents[FT3_pos] := FormatFloat(gNumberFormat, gResultMatrix[i-1, FT3_pos] * gParameterFactor[FT3_pos]); {FT3}
            theContents[cT3_pos] := FormatFloat(gNumberFormat, gResultMatrix[i-1, cT3_pos] * gParameterFactor[cT3_pos]); {T3z}
            writeTableCells(SimThyrLogWindow.ValuesGrid, theContents);
            if i mod 30 = 0 then
              SetStatusBarPanel0(iString, nmaxString);
            if nmax > 0 then
              SimThyrLogWindow.ProgressBar1.Position := trunc(100 * i / nmax)
            else
              SimThyrLogWindow.ProgressBar1.Position := 100;
            t := t + delt;
{Hypothalamus:}
            SimHypothalamus(inv_hpd);
{Pituitary:}
            SimPituitary(gainOfTSH, ultrashortFeebackGain, true);
{Thyroid:}
            SimThyroidGland(gainOfT4, true);
{5'-Deiodinase type II (central):}
            SimCentralDeiodination(gainOfCentralT3, true);
{5'-Deiodinase type I (peripheral):}
            SimPeripheralDeiodination(gainOfPeripheralT3, true);
            i := i + 1;
{Load:}
            if (i > i1) and testflag then
              begin
                signalflag := true;
                TRHe := UTRH * (200000 * alphaR * exp(-betaR * (t - i1 * delt)));
              end
              else if (i > i1) and tbgflag then
              begin
                signalflag := true;
                TBG := 4.5e-7;
              end;
          end;
      end;
    simready := true;
    DrawPlot(false);
    {$IFNDEF LCLCocoa} // temporary solution for a bug in Coca, needs evaluation
    SimThyrLogWindow.Caption := LOG_TITLE;
    {$ENDIF}
    SetStatusBarPanel0(iString, nmaxString);
    SimThyrLogWindow.ProgressBar1.Position := 0;
  finally
    SimCS.Leave;
    SimCS.Destroy;
  end;
end;

procedure TSimulationThread.SafeFree;  {avoids a dead-lock situation}
begin
  Terminate;
  WaitFor;
  if not FreeOnTerminate then
    Free;
end;

procedure simulate; {Creates and starts simulation thread}
const
  SECONDS_PER_DAY = 86400;
begin
  SetStatusBarPanel0('   0', IntToStr(nmax));
  graphready := false;
  ValuesPlot.Caption := WAIT_TITLE;
  SimThyrLogWindow.Caption := WAIT_TITLE;
  SetLength(gResultMatrix, nmax, RES_MAX_COLS);
  Notice.ShowOnTop;
  haltsim := false;
  f := 1 / SECONDS_PER_DAY; {Frequency of circadian rhythm of TRH secretion}
  SimCS := TCriticalSection.Create;
  if assigned(SimThread) then
    SimThread.Execute
  else
    SimThread := TSimulationThread.Create(false);
end;

procedure InitSimulation;  {sets initial conditions and calculates fixpoints}
begin
  i2 := 1;

  PredictEquilibrium;                        {predicted values}
  if previewflag and (i <= 1) then
   begin                                     {use calculated equilibrium values}
    TSH := gActiveModel.Equilibrium.TSH1;
    FT4 := gActiveModel.Equilibrium.FT41;
    T4 := FT4 * (1 + k41 * TBG + k42 * TBPA);
    FT3 := gActiveModel.Equilibrium.FT31;
    T3p := FT3 * (1 + k30 * TBG);
    T3z := gActiveModel.Equilibrium.T3z1;
    T3R := gActiveModel.Equilibrium.T3R1;
    x2 := TSH;
    x3 := T4;
    x4 := T3z;
    x5 := T3p;
   end;
  if i <= 1 then
   begin
    xt2 := InitialValues(xt2, nt2 + 1, TSH); {prepares dead-time elements}
    xt22 := InitialValues(xt22, nt22 + 1, TSHz);
    xt3 := InitialValues(xt3, nt3 + 1, T4);
    xt4 := InitialValues(xt4, nt4 + 1, T3z);
  end;

  ShowPredictedValues(false);
  GridRows := nmax + 2;
  SimThyrLogWindow.ValuesGrid.RowCount := GridRows;
  SimThyrLogWindow.Show;
  ValuesPlot.ShowOnTop;
  simulate;
end;

end.

{References:  }

{1. Neuber, H., "Simulation von Regelkreisen auf Personal Computern  }
{  in Pascal und Fortran 77", IWT, Vaterstetten 1989  }

{2. Mark, D., C. Reed, "Macintosh Pascal Programming Primer"  }
{  Addison-Wesley, Reading, Ma., Mento Park, Ca., New York 3° 1993  }

{3. Borenstein, Ph., J. Mattson, "THINK Pascal™ User Manual"  }
{  Symantec Corporation, Cupertino, Ca. 1990   }

{4. Dahl, G. E., N. P. Evans, L. A. Thrun, f. J. Karsch (1994) "A Central   }
{  Negative	Feedback Action of Thyroid Hormones on Thyrotropin-  }
{  Releasing Hormone Secretion", Endocrinology 195: 2392-7  }

{5. Samuels, M. H., P. Henry, M. Luther, E. C. Ridgway (1993) "Pulsatile  }
{  TSH Secretion during 48-Hour Continuous TRH Infusions", Thyroid  }
{  3: 201-6  }

{6. Li, G., B. Liu, Y. Liu (1995) "A dynamical model of the pulsatile  }
{  secretion of the hypothalamo-pituitary-thyroid axis", BioSystems  }
{  35: 83-92  }

{7. Greenspan, F. S. "The Thyroid Gland" in: Greenspan, F. S.,  }
{  Strewler, G. J., "Basic and Clinical Endocrinology",  }
{  Appleton & Lange, Stamford, CT, 1997  }

{8. Visser, T. J., Kaplan, M. M., Leonard, J. L., Larsen, P. R. (1983)  }
{  "Evidence for Two Pathways of Iodothyronine 5'-Deiodination in Rat  }
{  Pituitary That Differ in Kinetics, Propylthiouracil Sensitivity and  }
{  Respone to Hypothyroidism", J. Clin. Invest. 71: 992-1002  }
{9. Lee TW; Anderson LA; Eidne KA; Milligan G (1995)   }

{  "Comparison of the signalling properties of the long and short   }
{  isoforms of the rat thyrotropin - releasing - hormone receptor  }
{   following expression in rat 1 fibroblasts", Biochem J 310: 291-8  }

{10. Karhapaa L; Tornquist K (1995) "Caffeine inhibits the binding of  }
{  thyrotropin-releasing hormone in GH4C1 pituitary cells."  }
{  Biochem Biophys Res Commun 210(3):726-32  }

{11. Dumont, J. E., Vassart, G. "Thyroid Regulation" in De. Groot  }
{  "Endocrinology", WB Saunders 1995  }

{12. Jackson. I. M. D., "Thyrotropin Releasing Factor" in Adelman, G.  }
{  "Encyclopedia of Neuroscience", Volume II, Birkhäuser, Boston,  }
{  Basel, Stuttgart, 1987  }

{13. Okuno, A., Taguchi, T., Takimoto, M. (1979) "Kinetic Analysis of   }
{  Plasma TSH Dynamics after TRH Stimulation" Horm. Metab. Res. 11:  }
{  293-5  }

{14. D'Angelo, S. A., Paul, D. H., Wall, N. R., Lombardi, D. M. (1976)  }
{  "Pituitary Thyrotropin (TSH) Rebound Phenomenon and Kinetics of  }
{  Secretion in the Goitrous Rat: Differential Effects of Thyroxine on  }
{  Synthesis and Release of TSH" Endocrinology 99: 935-43  }

{15. Rondeel, J. M. M., de Greef, W. J., van der Schoot, P., Karels, B.,  }
{  Klootwijk, W., Visser, T. J. (1988) "Effect of Thyroid Status and  }
{  Paraventricular Area Lesions on the Release of Thyrotropin-  }
{  Releasing Hormone and Catecholamines into Hypophysial Portal  }
{  Blood" Endocrinology 123:523-7  }

{16. Oppenheimer, J. H., G. Bernstein, J. Hasen (1967) "Estimation of  }
{  Rapidly Exchangeable Cellular Thyroxine from the Plasma  }

{  Disappearance Curves of Simultaneously Administered Thyroxine-  }
{  131I and Albumin-125I" J. Clin. Invest 46: 762-77  }

{17. Edelman, I. S., J. Leibman (1959) "Anatomy of Body Water and  }
{  Electrolytes", Am. J. Med. 27, 256-77  }

{18. Kakita, T., Laborde, N. P., Odell, W. D. (1984) "Autoregulatory  }
{  Control of Thyrotrophin in Rabbits", Endocrinology 114: 2301-5  }

{19. Dietrich, J. W., Tesche, A., Pickardt, C. R., Mitzdorf, U. (2004) }
{  "Thyrotropic Feedback Control: Evidence for an Additional Ultrashort }
{  Feedback Loop from Fractal Analysis", Cybernetics and Systems 35: }
{  315-31 }
{20. Dietrich JW, Stachon A, Antic B, Klein HH, Hering S. (2008) }

{  "The AQUA-FONTIS study: protocol of a multidisciplinary, cross-sectional }
{  and prospective longitudinal study for developing standardized diagnostics }
{  and classification of non-thyroidal illness syndrome", BMC Endocr Disord. }
{  8:13. doi: 10.1186/1472-6823-8-13. PMID 18851740 }

{21. Dietrich JW, Landgrafe G, Fotiadou EH. (2012) "TSH and Thyrotropic }
{  Agonists: Key Actors in Thyroid Homeostasis", J Thyroid Res. 2012:351864. }
{  doi: 10.1155/2012/351864. PMID 23365787 }

{22. Hoermann R, Midgley JE, Larisch R, Dietrich JW. (2013) "Is pituitary }
{  TSH an adequate measure of thyroid hormone-controlled homoeostasis during }
{  thyroxine treatment?", Eur J Endocrinol. 168(2):271-80. }
{  doi: 10.1530/EJE-12-0819. PMID 23184912. }

{23. Midgley JE, Hoermann R, Larisch R, Dietrich JW. (2013) "Physiological }
{  states and functional relation between thyrotropin and free thyroxine in }
{  thyroid health and disease: in vivo and in silico data suggest a }
{  hierarchical model", J Clin Pathol. 66(4):335-42. }
{  doi: 10.1136/jclinpath-2012-201213. PMID 23423518. }

{24. Hoermann R, Midgley JE, Larisch R, Dietrich JW (2015). }
{  Integration of Peripheral and Glandular Regulation of }
{  Triiodothyronine Production by Thyrotropin in Untreated and }
{  Thyroxine-Treated Subjects. Hormone and Metabolic Research, }
{  47(9):674-80. PMID: 25750078 doi: 10.1055/s-0034-1398616. }

{25. Midgley JE, Larisch R, Dietrich JW, Hoermann R. (2015). }
{  Variation in the biochemical response to l-thyroxine therapy and }
{  relationship with peripheral thyroid hormone conversion efficiency. }
{  Endocr Connect. 2015 Dec;4(4):196-205. doi: 10.1530/EC-150056. }
{  PMID 26335522. }

{26. Dietrich JW, Midgley JE, Larisch R, Hoermann R. (2015). }
{  Of rats and men: thyroid homeostasis in rodents and human beings. }
{  Lancet Diabetes Endocrinol. 2015 Dec;3(12):932-3. }
{  doi: 10.1016/S2213-8587(15)00421-0. PMID 26590684. }

{27. Hoermann R, Midgley JE, Larisch R, Dietrich JW. (2015). }
{  Homeostatic Control of the Thyroid-Pituitary Axis: Perspectives for }
{  Diagnosis and Treatment. Front Endocrinol (Lausanne). 2015 }
{  Nov 20;6:177. doi: 10.3389/fendo.2015.00177. PMID 26635726. }

{28. Dietrich JW, Landgrafe-Mende G, Wiora E, Chatzitomaris A, }
{  Klein HH, Midgley JE, Hoermann R. (2016). Calculated Parameters }
{  of Thyroid Homeostasis: Emerging Tools for Differential Diagnosis }
{  and Clinical Research. Front Endocrinol (Lausanne). 2016 Jun 9;7: }
{  57. doi: 10.3389/fendo.2016.00057. PMID 27375554. }




