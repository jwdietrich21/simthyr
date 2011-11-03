unit Simulator;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2011 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2011 }

{ This unit implements the main simulation methods }

{ Source code released under the BSD License }

{$mode objfpc}{$R+}
{$define UseCThreads}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Forms, SyncObjs, SysUtils, SimThyrTypes, SimThyrServices, SimThyrLog,
  SimThyrPlot, SimThyrPrediction, HandleNotifier;

type
  TQueue_xt = array[0..300] of real;
  TSimulationThread = class(TThread)
  public
    procedure Pause;
    procedure Restart;
    procedure Execute; override;
  end;

var
  xt1, xt2, xt22, xt3, xt4: TQueue_xt;
  i, nt1, nt2, nt22, nt3, nt4: integer;
  i2, rastergap_text, rastergap_werte: integer;
  SimCS: TCriticalSection;
  SimThread: TSimulationThread;

function cycles (var n: longint; n_text: str255): integer;
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
  f := exp(-delt / tpt);
  ya := f * x0 + vpt * (1 - f) * xe;
  x0 := ya;
 end;


 function pt0 (var xt: TQueue_xt; nt: integer; xe: real): real;
 {dead-time element, according to Neuber, improved}
  var
   i: integer;
 begin
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
  for i := 0 to nt do
   begin
    xt[i] := xe;
   end;
  InitialValues := xt;
 end;

 function cycles (var n: longint; n_text: str255): integer;
 {determines number of loops from given simulation time ("cycles" in old SimThyr versions)}
 begin
  if ((pos('d', n_text) > 0) or (pos('T', n_text) > 0)) then
   factor := 86400
  else if ((pos('h', n_text) > 0) or (pos('St', n_text) > 0)) then
   factor := 3600
  else if pos('m', n_text) > 0 then
   factor := 60
  else if pos('s', n_text) > 0 then
   factor := 1
  else
   factor := round(delt);
  cycles := round(n * factor / delt) + 1;
 end;

 function getgauss (sigma: real): real;
 {Calculates Gaußian random numbers according to the Box-Müller-Verfahren}
  var
   u1, u2: real;
 begin
  if noiseflag then
   begin
    {Randomize;      THINK Pascal only: randSeed := TickCount;}
    {u1 := ((random + 32768) / 65536);
    u2 := ((random + 32768) / 65536);}
    u1 := random;
    u2 := random;
    getgauss := abs(1 + sqrt(-2 * (ln(u1))) * cos(2 * pi * u2) * sigma);
   end
  else
   getgauss := 1;
 end;

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

 procedure SetBaseVariables;
 begin
  i := 1;
  t := 0;
  delt := 100;                 {calculation steps in seconds; e.g. 1/3 of smallest Halflife}
  signalflag := false;         {Is test signal set?}
  i0 := 0;
  dTSH := 0.001;               {mU/s		Inhibited production rate [calculated according to D'Angelo 1976, Okuno 1979 and Greenspan 1997]}
{InitialValues:}
  TRHe := 0;                   {mol/l		exogeniously applied TRH}
  TRHi := 2500;                {ng/l		endogenious TRH, according to Rondeel et al. 1988}
  TRHi := TRHi * UTRH;         {mol/l}
  TRH0 := TRHi + TRHe;         {mol/l		portal total TRH concentration}
  TRH := TRH0;
  TSH := 2;                    {mU/l		from reference value}
  TSHz := 4;                   {Mittlerer Wert}
  T4 := 7;                     {µg/dl		from reference value}
  T4 := T4 * 1000 * UFT4;      {mol/l}
  FT4 := 1.3;                  {ng/dl		from reference value}
  FT4 := FT4 * UFT4;           {mol/l}
  T3p := 130;                  {ng/dl		from reference value}
  T3p := T3p * 10 * UFT3;      {mol/l}
  FT3 := 3.2;                  {pg/ml	        from reference value}
  FT3 := FT3 * UFT3;           {mol/l}
  T3z := 8200;                 {pg/ml}
  T3z := T3z * UFT3;           {mol/l}
  TBG := 3e-7;                 {mol/l		from reference value}
  TBPA := 4.5e-6;              {mol/l		from reference value}
  IBS := 8e-6;                 {mol/l		estimated from TBG level, corrected for intracellular accumulation [Hays et al. 1988]}
 end;

 procedure SetDerivedVariables;
 begin
  T3n := T3z / (1 + k31 * IBS);
  T3R := GR * T3n / (DR + T3n);	{Calculated from physiological initial values for T3z}
  gH := dTSH * (DH + TRH0) * (1 + LS * T3R) * (1 + Ss * alphaS2 * dTSH / (betaS2 * DS + alphaS2 * dTSH)) / TRH0;   {Max. ungebremste Sekretionsleitung der Hypophyse; Näherungswert: 817 mU/s}
 end;

 procedure StandardValues;
 begin
{ Set parameters and initial values }
  SetBaseVariables;

{Referenzwerte:}
{TRH: 5 — 6 ng/l im peripheren Blut, ca. 2000 ng/l im Portalsystem}
{TSH: 0,4 — 4 mU/l oder 9 — 29 µmol/l}
{TT4: 4,5 — 10 µg/dl oder 57,9 — 128,7 nmol/l}
{FT4: 0,8 — 1,8 ng/dl oder 10 — 23 pmol/l}
{TT3: 80 — 180 ng/dl oder 1,23 — 2,77 nmol/l}
{FT3: 2,3 — 4,2 pg/ml oder 3,5 — 6,5 µmol/l}

{Übertragungsbeiwerte:}
  alphaR := 0.4;               {/l			Verdünnungsfaktor für TRH bei TRH-Test, wie alphaS}
  alphaS := 0.4;               {/l			bei V0 = 2.5l (Peptidhormon, das sich nur im peripheren Plasma verteilt)}
  betaS := 2.3e-4;             {/s		bei HWZ von ca. 50 Min. [Li et al. 1995, Odell et al. 1967]}
  alphaS2 := 2.6e5;            {/l			bei V0 = 3,8µl (errechnet u. a. nach Edelman 1959)}
  betaS2 := 140;               {/s		Kinetik primär unbekannt, Analogieschluß aus typ. Rezeptorpotentialen, entspricht HWZ von 5 ms}
  dH := 47e-9;                 {mol/l		[Le Dafniet et al. 1994]: 47 nmol/l}
  dR := 0.1e-9;                {mol/l		[Vybok et al. 1994, Lazar et al., 1990]}
  GR := 1;                     {mol/s	auf 1 normiert, da Rückkoppelungsstärke durch LS quantifiziert}
  SS := 100;                   {l/mU		errechnet aus Werten von [Kakita et al. 1984] mit Lineweaver-Burk-Regression}
  DS := 50;                    {mU/l		errechnet aus Werten von [Kakita et al. 1984] mit Lineweaver-Burk-Regression}
  GT := 1.5e-12 * 2.25;        {mol/s	2.25-facher Wert der mittleren Produktionsrate [Li et al. 1995]}
  alphaT := 0.1;               {/l			bei V0 = 10 l [Greenspan 1997]}
  betaT := 1.1e-6;             {/s		bei HWZ von 7 Tagen [Greenspan 1997]}
  dT := 2.75;                  {mU/l		[gemittelt nach Dumont & Vassart 1995]}
  GD1 := 2.8e-8;               {mol/sec	aus Referenzwerten ermittelt}
  GD2 := 4.3e-15;              {mol/sec	aus hypophysärem T3-Gehalt errechnet}
  alpha31 := 2.6e-2;           {/l			bei V0 = 38 l [Greenspan 1997]}
  beta31 := 8e-6;              {/s		bei HWZ von 24h [Greenspan 1997]}
  alpha32 := 1.3e5;            {/l			aus V0 = 7,6 µl errechnet}
  beta32 := 8.3e-4;            {/s		bei HWZ von 15 Minuten [Oppenheimer et al. 1967}
  kM1 := 0.5e-6;               {mol/l		[Greenspan 1997]}
  kM2 := 1e-9;                 {mol/l		[Visser et al. 1983]}
  k30 := 2e9;                  {l/mol		[Li et al. 1995]}
  k31 := 2e9;                  {l/mol		geschätzt}
  k41 := 2e10;                 {l/mol		[Li et al. 1995]}
  k42 := 2e8;                  {l/mol		[Li et al. 1995]}
  k3 := GD2 * alpha32 / (beta32 * (1 + k31 * IBS)); {Zur Errechnung von LS aus klinischen Daten}
  G3 := k3 * GR / (k3 + dR);   {dto.: Verstärkungsfaktor für T3z }
  AC1 := 500;                  {dto.: typische ungebremste TSH-Konzentration}
  MI := AC1 / 0.001;           {dto.: Inhibitions-Stärke }
  LS := MI / G3;               {Ausgleich von GR; LS hängt dann nur von emp. Daten ab; Näherungswert: 1.68 e6 l/mol}

  SetDerivedVariables;

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
  tpt11 := t121 / ln(2);       {Verzögerungskonstante 1. Ordnung für TRH}
  tpt12 := t122 / ln(2);       {Verzögerungskonstante 1. Ordnung für TSH}
  tpt13 := t123 / ln(2);       {Verzögerungskonstante 1. Ordnung für T4}
  tpt14 := t124 / ln(2);       {Verzögerungskonstante 1. Ordnung für T3z}
  tpt15 := t125 / ln(2);       {Verzögerungskonstante 1. Ordnung für T3p}
  tpt16 := t126 / ln(2);       {Verzögerungskonstante 1. Ordnung für TSHz}
  Tt1 := 1800;                 {Totzeit für TRH}
  Tt2 := 120;                  {Totzeit für TSH}
  Tt22 := 3240;                {Totzeit für Ultrashort-Feedback-Wirkung [abgeleitet von Greenspan 1997]}
  Tt3 := 300;                  {Totzeit für T4}
  Tt4 := 3600;                 {Totzeit für T3z}
  nt2 := trunc(Tt2 / delt);    {'trunc' rundet immer ab, 'round' rundet zur nächsten ganzen Zahl}
  nt22 := trunc(Tt22 / delt);
  nt3 := trunc(Tt3 / delt);
  nt4 := trunc(Tt4 / delt);

 end;

procedure TSimulationThread.Pause;
begin
 Notice.Hide;
 haltsim := true;
end;

procedure TSimulationThread.Restart;
begin
 if not simready then Notice.ShowOnTop;
 haltsim := false;
end;

procedure TSimulationThread.Execute; {Main simulation thread}
var
  theContents: tResultContent;
  j: integer;
  circadianControl: real;
  nmaxString, iString: string;
begin
  simready := false;
  nmaxString := IntToStr(nmax);
  while i <= nmax do
    begin
      iString := IntToStr(i);
      if not haltsim then
       begin
         gResultMatrix[i-1, i_pos] := i;
         gResultMatrix[i-1, t_pos] := t;
         gResultMatrix[i-1, TRH_pos] := TRH / UTRH;
         gResultMatrix[i-1, pTSH_pos] := TSHz;
         gResultMatrix[i-1, TSH_pos] := TSH;
         gResultMatrix[i-1, TT4_pos] := T4 / UFT4;
         gResultMatrix[i-1, FT4_pos] := FT4 / UFT4;
         gResultMatrix[i-1, TT3_pos] := T3p / UFT3;
         gResultMatrix[i-1, FT3_pos] := FT3 / UFT3;
         gResultMatrix[i-1, cT3_pos] := T3z / UFT3;
         theContents[i_pos] := iString;
         theContents[t_pos] := stunden(t);
         theContents[TRH_pos] := FloatToStr(gResultMatrix[i-1, TRH_pos]* gParameterFactor[TRH_pos]); {TRH}
         theContents[pTSH_pos] := FloatToStr(gResultMatrix[i-1, pTSH_pos] * gParameterFactor[pTSH_pos]); {portal TSH}
         theContents[TSH_pos] := FloatToStr(gResultMatrix[i-1, TSH_pos] * gParameterFactor[TSH_pos]); {serum TSH}
         theContents[TT4_pos] := FloatToStr(gResultMatrix[i-1, TT4_pos] * gParameterFactor[TT4_pos]); {T4}
         theContents[FT4_pos] := FloatToStr(gResultMatrix[i-1, FT4_pos] * gParameterFactor[FT4_pos]); {FT4}
         theContents[TT3_pos] := FloatToStr(gResultMatrix[i-1, TT3_pos] * gParameterFactor[TT3_pos]); {T3p}
         theContents[FT3_pos] := FloatToStr(gResultMatrix[i-1, FT3_pos] * gParameterFactor[FT3_pos]); {FT3}
         theContents[cT3_pos] := FloatToStr(gResultMatrix[i-1, cT3_pos] * gParameterFactor[cT3_pos]); {T3z}
         SimCS.Enter;
         try
           writeTableCells(SimThyrLogWindow.ValuesGrid, theContents);
           if i mod 30 = 0 then
             SetStatusBarPanel0(iString, nmaxString);
           if nmax > 0 then
             SimThyrLogWindow.ProgressBar1.Position := trunc(100 * i / nmax)
           else
             SimThyrLogWindow.ProgressBar1.Position := 100;
           t := t + delt;
{Hypothalamus:}
           f := 1 / 86400; {Frequenz des circadianen TRH-Rhythmus}
           omega := 2 * pi * f; {Kreisfrequenz}
           chi := 2 * pi / 24 * 5;
           circadianControl := cos(omega * t - chi);
           TRHi := TRH1 + 1500 * circadianControl * UTRH;
           TRH := TRHi + TRHe;
           TRH := TRH * getgauss(0.5); {Rauscheinfluß}
{Pituitary:}
           T3n := T3z / (1 + k31 * IBS);
           T3R := GR * T3n / (dR + T3n);
           dTSH := gH * TRH / ((dH + TRH) * (1 + LS * T3R) * (1 + SS * TSHz / (DS + TSHz)));
           {nur Ausschüttungsrate, kein Abbau}
				{Näherungswert im Äquifinium: TSHz := (alphaS2 / betaS2) * dTSH;}
           vpt10 := alphaS2 / betaS2;
           pt1(vpt10, tpt16, x6, dTSH, TSHz);
           TSHz := pt0(xt22, nt22, TSHz);
        {TSHz := TSHz * getgauss(0.2); {Rauscheinfluß}}
           vpt10 := alphaS / betaS;
           pt1(vpt10, tpt12, x2, dTSH, TSH);
				{Näherungswert im Äquifinium: TSH := alphaS * dTSH / betaS;}
           TSH := pt0(xt2, nt2, TSH);
{Thyroid:}
           dT4 := GT * TSH / (dT + TSH);
           vpt10 := alphaT / betaT;
           pt1(vpt10, tpt13, x3, dT4, T4);
				{Näherungswert im Äquifinium: T4 := alphaT * dT4 / betaT;}
           T4 := pt0(xt3, nt3, T4);
           FT4 := T4 / (1 + k41 * TBG + k42 * TBPA);
{5'-Dejodinase II (central):}
           dT3z := GD2 * FT4 / (kM2 + FT4);
           vpt10 := alpha32 / beta32;
           pt1(vpt10, tpt14, x4, dT3z, T3z);
				{Näherungswert im Äquifinium: T3z := alpha32 * dT3z / beta32;}
           T3z := pt0(xt4, nt4, T3z);
{5'-Dejodinase I (peripheral):}
           dT3p := GD1 * FT4 / (kM1 + FT4);
           vpt10 := alpha31 / beta31;
           pt1(vpt10, tpt15, x5, dT3p, T3p);
				{Näherungswert im Äquifinium: T3p := alpha31 * dT3p / beta31;}
           FT3 := T3p / (1 + k30 * TBG);
         finally
           SimCS.Leave;
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
    end;
  simready := true;
  DrawPlot(false);
  SimThyrLogWindow.Caption := LOG_TITLE;
  SetStatusBarPanel0(iString, nmaxString);
  SimThyrLogWindow.ProgressBar1.Position := 0;
end;

procedure simulate;
var
  theContents: tResultContent;
begin
  SetStatusBarPanel0('   0:', IntToStr(nmax));
  graphready := false;
  ValuesPlot.Caption := WAIT_TITLE;
  SimThyrLogWindow.Caption := WAIT_TITLE;
  SetLength(gResultMatrix, nmax, RES_MAX_COLS);
  Notice.ShowOnTop;
  haltsim := false;
  SimCS := TCriticalSection.Create;
  SimThread := TSimulationThread.Create(false);
end;

procedure InitSimulation;  {sets initial conditions and calculates fixpoints}
begin
  i2 := 1;

{predicted values:}
  TRH1 := TRH0;
  k1 := gH * alphaS / betaS;
  k11 := alphaS / betaS;
  k2 := GT * alphaT / betaT;
  k21 := k2;
  k22 := k21 / (1 + k41 * TBG + k42 * TBPA);
    		{k3 := GD2 * alpha32 / beta32;}
  k5 := GD1 * alpha31 / beta31;
  k51 := k5 / (1 + k30 * TBG);
  k6 := 1 / (2 * (1 + SS) * alphaS2);
  k61 := k6 * k11;
  k7 := dH + TRH1;
  k8 := alphaS2 * gH * TRH1 / (dH + TRH1);
  k9 := DS * betaS2;
    		{G3 := k3 * GR / (k3 + dR);}
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
  if previewflag and (i <= 1) then
   begin                                     {use calculated equilibrium values}
    TSH := TSH1;
    FT4 := FT41;
    T4 := FT4 * (1 + k41 * TBG + k42 * TBPA);
    FT3 := FT31;
    T3p := FT3 * (1 + k30 * TBG);
    T3z := T3z1;
    T3R := T3R1;
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

  ShowPredictedValues;
  GridRows := nmax + 1;
  SimThyrLogWindow.ValuesGrid.RowCount := GridRows;
  ValuesPlot.Show;
  SimThyrLogWindow.ShowOnTop;
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

