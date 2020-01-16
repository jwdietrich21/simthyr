unit SimThyrTypes;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.2 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2020 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2020 }

{ This unit provides types and global variables for other SimThyr units }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms, Graphics, TAGraph, UnitConverter;

type
 TableCell = TPoint;
 Str3 = string[3];
 Str13 = string[13];
 Str255 = string[255];
 tResultMatrix = array of array of real;      {matrix with simulated values}
 tmode = (integerMode, realMode);
 tBParameter = (iItem, tItem, TRHItem, pTSHItem, TSHItem, TT4Item, FT4Item,
   TT3Item, FT3Item, cT3Item);
 tSParameter = (NullItem, GD1Item, GD2Item, KM1Item, KM2Item, GTItem, DTItem,
   GHItem, DHItem, SSItem, DSItem, GRItem, DRItem, LSItem, betaSItem,
   betaS2Item, betaTItem, beta31Item, beta32Item, TBGItem, TBPAItem);
 tEquilibrium = record
   TRHs, TRH1, TSH1, TSH2, TSH3, FT41, FT42, FT43: real;
   T3z1, T3z2, T3z3, T3n1, T3n2, T3n3, FT31, FT32, FT33: real;
   T41, T42, T43, T31, T32, T33: real;
   TSHz1, TSHz2, TSHz3, T3R1, T3R2, T3R3: real;
 end;
 tModel = record
   Equilibrium: tEquilibrium;
   Name: AnsiString; // MIRIAM 1
   Reference: AnsiString; // MIRIAM 2
   Species: AnsiString;
   Creators: AnsiString; // MIRIAM 3
   Created: TDateTime; // MIRIAM 4
   LastModified: TDateTime; // MIRIAM 4
   Terms: AnsiString; // MIRIAM 5
   Code: AnsiString; // MIASE
   Comments: AnsiString; // MIASE
 end;
 TPlotOptions = record
   titleString: AnsiString;
   titleColor: TColor;
   fontname: string;
   fontsize: integer;
 end;

const

 kNUL = char(0);           {Special ASCII characters}
 kENTER = char(3);
 kTAB = char(9);
 kLF = char(10);
 kRETURN = char(13);
 kESCAPE = char(27);
 kPERIOD = '.';
 kSPACE = ' ';
 kSLASH = '/';
 kCOLON = ':';
 kSEMICOLON = ';';
 kOMIT = '•';
 kCRLF = #13#10;

 RES_MAX_COLS = 10;       {number of columns of table in log window}
 RES_BLANK_ROWS = 26;     {number of blank rows for empty window}

 DEC_POINT = '.';
 DEC_COMMA = ',';

 TEST_ITEM = TRHItem;     // 2

 i_pos = integer(iItem);               {positions of simulated variables in}
 t_pos = integer(tItem);               {several arrays and in result matrix}
 TRH_pos = integer(TRHItem);
 pTSH_pos = integer(pTSHItem);
 TSH_pos = integer(TSHItem);
 TT4_pos = integer(TT4Item);
 FT4_pos = integer(FT4Item);
 TT3_pos = integer(TT3Item);
 FT3_pos = integer(FT3Item);
 cT3_pos = integer(cT3Item);

 T4_MOLAR_MASS = 776.87; {molar mass of T4}
 T3_MOLAR_MASS = 650.97; {molar mass of T3}

 UTRH = 2.76E-12; {Conversion factor ng/l -> mol/l (TRH; MW=362) [Jackson 1987]}
 UTSH = 1e-3;     {Dummy conversion factor for TSH}

 SECONDS_PER_MINUTE = 60;
 SECONDS_PER_HOUR = SECONDS_PER_MINUTE * 60;
 SECONDS_PER_DAY = SECONDS_PER_HOUR * 24;
 SECONDS_PER_WEEK = SECONDS_PER_DAY * 7;

 TWS_RESOLUTION = 91; {Resolution for two-way sensitivity analysis}

 ISO_8601_DATE_FORMAT = 'YYYY-MM-DD"T"hh:nn:ss'; {Date/time format in XML representation}

type
 tResultContent = array[0..RES_MAX_COLS-1] of Str255;

var
 nmax_old, tmax, tt, gridrows: integer;
 nmax: longint;
 T4UnitFactor, T3UnitFactor: array[0..MAXFACTORS - 1] of real;
 gLabel: array[1..3] of Str255;
 gMessage: array[1..3] of Str255;
 gStartup, gPigMode: boolean;
 delt: real;
 testflag, tbgflag, signalflag, previewflag, noiseflag, circadianflag: boolean;
 haltsim, runcommand, simready, splashflag, showSettingsAtStartup: boolean;
 G3, t1, x1, x2, x3, x4, x5, x6, xe, ya, t: real;
 a, b, c, d, e, xd, a1, b1, c1, d1, r1, s1, p1, q1, u, v, u1, u2, Det, y1, y2, y3: real;
 dTRH, TRH, TRHi, TRHe, dTSH, TSH, TSHz, dT4, T4, FT4: real;
 dT3z, T3z, T3n, T3R, dT3p, T3p, FT3, Albumin, TBG, TBPA, IBS: real;
 alphaR, betaR, gH, alphaS, betaS, alphaS2, betaS2, kDH, dH, LS, SS, dS, GT, alphaT, betaT, dT: real;
 GD1, GD2, GR, alpha31, beta31, alpha32, beta32, kM1, kM2, dR, D3, AC1, AC2, MI, MH: real;
 Tt1, Tt2, Tt22, Tt3, Tt4, tpt11, tpt12, tpt13, tpt14, tpt15, tpt16, vpt10: real;
 t121, t122, t123, t124, t125, t126: real;
 k1, k2, k21, k22, k3, k30, k31, k41, k42, k5, k51, k7, dTSH1, dTSH2: real;
 phi, chi, omega, f, dTSH3, gH0: real;
 k6, k11, k8, k9, k61: real;
 gResultMatrix: tResultMatrix;
 gParameterLabel: array[0..RES_MAX_COLS - 1] of Str255;
 gParameterUnit: array[0..RES_MAX_COLS -1] of String;
 gParameterFactor: array[0..RES_MAX_COLS -1] of real;
 gDefaultColors: array[0..RES_MAX_COLS -1] of TColor;
 gSelectedChart: tChart;
 tmax_text, tmax_unit, i1_text, i1_unit: str255;
 gNumberFormat, gDateTimeFormat: String;
 gLastActiveCustomForm: TForm;
 gActiveModel: TModel;

implementation

end.

