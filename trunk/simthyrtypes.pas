unit SimThyrTypes;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.2.5 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit provides types and global variables for other SimThyr units }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, Forms, Graphics, TAGraph;

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
 kOMIT = '•';
 kCRLF = #13#10;

 RES_MAX_COLS = 10;       {number of columns of table in log window}
 RES_BLANK_ROWS = 26;     {number of blank rows for empty window}

 DEC_POINT = '.';
 DEC_COMMA = ',';

 i_pos = 0;               {positions of simulated variables in}
 t_pos = 1;               {several arrays and in result matrix}
 TRH_pos = 2;
 pTSH_pos = 3;
 TSH_pos = 4;
 TT4_pos = 5;
 FT4_pos = 6;
 TT3_pos = 7;
 FT3_pos = 8;
 cT3_pos = 9;

 UFT4 = 1.28E-11; {Conversion factor ng/dl -> mol/l (T4)}
 UFT3 = 1.54E-12; {Conversion factor pg/ml -> mol/l (T3)}
 UTRH = 2.76E-12; {Conversion factor ng/l -> mol/l (TRH; MW=362) [Jackson 1987]}
 UTSH = 1e-3;     {Dummy conversion factor for TSH}

 MAXFACTORS = 10; {for measurement units and preferences}

 BASE_URL = 'http://simthyr.medical-cybernetics.de';
 SIMTHYR_GLOBAL_ID = 'net.sf.simthyr';
 HELP_URL = 'http://simthyr.sourceforge.net/manual.html';

 LOG_TITLE = 'Log Window';
 PLOT_TITLE = 'Plots of simulated parameters';
 WAIT_TITLE = 'Simulation running...';

 IMPLEMENTATION_MESSAGE = 'This function is not implemented in this version of SimThyr.';
 FORMAT_MESSAGE = 'Please check your input.';
 SAVE_ERROR_MESSAGE = 'Error saving the file';
 PREFERENCES_READ_ERROR_MESSAGE = 'Preferences could not be read. Please check access rights of your user or home folder';
 PREFERENCES_SAVE_ERROR_MESSAGE = 'The preferences could not be saved permanently, however, they are valid for this session';

 EXAMPLE_STRING = 'Example: ';
 NEGATIVE_VALUES_HINT = 'Negative values represent virtual solutions delivered by equilibrium polynomials.';
 DEVIATION_STRING = 'Small deviations may result from transition effects and rounding.';

 CHANGE_IN_STRING = 'Change in ';
 DEPENDEND_VAR_STRING = 'dependent variable';
 DECREASE_STRING = 'decrease';
 INCREASE_STRING = 'increase';

type
 TableCell = TPoint;
 Str3 = string[3];
 Str13 = string[13];
 Str255 = string[255];
 tResultMatrix = array of array of real;               {matrix with simulated values}
 tResultContent = array[0..RES_MAX_COLS-1] of Str255;
 tmode = (integerMode, realMode);

var
 nmax_old, tmax, tt, gridrows: integer;
 nmax: longint;
 PrefixLabel, T4UnitLabel, T3UnitLabel: array[0..MAXFACTORS - 1] of Str3;
 PrefixFactor, T4UnitFactor, T3UnitFactor: array[0..MAXFACTORS - 1] of real;
 gLabel: array[1..3] of Str255;
 gMessage: array[1..3] of Str255;
 gStartup: boolean;
 delt: real;
 testflag, tbgflag, signalflag, previewflag, noiseflag: boolean;
 haltsim, runcommand, simready, splashflag, showSettingsAtStartup: boolean;
 TRHs, TRH1, TSH1, TSH2, TSH3, FT41, FT42, FT43: real;
 T3z1, T3z2, T3z3, T3n1, T3n2, T3n3, FT31, FT32, FT33: real;
 T41, T42, T43, T31, T32, T33: real;
 TSHz1, TSHz2, TSHz3, T3R1, T3R2, T3R3: real;
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

implementation

end.

