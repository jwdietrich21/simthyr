unit ListParameters;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.6 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ This unit lists parameters and predictions to the console }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}
{$R+}
{$define UseCThreads}
{$ASSERTIONS ON}

interface

uses
  SysUtils, StrUtils, CRT, 
  SimThyrTypes, SimThyrResources, SimThyrBaseServices, Predictor,
  UnitConverter;
  
const
  kPrecision = 13;
  kDigits = 2;
  {$IFDEF Windows}
  kLength = 17;
  {$ELSE}
  kLength = 21;
  {$ENDIF}

procedure listpars;
procedure listpred;
procedure listvers;
procedure listdiag;

implementation

 function formatted(value: real): string;
 begin
   result := AddChar(' ', FloatToStrF(value, ffGeneral, kPrecision, kDigits), kLength);
 end;

 procedure listpars;
 begin
   writeln();
   writeln('Parameters:');
   writeln();
   writeln('alphaR:  ', formatted(alphaR));
   writeln('alphaS:  ', formatted(alphaS));
   writeln('betaS:   ', formatted(betaS));
   writeln('alphaS2: ', formatted(alphaS2));
   writeln('betaS2:  ', formatted(betaS2));
   writeln('dH:      ', formatted(dH));
   writeln('dR:      ', formatted(dR));
   writeln('GR:      ', formatted(GR));
   writeln('SS:      ', formatted(SS));         
   writeln('DS:      ', formatted(DS));
   writeln('GT:      ', formatted(GT));
   writeln('alphaT:  ', formatted(alphaT));
   writeln('betaT:   ', formatted(betaT));
   writeln('dT:      ', formatted(dT));            
   writeln('GD1:     ', formatted(GD1));
   writeln('GD2:     ', formatted(GD2));
   writeln('alpha31: ', formatted(alpha31));
   writeln('beta31:  ', formatted(beta31));
   writeln('alpha32: ', formatted(alpha32));
   writeln('beta32:  ', formatted(beta32));
   writeln('kM1:     ', formatted(kM1));
   writeln('kM2:     ', formatted(kM2));
   writeln('k30:     ', formatted(k30));
   writeln('k31:     ', formatted(k31));
   writeln('k41:     ', formatted(k41));
   writeln('k42:     ', formatted(k42));
   writeln('k3:      ', formatted(k3));
   writeln('G3:      ', formatted(G3));
   writeln('AC1:     ', formatted(AC1));
   writeln('MI:      ', formatted(MI));
   writeln('LS:      ', formatted(LS));
   writeln('gH:      ', formatted(gH));

   writeln('t121:    ', formatted(t121));
   writeln('betaR:   ', formatted(betaR));
   writeln('t122:    ', formatted(t122));
   writeln('t123:    ', formatted(t123));
   writeln('t124:    ', formatted(t124));
   writeln('t125:    ', formatted(t125));
   writeln('t126:    ', formatted(t126));
   writeln('tpt11:   ', formatted(tpt11));
   writeln('tpt12:   ', formatted(tpt12));
   writeln('tpt13:   ', formatted(tpt13));
   writeln('tpt14:   ', formatted(tpt14));
   writeln('tpt15:   ', formatted(tpt15));
   writeln('tpt16:   ', formatted(tpt16));
   writeln('Tt1:     ', formatted(Tt1));
   writeln('Tt2:     ', formatted(Tt2));
   writeln('Tt22:    ', formatted(Tt22));
   writeln('Tt3:     ', formatted(Tt3));
   writeln('Tt4:     ', formatted(Tt4));
 end;
 
 procedure listpred;
 var
   cfTT4, cfFT4, cfTT3, cfFT3, cfcT3: real;
 begin
   writeln;
   writeln('Prediction:');
   PredictEquilibrium;
   cfTT4 := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l', gParameterUnit[TT4_pos]);
   cfFT4 := ConvertedValue(1, T4_MOLAR_MASS, 'mol/l', gParameterUnit[FT4_pos]);
   cfTT3 := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[TT3_pos]);
   cfFT3 := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[FT3_pos]);
   cfcT3 := ConvertedValue(1, T3_MOLAR_MASS, 'mol/l', gParameterUnit[cT3_pos]);
   writeln;
   writeln('Predicted concentrations:');
   write('TRH:     ', formatted(gActiveModel.Equilibrium.TRH1 / UTRH));
   writeln(' ', gParameterUnit[TRH_pos]);
   
   write('TSH:     ', formatted(gActiveModel.Equilibrium.TSH1), ', ');
   write(formatted(gActiveModel.Equilibrium.TSH2), ', ');
   write(formatted(gActiveModel.Equilibrium.TSH3));
   writeln(' ', gParameterUnit[TSH_pos]);
   
   write('TT4:     ', formatted(gActiveModel.Equilibrium.T41 * cfTT4));
   write(', ', formatted(gActiveModel.Equilibrium.T42 * cfTT4), ', ');
   write(formatted(gActiveModel.Equilibrium.T43 * cfTT4));
   writeln(' ', gParameterUnit[TT4_pos]);
   
   write('FT4:     ', formatted(gActiveModel.Equilibrium.FT41 * cfFT4));
   write(', ', formatted(gActiveModel.Equilibrium.FT42 * cfFT4), ', ');
   write(formatted(gActiveModel.Equilibrium.FT43 * cfFT4));
   writeln(' ', gParameterUnit[FT4_pos]);
   
   write('TT3:     ', formatted(gActiveModel.Equilibrium.T31 * cfTT3), ', ');
   write(formatted(gActiveModel.Equilibrium.T32 * cfTT3), ', ');
   write(formatted(gActiveModel.Equilibrium.T33 * cfTT3));
   writeln(' ', gParameterUnit[TT3_pos]);
    
   write('FT3:     ', formatted(gActiveModel.Equilibrium.FT31 * cfFT3), ', ');
   write(formatted(gActiveModel.Equilibrium.FT32 * cfFT3), ', ');
   write(formatted(gActiveModel.Equilibrium.FT33 * cfFT3));
   writeln(' ', gParameterUnit[FT3_pos]);
    
   write('cT3:     ', formatted(gActiveModel.Equilibrium.T3z1 * cfcT3), ', ');
   write(formatted(gActiveModel.Equilibrium.T3z2 * cfcT3), ', ');
   write(formatted(gActiveModel.Equilibrium.T3z3 * cfcT3));
   writeln(' ', gParameterUnit[CT3_pos]);
    
   writeln;
   writeln(NEGATIVE_VALUES_HINT);
   writeln(DEVIATION_STRING);
   writeln;
 end;
 
 procedure listvers;
 begin
   writeln('Built at: ', {$I %DATE%} + ' ' + {$I %TIME%});
   writeln('');
   writeln('(c) 1984-2022 J. W. Dietrich');
   writeln('');
   writeln('In Cooperation with:');
   writeln('Klinikum der LMU München');
   writeln('Universitaetsklinikum Ulm');
   writeln('Ruhr-Universitaet Bochum');
   writeln('Universitaetsklinikum Bergmannsheil');
   writeln('Katholisches Klinikum Bochum');
   writeln('');
   writeln('This software is made available with a BSD license.');
   writeln('');
   writeln('RRID_SCR_014351');
   writeln('');
end;

procedure listdiag;
begin
  writeln('');
  {$IFDEF Windows}
  writeln('Screen width: ', WindMaxX);
  writeln('Screen height: ', WindMaxY);
  {$ELSE}
  writeln('Screen width: ', ScreenWidth);
  writeln('Screen height: ', ScreenHeight);
  {$ENDIF}
  writeln('');
end;

end.

