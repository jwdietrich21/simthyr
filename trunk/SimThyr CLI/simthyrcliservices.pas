unit SimThyrCLIServices;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 5.0.0 (Mirage) }

{ (c) J. W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ This unit provides some global functions for use by other units }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}


interface

uses
  Classes, SysUtils, StrUtils, CRT, UnitConverter, SimThyrTypes, SimThyrResources;
  
const
  kLength = 8;
  kScale = 160;
  minWidth = 4;
  
var
  gLength: integer;
  
procedure SetParameterUnits;
procedure SetConversionFactors;
procedure WriteTableHeader;
procedure WriteTableLine(theLine: tResultContent);
procedure ShowErrorMessage(theMessage: String);
procedure ShowFileError;
procedure ShowVersionError;
procedure ShowFormatError;

implementation

procedure SetParameterUnits;
begin
  gParameterUnit[t_pos] := 'day h:m:s';
  gParameterUnit[TRH_pos] := 'ng/L';
  gParameterUnit[pTSH_pos] := 'mIU/L';
  gParameterUnit[TSH_pos] := gParameterUnit[pTSH_pos];
  gParameterUnit[TT4_pos] := 'nmol/L';
  gParameterUnit[FT4_pos] := 'pmol/L';
  gParameterUnit[TT3_pos] := 'nmol/L';
  gParameterUnit[FT3_pos] := 'pmol/L';
  gParameterUnit[cT3_pos] := gParameterUnit[FT3_pos];
end;

procedure SetConversionFactors;
begin
  gParameterFactor[TRH_pos] := 1;
  gParameterFactor[pTSH_pos] := 1;
  gParameterFactor[TSH_pos] := gParameterFactor[pTSH_pos];
  gParameterFactor[TT4_pos] := ConvertedValue(1, T4_MOLAR_MASS, 'ng/dl', gParameterUnit[TT4_pos]);
  gParameterFactor[FT4_pos] := ConvertedValue(1, T4_MOLAR_MASS, 'ng/dl', gParameterUnit[FT4_pos]);
  gParameterFactor[TT3_pos] := ConvertedValue(1, T3_MOLAR_MASS, 'pg/ml', gParameterUnit[TT3_pos]);
  gParameterFactor[FT3_pos] := ConvertedValue(1, T3_MOLAR_MASS, 'pg/ml', gParameterUnit[FT3_pos]);
  gParameterFactor[cT3_pos] := ConvertedValue(1, T3_MOLAR_MASS, 'pg/ml', gParameterUnit[cT3_pos]);
end;

procedure WriteTableHeader;
var
  termWidth: integer;
begin
  {$IFDEF Windows}
  termWidth := WindMaxX;
  {$ELSE}
  termWidth := ScreenWidth;
  {$ENDIF}
  if termWidth > kScale then
    gLength := kLength
  else
    gLength := minWidth + termWidth div kScale * kLength;
  writeln;
  write(AddChar(' ', 'i', gLength) + kTab + AddChar(' ', 't', gLength) + kTab);
  write(AddChar(' ', 'TRH', gLength) + kTab + AddChar(' ', 'pTSH', gLength) + kTab);
  write(AddChar(' ', 'TSH', gLength) + kTab + AddChar(' ', 'TT4', gLength) + kTab);
  write(AddChar(' ', 'FT4', gLength) + kTab + AddChar(' ', 'TT3', gLength) + kTab);
  write(AddChar(' ', 'FT3', gLength) + kTab + AddChar(' ', 'cT3', gLength) + kTab);
  writeln();
  write(AddChar(' ', ' ', gLength) + kTab);
  write(AddChar(' ', gParameterUnit[t_pos], gLength) + kTab);
  write(AddChar(' ', gParameterUnit[TRH_pos], gLength) + kTab);
  write(AddChar(' ', gParameterUnit[pTSH_pos], gLength) + kTab);
  write(AddChar(' ', gParameterUnit[TSH_pos], gLength) + kTab);
  write(AddChar(' ', gParameterUnit[TT4_pos], gLength) + kTab);
  write(AddChar(' ', gParameterUnit[FT4_pos], gLength) + kTab);
  write(AddChar(' ', gParameterUnit[TT3_pos], gLength) + kTab);
  write(AddChar(' ', gParameterUnit[FT3_pos], gLength) + kTab);
  write(AddChar(' ', gParameterUnit[cT3_pos], gLength) + kTab);
  writeln();
end;

procedure WriteTableLine(theLine: tResultContent);
  {Writes contents of table line to the console (SimThyr CLI only)}
var
  i: integer;
begin
  for i := 0 to length(theLine) - 1 do
  begin
    if i = 2 then
      write(AddChar(' ', theLine[i], 10))
    else
      write(AddChar(' ', theLine[i], gLength));
    write(kTab);
  end;
  writeln;
end;

procedure ShowErrorMessage(theMessage: String);
begin
  writeln(theMessage);
end;

procedure ShowFileError;
begin
  writeln(FILE_FORMAT_ERROR_MESSAGE);
end;

procedure ShowVersionError;
begin
  writeln(FILE_VERSION_MESSAGE);
end;

procedure ShowFormatError;
begin
  writeln(FORMAT_MESSAGE);
end;


end.
