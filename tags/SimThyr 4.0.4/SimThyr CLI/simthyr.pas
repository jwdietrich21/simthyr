program SimThyr;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.4 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2021 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2021 }

{ This is the main unit of SimThyr CLI }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$define UseCThreads}

uses 
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  getopts, SimThyrTypes, SimThyrBaseServices, Simulator;

var
  c: char;
  optionindex: longint;
  shortOptions: string;
  longOptions: array[1..3] of TOption;

begin
  gNumberFormat := STANDARD_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;
      
  shortOptions := 'bt';
  with longOptions[1] do
  begin
    name := 'scenario';
    has_arg := 1;
    flag := nil;
    value := #0;
  end;
  with longOptions[2] do
  begin
    name := 'log';
    has_arg := 1;
    flag := nil;
    value := #0;
   end;
  with longOptions[3] do
  begin
    name := '';
    has_arg := 0;
    flag := nil;
    value := #0;
  end;
  c := #0;

  writeln('SimThyr CLI');
  writeln('Version 4.0.4');

  repeat
    { read long and short options: }
    c := getlongopts(shortOptions, @longOptions[1], optionindex);
    case c of
      '1', '2', '3', '4', '5', '6', '7', '8', '9':
      begin
        writeln('Got optind: ', c);
      end;
      #0: 
      { Long options (preceded by "--"): }
      begin
        write ('Long option: ', longOptions[optionindex].name);
        if longOptions[optionindex].has_arg > 0 then
          writeln (' with value: ', optarg)
        else
          writeln;
      end;
      { Short options (preceded by "-"): }
      't': writeln('Option t.');
      'b': writeln('Option b.');
    end;
  until c = endofoptions;
 
  if optind <= paramcount then
  { parameters that are not options }
  begin
    write ('Non options: ');
    while optind <= paramcount do
      begin
        write(paramstr(optind), ' ');
        inc(optind)
      end;
    writeln;
  end;

  writeln();
  writeln('Duration of simulated time not set.');
  writeln('Running for 24 hours per default settings.');
  writeln();

  SetStandardFactors;
  InitSimulationControl;
  nmax := cycles(24, 'h');
  write('Iterations: ');
  writeln(nmax);
  writeln();
  SetBaseVariables;
  StandardValues;
  InitSimulation;
  
end.
