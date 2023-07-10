program SimThyr;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.6 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ This is the main unit of SimThyr CLI }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$define UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  getopts, CRT, SysUtils, StrUtils, Types,
  SimThyrTypes, SimThyrBaseServices, SimThyrCLIServices,
  ScenarioHandler, Predictor, Simulator, ListParameters;

type
  TModes = (simul, list, error);

var
  c: char;
  optionindex: longint;
  shortOptions, longOption, argument, scenarioversion: string;
  loads: TStringDynArray;
  longOptions: array[1..4] of TOption;
  mode: TModes;
  termWidth: integer;

{$IFDEF GUI}
{$R *.res}
{$ENDIF}

begin
  {$IFDEF Windows}
  termWidth := WindMaxX;
  {$ELSE}
  termWidth := ScreenWidth;
  {$ENDIF}
  if termWidth > kScale then
    gNumberFormat := STANDARD_NUM_FORMAT
  else
    gNumberFormat := SHORT_NUM_FORMAT;
  gDateTimeFormat := STANDARD_TIME_FORMAT;

  gStartup := true;
  previewflag := true;            // default values
  noiseflag := true;
  mode := simul;
  tmax := 24;
  tmax_unit := 'h';

  shortOptions := 'pvn:i:ed';
  with longOptions[1] do
  begin
    name := 'scenario';
    has_arg := 1;
    flag := nil;
    value := #0;
  end;
  with longOptions[2] do
  begin
    name := 'duration';
    has_arg := 1;
    flag := nil;
    value := #0;
   end;
  with longOptions[3] do
  begin
    name := 'load';
    has_arg := 1;
    flag := nil;
    value := #0;
  end;
  with longOptions[4] do
  begin
    name := '';
    has_arg := 0;
    flag := nil;
    value := #0;
  end;
  c := #0;

  SetParameterUnits;
  SetStandardFactors;
  InitSimulationControl;
  SetBaseVariables;
  StandardValues;

  writeln('SimThyr CLI');
  writeln('Version ', SimThyr_version);

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
        longOption := longOptions[optionindex].name;
        if longOptions[optionindex].has_arg > 0 then
          begin
            argument := optarg;
          end
        else
          argument := '';
        case longOption of
          'scenario':
          begin
            writeln('Using scenario: ', argument);
            ReadScenario(argument, scenarioversion);  {XML file}
            if (scenarioversion <> '') and (scenarioversion <> '10.0') then 
              begin
                ShowVersionError;
                mode := error;
              end;
          end;
          'duration':
          begin
            write('duration: ');
            tmax_text := LeftStr(argument, length(argument) - 1);
            tmax_unit := RightStr(argument, 1);
            writeln(tmax_text, ' ', tmax_unit);
            tmax := StrToIntDef(tmax_text, 0);
          end;
          'load':
          begin
            write('with load: ');
            loads := SplitString(argument, '@');
            write(loads[0], ' at: ');
            i1_text := LeftStr(loads[1], length(loads[1]) - 1);
            i1_unit := RightStr(loads[1], 1);            
            writeln(i1_text, ' ', i1_unit);
            lt := StrToIntDef(i1_text, 0);
            gLoadInfo.startTime   := nmax_old + cycles(lt, i1_unit);
            case loads[0] of
            'TRH':
              gLoadInfo.kind := lkTRH;
            'TBG':
              gLoadInfo.kind := lkTBG;
            end;
          end;
          otherwise
          begin
            write ('Long option: ', longOption);
            if argument <> '' then
              writeln (' with value: ', argument)
            else
              writeln;
          end;
        end;
      end;
      { Short options (preceded by "-"): }
      'p': begin
             mode := list;
             writeln('Parameters:');
             listpars;
           end;
      'v': begin
             mode := list;
             listvers;
           end;
      'd': begin
             mode := list;
             listdiag;
           end;
      'e': begin
             mode := list;
             listpred;
           end;
      'n': if optarg = '-' then
             noiseflag := false
           else if optarg = '+' then
             noiseflag := true;
      'i': if optarg = '-' then
             previewflag := false
           else if optarg = '+' then
             previewflag := true;
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

  if mode = simul then
  begin
    writeln;
    writeln('Duration of simulated time not set.');
    writeln('Running for 24 hours per default settings.');
    writeln;

    nmax := cycles(tmax, tmax_unit);
    write('Iterations: ');
    writeln(nmax);
    writeln;
    InitSimulation;
    writeln;
  end;
  
end.
