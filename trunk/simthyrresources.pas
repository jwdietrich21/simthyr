unit SimThyrResources;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2016 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2016 }

{ This unit provides URLs and global strings for other SimThyr units }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SimThyrTypes;

const
  BASE_URL = 'http://simthyr.medical-cybernetics.de';
  SIMTHYR_GLOBAL_ID = 'net.sf.simthyr';
  HELP_URL = 'http://simthyr.sourceforge.net/manual.html';

  LOG_TITLE = 'Log Window';
  PLOT_TITLE = 'Plots of simulated parameters';
  WAIT_TITLE = 'Simulation running...';

  IMPLEMENTATION_MESSAGE = 'This function is not implemented in this version of SimThyr.';
  FILE_VERSION_MESSAGE = 'This scenario file has a file version that is not supported by SimThyr';
  DEBUG_VERSION_MESSAGE = 'This is a pre-release version of SimThyr, which is intended for testing purposes only. Usage is on your own risk.';
  FORMAT_MESSAGE = 'Please check your input.';
  SAVE_ERROR_MESSAGE = 'Error saving the file';
  PREFERENCES_READ_ERROR_MESSAGE = 'Preferences could not be read. Please check access rights of your user or home folder';
  PREFERENCES_SAVE_ERROR_MESSAGE = 'The preferences could not be saved permanently, however, they are valid for this session';
  INSUFFICIENT_MEMORY_MESSAGE = 'This machine has not enough free memory to perform this function';

  EXAMPLE_STRING = 'Example: ';
  NEGATIVE_VALUES_HINT = 'Negative values represent virtual solutions delivered by equilibrium polynomials.';
  DEVIATION_STRING = 'Small deviations between predictions and simulated values may result from temporal dynamics including circadian and ultradian rhythms, transition effects and rounding.';

  CHANGE_IN_STRING = 'Change in ';
  DEPENDEND_VAR_STRING = 'dependent variable';
  DECREASE_STRING = 'decrease';
  INCREASE_STRING = 'increase';

  ISOKLINE_1_STRING = 'Concentration 1';
  ISOKLINE_2_STRING = 'Concentration 2';
  A_AXIS_STRING = ' a';
  B_AXIS_STRING = ' b';

  I_LABEL = 'i';
  TIME_LABEL = 'Time';
  TRH_LABEL = 'Portal TRH';
  P_TSH_LABEL = 'Pituitary TSH';
  S_TSH_LABEL = 'Serum TSH';
  TT4_LABEL = 'Serum total T4';
  FT4_LABEL = 'Serum free T4';
  TT3_LABEL = 'Serum total T3';
  FT3_LABEL = 'Serum free T3';
  C_T3_LABEL = 'Central T3';

  kError101 = 'Runtime error: Negative parameter(s)';
  kError102 = 'Runtime error: Parameter(s) out of range';
  kError103 = 'Runtime error: min > max';
  kError104 = 'Runtime error: max = 0';
  kError105 = 'Runtime error: max = NaN';

  {$IFDEF UNIX}
  kHeapTraceFile = '~/heaptrace.trc';
  {$ELSE}
  kHeapTraceFile = 'heaptrace.trc';
  {$ENDIF}

implementation

end.

