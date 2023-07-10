unit SimThyrResources;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.5 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

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

  MIASE_URL = 'http://co.mbine.org/standards/miase';
  MIASE_SIMTHYR_STANDARD_CODE = 'Model of Thyroid Homeostasis for use with SimThyr, as available from ' +  BASE_URL;
  MIRIAM_URL = 'http://www.ebi.ac.uk/miriam/main/';
  MIBBI_URL = 'http://biosharing.org/collection/MIBBI';

  kSTANDARD_MODEL_NAME = 'Model 10';
  kSTANDARD_MODEL_REFERENCE = 'Dietrich, J. W.; Tesche, A.; Pickardt, C. R.; Mitzdorf, U. (2004), Thyrotropic Feedback Control: Evidence For An Additional Ultrashort Feedback Loop From Fractal Analysis., Cybernetics and Systems 35 (4), 315-331. http://dx.doi.org/10.1080/01969720490443354';
  kSTANDARD_MODEL_SPECIES = 'Homo sapiens (NCBI Taxonomy ID 9606)';
  kSTANDARD_MODEL_CREATORS = 'Dietrich, Tesche, Pickardt and Mitzdorf';
  kSTANDARD_MODEL_CREATED_Y = 2001;
  kSTANDARD_MODEL_CREATED_M = 8;
  kSTANDARD_MODEL_CREATED_D = 23;
  kSTANDARD_MODEL_CREATED_H = 13;
  kSTANDARD_MODEL_CREATED_N = 7;
  kSTANDARD_MODEL_CREATED_S = 5;
  kSTANDARD_MODEL_MODIFIED_Y = 2015;
  kSTANDARD_MODEL_MODIFIED_M = 12;
  kSTANDARD_MODEL_MODIFIED_D = 21;
  kSTANDARD_MODEL_MODIFIED_H = 9;
  kSTANDARD_MODEL_MODIFIED_N = 57;
  kSTANDARD_MODEL_MODIFIED_S = 13;
  kSTANDARD_MODEL_TERMS = 'Creative Commons Attributions License 4.0 (CC BY 4.0)';

  LOG_TITLE = 'Log Window';
  PLOT_TITLE = 'Plots of simulated parameters';
  WAIT_TITLE = 'Simulation running...';

  IMPLEMENTATION_MESSAGE = 'Function not implemented in this version of SimThyr';
  FILE_VERSION_MESSAGE = 'This scenario file has a file version that is not supported by SimThyr.';
  FILE_FORMAT_ERROR_MESSAGE = 'This is not a valid XML file that can be used by SimThyr.';
  DEBUG_VERSION_MESSAGE = 'This is a pre-release version of SimThyr, which is intended for testing purposes only. Usage is on your own risk.';
  FORMAT_MESSAGE = 'Please check your input.';
  URL_STATUS_MESSAGE = 'Server status code: ';
  SAVE_ERROR_MESSAGE = 'Error saving the file';
  PREFERENCES_READ_ERROR_MESSAGE = 'Preferences could not be read. Please check access rights of your user or home folder';
  PREFERENCES_SAVE_ERROR_MESSAGE = 'The preferences could not be saved permanently, however, they are valid for this session';
  INSUFFICIENT_MEMORY_MESSAGE = 'This machine has not enough free memory to perform this function';

  URL_TITLE = 'Read URL';
  URL_QUERY = 'Please enter the URL of a Scenario:';

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
  kHeapTraceFile = '~/simthyr_heaptrace.trc';
  {$ELSE}
  kHeapTraceFile = 'simthyr_heaptrace.trc';
  {$ENDIF}

implementation

end.

