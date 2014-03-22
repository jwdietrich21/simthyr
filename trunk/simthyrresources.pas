unit SimThyrResources;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 3.2.5 }

{ (c) J. W. Dietrich, 1994 - 2014 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2014 }

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

implementation

end.

