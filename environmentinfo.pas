unit EnvironmentInfo;

{ EnvironmentInfo }
{ Provides information on hardware and software environment}

{ (c) J. W. Dietrich, 1994 - 2019 }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, LCLVersion, InterfaceBase, versiontypes,
  versionresource, LCLPlatformDef;

type
  TPlatformInfo = record
    CPU, OS: string;
  end;

function DateOfCompiling: TDateTime;
function DateOfCompilingAsString: String;
function CompilerVersion: String;
function LCLVersion: String;
function PlatformInfo: TPlatformInfo;
function CurrentWidgetSet: String;

Const
  GTK_WIDGETSET          = 'GTK widget set';
  GTK2_WIDGETSET         = 'GTK 2 widget set';
  GTK3_WIDGETSET         = 'GTK 2 widget set';
  WIN_WIDGETSET          = 'Win32/Win64 widget set';
  WINCE_WIDGETSET        = 'WinCE widget set';
  CARBON_WIDGETSET       = 'Carbon widget set';
  COCOA_WIDGETSET        = 'Cocoa widget set';
  QT_WIDGETSET           = 'QT widget set';
  QT5_WIDGETSET          = 'QT 5 widget set';
  fpGUI_WIDGETSET        = 'fpGUI widget set';
  noGUI_WIDGETSET        = 'no GUI widget set';
  CUSTROMDRAWN_WIDGETSET = 'CustomDrawn widget set';
  MUI_WIDGETSET          = 'MUI widget set';
  OTHER_WIDGETSET        = 'Other gui';

implementation

function DateOfCompiling: TDateTime;
var
  tempString, dateString, timeString: String;
  theDate, theTime: TDateTime;
  delims: TSysCharSet;
  theFormat: TFormatSettings;
begin
  theFormat := DefaultFormatSettings;
  theFormat.DateSeparator := '/';
  theFormat.ShortDateFormat := 'yyyy/mm/dd';
  theFormat.TimeSeparator := ':';
  theFormat.ShortTimeFormat := 'hh:nn:ss';
  tempString := DateOfCompilingAsString;
  delims := [',', ' '];
  dateString := ExtractWord(0, tempString, delims);
  timeString := ExtractWord(1, tempString, delims);
  if not TryStrToDate(dateString, theDate, theFormat) then
    theDate := 0;
  if not TryStrToTime(timeString, theTime, theFormat) then
    thetime := 0;
  result := ComposeDateTime(theDate, theTime);
end;

function DateOfCompilingAsString: String;
var
  theDate, theTime: String;
begin
  theDate := {$I %DATE%};
  theTime := {$I %TIME%};
  Result := theDate + ', ' + theTime;
end;

function CompilerVersion: String;
begin
  result := 'FPC ' + {$I %FPCVERSION%};
end;

function LCLVersion: String;
begin
  result := 'LCL ' + lcl_version;
end;

function PlatformInfo: TPlatformInfo;
begin
  result.CPU := {$I %FPCTARGETCPU%};
  result.OS := {$I %FPCTARGETOS%};
end;

function CurrentWidgetSet: String;
begin
  case WidgetSet.LCLPlatform of
    lpGtk:         result := GTK_WIDGETSET;
    lpGtk2:        result := GTK2_WIDGETSET;
    lpGtk3:        result := GTK3_WIDGETSET;
    lpWin32:       result := WIN_WIDGETSET;
    lpWinCE:       result := WINCE_WIDGETSET;
    lpCarbon:      result := CARBON_WIDGETSET;
    lpCocoa:       result := COCOA_WIDGETSET;
    lpQT:          result := QT_WIDGETSET;
    lpQt5:         result := QT5_WIDGETSET;
    lpfpGUI:       result := fpGUI_WIDGETSET;
    lpNoGUI:       result := noGUI_WIDGETSET;
    lpCustomDrawn: result := CUSTROMDRAWN_WIDGETSET;
    lpMUI:         result := MUI_WIDGETSET
  otherwise
    result := OTHER_WIDGETSET;
  end;
end;


end.

