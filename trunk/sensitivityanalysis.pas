unit Sensitivityanalysis;
{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ This unit implements sensitivity analysis }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  TAGraph, SimThyrTypes, SimThyrServices, SimThyrPrediction;

type

  { TSensitivityAnalysisForm }

  TSensitivityAnalysisForm = class(TForm)
    Chart1: TChart;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  SensitivityAnalysisForm: TSensitivityAnalysisForm;

implementation

initialization
  {$I sensitivityanalysis.lrs}

end.

