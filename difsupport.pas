unit DIFSupport;

{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2012 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2012 }

{ This unit provides support for DIF file handling }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils;

type
  TDIFFile = class
    content: TStringList;
    procedure InsertHead;
    procedure InsertTuple;
  end;

implementation

procedure TDifFile.InsertHead;
begin

end;

procedure TDIFFIle.InsertTuple;
begin

end;

end.

