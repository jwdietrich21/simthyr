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
  tHeader = Record
    version: Integer;
    Identifier: String;
    vectors, tuples: Integer;
  end;
  TDIFDocument = class
    content: TStringList;
    Header: tHeader;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetHead(Identifier: String);
      procedure AppendTuple(Value: String);
      procedure AppendTuple(Value: Real);
      procedure AppendTuple(Value: Boolean);
  end;

procedure WriteDIFFile(Doc: TDIFDocument; path: String);

implementation

constructor TDIFDocument.Create;
begin
  content.Create;
end;

destructor TDIFDocument.Destroy;
begin
  content.Destroy;
  Inherited destroy;
end;

procedure TDIFDocument.SetHead(Identifier: String);
begin
  Header.Identifier := Identifier;
end;

procedure TDIFDocument.AppendTuple(Value: String);
begin

end;

procedure TDIFDocument.AppendTuple(Value: Real);
begin

end;

procedure TDIFDocument.AppendTuple(Value: Boolean);
begin

end;

procedure WriteDIFFile(Doc: TDIFDocument; path: String);
begin

end;

end.
