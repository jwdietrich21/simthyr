unit DIFSupport;

{ SimThyr Project }
{ (c) J. W. Dietrich, 1994 - 2013 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2013 }

{ This unit provides support for DIF file handling }

{ Source code released under the BSD License }

{$mode objfpc}

interface

uses
  Classes, SysUtils;

const
  kCRLF = #13#10;
  kQUOT = #34;

type
  tHeader = Record
    version: Integer;
    title: String;
    vectors, tuples: Integer;
  end;
  TDIFDocument = class
    content: TStrings;
    Header: tHeader;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetHead(Identifier: String);
      procedure NewTuple;
      procedure AppendCell(Value: String);
      procedure AppendCell(Value: Real);
      procedure AppendCell(Value: Boolean);
  end;

procedure WriteDIFFile(Doc: TDIFDocument; path: String);

var
  gNumVectors: integer;

implementation

constructor TDIFDocument.Create;
begin
  inherited Create;
  content := TStringList.Create;
  content.TextLineBreakStyle := tlbsCRLF;
end;

destructor TDIFDocument.Destroy;
begin
  content.Free;
  Inherited destroy;
end;

procedure TDIFDocument.SetHead(Identifier: String);
begin
  with Header do
  begin
  title := Identifier;
  version := 1;
  vectors := 0;
  tuples := 0;
  end;
end;

procedure TDIFDocument.NewTuple;
begin
  gNumVectors := 0;
  inc(header.tuples);
  content.Append('-1,0');
  content.Append('BOT');
end;

procedure TDIFDocument.AppendCell(Value: String);
begin
  inc(gNumVectors);
  if gNumVectors > header.vectors then
    header.vectors := gNumVectors;
  content.Append('1,0');
  content.Append(kQUOT + Value + kQUOT);
end;

procedure TDIFDocument.AppendCell(Value: Real);
begin
  AppendCell(FloatToStr(Value));
end;

procedure TDIFDocument.AppendCell(Value: Boolean);
begin
  if value = true then
    AppendCell('TRUE')
  else
    AppendCell('FALSE');
end;

procedure WriteDIFFile(Doc: TDIFDocument; path: String);
var
  headerChunk, dataChunk, endChunk: String;
begin
  headerChunk := 'TABLE' + kCRLF + '0,' + IntToStr(Doc.Header.version) + kCRLF;
  headerChunk := headerChunk + kQUOT + Doc.Header.title + kQUOT + kCRLF;
  headerChunk := headerChunk + 'VECTORS' + kCRLF + '0,' + IntToStr(Doc.Header.vectors) + kCRLF + kQUOT + kQUOT + kCRLF;
  headerChunk := headerChunk + 'TUPLES' + kCRLF + '0,' + IntToStr(Doc.Header.tuples) + kCRLF + kQUOT + kQUOT + kCRLF;
  dataChunk := 'DATA' + kCRLF + '0,0' + kCRLF + kQUOT + kQUOT;
  endChunk := '-1,0' + kCRLF + 'EOD';
  Doc.content.Insert(0, headerChunk + dataChunk);
  Doc.content.Append(endChunk);
  try
    Doc.content.SaveToFile(path);
  finally
  end;
end;

end.
