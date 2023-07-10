unit SimThyrBaseServices;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.6 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2022 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2022 }

{ This unit provides some global functions for use by other units }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}{$R+}
{$IFDEF LCLCocoa}
  {$modeswitch objectivec1}
{$ENDIF}


interface

uses
  Classes, SysUtils, StrUtils, SimThyrTypes,
  SimThyrResources, UnitConverter, DIFSupport, DOM, DateUtils;
  
const
  iuSystemScript = -1;
  iuCurrentScript = -2;
  iuWordSelectTable = 0;
  iuWordWrapTable = 1;
  iuNumberPartsTable = 2;
  iuUnTokenTable = 3;
  iuWhiteSpaceList = 4;


function EncodeGreek(theString: string): string;
function DecodeGreek(theString: string): string;
function XMLDateTime2DateTime(const XMLDateTime: string): TDateTime;
function TryXMLDateTime2DateTime(const S: ShortString; out Value: TDateTime): boolean;
function NodeContent(theRoot: TDOMNode; Name: string): string;
procedure VarFromNode(theRoot: TDOMNode; Name: string; var theVar: real);
function SimpleNode(Doc: TXMLDocument; Name, Value: string): TDOMNode;
function AsTime(x: real): TDateTime;
function FormattedTime(x: real): Str255;
procedure SetStandardFactors;

implementation

function EncodeGreek(theString: string): string;
  {encodes greek mu letter as ASCII substitution sequence}
var
  theFlags: TReplaceFlags;
begin
  theFlags := [rfReplaceAll, rfIgnoreCase];
  Result := StringReplace(theString, #194#181, 'mc', theFlags);
end;

function DecodeGreek(theString: string): string;
  {decodes ASCII substitution sequence for greek mu letter}
begin
  Result := UTF8Decode(StringReplace(theString, 'mc', PrefixLabel[4],
    [rfReplaceAll, rfIgnoreCase]));
end;

function XMLDateTime2DateTime(const XMLDateTime: string): TDateTime;
  { adapted and expanded from a suggestion by Luiz Americo Pereira Camara }
var
  DateOnly, TimeOnly: string;
  theDate, theTime: TDateTime;
  TPos: integer;
begin
  TPos := Pos('T', XMLDateTime);
  if TPos <> 0 then
  begin
    DateOnly := Copy(XMLDateTime, 1, TPos - 1);
    TimeOnly := Copy(XMLDateTime, TPos + 1, Length(XMLDateTime));
    theDate := ScanDateTime(LeftStr(ISO_8601_DATE_FORMAT, 10), DateOnly);
    theTime := ScanDateTime(RightStr(ISO_8601_DATE_FORMAT, 8), TimeOnly);
    Result := theDate + theTime;
  end
  else
  begin
    DateOnly := XMLDateTime;
    Result := ScanDateTime(LeftStr(ISO_8601_DATE_FORMAT, 10), DateOnly);
  end;
end;

function TryXMLDateTime2DateTime(const S: ShortString; out Value: TDateTime): boolean;
begin
  Result := True;
  try
    Value := XMLDateTime2DateTime(s);
  except
    on EConvertError do
      Result := False
  end;
end;

function NodeContent(theRoot: TDOMNode; Name: string): string;
  {supports XML routines, gets the contents of a node in a file}
var
  theNode: TDOMNode;
  theText: string;
begin
  if assigned(theRoot) then
  begin
    Result := 'NA';
    theNode := theRoot.FindNode(Name);
    if assigned(theNode) then
    begin
      try
        theText := UTF8Encode(theNode.TextContent);
        if theText <> '' then
          Result := theText;
      except
        Result := 'NA';
      end;
      theNode.Destroy;
    end
    else
      Result := 'NA';
  end;
end;

procedure VarFromNode(theRoot: TDOMNode; Name: string; var theVar: real);
{supports XML routines}
var
  oldSep: char;
  theString: string;
begin
  oldSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := kPERIOD;
  theString := NodeContent(theRoot, Name);
  if theString <> 'NA' then
    theVar := StrToFloat(theString);
  DefaultFormatSettings.DecimalSeparator := oldSep;
end;

function SimpleNode(Doc: TXMLDocument; Name, Value: string): TDOMNode;
  {supports XML routines, creates an XML node from the contents of a string}
var
  ItemNode, TextNode: TDOMNode;
begin
  ItemNode := Doc.CreateElement(Name);
  TextNode := Doc.CreateTextNode(UTF8Decode(Value));
  ItemNode.AppendChild(TextNode);
  Result := ItemNode;
end;

function AsTime(x: real): TDateTime;
  {Converts second values to TDateTime representation}
var
  r: longint;
  y, d: word;
  theTime, theDate: TDateTime;
begin
  y := 1900;
  d := 1;
  theDate := EncodeDateDay(y, d);
  r := trunc(x);
  theTime := IncSecond(theDate, r);
  AsTime := theTime;
end;

function FormattedTime(x: real): Str255;   {Converts second values to a formatted time}
begin
  FormattedTime := FormatDateTime(gDateTimeFormat, AsTime(x));
end;

procedure SetStandardFactors;
var
  j: integer;
begin
for j := i_pos to cT3_pos do
    gParameterFactor[j] := 1; // default values, to be changed later in program run
end;

end.
