unit ScenarioHandler;

{ SimThyr Project }
{ A numerical simulator of thyrotropic feedback control }

{ Version 4.0.0 (Merlion) }

{ (c) J. W. Dietrich, 1994 - 2016 }
{ (c) Ludwig Maximilian University of Munich 1995 - 2002 }
{ (c) Ruhr University of Bochum 2005 - 2016 }

{ This unit reads and writes scenarios as XML files }

{ Source code released under the BSD License }
{ See http://simthyr.sourceforge.net for details }

{$mode objfpc}

interface

uses
  Classes, SysUtils, DateUtils, DOM, XMLRead, XMLWrite, Forms,
  SimThyrTypes, SimThyrServices, MiriamForm;

procedure ReadScenario(theFileName: string; var modelVersion: Str13);
procedure SaveScenario(theFileName: string);

implementation

procedure ReadScenario(theFileName: string; var modelVersion: Str13);
{reads a simulation scenario}
var
  i: integer;
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  oldSep: Char;
  standardDate: TDateTime;
begin
  if FileExists(theFileName) then
  oldSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := kPERIOD;
  begin
    try
      standardDate := EncodeDateTime(1904, 01, 01, 00, 00, 00, 00);
      Doc := TXMLDocument.Create();
      ReadXMLFile(Doc, theFileName);
      RootNode := Doc.DocumentElement;
      if RootNode.HasAttributes and (RootNode.Attributes.Length > 0) then
        for i := 0 to RootNode.Attributes.Length - 1 do
          with RootNode.Attributes[i] do
          begin
            if NodeName = 'modelversion' then
              modelVersion := UTF8Encode(NodeValue);
          end;
      RootNode := Doc.DocumentElement.FindNode('MIRIAM');
      if assigned(RootNode) then
      begin
        gActiveModel.Name := NodeContent(RootNode, 'Name');
        gActiveModel.Reference := NodeContent(RootNode, 'Reference');
        gActiveModel.Species := NodeContent(RootNode, 'Species');
        gActiveModel.Creators := NodeContent(RootNode, 'Creators');
        if not TryXMLDateTime2DateTime(NodeContent(RootNode, 'Created'), gActiveModel.Created) then
          gActiveModel.Created := standardDate;
        if not TryXMLDateTime2DateTime(NodeContent(RootNode, 'LastModified'), gActiveModel.LastModified) then
          gActiveModel.LastModified := standardDate;
        gActiveModel.Terms := NodeContent(RootNode, 'Terms');
      end;
      if (modelVersion = '') or (LeftStr(modelVersion, 3) = '10.') then
      begin
        RootNode := Doc.DocumentElement.FindNode('strucpars');
        VarFromNode(RootNode, 'alphaR', AlphaR);
        VarFromNode(RootNode, 'betaR', BetaR);
        VarFromNode(RootNode, 'GR', GR);
        VarFromNode(RootNode, 'dR', dR);
        VarFromNode(RootNode, 'alphaS', AlphaS);
        VarFromNode(RootNode, 'betaS', BetaS);
        VarFromNode(RootNode, 'alphaS2', AlphaS2);
        VarFromNode(RootNode, 'betaS2', BetaS2);
        VarFromNode(RootNode, 'GH', GH);
        VarFromNode(RootNode, 'dH', dH);
        VarFromNode(RootNode, 'LS', LS);
        VarFromNode(RootNode, 'SS', SS);
        VarFromNode(RootNode, 'DS', DS);
        VarFromNode(RootNode, 'alphaT', AlphaT);
        VarFromNode(RootNode, 'betaT', BetaT);
        VarFromNode(RootNode, 'GT', GT);
        VarFromNode(RootNode, 'dT', dT);
        VarFromNode(RootNode, 'alpha31', alpha31);
        VarFromNode(RootNode, 'beta31', beta31);
        VarFromNode(RootNode, 'GD1', GD1);
        VarFromNode(RootNode, 'KM1', KM1);
        VarFromNode(RootNode, 'alpha32', alpha32);
        VarFromNode(RootNode, 'beta32', beta32);
        VarFromNode(RootNode, 'GD2', GD2);
        VarFromNode(RootNode, 'KM2', KM2);
        VarFromNode(RootNode, 'K30', K30);
        VarFromNode(RootNode, 'K31', K31);
        VarFromNode(RootNode, 'K41', K41);
        VarFromNode(RootNode, 'K42', K42);
        VarFromNode(RootNode, 'Tau0R', TT1);
        VarFromNode(RootNode, 'Tau0S', TT2);
        VarFromNode(RootNode, 'Tau0S2', TT22);
        VarFromNode(RootNode, 'Tau0T', TT3);
        VarFromNode(RootNode, 'Tau03z', TT4);
      end;
    finally
      Doc.Free;
    end;
  end;
  if AnnotationForm.Visible then
    AnnotationForm.ShowAnnotation;
  DefaultFormatSettings.DecimalSeparator := oldSep;
end;

procedure SaveScenario(theFileName: string); {saves scenario as XML file}
var
  oldSep: Char;
  Doc: TXMLDocument;
  RootNode, ElementNode: TDOMNode;
  theDate: AnsiString;
begin
  oldSep := DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator := kPERIOD;
  try
    Doc := TXMLDocument.Create;

    RootNode := Doc.CreateElement('scenario');
    TDOMElement(RootNode).SetAttribute('modelversion', '10.0');
    Doc.Appendchild(RootNode);
    RootNode := Doc.DocumentElement;

    ElementNode := Doc.CreateElement('MIRIAM');
    ElementNode.AppendChild(SimpleNode(Doc, 'Name', gActiveModel.Name));
    ElementNode.AppendChild(SimpleNode(Doc, 'Reference', gActiveModel.Reference));
    ElementNode.AppendChild(SimpleNode(Doc, 'Species', gActiveModel.Species));
    ElementNode.AppendChild(SimpleNode(Doc, 'Creators', gActiveModel.Creators));
    DateTimeToString(theDate, ISO_8601_DATE_FORMAT, gActiveModel.Created);
    ElementNode.AppendChild(SimpleNode(Doc, 'Created', theDate));
    DateTimeToString(theDate, ISO_8601_DATE_FORMAT, gActiveModel.LastModified);
    ElementNode.AppendChild(SimpleNode(Doc, 'LastModified', theDate));
    ElementNode.AppendChild(SimpleNode(Doc, 'Terms', gActiveModel.Terms));
    RootNode.AppendChild(ElementNode);

    ElementNode := Doc.CreateElement('strucpars');

    ElementNode.AppendChild(SimpleNode(Doc, 'alphaR', FloatToStr(alphaR)));
    ElementNode.AppendChild(SimpleNode(Doc, 'betaR', FloatToStr(betaR)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GR', FloatToStr(GR)));
    ElementNode.AppendChild(SimpleNode(Doc, 'dR', FloatToStr(dR)));
    ElementNode.AppendChild(SimpleNode(Doc, 'alphaS', FloatToStr(alphaS)));
    ElementNode.AppendChild(SimpleNode(Doc, 'betaS', FloatToStr(betaS)));
    ElementNode.AppendChild(SimpleNode(Doc, 'alphaS2', FloatToStr(alphaS2)));
    ElementNode.AppendChild(SimpleNode(Doc, 'betaS2', FloatToStr(betaS2)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GH', FloatToStr(GH)));
    ElementNode.AppendChild(SimpleNode(Doc, 'dH', FloatToStr(dH)));
    ElementNode.AppendChild(SimpleNode(Doc, 'LS', FloatToStr(LS)));
    ElementNode.AppendChild(SimpleNode(Doc, 'SS', FloatToStr(SS)));
    ElementNode.AppendChild(SimpleNode(Doc, 'DS', FloatToStr(DS)));
    ElementNode.AppendChild(SimpleNode(Doc, 'alphaT', FloatToStr(alphaT)));
    ElementNode.AppendChild(SimpleNode(Doc, 'betaT', FloatToStr(betaT)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GT', FloatToStr(GT)));
    ElementNode.AppendChild(SimpleNode(Doc, 'dT', FloatToStr(dT)));
    ElementNode.AppendChild(SimpleNode(Doc, 'alpha31', FloatToStr(alpha31)));
    ElementNode.AppendChild(SimpleNode(Doc, 'beta31', FloatToStr(beta31)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GD1', FloatToStr(GD1)));
    ElementNode.AppendChild(SimpleNode(Doc, 'KM1', FloatToStr(KM1)));
    ElementNode.AppendChild(SimpleNode(Doc, 'alpha32', FloatToStr(alpha32)));
    ElementNode.AppendChild(SimpleNode(Doc, 'beta32', FloatToStr(beta32)));
    ElementNode.AppendChild(SimpleNode(Doc, 'GD2', FloatToStr(GD2)));
    ElementNode.AppendChild(SimpleNode(Doc, 'KM2', FloatToStr(KM2)));
    ElementNode.AppendChild(SimpleNode(Doc, 'K30', FloatToStr(K30)));
    ElementNode.AppendChild(SimpleNode(Doc, 'K31', FloatToStr(K31)));
    ElementNode.AppendChild(SimpleNode(Doc, 'K41', FloatToStr(K41)));
    ElementNode.AppendChild(SimpleNode(Doc, 'K42', FloatToStr(K42)));
    ElementNode.AppendChild(SimpleNode(Doc, 'Tau0R', FloatToStr(TT1)));
    ElementNode.AppendChild(SimpleNode(Doc, 'Tau0S', FloatToStr(TT2)));
    ElementNode.AppendChild(SimpleNode(Doc, 'Tau0S2', FloatToStr(TT22)));
    ElementNode.AppendChild(SimpleNode(Doc, 'Tau0T', FloatToStr(TT3)));
    ElementNode.AppendChild(SimpleNode(Doc, 'Tau03z', FloatToStr(TT4)));

    RootNode.AppendChild(ElementNode);

    WriteXMLFile(Doc, theFileName);
  finally
    Doc.Free;
  end;
  DefaultFormatSettings.DecimalSeparator := oldSep;
end;

end.
