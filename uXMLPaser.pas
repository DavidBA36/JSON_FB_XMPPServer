unit uXMLPaser;

interface

uses system.Classes, system.RegularExpressions, system.Generics.Collections, system.SysUtils, system.Variants, OXmlPDOM;

procedure ParseClientQuery(AXmlQuery: string);

implementation

procedure ParseClientQuery(AXmlQuery: string);
var
  wFixedXml: String;
  wNode: PxmlNode;
  i: integer;
  wXml: OXmlPDOM.IXMLDocument;
  a: string;
begin

end;

end.
