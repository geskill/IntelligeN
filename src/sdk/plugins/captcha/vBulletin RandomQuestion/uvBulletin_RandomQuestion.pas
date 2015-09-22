unit uvBulletin_RandomQuestion;

interface

uses
  // Delphi
  Windows, SysUtils, XMLDoc, XMLIntf, ActiveX, Variants,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCAPTCHAClass, uPlugInConst;

type
  TvBulletin_RandomQuestion = class(TCAPTCHAPlugIn)
  public
    function GetName: WideString; override;
    function Exec: WordBool; override;
  end;

  TCAPTCHAXmlReader = class
  public
    class function GetCAPTCHAValue(AFileName, ACAPTCHA: string): string;
  end;

implementation

function GetModulePath: string;
var
  QueryRes: TMemoryBasicInformation;
  LBuffer: string;
begin
  VirtualQuery(@GetModulePath, QueryRes, SizeOf(QueryRes));
  SetLength(LBuffer, MAX_PATH);
  SetLength(LBuffer, GetModuleFileName(Cardinal(QueryRes.AllocationBase), PChar(LBuffer), Length(LBuffer)));
  Result := LBuffer;
end;

{ TvBulletin_RandomQuestion }

function TvBulletin_RandomQuestion.GetName: WideString;
begin
  Result := 'vBulletin RandomQuestion';
end;

function TvBulletin_RandomQuestion.Exec: WordBool;
var
  XMLResult: string;
begin
  Result := False;

  if (CAPTCHAType = ctText) and SameText('vBulletin RandomQuestion', CAPTCHAName) then
  begin
    XMLResult := TCAPTCHAXmlReader.GetCAPTCHAValue(ExtractFilePath(GetModulePath) + ChangeFileExt(ExtractFileName(GetModulePath), '.xml'), CAPTCHA);

    if not SameText('', XMLResult) then
    begin
      CAPTCHAResult := XMLResult;
      Result := True;
    end;

  end;
end;

{ TCAPTCHAXmlReader }

class function TCAPTCHAXmlReader.GetCAPTCHAValue(AFileName, ACAPTCHA: string): string;
var
  XMLDoc: IXMLDocument;
  CAPTCHACount: Integer;
begin
  Result := '';

  OleInitialize(nil);
  try
    XMLDoc := NewXMLDocument;

    with XMLDoc do
    begin
      LoadFromFile(AFileName);
      Active := True;
    end;

    with XMLDoc.DocumentElement do
      if HasChildNodes then
        for CAPTCHACount := 0 to ChildNodes.Count - 1 do
          with ChildNodes.Nodes[CAPTCHACount] do
          begin
            if SameText(ACAPTCHA, VarToStrDef(ChildNodes.Nodes['question'].NodeValue, '')) then
            begin
              Result := VarToStrDef(ChildNodes.Nodes['answer'].NodeValue, '');
              break;
            end;
          end;

  finally
    XMLDoc := nil;
    OleUninitialize;
  end;
end;

end.
