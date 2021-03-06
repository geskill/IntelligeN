unit uvBulletin_RandomQuestion;

interface

uses
  // Delphi
  Windows, SysUtils, XMLDoc, XMLIntf, ActiveX, Variants,
  // Plugin system
  uPlugInCAPTCHAClass, uPlugInConst,
  // Utils
  uSystemUtils;

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
  LNeedToUninitialize: Boolean;
  XMLDoc: IXMLDocument;
  CAPTCHACount: Integer;
begin
  Result := '';

  LNeedToUninitialize := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
  try
    XMLDoc := NewXMLDocument;
    try
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
    end;
  finally
    if LNeedToUninitialize then
      CoUninitialize;
  end;
end;

end.
