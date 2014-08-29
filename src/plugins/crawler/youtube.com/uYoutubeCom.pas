unit uYoutubeCom;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  TYoutubeCom = class(TCrawlerPlugIn)
  protected
    function SimpleGETRequest(AURL: string; AIdHTTPHelper: TIdHTTPHelper): string;
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override; stdcall;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override; stdcall;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override; stdcall;
    function GetLimitDefaultValue: Integer; override; stdcall;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override; stdcall;
  end;

implementation

{ TYoutubeCom }

function TYoutubeCom.SimpleGETRequest;
var
  ReplyData: TStringStream;
begin
  ReplyData := TStringStream.Create('', CP_UTF8);
  try
    AIdHTTPHelper.Get(AURL, ReplyData);

    Result := ReplyData.DataString;
  finally
    ReplyData.Free;
  end;
end;

function TYoutubeCom.GetName;
begin
  Result := 'Youtube.com';
end;

function TYoutubeCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)] - [cXXX];
  Result := Word(_TemplateTypeIDs);
end;

function TYoutubeCom.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cTrailer];

  Result := LongWord(_ComponentIDs);
end;

function TYoutubeCom.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TYoutubeCom.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TYoutubeCom.Exec;
const
  website = 'https://www.youtube.com/';
var
  _Title: string;
  _ComponentIDs: TComponentIDs;
  _Count: Integer;

  IdHTTPHelper: TIdHTTPHelper;
  ResponeStr: string;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  _Title := AComponentController.FindControl(cTitle).Value;
  _Count := 0;

  if (AComponentController.FindControl(cTrailer) <> nil) and (cTrailer in _ComponentIDs) then
  begin
    IdHTTPHelper := TIdHTTPHelper.Create(Self);
    try
      ResponeStr := SimpleGETRequest(website + 'results?search_type=videos&search_query=' + HTTPEncode(_Title + ' hd trailer'), IdHTTPHelper);

      with TRegExpr.Create do
      begin
        try
          ModifierS := False;
          InputString := ExtractTextBetween(ResponeStr, '"item-section">', '</ol>');
          Expression := '<h3(.*?)href="\/(.*?)"(.*?)dir="ltr">(.*?)<\/';

          if Exec(InputString) then
          begin
            repeat
              (AComponentController.FindControl(cTrailer) as ITrailer).AddValue(website + Match[2], HTML2Text(Match[4]), GetName);
              Inc(_Count);
            until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
          end;
        finally
          Free;
        end;
      end;
    finally
      IdHTTPHelper.Free;
    end;
  end;
end;

end.
