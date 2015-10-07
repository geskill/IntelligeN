unit uZelluloidDe;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // Utils
  uHTMLUtils,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TZelluloidDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    procedure Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase); override; safecall;
  end;

implementation

procedure ALUTF8ExtractHTMLText(HtmlContent: string; LstExtractedResourceText: Tstrings);
begin
  LstExtractedResourceText.Text := Trim(HTML2Text(HtmlContent));
end;

function TZelluloidDe.GetName;
begin
  Result := 'Zelluloid.de';
end;

function TZelluloidDe.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cMovie];
  Result := Word(_TemplateTypeIDs);
end;

function TZelluloidDe.GetAvailableControlIDs;
var
  _ComponentIDs: TControlIDs;
begin
  _ComponentIDs := [cPicture, cGenre, cDescription];
  Result := LongWord(_ComponentIDs);
end;

function TZelluloidDe.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TZelluloidDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

procedure TZelluloidDe.Exec;
const
  zurl = 'http://www.zelluloid.de';
  zsurl = zurl + '/suche';
var
  _ComponentIDs: TControlIDs;
  _Title: WideString;
  _Count: Integer;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    s: string;
    des: TStringList;
  begin
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<a href="#" pic="(.*?)"';

          if Exec(InputString) then
          begin
            repeat
              AControlController.FindControl(cPicture).AddProposedValue(GetName, zurl + Match[1]);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

    if (AControlController.FindControl(cGenre) <> nil) and (cGenre in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'az\.php3\?g=(\d*?)">(.*?)<';

          if Exec(InputString) then
          begin
            repeat
              s := Match[2];
              if Pos(',', s) > 0 then
                s := copy(s, 1, Pos(',', s) - 1);
              AControlController.FindControl(cGenre).AddProposedValue(GetName, s);
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;

    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<div class="bigtext">(.*?)<\/div>';

          if Exec(InputString) then
          begin
            repeat
              des := TStringList.Create;
              try
                ALUTF8ExtractHTMLText(Match[1], des);
                AControlController.FindControl(cDescription).AddProposedValue(GetName, des.Text);
              finally
                des.Free;
              end;
            until not ExecNext;
          end;
        finally
          Free;
        end;
    end;
  end;

var
  HTTPRequest: IHTTPRequest;

  RequestID1, RequestID2: Double;

  ResponeStr: string;
begin
  LongWord(_ComponentIDs) := AControlIDs;
  _Title := AControlController.FindControl(cTitle).Value;
  _Count := 0;

  HTTPRequest := THTTPRequest.Create(zsurl + '/index.php3?qstring=' + HTTPEncode(_Title));
  HTTPRequest.Referer := zurl;

  RequestID1 := HTTPManager.Get(HTTPRequest, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponeStr := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  if not(Pos('Suchbegriff', ResponeStr) = 0) then
  begin
    with TRegExpr.Create do
      try
        ModifierG := False;
        InputString := ResponeStr;
        Expression := '<B><A HREF="(.*?)"';

        if Exec(InputString) then
        begin
          repeat
            RequestID2 := HTTPManager.Get(zsurl + '/' + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

            repeat
              sleep(50);
            until HTTPManager.HasResult(RequestID2);

            ResponeStr := HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode;

            deep_search(ResponeStr);
            Inc(_Count);
          until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
        end;
      finally
        Free;
      end;
  end
  else
    deep_search(ResponeStr);
end;

end.
