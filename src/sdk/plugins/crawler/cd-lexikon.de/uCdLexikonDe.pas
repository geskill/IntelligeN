unit uCdLexikonDe;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TCdLexikonDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): WordBool; override;
    function GetLimitDefaultValue: Integer; override;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override;
  end;

implementation

{ TCdLexikonDe }

function TCdLexikonDe.GetName;
begin
  Result := 'cd-lexikon.de';
end;

function TCdLexikonDe.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cAudio];
  Result := Word(_TemplateTypeIDs);
end;

function TCdLexikonDe.GetAvailableComponentIDs;
var
  // _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  // _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cArtist, cPicture, cDescription];

  Result := LongWord(_ComponentIDs);
end;

function TCdLexikonDe.GetComponentIDDefaultValue;
begin
  Result := True;
end;

function TCdLexikonDe.GetLimitDefaultValue;
begin
  Result := 5;
end;

procedure TCdLexikonDe.Exec;
const
  website = 'http://cd-lexikon.de/';
var
  _Count: Integer;
  _Title: string;
  _ComponentIDs: TComponentIDs;

  procedure deep_search(ASourceCode: string);
  var
    _Tracklist: string;
  begin
    if (AComponentController.FindControl(cArtist) <> nil) and (cArtist in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := ASourceCode;
          Expression := 'Interpret: <td><td><font size=''2'' face=''Arial''>(.*?)<tr>';

          if Exec(InputString) then
            AComponentController.FindControl(cArtist).AddValue(Match[1], GetName);
        finally
          Free;
        end;
    end;
    if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := ASourceCode;
          Expression := '<tr><td valign=top><img src=''(.*?)''';

          if Exec(InputString) then
            AComponentController.FindControl(cPicture).AddValue(website + Match[1], GetName);
        finally
          Free;
        end;
    end;
    if (AComponentController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
    begin
      _Tracklist := '';
      with TRegExpr.Create do
        try
          InputString := ASourceCode;
          Expression := '<font size=''2'' face=''Arial''>(&nbsp;&nbsp;|)(\d+)\. .*?<a href=''.*?''>(.*?)<\/a>';

          if Exec(InputString) then
          begin
            repeat
              _Tracklist := _Tracklist + Match[2] + '. ' + Match[3] + sLineBreak;
            until not ExecNext;
            AComponentController.FindControl(cDescription).AddValue(_Tracklist, GetName);
          end;
        finally
          Free;
        end;
    end;
  end;

var
  RequestID1, RequestID2: Double;

  ResponseStrSearchResult: string;
begin
  _Count := 0;
  _Title := TrimLeft(Copy(AComponentController.FindControl(cTitle).Value, Pos('-', AComponentController.FindControl(cTitle).Value) + 1));
  LongWord(_ComponentIDs) := AComponentIDs;

  RequestID1 := HTTPManager.Get(THTTPRequest.Create(website + '/suchen/albumsuche.php?r=0&q=' + HTTPEncode(_Title)), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID1);

  ResponseStrSearchResult := HTTPManager.GetResult(RequestID1).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrSearchResult;
      Expression := '<\/a><td><a href=''\.\.\/(.*?)'' title=''.*?''>.*?<\/a><td>';

      if Exec(InputString) then
      begin
        repeat

          RequestID2 := HTTPManager.Get(website + Match[1], RequestID1, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID2);

          deep_search(HTTPManager.GetResult(RequestID2).HTTPResult.SourceCode);

          Inc(_Count);
        until not(ExecNext and ((_Count < ALimit) or (ALimit = 0)));
      end;
    finally
      Free;
    end;

end;

end.
