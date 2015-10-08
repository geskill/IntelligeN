unit uCdLexikonDe;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uBaseInterface,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInCrawlerClass, uPlugInHTTPClasses;

type
  TCdLexikonDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetAvailableTypeIDs: Integer; override; safecall;
    function GetAvailableControlIDs(const ATypeID: Integer): Integer; override; safecall;
    function GetControlIDDefaultValue(const ATypeID, AControlID: Integer): WordBool; override; safecall;
    function GetResultsLimitDefaultValue: Integer; override; safecall;

    function Exec(const ATypeID, AControlIDs, ALimit: Integer; const AControlController: IControlControllerBase): WordBool; override; safecall;
  end;

implementation

{ TCdLexikonDe }

function TCdLexikonDe.GetName;
begin
  Result := 'cd-lexikon.de';
end;

function TCdLexikonDe.GetAvailableTypeIDs;
var
  _TemplateTypeIDs: TTypeIDs;
begin
  _TemplateTypeIDs := [cAudio];
  Result := Word(_TemplateTypeIDs);
end;

function TCdLexikonDe.GetAvailableControlIDs;
var
  // _TemplateTypeID: TTypeID;
  _ComponentIDs: TControlIDs;
begin
  // _TemplateTypeID := TTypeID(ATypeID);

  _ComponentIDs := [cArtist, cPicture, cDescription];

  Result := LongWord(_ComponentIDs);
end;

function TCdLexikonDe.GetControlIDDefaultValue;
begin
  Result := True;
end;

function TCdLexikonDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

function TCdLexikonDe.Exec;
const
  website = 'http://cd-lexikon.de/';
var
  _Count: Integer;
  _Title: string;
  _ComponentIDs: TControlIDs;

  procedure deep_search(ASourceCode: string);
  var
    _Tracklist: string;
  begin
    if (AControlController.FindControl(cArtist) <> nil) and (cArtist in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := ASourceCode;
          Expression := 'Interpret: <td><td><font size=''2'' face=''Arial''>(.*?)<tr>';

          if Exec(InputString) then
            AControlController.FindControl(cArtist).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
    end;
    if (AControlController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
    begin
      with TRegExpr.Create do
        try
          InputString := ASourceCode;
          Expression := '<tr><td valign=top><img src=''(.*?)''';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, website + Match[1]);
        finally
          Free;
        end;
    end;
    if (AControlController.FindControl(cDescription) <> nil) and (cDescription in _ComponentIDs) then
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
            AControlController.FindControl(cDescription).AddProposedValue(GetName, _Tracklist);
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
  _Title := TrimLeft(Copy(AControlController.FindControl(cTitle).Value, Pos('-', AControlController.FindControl(cTitle).Value) + 1));
  LongWord(_ComponentIDs) := AControlIDs;

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
