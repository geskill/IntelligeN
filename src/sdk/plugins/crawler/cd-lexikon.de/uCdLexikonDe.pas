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
  protected { . }
  const
    WEBSITE = 'http://cd-lexikon.de/';
  public
    function GetName: WideString; override; safecall;

    function InternalGetAvailableTypeIDs: TTypeIDs; override; safecall;
    function InternalGetAvailableControlIDs(const ATypeID: TTypeID): TControlIDs; override; safecall;
    function InternalGetControlIDDefaultValue(const ATypeID: TTypeID; const AControlID: TControlID): WordBool; override; safecall;
    function InternalGetDependentControlIDs: TControlIDs; override; safecall;

    function InternalExecute(const ATypeID: TTypeID; const AControlIDs: TControlIDs; const ALimit: Integer; const AControlController: IControlControllerBase; ACanUse: TCrawlerCanUseFunc): WordBool; override; safecall;

    function GetResultsLimitDefaultValue: Integer; override; safecall;
  end;

implementation

{ TCdLexikonDe }

function TCdLexikonDe.GetName;
begin
  Result := 'cd-lexikon.de';
end;

function TCdLexikonDe.InternalGetAvailableTypeIDs;
begin
  Result := [cAudio];
end;

function TCdLexikonDe.InternalGetAvailableControlIDs;
begin
  Result := [cCreator, cPublisher, cPicture, cDescription];
end;

function TCdLexikonDe.InternalGetControlIDDefaultValue;
begin
  Result := True;
end;

function TCdLexikonDe.InternalGetDependentControlIDs;
begin
  Result := [cTitle];
end;

function TCdLexikonDe.InternalExecute;

  procedure deep_search(AWebsiteSourceCode: string);
  var
    _Tracklist: string;
  begin
    if ACanUse(cPicture) then
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := '<tr><td valign=top><img src=''(.*?)''';

          if Exec(InputString) then
            AControlController.FindControl(cPicture).AddProposedValue(GetName, website + Match[1]);
        finally
          Free;
        end;

    if ACanUse(cCreator) then
    begin
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
          Expression := 'Interpret: <td><td><font size=''2'' face=''Arial''>(.*?)<tr>';

          if Exec(InputString) then
            AControlController.FindControl(cCreator).AddProposedValue(GetName, Match[1]);
        finally
          Free;
        end;
    end;

    if ACanUse(cDescription) then
    begin
      _Tracklist := '';
      with TRegExpr.Create do
        try
          InputString := AWebsiteSourceCode;
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
  LTitle: string;
  LCount: Integer;

  LRequestID1, LRequestID2: Double;

  LResponeStr: string;
begin
  LTitle := AControlController.FindControl(cTitle).Value;
  LCount := 0;

  // _Title := TrimLeft(Copy(AControlController.FindControl(cTitle).Value, Pos('-', AControlController.FindControl(cTitle).Value) + 1));

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

function TCdLexikonDe.GetResultsLimitDefaultValue;
begin
  Result := 5;
end;

end.
