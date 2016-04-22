unit uURLUtils;

interface

uses
  // Delphi
  Windows, SysUtils,
  // Indy
  IdURI,
  // RegEx
  RegExpr,
  // Utils
  uStringUtils;

const
  HTTP: string = 'http';

function BeginsWithHTTP(const AUrl: string): Boolean;
function IsUrl(const AUrl: string): Boolean;

function RemoveW(const AHost: string): string;

function CombineUrl(const AUrl, ABaseUrl: string): string;

function ExtractUrlFileName(const AUrl: string): string;
function ExtractUrlPath(const AUrl: string): string;
function ExtractUrlProtocol(const AUrl: string): string;
{$REGION 'Documentation'}
/// <summary>
///   Extracts the url host.
/// </summary>
/// <remarks>
///   Delegation function for Indy's TIdURL class.
/// </remarks>
/// <example>
///   http://www.sub.example.org/path/?name=value will be
///   converted to www.sub.example.org.
/// </example>
{$ENDREGION}
function ExtractUrlHost(const AUrl: string; ARemoveW: Boolean = False): string;
function ExtractUrlHostWithPath(const AUrl: string; ARemoveW: Boolean = False): string;
function BuildWebsiteUrl(const AUrl: string): string;
function IncludeTrailingUrlDelimiter(const AUrl: string): string;
function ExcludeTrailingUrlDelimiter(const AUrl: string): string;

implementation

function BeginsWithHTTP(const AUrl: string): Boolean;
begin
  Result := (copy(LowerCase(AUrl), 1, length(HTTP)) = HTTP);
end;

function IsUrl(const AUrl: string): Boolean;
begin
  Result := Pos('://', AUrl) > 0;
end;

function RemoveW(const AHost: string): string;
begin
  with TRegExpr.Create do
    try
      InputString := AHost;
      Expression := 'www\d{0,3}\.';

      if Exec(InputString) then
        Result := copy(AHost, Pos(string(Match[0]), AHost) + Length(Match[0]))
      else
        Result := AHost;
    finally
      Free;
    end;
end;

function CombineUrl(const AUrl, ABaseUrl: string): string;
var
  LIdURI: TIdURI;
begin
  Result := AURL;
  LIdURI := TIdURI.Create(ABaseURL);
  try
    if (Length(Result) > 1) and (Result[1] = '/') and (Result[2] = '/') then
    begin
      Result := LIdURI.Protocol + ':' + Result;
    end;
    if (Length(Result) > 0) and (Result[1] = '/') then
    begin
      Result := LIdURI.Protocol + '://' + LIdURI.Host + Result;
    end;
    if (Length(Result) > 0) and ((Result[1] = '?') or (Result[1] = '#')) then
    begin
      Result := ABaseURL + Result;
    end;
    if (Length(Result) > 1) and (Result[1] = '.') and (Result[2] = '/') then
    begin
      Result := LIdURI.Protocol + '://' + LIdURI.Host + LIdURI.Path + copy(Result, 3);
    end;
    if (Pos('://', Result) = 0) then
    begin
      Result := LIdURI.Protocol + '://' + LIdURI.Host + LIdURI.Path + Result;
    end;
    // TODO: Handle ../../
  finally
    LIdURI.Free;
  end;
end;

function ExtractUrlFileName(const AUrl: string): string;
var
  LPosition: Integer;
begin
  LPosition := LastDelimiter('/', AUrl);
  Result := copy(AUrl, LPosition + 1, length(AUrl) - (LPosition));
end;

function ExtractUrlPath(const AUrl: string): string;
var
  LPosition: Integer;
begin
  LPosition := LastDelimiter('/', AUrl);
  Result := copy(AUrl, 1, LPosition);
end;

function ExtractUrlProtocol(const AUrl: string): string;
var
  LPosition: Integer;
begin
  LPosition := Pos('://', AUrl);
  if (LPosition = 0) then
    Result := ''
  else
    Result := copy(AUrl, 1, LPosition - 1);
end;

function ExtractUrlHost(const AUrl: string; ARemoveW: Boolean = False): string;
begin
  with TIdURI.Create(AUrl) do
    try
      if ARemoveW then
        Result := RemoveW(Host)
      else
        Result := Host;
    finally
      Free;
    end;
end;

function ExtractUrlHostWithPath(const AUrl: string; ARemoveW: Boolean = False): string;
begin
  with TIdURI.Create(AUrl) do
    try
      if ARemoveW then
        Result := RemoveW(Host) + Path
      else
        Result := Host + Path;
    finally
      Free;
    end;
end;

function BuildWebsiteUrl(const AUrl: string): string;
var
  LUrl: string;
begin
  if not BeginsWithHTTP(AUrl) then
    LUrl := HTTP + '://' + AUrl
  else
    LUrl := AUrl;

  if not(LUrl[length(LUrl)] = '/') then
  begin
    if (CharCount('/', LUrl) < 3) then
      LUrl := IncludeTrailingUrlDelimiter(LUrl)
    else
      LUrl := copy(LUrl, 1, LastDelimiter('/', LUrl))
  end;
  Result := LUrl;
end;

function IncludeTrailingUrlDelimiter(const AUrl: string): string;
const
  UrlDelim = '/';
begin
  Result := AUrl;
  if not IsDelimiter(UrlDelim, Result, length(Result)) then
    Result := Result + UrlDelim;
end;

function ExcludeTrailingUrlDelimiter(const AUrl: string): string;
const
  UrlDelim = '/';
begin
  Result := AUrl;
  if IsDelimiter(UrlDelim, Result, length(Result)) then
    SetLength(Result, length(Result) - 1);
end;

end.
