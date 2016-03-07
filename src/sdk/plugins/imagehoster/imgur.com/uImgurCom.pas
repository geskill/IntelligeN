unit uImgurCom;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TImgurCom = class(TImageHosterPlugIn)
  protected { . }
  const
    WEBSITE: string = 'http://imgur.com/';
    function Upload(const AImageHosterData: IImageHosterData; const AHTTPParams: IHTTPParams; out AImageUrl: WideString; AFileExt: string): Boolean;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function AddLocalImage(const AImageHosterData: IImageHosterData; const ALocalPath: WideString; out AUrl: WideString): WordBool; override;
    function AddWebImage(const AImageHosterData: IImageHosterData; const ARemoteUrl: WideString; out AUrl: WideString): WordBool; override;
  end;

implementation

{ TImgurCom }

function TImgurCom.Upload;
var
  LHTTPRequest: IHTTPRequest;

  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  Result := False;

  LHTTPRequest := THTTPRequest.Create(WEBSITE + 'upload');
  with LHTTPRequest do
  begin
    CustomHeaders.Add('X-Requested-With: XMLHttpRequest');
    Referer := WEBSITE;
  end;

  with AHTTPParams do
  begin
    AddFormField('create_album', '0');
    AddFormField('album_title', 'Optional+Album+Title');
    AddFormField('album_layout', 'b');
    AddFormField('edit_url', '0');
    AddFormField('catify', '0');
  end;

  LRequestID := HTTPManager.Post(LHTTPRequest, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if (Pos('hash', string(LHTTPProcess.HTTPResult.SourceCode)) > 0) then
  begin

    with TRegExpr.Create do
      try
        InputString := LHTTPProcess.HTTPResult.SourceCode;
        Expression := '"hash":"(.*?)"';

        if Exec(InputString) then
        begin
          AImageUrl := 'http://i.imgur.com/' + Match[1] + AFileExt;
          Result := True;
        end;

        // http://i.imgur.com/voxuS.png
      finally
        Free;
      end;

  end
  else if LHTTPProcess.HTTPResult.HasError then
  begin
    ErrorMsg := LHTTPProcess.HTTPResult.HTTPResponseInfo.ErrorMessage;
  end
  else
  begin
    with TRegExpr.Create do
      try
        InputString := string(LHTTPProcess.HTTPResult.SourceCode);
        Expression := '"message":"(.*?)"';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := Match[1];
        end;
      finally
        Free;
      end;
  end;
end;

function TImgurCom.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TImgurCom.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TImgurCom.GetDescription;
begin
  Result := GetName + ' image hoster plug-in.';
end;

function TImgurCom.GetName: WideString;
begin
  Result := 'Imgur.com';
end;

function TImgurCom.AddLocalImage;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
    AddFile('Filedata', ALocalPath);

  Result := Upload(AImageHosterData, LHTTPParams, AUrl, ExtractFileExt(ALocalPath));
end;

function TImgurCom.AddWebImage;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
    AddFormField('url', ARemoteUrl);

  Result := Upload(AImageHosterData, LHTTPParams, AUrl, ExtractFileExt(ARemoteUrl));
end;

end.
