unit uDirectuploadNet;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInInterface, uPlugInImageHosterClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils;

type
  TDirectuploadNet = class(TImageHosterPlugIn)
  protected { . }
  const
    WEBSITE: string = 'http://www.directupload.net/';
    function Upload(const AImageHosterData: IImageHosterData; const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
  public
    function GetAuthor: WideString; override;
    function GetAuthorURL: WideString; override;
    function GetDescription: WideString; override;
    function GetName: WideString; override;

    function AddLocalImage(const AImageHosterData: IImageHosterData; const ALocalPath: WideString; out AUrl: WideString): WordBool; override;
    function AddWebImage(const AImageHosterData: IImageHosterData; const ARemoteUrl: WideString; out AUrl: WideString): WordBool; override;
  end;

implementation

{ TDirectuploadNet }

function TDirectuploadNet.Upload;
var
  LHTTPRequest: IHTTPRequest;

  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  Result := False;

  LHTTPRequest := THTTPRequest.Create(WEBSITE + 'index.php?mode=upload');
  with LHTTPRequest do
  begin
    Referer := WEBSITE;
  end;

  with AHTTPParams do
    AddFormField('f_up_form', '');

  LRequestID := HTTPManager.Post(LHTTPRequest, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  HTTPManager.WaitFor(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if (Pos('[IMG]', string(LHTTPProcess.HTTPResult.SourceCode)) > 0) then
  begin

    with TRegExpr.Create do
      try
        InputString := string(LHTTPProcess.HTTPResult.SourceCode);
        Expression := 'directupload\.net\]\[IMG\](.*?)\[\/IMG\]';

        if Exec(InputString) then
        begin
          AImageUrl := Match[1];
          Result := True;
        end;
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
        Expression := '"message error">(.*?)<\/div>';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := Trim(HTML2Text(Match[1]));
        end;
      finally
        Free;
      end;
  end;
end;

function TDirectuploadNet.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TDirectuploadNet.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TDirectuploadNet.GetDescription;
begin
  Result := GetName + ' image hoster plug-in.';
end;

function TDirectuploadNet.GetName: WideString;
begin
  Result := 'Directupload.net';
end;

function TDirectuploadNet.AddLocalImage;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('input', 'file');
    AddFile('bilddatei', ALocalPath);
  end;

  Result := Upload(AImageHosterData, LHTTPParams, AUrl);
end;

function TDirectuploadNet.AddWebImage;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('input', 'url');
    AddFormField('image_link', ARemoteUrl);
  end;

  Result := Upload(AImageHosterData, LHTTPParams, AUrl);
end;

end.
