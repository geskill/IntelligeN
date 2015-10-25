unit uTinypicCom;

interface

uses
  // Delphi
  SysUtils, StrUtils, Classes, Controls, HTTPApp,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses,
  // Utils
  uHTMLUtils;

type
  TTinypicCom = class(TImageHosterPlugIn)
  private
    function Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString; out AUrl: WideString): WordBool; override;
    function RemoteUpload(ARemoteUrl: WideString; out AUrl: WideString): WordBool; override;
  end;

implementation

{ TTinypicCom }

function TTinypicCom.Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
var
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;

  LResponsePluginPage: string;

  LUploadServer, LUID, LUPK: string;
begin
  Result := False;

  LRequestID := HTTPManager.Get(THTTPRequest.Create('http://plugin.tinypic.com/plugin/index.php'), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LResponsePluginPage := HTTPManager.GetResult(LRequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := LResponsePluginPage;

      Expression := '<form action="(.*?)"';
      if Exec(InputString) then
        LUploadServer := Match[1];

      Expression := 'name="UPLOAD_IDENTIFIER".*?value="(.*?)"';
      if Exec(InputString) then
        LUID := Match[1];

      Expression := 'name="upk".*?value="(.*?)"';
      if Exec(InputString) then
        LUPK := Match[1];
    finally
      Free;
    end;

  with AHTTPParams do
  begin
    AddFormField('upload_form', '');
    AddFormField('UPLOAD_IDENTIFIER', LUID);
    AddFormField('upk', LUPK);
    AddFormField('domain_lang', 'en');
    AddFormField('action', 'upload');
    AddFormField('popts', 'l,narrow|t,both|c,html|i,en|s,true');
    AddFormField('MAX_FILE_SIZE', '200000000');

    AddFormField('description', '');
    AddFormField('dimension', '1600');
    AddFormField('upload_form', '');
  end;

  LRequestID := HTTPManager.Post(LUploadServer, LRequestID, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if (Pos('name="pic"', string(LHTTPProcess.HTTPResult.SourceCode)) > 0) then
  begin

    with TRegExpr.Create do
      try
        InputString := LHTTPProcess.HTTPResult.SourceCode;

        ModifierS := True;
        Expression := 'name="pic" value="(.*?)".*?name="s" value="(\d+)".*?name="ival" value="(.*?)".*?name="ext" value="(.*?)"';
        if Exec(InputString) then
        begin
          // http://oi61.tinypic.com/or851t.jpg

          AImageUrl := 'http://oi' + Match[3] + '.tinypic.com/' + Match[1] + '.jpg';
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
        Expression := 'name="message" value="(.*?)".*?<strong>(.*?)<\/';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := Trim(HTML2Text(Match[2])) + ': ' + Match[1];
        end;
      finally
        Free;
      end;
  end;

end;

function TTinypicCom.GetName;
begin
  Result := 'Tinypic.com';
end;

function TTinypicCom.LocalUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFile('the_file', ALocalPath);
    AddFormField('file_type', 'image');
  end;

  Result := Upload(LHTTPParams, AUrl);
end;

function TTinypicCom.RemoteUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('url', ARemoteUrl);
    AddFormField('file_type', 'url');
  end;

  Result := Upload(LHTTPParams, AUrl);
end;

end.
