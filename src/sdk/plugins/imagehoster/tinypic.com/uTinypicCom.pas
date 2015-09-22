unit uTinypicCom;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, HTTPApp,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses;

type
  TTinypicCom = class(TImageHosterPlugIn)
  private
    function Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString): WideString; override;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

{ TTinypicCom }

function TTinypicCom.Upload(AHTTPParams: IHTTPParams; out AImageUrl: string): Boolean;
var
  RequestID: Double;

  ResponseStrUploadPage, ResponseStr: string;

  _uploadserver, _uid, _upk: string;
begin
  Result := False;

  RequestID := HTTPManager.Get(THTTPRequest.Create('http://plugin.tinypic.com/plugin/index.php'), TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStrUploadPage := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStrUploadPage;

      Expression := '<form action="(.*?)"';
      if Exec(InputString) then
        _uploadserver := Match[1];

      Expression := 'name="UPLOAD_IDENTIFIER".*?value="(.*?)"';
      if Exec(InputString) then
        _uid := Match[1];

      Expression := 'name="upk".*?value="(.*?)"';
      if Exec(InputString) then
        _upk := Match[1];
    finally
      Free;
    end;

  with AHTTPParams do
  begin
    AddFormField('upload_form', '');
    AddFormField('UPLOAD_IDENTIFIER', _uid);
    AddFormField('upk', _upk);
    AddFormField('domain_lang', 'en');
    AddFormField('action', 'upload');
    AddFormField('popts', 'l,narrow|t,both|c,html|i,en|s,true');
    AddFormField('MAX_FILE_SIZE', '200000000');

    AddFormField('description', '');
    AddFormField('dimension', '1600');
    AddFormField('upload_form', '');
  end;

  RequestID := HTTPManager.Post(_uploadserver, RequestID, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(RequestID);

  ResponseStr := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;

  with TRegExpr.Create do
    try
      InputString := ResponseStr;

      ModifierS := True;
      Expression := 'name="pic" value="(.*?)".*?name="s" value="(\d+)".*?name="ival" value="(.*?)".*?name="ext" value="(.*?)"';
      if Exec(InputString) then
      begin
        AImageUrl := 'http://i' + Match[3] + '.tinypic.com/' + Match[1] + Match[4];
        Result := True;
      end;
    finally
      Free;
    end;
end;

function TTinypicCom.GetName;
begin
  result := 'Tinypic.com';
end;

function TTinypicCom.LocalUpload(ALocalPath: WideString): WideString;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := '';

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFile('the_file', ALocalPath);
    AddFormField('file_type', 'image');
  end;

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

function TTinypicCom.RemoteUpload;
var
  HTTPParams: IHTTPParams;
  LImageUrl: string;
begin
  Result := AImageUrl;

  HTTPParams := THTTPParams.Create;
  with HTTPParams do
  begin
    AddFormField('url', AImageUrl);
    AddFormField('file_type', 'url');
  end;

  if Upload(HTTPParams, LImageUrl) then
    Result := LImageUrl;
end;

end.
