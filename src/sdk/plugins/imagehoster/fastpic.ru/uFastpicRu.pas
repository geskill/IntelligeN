unit uFastpicRu;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, StrUtils, HTTPApp,
  // DEC
  DECFmt,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst,
  // Utils
  uHTMLUtils;

type
  TFastpicRu = class(TImageHosterPlugIn)
  protected { . }
  const
    WEBSITE: string = 'http://fastpic.ru/';
    function Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString; AUploadURI: string): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(const ALocalPath: WideString; out AUrl: WideString): WordBool; override;
    function RemoteUpload(const ARemoteUrl: WideString; out AUrl: WideString): WordBool; override;
  end;

implementation

{ TFastpicRu }

function TFastpicRu.Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString; AUploadURI: string): Boolean;
var
  LHTTPRequest: IHTTPRequest;
  LHTTPOptions: IHTTPOptions;

  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  Result := False;
  AImageUrl := '';

  LHTTPRequest := THTTPRequest.Create(WEBSITE + AUploadURI + '?api=1');
  with LHTTPRequest do
  begin
    LHTTPRequest.Referer := WEBSITE;
  end;

  with AHTTPParams do
  begin
    AddFormField('thumb_text', '');

    AddFormField('check_thumb', 'no');

    AddFormField('thumb_size', '170');

    AddFormField('orig_rotate', '0');

    AddFormField('jpeg_quality', '80');

    AddFormField('submit', '');

    AddFormField('uploading', '1');

    if not(ImageHostResize = irNone) then
    begin
      AddFormField('check_orig_resize', '1');

      case ImageHostResize of
        ir320x240:
          begin
            AddFormField('res_select', '320');
            AddFormField('orig_resize', '240');
          end;
        ir450x338:
          begin
            AddFormField('res_select', '450');
            AddFormField('orig_resize', '338');
          end;
        ir640x480:
          begin
            AddFormField('res_select', '640');
            AddFormField('orig_resize', '480');
          end;
        ir800x600:
          begin
            AddFormField('res_select', '800');
            AddFormField('orig_resize', '600');
          end;
      end;

    end
    else
    begin
      AddFormField('check_orig_resize', '0');
    end;
  end;

  LHTTPOptions := TPlugInHTTPOptions.Create(Self);
  with LHTTPOptions do
  begin

  end;

  LRequestID := HTTPManager.Post(LHTTPRequest, AHTTPParams, LHTTPOptions);

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if (Pos('<status>ok</status>', string(LHTTPProcess.HTTPResult.SourceCode)) > 0) then
  begin

    with TRegExpr.Create do
      try
        InputString := string(LHTTPProcess.HTTPResult.SourceCode);
        Expression := '<imagepath>(.*?)<\/';

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
        Expression := '<error>(.*?)<\/';

        if Exec(InputString) then
        begin
          Self.ErrorMsg := HTML2Text(Match[1]);
        end;
      finally
        Free;
      end;
  end;
end;

function TFastpicRu.GetName;
begin
  Result := 'Fastpic.ru';
end;

function TFastpicRu.LocalUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFile('file[]', ALocalPath);
  end;

  Result := Upload(LHTTPParams, AUrl, 'uploadmulti');
end;

function TFastpicRu.RemoteUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
  begin
    AddFormField('files', ARemoteUrl);
  end;

  Result := Upload(LHTTPParams, AUrl, 'upload_copy');
end;

end.
