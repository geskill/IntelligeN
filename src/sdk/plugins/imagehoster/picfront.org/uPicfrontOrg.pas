unit uPicfrontOrg;

interface

uses
  // Delphi
  Windows, SysUtils,
  // RegEx
  RegExpr,
  // Common
  uBaseConst,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // Plugin system
  uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TPicfrontOrg = class(TImageHosterPlugIn)
  private
    function Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
  public
    function GetName: WideString; override;
    function LocalUpload(ALocalPath: WideString; out AUrl: WideString): WordBool; override;
    function RemoteUpload(ARemoteUrl: WideString; out AUrl: WideString): WordBool; override;
  end;

implementation

{ TPicfrontOrg }

function TPicfrontOrg.Upload(const AHTTPParams: IHTTPParams; out AImageUrl: WideString): Boolean;
var
  LHTTPRequest: IHTTPRequest;

  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  result := False;

  LHTTPRequest := THTTPRequest.Create('http://www12.picfront.org/index.php');

  with AHTTPParams do
  begin
    AddFormField('SID', '');
    AddFormField('HOST_WWW', '0');
    AddFormField('HOST_END', 'org');
    AddFormField('UPLOAD_IDENTIFIER', '');
    AddFormField('urlimage', '');
    if not(ImageHostResize = irNone) then
    begin
      case ImageHostResize of
        ir320x240:
          begin
            AddFormField('resize_x', '320');
            AddFormField('resize_y', '240');
          end;
        ir450x338:
          begin
            AddFormField('resize_x', '450');
            AddFormField('resize_y', '338');
          end;
        ir640x480:
          begin
            AddFormField('resize_x', '640');
            AddFormField('resize_y', '480');
          end;
        ir800x600:
          begin
            AddFormField('resize_x', '800');
            AddFormField('resize_y', '600');
          end;
      end;
    end;
  end;

  LRequestID := HTTPManager.Post(LHTTPRequest, AHTTPParams, TPlugInHTTPOptions.Create(Self));

  repeat
    sleep(50);
  until HTTPManager.HasResult(LRequestID);

  LHTTPProcess := HTTPManager.GetResult(LRequestID);

  if (Pos('[IMG]', string(LHTTPProcess.HTTPResult.SourceCode)) > 0) then
  begin

    with TRegExpr.Create do
      try
        InputString := string(LHTTPProcess.HTTPResult.SourceCode);
        Expression := '\[IMG\](.*?)\[\/IMG\]';

        if Exec(InputString) then
        begin
          AImageUrl := StringReplace(Match[1], '/thb/', '/img/', []);
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
    Self.ErrorMsg := 'Unknown error';
  end;
end;

function TPicfrontOrg.GetName;
begin
  result := 'Picfront.org';
end;

function TPicfrontOrg.LocalUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
    AddFile('file_0', ALocalPath);

  Result := Upload(LHTTPParams, AUrl);
end;

function TPicfrontOrg.RemoteUpload;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
    AddFormField('urluploadfile_0', ARemoteUrl);

  Result := Upload(LHTTPParams, AUrl);
end;

end.
