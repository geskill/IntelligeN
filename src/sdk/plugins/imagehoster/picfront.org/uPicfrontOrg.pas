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
  uPlugInInterface, uPlugInImageHosterClass, uPlugInHTTPClasses, uPlugInConst;

type
  TPicfrontOrg = class(TImageHosterPlugIn)
  protected { . }
  const
    WEBSITE: string = 'http://picfront.org/';
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

{ TPicfrontOrg }

function TPicfrontOrg.Upload;
var
  LHTTPRequest: IHTTPRequest;

  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
begin
  result := False;

  LHTTPRequest := THTTPRequest.Create('http://www12.picfront.org/index.php');
  with LHTTPRequest do
  begin
    Referer := WEBSITE;
  end;

  with AHTTPParams do
  begin
    AddFormField('SID', '');
    AddFormField('HOST_WWW', '0');
    AddFormField('HOST_END', 'org');
    AddFormField('UPLOAD_IDENTIFIER', '');
    AddFormField('urlimage', '');
    if not(AImageHosterData.ImageHostResize = irNone) then
    begin
      case AImageHosterData.ImageHostResize of
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

  HTTPManager.WaitFor(LRequestID);

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

function TPicfrontOrg.GetAuthor;
begin
  Result := 'Sebastian Klatte';
end;

function TPicfrontOrg.GetAuthorURL;
begin
  Result := 'http://www.intelligen2009.com/';
end;

function TPicfrontOrg.GetDescription;
begin
  Result := GetName + ' image hoster plug-in.';
end;

function TPicfrontOrg.GetName;
begin
  result := 'Picfront.org';
end;

function TPicfrontOrg.AddLocalImage;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
    AddFile('file_0', ALocalPath);

  Result := Upload(AImageHosterData, LHTTPParams, AUrl);
end;

function TPicfrontOrg.AddWebImage;
var
  LHTTPParams: IHTTPParams;
begin
  Result := False;

  LHTTPParams := THTTPParams.Create;
  with LHTTPParams do
    AddFormField('urluploadfile_0', ARemoteUrl);

  Result := Upload(AImageHosterData, LHTTPParams, AUrl);
end;

end.
