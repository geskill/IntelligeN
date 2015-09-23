unit uYoupicIn;

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
  TYoupicIn = class(TImageHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

function TYoupicIn.GetName;
begin
  result := 'Youpic.in';
end;

function TYoupicIn.RemoteUpload;
const
  Website = 'http://youpic.in/';
var
  //_params: TIdMultiPartFormDataStream;
  _reply: TStringStream;
begin
  result := AImageUrl;
  {
  with TIdHTTPHelper.Create(Self) do
    try
      Request.Referer := Website;

      _params := TIdMultiPartFormDataStream.Create;
      with _params do
        try
          AddFormField('url_upload_type', 'normal_upload');

          AddFormField('userfile[]', AImageUrl);

          AddFormField('private_upload', '1');

          AddFormField('upload_type', 'url-standard');

          AddFormField('upload_type', 'url-standard');

          if not(ImageHostResize = irNone) then
          begin
            case ImageHostResize of
              ir320x240:
                AddFormField('image_resize', '3');
              ir450x338:
                AddFormField('image_resize', '3');
              ir640x480:
                AddFormField('image_resize', '4');
              ir800x600:
                AddFormField('image_resize', '5');
            end;

          end
          else
            AddFormField('image_resize', '0');

          Request.ContentType := 'multipart/form-data';

          _reply := TStringStream.Create('');
          try
            Post(Website + 'upload.php', _params, _reply);

            with TRegExpr.Create do
              try
                InputString := _reply.DataString;
                Expression := '\[URL=http:\/\/youpic\.in\/\]\[IMG\](.*?)\[';

                if Exec(InputString) then
                  result := Match[1];
              finally
                Free;
              end;
          finally
            _reply.Free;
          end;
        finally
          Free;
        end;
    finally
      Free;
    end;
  }
end;

end.
