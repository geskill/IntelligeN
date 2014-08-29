unit uTinypicCom;

interface

uses
  // Delphi
  SysUtils, Classes, Controls, HTTPApp,
  // Indy
  IdMultipartFormData,
  // RegEx
  RegExpr,
  // Plugin system
  uPlugInImageHosterClass, uIdHTTPHelper;

type
  TTinypicCom = class(TImageHosterPlugIn)
  public
    function GetName: WideString; override; safecall;
    function RemoteUpload(AImageUrl: WideString): WideString; override;
  end;

implementation

function TTinypicCom.GetName;
begin
  result := 'Tinypic.com';
end;

function TTinypicCom.RemoteUpload;
const
  Website = 'http://tinypic.com/';
  security_inputs: array [0 .. 1] of string = ('UPLOAD_IDENTIFIER', 'upk');
  return_inputs: array [0 .. 1] of string = ('pic', 'ival');
  returnstring = 'http://i[ival].tinypic.com/[pic].jpg';
var
  Params: TIdMultiPartFormDataStream;
  s, uploadserver: string;
  I: Integer;
var
  _uid, _storecaptcha, _site, _challenge, _captcha, _cookies: widestring;
begin
  result := AImageUrl;

  with TIdHTTPHelper.Create(Self) do
    try
      Request.Referer := Website;

      s := Get(Website);

      with TRegExpr.Create do
        try
          InputString := s;
          Expression := 'name="UPLOAD_IDENTIFIER".*?value="(.*?)"';

          if Exec(InputString) then
            _uid := Match[1];

        finally
          Free;
        end;

      _storecaptcha := Get(Website + 'storecaptcha.php?uid=' + _uid);

      with TRegExpr.Create do
        try
          InputString := _storecaptcha;
          Expression := 'challenge\?k=(.*?)"';

          if Exec(InputString) then
            _site := Match[1];

        finally
          Free;
        end;

      _storecaptcha := Get('http://www.google.com/recaptcha/api/challenge?k=' + _site);

      with TRegExpr.Create do
        try
          InputString := _storecaptcha;
          Expression := 'challenge : ''(.*?)''';

          if Exec(InputString) then
            _challenge := Match[1];

        finally
          Free;
        end;

      if not CAPTCHAInput('http://www.google.com/recaptcha/api/image?c=' + _challenge, 'reCAPTCHA', _captcha, _cookies) then
        Exit;

      Params := TIdMultiPartFormDataStream.Create;
      try
        with TRegExpr.Create do
          try
            InputString := s;
            Expression := '<form action="(.*?)"';

            if Exec(InputString) then
            begin
              repeat
                uploadserver := Match[1];
              until not ExecNext;
            end;
          finally
            Free;
          end;

        with TRegExpr.Create do
          try
            for I := 0 to length(security_inputs) - 1 do
            begin
              InputString := s;
              Expression := 'name="' + security_inputs[I] + '"(.*?)value="(.*?)"';

              if Exec(InputString) then
              begin
                repeat
                  Params.AddFormField(security_inputs[I], Match[2]);
                until not ExecNext;
              end;
            end;
          finally
            Free;
          end;

        Params.AddFormField('rcf', _challenge);
        Params.AddFormField('rrf', _captcha);
        Params.AddFormField('domain_lang', 'de');
        Params.AddFormField('action', 'upload');
        Params.AddFormField('shareopt', 'true');
        Params.AddFormField('url', AImageUrl);
        Params.AddFormField('MAX_FILE_SIZE', '500000000');
        Params.AddFormField('description', '');
        Params.AddFormField('file_type', 'url');
        Params.AddFormField('dimension', '1600');

        Request.Referer := Website;
        Request.Connection := 'Keep-Alive, TE';
        // ResetCookies(Request);
        s := Post(uploadserver, Params);

        {
          with TStringList.Create do
          try
          Text := s;
          SaveToFile('a.htm');
          finally
          Free;
          end;
          }

        with TRegExpr.Create do
          try
            result := returnstring;
            for I := 0 to length(return_inputs) - 1 do
            begin
              InputString := s;
              Expression := 'name="' + return_inputs[I] + '" value="(.*?)"';

              if Exec(InputString) then
              begin
                repeat
                  result := StringReplace(result, '[' + return_inputs[I] + ']', Match[1], [rfReplaceAll, rfIgnoreCase]);
                until not ExecNext;
              end;
            end;
          finally
            Free;
          end;
      finally
        Params.Free;
      end;
    finally
      Free;
    end;
end;

end.
