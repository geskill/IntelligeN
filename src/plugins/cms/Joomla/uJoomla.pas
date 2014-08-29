unit uJoomla;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, HTTPApp, XMLDoc, XMLIntf, ActiveX, Variants,
  // Indy
  IdMultipartFormData,
  // RegEx
  RegExpr,
  // Utils,
  uHTMLUtils, uSpecialStringUtils,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCMSClass, uPlugInConst, uIdHTTPHelper;

type
  TJoomla = class abstract(TCMSPlugIn)
  protected
    function Login(AIdHTTPHelper: TIdHTTPHelper): Boolean; override;
  public
    function GetName: WideString; override; safecall;
    function CMSType: TCMSType; override;
    function DefaultCharset: WideString; override;
    function BelongsTo(AWebsiteSourceCode: WideString): Boolean; override;
  end;

implementation

{ TJoomla }

function TJoomla.Login(AIdHTTPHelper: TIdHTTPHelper): Boolean;
var
  Params, ReplyData: TStringStream;
  _security_input_return, _security_input_1: string;
begin
  Result := false;

  with TIdHTTPHelper.Create(Self) do
    try
      RedirectMaximum := 1;
      Request.Referer := Website;
      if not(AccountName = '') then
      begin
        ReplyData := TStringStream.Create('', CP_UTF8);
        try
          Get(Website, ReplyData);

          with TRegExpr.Create do
            try
              InputString := ReplyData.DataString;
              Expression := 'type="hidden" name="return" value="(.*?)"';

              if Exec(InputString) then
                _security_input_return := Match[1];

              Expression := 'type="hidden" name="(\w+)" value="1"';

              if Exec(InputString) then
                _security_input_1 := Match[1];
            finally
              Free;
            end;
        finally
          ReplyData.Free;
        end;

        Params := TStringStream.Create('', CP_UTF8);
        ReplyData := TStringStream.Create('', CP_UTF8);
        try
          with Params do
          begin
            // ab Joomla 1.5 // < 1.2 : name="op2" value="login"
            WriteString('username=' + (AccountName) + '&');
            WriteString('passwd=' + (AccountPassword) + '&');
            WriteString('remember=yes&');
            WriteString('Submit=&');
            WriteString('option=com_user&');
            WriteString('task=login&');
            WriteString('return=' + (_security_input_return) + '&'); // Base64 decoded
            WriteString(_security_input_1 + '=1');
          end;

          try
            Post(Website + 'index.php?task=login', Params, ReplyData);
          except
            on E: Exception do
            begin
              ErrorMsg := E.message;
              Exit;
            end;
          end;

          if (Pos('name="task" value="logout"', ReplyData.DataString) = 0) then
          begin
            with TRegExpr.Create do
              try
                InputString := ReplyData.DataString;
                Expression := 'message.*?\s+<ul>\s+<li>(.*?)<\/li>';

                if Exec(InputString) then
                  Self.ErrorMsg := HTML2Text(Trim(Match[1]));
              finally
                Free;
              end;
            Exit;
          end;
        finally
          ReplyData.Free;
          Params.Free;
        end;
      end;

    finally
      Free;
    end;
end;

function TJoomla.GetName: WideString;
begin
  Result := 'Joomla';
end;

function TJoomla.CMSType;
begin
  Result := cmsBlog;
end;

function TJoomla.DefaultCharset;
begin
  Result := 'UTF-8';
end;

function TJoomla.BelongsTo(AWebsiteSourceCode: WideString): Boolean;
begin
  Result := (Pos('content="Joomla! 1.', string(AWebsiteSourceCode)) > 0)
end;

end.
