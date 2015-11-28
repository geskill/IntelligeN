unit uHTTPIndyImplementor;

interface

uses
  // Interface
  uHTTPInterface,
  // Classes
  uHTTPManager, uHTTPManagerClasses, uHTTPClasses, uHTTPImplementation,
  // Const
  uHTTPConst,
  // Indy MOD
  uHTTPIndyHelper,
  // Delphi
  Windows, SysUtils, Classes, Math, Generics.Collections,
  // Indy
  IdBaseComponent, IdGlobalProtocols, IdMultipartFormData;

type
  THTTPIndyImplementation = class(THTTPImplementation)
  public
    class function GetImplementationName: string; override;
    procedure Handle(const AHTTPData: IHTTPData; out AHTTPResult: IHTTPResult); override;
  end;

implementation

{ THTTPIndyImplementation }

class function THTTPIndyImplementation.GetImplementationName: string;
var
  LDummy: TIdBaseComponent;
begin
  LDummy := TIdBaseComponent.Create(nil);
  try
    Result := 'Indy (' + LDummy.Version + ')';
  finally
    LDummy.Free;
  end;
end;

procedure THTTPIndyImplementation.Handle(const AHTTPData: IHTTPData; out AHTTPResult: IHTTPResult);
var
  HTTPResponse: IHTTPResponse;
  HTTPResponseInfo: IHTTPResponseInfo;

  HTTPHelper: THTTPIndyHelper;

  MemoryStream: TMemoryStream;
  ParamsList: TStringList;
  ParamsData: TMemoryStream;
  ParamsMultipartFormData: TIdMultiPartFormDataStream;

  ErrorMsg, ErrorClass: string;

  I: Integer;
begin
    try
      HTTPHelper := THTTPIndyHelper.Create(AHTTPData.HTTPOptions.Proxy);
      try

        with HTTPHelper do
        begin
          UseCompressor := AHTTPData.HTTPOptions.UseCompressor;

          ConnectTimeout := AHTTPData.HTTPOptions.ConnectTimeout;
          ReadTimeout := AHTTPData.HTTPOptions.ReadTimeout;

          HandleRedirects := AHTTPData.HTTPOptions.HandleRedirects;
          HandleSketchyRedirects := AHTTPData.HTTPOptions.HandleSketchyRedirects;
          RedirectMaximum := AHTTPData.HTTPOptions.RedirectMaximum;
        end;

        with HTTPHelper.Request do
        begin
          Accept := AHTTPData.HTTPRequest.Accept;
          AcceptCharSet := AHTTPData.HTTPRequest.AcceptCharSet;
          AcceptEncoding := AHTTPData.HTTPRequest.AcceptEncoding;
          AcceptLanguage := AHTTPData.HTTPRequest.AcceptLanguage;

          Referer := AHTTPData.HTTPRequest.Referer;
          UserAgent := AHTTPData.HTTPRequest.UserAgent;

          HTTPHelper.CookieList := AHTTPData.HTTPRequest.Cookies.Text;

          CacheControl := AHTTPData.HTTPRequest.CacheControl;
          CharSet := AHTTPData.HTTPRequest.CharSet;
          Connection := AHTTPData.HTTPRequest.Connection;
          ContentDisposition := AHTTPData.HTTPRequest.ContentDisposition;
          ContentEncoding := AHTTPData.HTTPRequest.ContentEncoding;
          ContentLanguage := AHTTPData.HTTPRequest.ContentLanguage;
          ContentType := AHTTPData.HTTPRequest.ContentType;

          CustomHeaders.Text := AHTTPData.HTTPRequest.CustomHeaders.Text;
        end;

        if Assigned(AHTTPData.HTTPParams) then
        begin
          case AHTTPData.HTTPParams.ParamType of
            ptList:
              begin
                ParamsList := TStringList.Create;

                for I := 0 to AHTTPData.HTTPParams.Count - 1 do
                  ParamsList.Add(AHTTPData.HTTPParams.FieldNames[I] + '=' + AHTTPData.HTTPParams.FieldValueFromIndex[I]);

              end;
            ptData:
              begin
                ParamsData := TMemoryStream.Create;

                // TEST: CharSet conversation
                WriteStringAsCharset(ParamsData, AHTTPData.HTTPParams.RawData, HTTPHelper.Request.CharSet);
              end;
            ptMultipartFormData:
              begin
                ParamsMultipartFormData := TIdMultiPartFormDataStream.Create;

                for I := 0 to AHTTPData.HTTPParams.Count - 1 do
                  if AHTTPData.HTTPParams.IsFile[I] then
                    // normal file
                    ParamsMultipartFormData.AddFile(AHTTPData.HTTPParams.FieldNames[I], AHTTPData.HTTPParams.FieldFileNameFromIndex[I], GetMIMETypeFromFile(AHTTPData.HTTPParams.FieldFileNameFromIndex[I]))
                  else if SameStr('', AHTTPData.HTTPParams.FieldFileNameFromIndex[I]) then
                    // normal name-value
                    ParamsMultipartFormData.AddFormField(AHTTPData.HTTPParams.FieldNames[I], AHTTPData.HTTPParams.FieldValueFromIndex[I], HTTPHelper.Request.CharSet).ContentTransfer := 'binary'
                  else
                  begin
                    // text file stream
                    // TEST: CharSet conversation
                    ParamsMultipartFormData.AddFormField(AHTTPData.HTTPParams.FieldNames[I], 'text/plain', HTTPHelper.Request.CharSet, AHTTPData.HTTPParams.FieldValueFromIndex[I], AHTTPData.HTTPParams.FieldFileNameFromIndex[I]).ContentTransfer := 'binary';
                  end;
              end;
          end;
        end;

        ErrorMsg := '';

        MemoryStream := TMemoryStream.Create;
        try
          case AHTTPData.HTTPRequest.Method of
            mGET:
              try
                HTTPHelper.Get(AHTTPData.HTTPRequest.URL, MemoryStream, []);
              except
                on E: Exception do
                begin
                  ErrorMsg := E.message;
                  ErrorClass := E.ClassName;
                end;
              end;
            mPOST:
              begin
                case AHTTPData.HTTPParams.ParamType of
                  ptList:
                    begin
                      HTTPHelper.Request.ContentType := HTTPRequestContentTypeList;
                      try
                        HTTPHelper.Post(AHTTPData.HTTPRequest.URL, ParamsList, MemoryStream, CharsetToEncoding(HTTPHelper.Request.CharSet));
                      except
                        on E: Exception do
                        begin
                          ErrorMsg := E.message;
                          ErrorClass := E.ClassName;
                        end;
                      end;
                    end;
                  ptData:
                    begin
                      try
                        HTTPHelper.Post(AHTTPData.HTTPRequest.URL, ParamsData, MemoryStream);
                      except
                        on E: Exception do
                        begin
                          ErrorMsg := E.message;
                          ErrorClass := E.ClassName;
                        end;
                      end;
                    end;
                  ptMultipartFormData:
                    begin
                      /// Not allowed to set CharSet when posting with multipart/form-data
                      /// see: http://forums2.atozed.com/viewtopic.php?f=7&t=26262
                      HTTPHelper.Request.CharSet := '';
                      HTTPHelper.Request.ContentType := HTTPRequestContentTypeMultipart;

                      try
                        HTTPHelper.Post(AHTTPData.HTTPRequest.URL, ParamsMultipartFormData, MemoryStream);
                      except
                        on E: Exception do
                        begin
                          ErrorMsg := E.message;
                          ErrorClass := E.ClassName;
                        end;
                      end;
                    end;

                end;
              end;
          end;

          if Assigned(AHTTPData.HTTPParams) then
          begin
            // FreeAndNil hides compiler W1036 warnings
            case AHTTPData.HTTPParams.ParamType of
              ptList:
                FreeAndNil(ParamsList);
              // ParamsList.Free;
              ptData:
                FreeAndNil(ParamsData);
              // ParamsData.Free;
              ptMultipartFormData:
                begin
                  FreeAndNil(ParamsMultipartFormData);
                end;
              // ParamsMultipartFormData.Free;
            end;
          end;

          HTTPResponse := THTTPResponse.Create(MemoryStream);
          with HTTPResponse do
          begin
            Location := HTTPHelper.Response.Location;
            Refresh := HTTPHelper.Response_Refresh;
            Text := HTTPHelper.ResponseText;
            Code := HTTPHelper.ResponseCode;
            Server := HTTPHelper.Response.Server;
            Content := HTTPHelper.ResponseContentString;

            Cookies.Text := HTTPHelper.CookieList;

            CacheControl := HTTPHelper.MetaHTTPEquiv.CacheControl;
            CharSet := HTTPHelper.MetaHTTPEquiv.CharSet;
            Connection := HTTPHelper.MetaHTTPEquiv.Connection;
            ContentDisposition := HTTPHelper.MetaHTTPEquiv.ContentDisposition;
            ContentEncoding := HTTPHelper.MetaHTTPEquiv.ContentEncoding;
            ContentLanguage := HTTPHelper.MetaHTTPEquiv.ContentLanguage;
            ContentType := HTTPHelper.MetaHTTPEquiv.ContentType;

            CustomHeaders.Text := HTTPHelper.MetaHTTPEquiv.CustomHeaders.Text;
          end;

        finally
          MemoryStream.Free;
        end;

        HTTPResponseInfo := THTTPResponseInfo.Create();
        with HTTPResponseInfo do
        begin
          LastRedirect := HTTPHelper.LastRedirect;
          RedirectCount := HTTPHelper.RedirectCount;

          ErrorClassName := ErrorClass;
          ErrorMessage := ErrorMsg;
        end;

        // HandleBlockingScripts(HTTPHelper, HTTPData.HTTPRequest.URL, HTTPResponse);
      finally
        HTTPHelper.Free;
      end;
    except
      on E: Exception do
      begin
        HTTPResponse := THTTPResponse.Create(nil);

        HTTPResponseInfo := THTTPResponseInfo.Create();
        with HTTPResponseInfo do
        begin
          ErrorClassName := E.ClassName;
          ErrorMessage := E.Message;
        end;
      end;
    end;

  AHTTPResult := THTTPResult.Create(HTTPResponse, HTTPResponseInfo);
end;

initialization
  THTTPManager.Instance().ImplementationManager.Register(THTTPIndyImplementation.Create);

finalization
  THTTPManager.Instance().ImplementationManager.Unregister(THTTPIndyImplementation.GetImplementationName);

end.
