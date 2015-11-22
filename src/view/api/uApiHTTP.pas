unit uApiHTTP;

interface

uses
  // Delphi
  Classes, ActiveX, AxCtrls,
  // HTTPManager
  uHTTPInterface, uHTTPClasses, uHTTPManager;

type
  TApiHTTP = class
  public
    class procedure DownloadData(const AURL: WideString; out AMemoryStream: TMemoryStream; var ACookies: WideString; const AProxy: IProxy = nil; AConnectTimeout: Integer = 5000; AReadTimeout: Integer = 10000);
  end;

implementation

{ TApiHTTP }

class procedure TApiHTTP.DownloadData(const AURL: WideString; out AMemoryStream: TMemoryStream; var ACookies: WideString; const AProxy: IProxy; AConnectTimeout, AReadTimeout: Integer);
var
  LHTTPManager: IHTTPManager;
  LHTTPRequest: IHTTPRequest;
  LHTTPOptions: IHTTPOptions;
  LRequestID: Double;
  LHTTPProcess: IHTTPProcess;
  LOleStream: TOleStream;
  LDummy: Int64;
begin
  AMemoryStream := TMemoryStream.Create;

  CoInitializeEx(nil, COINIT_MULTITHREADED);
  try
    LHTTPManager := THTTPManager.Instance();

    LHTTPRequest := THTTPRequest.Create(AURL);
    with LHTTPRequest do
    begin
      Referer := AURL;
      Cookies.Text := ACookies;
    end;

    LHTTPOptions := THTTPOptions.Create(AProxy);
    with LHTTPOptions do
    begin
      ConnectTimeout := AConnectTimeout;
      ReadTimeout := AReadTimeout;
    end;

    LRequestID := LHTTPManager.Get(LHTTPRequest, LHTTPOptions);

    THTTPManager.Wait(LRequestID, 75);

    LHTTPProcess := LHTTPManager.GetResult(LRequestID);

    ACookies := LHTTPProcess.HTTPResult.HTTPResponse.Cookies.Text;

    LOleStream := TOleStream.Create(LHTTPProcess.HTTPResult.HTTPResponse.ContentStream);
    try
      LHTTPProcess.HTTPResult.HTTPResponse.ContentStream.Seek(0, STREAM_SEEK_SET, LDummy);
      LOleStream.Seek(0, STREAM_SEEK_SET);
      AMemoryStream.CopyFrom(LOleStream, LOleStream.Size);
    finally
      LOleStream.Free;
    end;
  finally
    CoUninitialize;
  end;

  AMemoryStream.Position := 0;

  LHTTPProcess := nil;
  LHTTPOptions := nil;
  LHTTPRequest := nil;
  LHTTPManager := nil;
end;

end.
