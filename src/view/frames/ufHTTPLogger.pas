unit ufHTTPLogger;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, cxLabel, cxTextEdit, cxBlobEdit, cxNavigator,
  // HTTPManager
  uHTTPInterface, uHTTPConst, uHTTPEvent, uHTTPManager;

type
  TfHTTPLogger = class(TFrame)
    cxGrid: TcxGrid;
    tvHTTPProcess: TcxGridTableView;
    tvHTTPProcessUniqueID: TcxGridColumn;
    tvHTTPProcessMethod: TcxGridColumn;
    tvHTTPProcessURI: TcxGridColumn;
    tvHTTPProcessStatusCode: TcxGridColumn;
    tvHTTPRequest: TcxGridTableView;
    tvHTTPRequestName: TcxGridColumn;
    tvHTTPRequestValue: TcxGridColumn;
    tvHTTPParams: TcxGridTableView;
    tvHTTPParamsColumnName: TcxGridColumn;
    tvHTTPParamsColumnValue: TcxGridColumn;
    tvHTTPResponse: TcxGridTableView;
    tvHTTPResponseName: TcxGridColumn;
    tvHTTPResponseValue: TcxGridColumn;
    HTTPProcess: TcxGridLevel;
    HTTPRequest: TcxGridLevel;
    HTTPParams: TcxGridLevel;
    HTTPResponse: TcxGridLevel;
  private
    FIHTTPProcessEventHandler: IHTTPProcessEventHandler;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddHTTPProcess(const AHTTPProcess: IHTTPProcess);
  end;

implementation

{$R *.dfm}

constructor TfHTTPLogger.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIHTTPProcessEventHandler := TIHTTPProcessEventHandler.Create(AddHTTPProcess);

  /// calling this in uApiHTTP inside initialization block
  /// is problematic, because THTTPLogger is not created at
  /// this point.
  with THTTPManager.Instance() do
    OnRequestDone.Add(FIHTTPProcessEventHandler);
end;

destructor TfHTTPLogger.Destroy;
begin
  with THTTPManager.Instance() do
    OnRequestDone.Remove(FIHTTPProcessEventHandler);

  FIHTTPProcessEventHandler := nil;

  inherited Destroy;
end;

procedure TfHTTPLogger.AddHTTPProcess(const AHTTPProcess: IHTTPProcess);
var
  I: Integer;
  CustomDataController: TcxCustomDataController;
begin
  with tvHTTPProcess.DataController do
  begin
    BeginUpdate;
    try
      RecordCount := RecordCount + 1;

      Values[RecordCount - 1, tvHTTPProcessUniqueID.Index] := AHTTPProcess.UniqueID;
      case AHTTPProcess.HTTPData.HTTPRequest.Method of
        mGET:
          Values[RecordCount - 1, tvHTTPProcessMethod.Index] := 'GET';
        mPOST:
          Values[RecordCount - 1, tvHTTPProcessMethod.Index] := 'POST';
      end;
      Values[RecordCount - 1, tvHTTPProcessURI.Index] := AHTTPProcess.HTTPData.Website;
      Values[RecordCount - 1, tvHTTPProcessStatusCode.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Code;
    finally
      EndUpdate;
    end;
  end;

  with tvHTTPProcess.DataController do
    CustomDataController := GetDetailDataController(RecordCount - 1, HTTPRequest.Index);
  with CustomDataController do
  begin
    RecordCount := 8;

    Values[0, tvHTTPRequestName.Index] := 'URL';
    Values[0, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.URL;

    Values[1, tvHTTPRequestName.Index] := 'Accept';
    Values[1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.Accept;

    Values[2, tvHTTPRequestName.Index] := 'AcceptCharSet';
    Values[2, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.AcceptCharSet;

    Values[3, tvHTTPRequestName.Index] := 'AcceptEncoding';
    Values[3, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.AcceptEncoding;

    Values[4, tvHTTPRequestName.Index] := 'AcceptLanguage';
    Values[4, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.AcceptLanguage;

    Values[5, tvHTTPRequestName.Index] := 'Host';
    Values[5, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.Host;

    Values[6, tvHTTPRequestName.Index] := 'Referer';
    Values[6, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.Referer;

    Values[7, tvHTTPRequestName.Index] := 'UserAgent';
    Values[7, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.UserAgent;

    for I := 0 to AHTTPProcess.HTTPData.HTTPRequest.Cookies.Count - 1 do
    begin
      RecordCount := RecordCount + 1;
      Values[RecordCount - 1, tvHTTPRequestName.Index] := 'Cookie';
      Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.Cookies.Strings[I];
    end;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'CacheControl';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.CacheControl;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'CharSet';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.CharSet;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'Connection';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.Connection;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentDisposition';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.ContentDisposition;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentEncoding';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.ContentEncoding;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentLanguage';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.ContentLanguage;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentType';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.ContentType;

    for I := 0 to AHTTPProcess.HTTPData.HTTPRequest.CustomHeaders.Count - 1 do
    begin
      RecordCount := RecordCount + 1;
      Values[RecordCount - 1, tvHTTPRequestName.Index] := 'CustomHeader';
      Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPData.HTTPRequest.CustomHeaders.Strings[I];
    end;
  end;

  // only POST Requests have params
  if Assigned(AHTTPProcess.HTTPData.HTTPParams) then
  begin
    with tvHTTPProcess.DataController do
      CustomDataController := GetDetailDataController(RecordCount - 1, HTTPParams.Index);
    with CustomDataController do
    begin
      case AHTTPProcess.HTTPData.HTTPParams.ParamType of
        ptList, ptMultipartFormData:
          begin
            RecordCount := AHTTPProcess.HTTPData.HTTPParams.Count;

            for I := 0 to RecordCount - 1 do
            begin
              Values[I, tvHTTPParamsColumnName.Index] := AHTTPProcess.HTTPData.HTTPParams.FieldNames[I];
              if AHTTPProcess.HTTPData.HTTPParams.IsFile[I] then
                Values[I, tvHTTPParamsColumnValue.Index] := AHTTPProcess.HTTPData.HTTPParams.FieldFileNameFromIndex[I]
              else
                Values[I, tvHTTPParamsColumnValue.Index] := AHTTPProcess.HTTPData.HTTPParams.FieldValueFromIndex[I];
            end;
          end;
        ptData:
          begin
            RecordCount := 1;

            Values[0, tvHTTPParamsColumnName.Index] := 'RawData';
            Values[0, tvHTTPParamsColumnValue.Index] := AHTTPProcess.HTTPData.HTTPParams.RawData;
          end;
      end;
    end;
  end;

  with tvHTTPProcess.DataController do
    CustomDataController := GetDetailDataController(RecordCount - 1, HTTPResponse.Index);
  with CustomDataController do
  begin
    RecordCount := RecordCount + 6;

    Values[0, tvHTTPResponseName.Index] := 'Location';
    Values[0, tvHTTPResponseValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Location;

    Values[1, tvHTTPResponseName.Index] := 'Refresh';
    Values[1, tvHTTPResponseValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Refresh;

    Values[2, tvHTTPResponseName.Index] := 'Text';
    Values[2, tvHTTPResponseValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Text;

    Values[3, tvHTTPResponseName.Index] := 'Code';
    Values[3, tvHTTPResponseValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Code;

    Values[4, tvHTTPResponseName.Index] := 'Server';
    Values[4, tvHTTPResponseValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Server;

    Values[5, tvHTTPResponseName.Index] := 'Content';
    Values[5, tvHTTPResponseValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Content;

    for I := 0 to AHTTPProcess.HTTPResult.HTTPResponse.Cookies.Count - 1 do
    begin
      RecordCount := RecordCount + 1;
      Values[RecordCount - 1, tvHTTPRequestName.Index] := 'Cookie';
      Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Cookies.Strings[I];
    end;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'CacheControl';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.CacheControl;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'CharSet';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.CharSet;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'Connection';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.Connection;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentDisposition';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.ContentDisposition;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentEncoding';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.ContentEncoding;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentLanguage';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.ContentLanguage;

    RecordCount := RecordCount + 1;
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentLanguage';
    Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.ContentType;

    for I := 0 to AHTTPProcess.HTTPResult.HTTPResponse.CustomHeaders.Count - 1 do
    begin
      RecordCount := RecordCount + 1;
      Values[RecordCount - 1, tvHTTPRequestName.Index] := 'CustomHeader';
      Values[RecordCount - 1, tvHTTPRequestValue.Index] := AHTTPProcess.HTTPResult.HTTPResponse.CustomHeaders.Strings[I];
    end;
  end;
end;

end.
