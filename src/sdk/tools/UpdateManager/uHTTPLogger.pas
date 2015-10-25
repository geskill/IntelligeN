unit uHTTPLogger;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit,
  cxGridCustomTableView, cxGridTableView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, cxLabel, cxTextEdit, cxBlobEdit,
  cxNavigator,
  // HTTPManager
  uHTTPInterface, uHTTPConst, uHTTPEvent, uHTTPManager;

type
  THTTPLogger = class(TForm)
    HTTPProcess: TcxGridLevel;
    cxGrid: TcxGrid;
    tvHTTPProcess: TcxGridTableView;
    tvHTTPProcessMethod: TcxGridColumn;
    tvHTTPProcessURI: TcxGridColumn;
    tvHTTPProcessUniqueID: TcxGridColumn;
    tvHTTPProcessStatusCode: TcxGridColumn;
    HTTPRequest: TcxGridLevel;
    tvHTTPRequest: TcxGridTableView;
    tvHTTPRequestName: TcxGridColumn;
    tvHTTPRequestValue: TcxGridColumn;
    HTTPParams: TcxGridLevel;
    HTTPResponse: TcxGridLevel;
    tvHTTPParams: TcxGridTableView;
    tvHTTPResponse: TcxGridTableView;
    tvHTTPParamsColumnName: TcxGridColumn;
    tvHTTPParamsColumnValue: TcxGridColumn;
    tvHTTPResponseName: TcxGridColumn;
    tvHTTPResponseValue: TcxGridColumn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    FIHTTPProcessEventHandler: IHTTPProcessEventHandler;
  public
    procedure AddHTTPProcess(const AHTTPProcess: IHTTPProcess);
  end;

var
  HTTPLogger: THTTPLogger;

implementation

{$R *.dfm}

procedure THTTPLogger.FormCreate(Sender: TObject);
begin
  FIHTTPProcessEventHandler := TIHTTPProcessEventHandler.Create(AddHTTPProcess);

  /// calling this in uApiHTTP inside initialization block
  /// is problematic, because THTTPLogger is not created at
  /// this point.
  with THTTPManager.Instance() do
    OnRequestDone.Add(FIHTTPProcessEventHandler);
end;

procedure THTTPLogger.FormDestroy(Sender: TObject);
begin
  with THTTPManager.Instance() do
    OnRequestDone.Remove(FIHTTPProcessEventHandler);

  FIHTTPProcessEventHandler := nil;
end;

procedure THTTPLogger.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure THTTPLogger.FormShow(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_ExStyle, WS_Ex_AppWindow);
end;

procedure THTTPLogger.AddHTTPProcess(const AHTTPProcess: IHTTPProcess);
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
    Values[RecordCount - 1, tvHTTPRequestName.Index] := 'ContentLanguage';
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
