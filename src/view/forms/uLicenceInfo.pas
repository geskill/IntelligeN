unit uLicenceInfo;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, Menus, StdCtrls,
  IdMultipartFormData, DECCipher, DECHash, DECFmt, XMLDoc, XMLIntf, Math,
  // Dev Express
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxEdit, cxTextEdit,
  cxGridCustomTableView, cxGridCardView, cxGridCustomView, cxClasses, cxGridLevel, cxGrid, cxLabel, cxButtons, cxGridCustomLayoutView, cxNavigator,
  // Api
  uApiConst, uApiMain,
  // Utils
  uVariantUtils;

type
  TLicenceInfo = class(TForm)
    cxGridLevel1: TcxGridLevel;
    cxGrid: TcxGrid;
    cxGridCardView1: TcxGridCardView;
    cxGridCardView1Row1: TcxGridCardViewRow;
    cxGridCardView1Row2: TcxGridCardViewRow;
    cxGridCardView1Row3: TcxGridCardViewRow;
    cxBCancel: TcxButton;
    cxBRefresh: TcxButton;
    cxGridCardView1Row4: TcxGridCardViewRow;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure cxBCancelClick(Sender: TObject);
    procedure cxBRefreshClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    procedure RefreshAccountInfo;
  end;

var
  LicenceInfo: TLicenceInfo;

implementation

uses
  uMain,
  // Api
  uApiSettings;

{$R *.dfm}

procedure TLicenceInfo.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TLicenceInfo.FormShow(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_ExStyle, WS_Ex_AppWindow);
end;

procedure TLicenceInfo.cxBCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TLicenceInfo.cxBRefreshClick(Sender: TObject);
begin
  RefreshAccountInfo;
end;

procedure TLicenceInfo.RefreshAccountInfo;
const
  u = 'acc/';
var
  Params: TIdMultiPartFormDataStream;
  ReplyData: TStringStream;
  XMLDoc: IXMLDocument;
  _a, _errormsg: string;
  I: Integer;
begin
  cxGridCardView1.DataController.RecordCount := 0;

  _errormsg := 'Unknown error';

  {
  with TApiHTTP.CreateAccounting do
    try
      Params := TIdMultiPartFormDataStream.Create;
      try
        Params.AddFormField('action', 'account_v1', 'utf-8');
        with TCipher_Rijndael.Create do
          try
            Mode := cmCFB8;
            Init(THash_MD5.CalcBinary(THash_SHA512.CalcBinary(Main.fLogin.eLoginpassword.Text, TFormat_MIME64), TFormat_HEXL));
            _a := EncodeBinary(Main.fLogin.eLoginname.Text, TFormat_MIME64);
          finally
            Free;
          end;
        Params.AddObject('username', '', '', TStringStream.Create(THash_SHA512.CalcBinary(Main.fLogin.eLoginpassword.Text, TFormat_MIME64), TEncoding.UTF8));
        Params.AddObject('password', '', '', TStringStream.Create(_a, TEncoding.UTF8));

        Params.Position := 0;

        ReplyData := TStringStream.Create('', CP_UTF8);
        try
          XMLDoc := NewXMLDocument;
          with XMLDoc do
          begin
            try
              Post(Homepage + u + copy(u, 2, 1) + '.php', Params, ReplyData);
            except
              on E: Exception do
              begin
                try
                  Post('http://geskill.bplaced.net/intelligen2009/' + u + copy(u, 2, 1) + '.php', Params, ReplyData);
                except
                  on E: Exception do
                  begin
                    MessageDlg(_errormsg, mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
              end;
            end;
            if ReplyData.DataString = '' then
              Exit;
            try
              LoadFromXML(ReplyData.DataString);
              Active := True;
            except
              on E: Exception do
              begin
                MessageDlg(_errormsg, mtError, [mbOK], 0);
                Exit;
              end;
            end;
          end;
          with XMLDoc.DocumentElement do
            if HasChildNodes then
            begin
              case ChildNodes.Nodes['code'].NodeValue of
                0:
                  MessageDlg('Your username or password is invalid', mtError, [mbOK], 0);
                1:
                  with ChildNodes.Nodes['msg'] do
                    if HasChildNodes then
                      with cxGridCardView1.DataController do
                      begin
                        BeginUpdate;
                        try
                          cxGridCardView1.DataController.RecordCount := ChildNodes.Count;

                          for I := 0 to ChildNodes.Count - 1 do
                            with ChildNodes.Nodes[I] do
                            begin
                              cxGridCardView1.DataController.Values[I, cxGridCardView1Row1.index] := VarToStr(ChildNodes.Nodes['key'].NodeValue);
                              cxGridCardView1.DataController.Values[I, cxGridCardView1Row2.index] := VarToStr(ChildNodes.Nodes['date'].NodeValue);
                              cxGridCardView1.DataController.Values[I, cxGridCardView1Row3.index] := VarToStr(ChildNodes.Nodes['days'].NodeValue)
                                + ' (+' + IntToStr(VarToIntDef(ChildNodes.Nodes['days'].NodeValue, 30) div 30) + ')';
                              //cxGridCardView1.DataController.Values[I, cxGridCardView1Row4.index] := ProgrammVersion[Min(length(ProgrammVersion) - 1,
                              //  VarToIntDef(ChildNodes.Nodes['type'].NodeValue, 0))];
                            end;
                        finally
                          EndUpdate;
                        end;
                      end;
                255:
                  MessageDlg(_errormsg, mtError, [mbOK], 0);
              end;
            end;
        finally
          ReplyData.Free;
        end;
      finally
        Params.Free;
      end;
    finally
      Free;
    end;
  }
end;

end.
