unit uApiBackupManager;

interface

uses
  // Delphi
  SysUtils, Classes, DB, Dialogs,
  // Common
  uConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst;

type
  TBackupManager = class
  private
    //FADConnection: TADConnection;
    //FADQuery: TADQuery;

  const
    BackupReleases = 'int2k9_releases';
    BackupDownloadlinks = 'int2k9_downloadlinks';
    BackupCrypterlinks = 'int2k9_crypterlinks';
    BackupCmslinks = 'int2k9_cmslinks';
    BackupStructureFilename = 'int2k9_database.sql';
    procedure CreateTables;
  public
    constructor Create;
    procedure Backup(ATabSheetController: ITabSheetController);
    destructor Destroy; override;
  end;

implementation

procedure TBackupManager.CreateTables;
begin
  with TStringList.Create do
    try
      //LoadFromFile(GetConfigurationFolder + BackupStructureFilename);
      //FADConnection.ExecSQL(Text);
    finally
      Free;
    end;
end;

constructor TBackupManager.Create;
begin
  //FADConnection := TADConnection.Create(nil);

  //FADQuery := TADQuery.Create(nil);
  //FADQuery.Connection := FADConnection;

  {
  with FADConnection do
  begin
    LoginPrompt := false;

    Params.Add('DriverID=SQLite');
    Params.Add('Database=' + GetSettingsFolder + BackupFilename);
  end;
  }

  if not FileExists(GetSettingsFolder + BackupFilename) then
    CreateTables;
end;

procedure TBackupManager.Backup(ATabSheetController: ITabSheetController);
var
  StringTemplateTypeID, Releasename, s: string;
  //I, J, maxid, relid: Integer;
begin
  with ATabSheetController do
    if not(ComponentController.FindControl(cReleaseName) = nil) then
    begin
      StringTemplateTypeID := TTemplateTypeIDToString
        (ComponentController.FindControl(cReleaseName).TemplateTypeID);
      Releasename := ComponentController.FindControl(cReleaseName).Value;

      if Length(Releasename) > 3 then
      begin
{$REGION 'releases'}{
        FADQuery.Open('select id from ' + BackupReleases + ' where ' + TComponentIDToString
            (cReleaseName) + ' = ''' + Releasename + '''');

        if FADQuery.RecordCount = 0 then
        begin
          FADQuery.SQL.Text := 'select max(id) as maxid from ' + BackupReleases;
          FADQuery.OpenOrExecute;
          maxid := StrToIntDef(FADQuery.FieldByName('maxid').AsString, 0) + 1;

          s := 'insert into ' + BackupReleases + ' (id, IType, ';

          for I := 0 to ComponentController.ControlCount - 1 do
          begin
            s := s + TComponentIDToString(ComponentController.Control[I].ComponentID);

            if not(I = (ComponentController.ControlCount - 1)) then
              s := s + ', ';
          end;

          s := s + ') values(' + IntToStr(maxid) + ', ''' + StringTemplateTypeID + ''', ';

          for I := 0 to ComponentController.ControlCount - 1 do
          begin
            s := s + '''' + StringReplace(ComponentController.Control[I].Value, '''', '''''', [rfReplaceAll])
              + '''';

            if not(I = (ComponentController.ControlCount - 1)) then
              s := s + ', ';
          end;

          s := s + ')';

          FADQuery.SQL.Text := s;
          FADQuery.ExecSQL;
        end
        else
        begin
          maxid := FADQuery.FieldByName('id').AsInteger;

          s := 'update ' + BackupReleases + ' set';

          for I := 0 to ComponentController.ControlCount - 1 do
          begin
            s := s + ' ' + TComponentIDToString(ComponentController.Control[I].ComponentID)
              + ' = ''' + StringReplace(ComponentController.Control[I].Value, '''', '''''', [rfReplaceAll])
              + '''';

            if not(I = (ComponentController.ControlCount - 1)) then
              s := s + ', ';
          end;

          s := s + ' where id = ' + IntToStr(maxid);

          FADQuery.SQL.Text := s;
          FADQuery.ExecSQL;
        end;

        relid := maxid;   }
{$ENDREGION}
{$REGION 'downloadlinks'}       {
        FADQuery.Open('select id, mirid, submirid from ' + BackupDownloadlinks + ' where relid = ''' + IntToStr
            (relid) + '''');

        if not(FADQuery.RecordCount = 0) then
        begin
          s := '';
          for I := 0 to FADQuery.RecordCount - 1 do
          begin
            maxid := FADQuery.FieldByName('id').AsInteger;

            s := s + 'delete from ' + BackupDownloadlinks + ' where id = ''' + IntToStr(maxid) + ''';';
            FADQuery.Next;
          end;

          FADQuery.SQL.Text := s;
          FADQuery.ExecSQL;
        end;

        if FADQuery.RecordCount = 0 then
        begin
          for I := 0 to MirrorController.MirrorCount - 1 do
            for J := 0 to MirrorController.Mirror[I].DirectlinksMirrorCount - 1 do
            begin
              FADQuery.SQL.Text := 'select max(id) as maxid from ' + BackupDownloadlinks;
              FADQuery.OpenOrExecute;
              maxid := StrToIntDef(FADQuery.FieldByName('maxid').AsString, 0) + 1;

              s := 'insert into ' + BackupDownloadlinks + ' (id, relid, mirid, submirid, direktlinks';

              s := s + ') values(' + IntToStr(maxid) + ', ' + IntToStr(relid) + ', ' + IntToStr(I)
                + ', ' + IntToStr(J) + ', ''' + MirrorController.Mirror[I].DirectlinksMirror[J] + ''')';

              FADQuery.SQL.Text := s;
              FADQuery.ExecSQL;
            end;
        end;      }
{$ENDREGION}
{$REGION 'crypterlinks'}  {
        FADQuery.Open('select id from ' + BackupCrypterlinks + ' where relid = ''' + IntToStr(relid) + '''');

        if not(FADQuery.RecordCount = 0) then
        begin
          s := '';
          for I := 0 to FADQuery.RecordCount - 1 do
          begin
            maxid := FADQuery.FieldByName('id').AsInteger;

            s := s + 'delete from ' + BackupCrypterlinks + ' where id = ''' + IntToStr(maxid) + ''';';
            FADQuery.Next;
          end;

          FADQuery.SQL.Text := s;
          FADQuery.ExecSQL;
        end;

        if FADQuery.RecordCount = 0 then
        begin
          for I := 0 to MirrorController.MirrorCount - 1 do
            for J := 0 to MirrorController.Mirror[I].CrypterCount - 1 do
            begin
              FADQuery.SQL.Text := 'select max(id) as maxid from ' + BackupCrypterlinks;
              FADQuery.OpenOrExecute;
              maxid := StrToIntDef(FADQuery.FieldByName('maxid').AsString, 0) + 1;

              s := 'insert into ' + BackupCrypterlinks + ' (id, relid, mirid, crypter, link, hoster, size, online';

              s := s + ') values(' + IntToStr(maxid) + ', ' + IntToStr(relid) + ', ' + IntToStr(I)
                + ', ''' + MirrorController.Mirror[I].Crypter[J].Name + ''', ''' + MirrorController.Mirror[I]
                .Crypter[J].Link + ''', ''' + MirrorController.Mirror[I].Crypter[J]
                .CrypterFolderInfo.Hoster + ''', ''' + FloatToStr
                (MirrorController.Mirror[I].Crypter[J].CrypterFolderInfo.Size) + ''', ' + IntToStr
                (MirrorController.Mirror[I].Crypter[J].CrypterFolderInfo.Status) + ')';

              FADQuery.SQL.Text := s;
              FADQuery.ExecSQL;
            end;
        end;     }
{$ENDREGION}
      end;
    end;
end;

destructor TBackupManager.Destroy;
begin
  // FADQuery.Free;
  // FADConnection.Free;
end;

end.
