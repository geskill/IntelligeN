{ ********************************************************
  *                                                      *
  *  IntelligeN 2009 Linkgrabber for UL / NL / SO / FO   *
  *  Version 2.0.0.0                                     *
  *  Copyright (c) 2011 - 2013 Sebastian Klatte          *
  *                                                      *
  ******************************************************** }
unit uLinkGrabber;

interface

uses
  Windows, SysUtils, Classes, Dialogs, Menus, HTTPApp, DateUtils, IniFiles, Generics.Collections, Math,
  // RegEx
  RegExpr,
  // HTTPManager
  uHTTPInterface, uHTTPClasses,
  // MultiEvent
  Generics.MultiEvents.NotifyHandler,
  // LkJSON
  uLkJSON,
  // Common
  uConst, uAppInterface,
  // Utils
  uPathUtils, uStringUtils,
  // Plugin system
  uPlugInAppClass, uPlugInHTTPClasses;

type
  TLinkGrabber = class(TAppPlugIn)
  private
    FAppController: IAppController;
    FNewMenuItem: IMenuItem;
    FNotifyEventHandler: TINotifyEventHandler;
    procedure OnClick(const Sender: IUnknown);
    function GetULLinks: string;
    function GetSOLinks: string;
  public
    function GetName: WideString; override;
    function Start(const AAppController: IAppController): WordBool; override;
    procedure Stop; override;
  end;

implementation

function GetModulePath: string;
var
  QueryRes: TMemoryBasicInformation;
  LBuffer: string;
begin
  VirtualQuery(@GetModulePath, QueryRes, SizeOf(QueryRes));
  SetLength(LBuffer, MAX_PATH);
  SetLength(LBuffer, GetModuleFileName(Cardinal(QueryRes.AllocationBase), PChar(LBuffer), Length(LBuffer)));
  Result := LBuffer;
end;

{ TLinkGrabber }

procedure TLinkGrabber.OnClick(const Sender: IInterface);

  function GetRealFileName(AFileName: string): string;
  begin
    AFileName := Trim(AFileName);
    if SameText(ExtractFileExt(AFileName), '.htm') or SameText(ExtractFileExt(AFileName), '.html') then
      AFileName := ChangeFileExt(AFileName, '');
    Result := ChangeFileExt(ExtractUrlFileName(AFileName), '');
    if not(Pos('part', ExtractFileExt(Result)) = 0) then
      Result := ChangeFileExt(Result, '');
  end;

  procedure sorttotab(ALink: string; ATabSheetController: ITabSheetController = nil);
  var
    _MirrorIndex, _LinkListIndex: Integer;
    _LinkList: TList<string>;
    _Found: Boolean;

    FirstEmptyMirrorIndex: Integer;
    _Mirror, _Link: string;
  begin
    if not Assigned(ATabSheetController) then
      ATabSheetController := FAppController.PageController.ActiveTabSheetController;

    _LinkList := TList<string>.Create;
    try
      with ATabSheetController.MirrorController do
      begin
        for _MirrorIndex := 0 to MirrorCount - 1 do
          _LinkList.Add(Mirror[_MirrorIndex].DirectlinksMirror[0]);
      end;

      _Found := False;

      FirstEmptyMirrorIndex := -1;
      for _LinkListIndex := 0 to _LinkList.Count - 1 do
      begin
        _Mirror := _LinkList[_LinkListIndex];
        if not SameStr('', _Mirror) then
        begin
          _Link := _Mirror;
          if not(Pos(sLineBreak, _Link) = 0) then
            _Link := copy(_Link, 1, Pos(sLineBreak, _Link) - 1);

          if SameText(RemoveW(ExtractUrlHost(_Link)), RemoveW(ExtractUrlHost(ALink))) then
          begin
            with TStringList.Create do
              try
                Text := _Mirror;
                Add(ALink);
                _LinkList[_LinkListIndex] := Text;
              finally
                Free;
              end;
            _Found := True;
            break;
          end;
        end
        else
        begin
          if FirstEmptyMirrorIndex = -1 then
            FirstEmptyMirrorIndex := _LinkListIndex
          else
            FirstEmptyMirrorIndex := Min(FirstEmptyMirrorIndex, _LinkListIndex);
        end;
      end;

      if not _Found then
      begin
        if not(FirstEmptyMirrorIndex = -1) then
        begin
          _LinkList[FirstEmptyMirrorIndex] := ALink;
          _Found := True;
        end;
      end;
      if not _Found then
        _LinkList.Add(ALink);

      for _LinkListIndex := 0 to _LinkList.Count - 1 do
      begin
        with ATabSheetController.MirrorController do
          if MirrorCount <= _LinkListIndex then
            Mirror[Add].Directlink.Add(_LinkList.Items[_LinkListIndex])
          else
            Mirror[_LinkListIndex].DirectlinksMirror[0] := _LinkList.Items[_LinkListIndex];
      end;
      FAppController.PageController.CallComponentparser;
    finally
      _LinkList.Free;
    end;
  end;

var
  I, J: Integer;
  LinkList: TStringList;
begin
  with FAppController.PageController do
  begin
    if (TabSheetCount > 0) then
    begin

      LinkList := TStringList.Create;
      try
        LinkList.Text := GetULLinks + GetSOLinks;

        for I := 0 to LinkList.Count - 1 do
          for J := 0 to TabSheetCount - 1 do
          begin
            if SameText(TabSheetController[J].ReleaseName, GetRealFileName(LinkList.Strings[I])) then
              sorttotab(LinkList.Strings[I], TabSheetController[J]);
          end;
      finally
        LinkList.Free;
      end;
    end
    else
      MessageDlg('First of all add a new tab!', mtWarning, [mbOK], 0);
  end;
end;

function TLinkGrabber.GetULLinks: string;
var
  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;
  HTTPOptions: IHTTPOptions;
  RequestID: Double;

  FileIndex, PageIndex, MaxPageNumber: Integer;
  Ini: TIniFile;
  LinkListData, LinkListParams: TStringStream;
  UserName, Password: string;
  HasNextPage: Boolean;
  LinkList: TStringList;
  lkJSONobject: TlkJSONobject;
begin
  Ini := TIniFile.Create(ExtractFilePath(GetModulePath) + ChangeFileExt(ExtractFileName(GetModulePath), '.ini'));
  try
    UserName := Ini.ReadString('Uploaded.net', 'USERNAME', '');
    Password := Ini.ReadString('Uploaded.net', 'PASSWORD', '');
    MaxPageNumber := Ini.ReadInteger('Uploaded.net', 'MAXPAGES', 5);
  finally
    Ini.Free;
  end;

  if (Length(UserName) > 2) and (Length(Password) > 2) then
  begin

    HTTPRequest := THTTPRequest.Create('http://uploaded.net/io/login');
    with HTTPRequest do
    begin
      Referer := 'http://uploaded.net/';
    end;

    HTTPParams := THTTPParams.Create;
    with HTTPParams do
    begin
      AddFormField('id', UserName);
      AddFormField('pw', Password);
    end;

    HTTPOptions := TPlugInHTTPOptions.Create(Self);
    with HTTPOptions do
    begin
      RedirectMaximum := 1;
    end;

    RequestID := HTTPManager.Post(HTTPRequest, HTTPParams, HTTPOptions);

    repeat
      sleep(50);
    until HTTPManager.HasResult(RequestID);

    if not HTTPManager.GetResult(RequestID).HTTPResult.HasError then
    begin
      PageIndex := 0;
      HasNextPage := True;

      LinkList := TStringList.Create;
      try
        repeat
          HTTPParams := THTTPParams.Create;
          with HTTPParams do
          begin
            AddFormField('page', IntToStr(PageIndex));
            AddFormField('limit', '100');
            AddFormField('order', 'date');
            AddFormField('dir', 'desc');
            AddFormField('search', '');
          end;

          RequestID := HTTPManager.Post('http://uploaded.net/io/me/list/files', RequestID, HTTPParams, TPlugInHTTPOptions.Create(Self));

          repeat
            sleep(50);
          until HTTPManager.HasResult(RequestID);

          try
            lkJSONobject := TlkJSON.ParseText(HTTPManager.GetResult(RequestID).HTTPResult.SourceCode) as TlkJSONobject;

            HasNextPage := lkJSONobject.Field['listopts'].Field['hasNext'].Value;

            with lkJSONobject.Field['list'] do
              for FileIndex := 0 to Count - 1 do
                LinkList.Add('http://uploaded.net/file/' + Child[FileIndex].Field['id'].Value + '/' + Child[FileIndex].Field['filename'].Value);
          finally
            lkJSONobject.Free;
          end;

          Inc(PageIndex);

          sleep(150);
        until not HasNextPage or (PageIndex = MaxPageNumber);

        Result := LinkList.Text;
      finally
        LinkList.Free;
      end;
    end;
  end;
end;

function TLinkGrabber.GetSOLinks: string;
var
  HTTPRequest: IHTTPRequest;
  HTTPParams: IHTTPParams;
  HTTPOptions: IHTTPOptions;
  RequestID: Double;

  Ini: TIniFile;
  UserName, Password: string;

  LinkList: TStringList;
begin
  Ini := TIniFile.Create(ExtractFilePath(GetModulePath) + ChangeFileExt(ExtractFileName(GetModulePath), '.ini'));
  try
    UserName := Ini.ReadString('Share-online.biz', 'USERNAME', '');
    Password := Ini.ReadString('Share-online.biz', 'PASSWORD', '');
  finally
    Ini.Free;
  end;

  if (Length(UserName) > 2) and (Length(Password) > 2) then
  begin

    HTTPRequest := THTTPRequest.Create('http://api.share-online.biz/account.php');
    with HTTPRequest do
    begin
      Referer := 'http://www.share-online.biz/';
    end;

    HTTPParams := THTTPParams.Create;
    with HTTPParams do
    begin
      AddFormField('username', UserName);
      AddFormField('password', Password);

      AddFormField('act', 'files');

      AddFormField('f_num', '500');
      AddFormField('f_start', '0');
      AddFormField('f_end_date', IntToStr(DateTimeToUnix(Tomorrow)));
      AddFormField('f_order_by', 'uploaded');
      AddFormField('f_order_dir', 'desc');
      AddFormField('f_abuse', '0');
    end;

    HTTPOptions := TPlugInHTTPOptions.Create(Self);
    with HTTPOptions do
    begin
      RedirectMaximum := 1;
    end;

    RequestID := HTTPManager.Post(HTTPRequest, HTTPParams, HTTPOptions);

    repeat
      sleep(50);
    until HTTPManager.HasResult(RequestID);

    if not HTTPManager.GetResult(RequestID).HTTPResult.HasError then
    begin

      LinkList := TStringList.Create;
      try
        with TRegExpr.Create do
          try
            InputString := HTTPManager.GetResult(RequestID).HTTPResult.SourceCode;
            Expression := '(\S+);(\w+);(\w{32});(\d+);(\d+);(\d+);(\d+)';

            if Exec(InputString) then
            begin
              repeat
                LinkList.Add('http://www.share-online.biz/dl/' + Match[2] + '/' + Match[1]);
              until not ExecNext;
            end;
          finally
            Free;
          end;
        Result := LinkList.Text;
      finally
        LinkList.Free;
      end;

    end;
  end;
end;

function TLinkGrabber.GetName: WideString;
begin
  Result := 'Link Grabber';
end;

function TLinkGrabber.Start(const AAppController: IAppController): WordBool;
begin
  FAppController := AAppController;

  Result := True;

  FNotifyEventHandler := TINotifyEventHandler.Create(OnClick);
  with FAppController.MainMenu.GetMenuItems.GetItem(3) do
    FNewMenuItem := InsertMenuItem(GetMenuItems.GetCount, 'LinkGraber', 'This grabs links', 0, -1, 0, FNotifyEventHandler);
end;

procedure TLinkGrabber.Stop;
begin
  FAppController.MainMenu.GetMenuItems.GetItem(3).GetMenuItems.RemoveItem(FNewMenuItem);
  FNotifyEventHandler := nil;
end;

end.
