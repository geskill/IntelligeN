{ ********************************************************
  *                                                      *
  *  IntelligeN 2009 Linkgrabber for UL / NL / SO        *
  *  Version 1.0.0.0                                     *
  *  Copyright (c) 2011 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uLinkGrabber;

interface

uses
  Windows, SysUtils, Classes, Dialogs, Menus, HTTPApp, DateUtils, IniFiles, Generics.Collections, Math,
  // RegEx
  RegExpr,
  // LkJSON
  uLkJSON,
  // Common
  uConst, uAppInterface,
  // Utils
  uPathUtils, uSpecialStringUtils,
  // Plugin system
  uPlugInAppClass, uIdHTTPHelper;

type
  TNotifyMethod = procedure(const Sender: IUnknown) of object;

  TINotifyEvent = class(TInterfacedObject, INotifyEvent)
  private
    FOnNotify: TNotifyMethod;
  public
    property OnNotifyHandler: TNotifyMethod read FOnNotify write FOnNotify;
    procedure OnNotify(const Sender: IUnknown); stdcall;
  end;

  TLinkGrabber = class(TAppPlugIn)
  private
    FAppController: IAppController;
    FNewMenuItem: IMenuItem;
    FNotifyEvent: TINotifyEvent;
    procedure OnClick(const Sender: IUnknown);
    function GetULLinks: string;
    function GetNLLinks: string;
    function GetSOLinks: string;
  public
    function GetName: WideString; override; safecall;
    function Start(const AAppController: IAppController): Boolean; override; stdcall;
    procedure Stop; override; stdcall;
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

procedure TINotifyEvent.OnNotify;
begin
  if (@FOnNotify <> nil) then
    FOnNotify(Sender);
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
        LinkList.Text := GetULLinks + GetNLLinks + GetSOLinks;

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
  FileIndex, PageIndex, MaxPageNumber: Integer;
  Ini: TIniFile;
  Params, ReplyData, LinkListData, LinkListParams: TStringStream;
  UserName, Password: string;
  HasNextPage: Boolean;
  LinkList: TStringList;
  lkJSONobject: TlkJSONobject;

  function SmallHTTPEncode(AStr: string): string;
  begin
    result := StringReplace(AStr, '&', '%26', [rfReplaceAll]);
  end;

begin
  Ini := TIniFile.Create(ExtractFilePath(GetModulePath) + ChangeFileExt(ExtractFileName(GetModulePath), '.ini'));
  try
    UserName := Ini.ReadString('Uploaded.to', 'USERNAME', '');
    Password := Ini.ReadString('Uploaded.to', 'PASSWORD', '');
    MaxPageNumber := Ini.ReadInteger('Uploaded.to', 'MAXPAGES', 5);
  finally
    Ini.Free;
  end;

  if (Length(UserName) > 2) and (Length(Password) > 2) then
    with TIdHTTPHelper.Create(Self) do
      try
        RedirectMaximum := 1;
        Request.Referer := 'http://uploaded.to/';

        Params := TStringStream.Create('');
        ReplyData := TStringStream.Create('');
        try
          with Params do
          begin
            WriteString('id=' + SmallHTTPEncode(UserName) + '&');
            WriteString('pw=' + SmallHTTPEncode(Password));
          end;

          Post('http://uploaded.to/io/login', Params, ReplyData);

        finally
          ReplyData.Free;
          Params.Free;
        end;
        PageIndex := 0;
        HasNextPage := True;

        LinkList := TStringList.Create;
        try
          repeat
            LinkListData := TStringStream.Create('');
            LinkListParams := TStringStream.Create('page=' + IntToStr(PageIndex) + '&limit=100&order=date&dir=desc&search=');
            try
              Post('http://uploaded.to/io/me/list/files', LinkListParams, LinkListData);
              try
                lkJSONobject := TlkJSON.ParseText(LinkListData.DataString) as TlkJSONobject;

                HasNextPage := lkJSONobject.Field['listopts'].Field['hasNext'].Value;

                with lkJSONobject.Field['list'] do
                  for FileIndex := 0 to Count - 1 do
                    LinkList.Add('http://uploaded.to/file/' + Child[FileIndex].Field['id'].Value + '/' + Child[FileIndex].Field['filename'].Value);
              finally
                lkJSONobject.Free;
              end;
            finally
              LinkListParams.Free;
              LinkListData.Free;
            end;
            Inc(PageIndex);
            Sleep(150);
          until not HasNextPage or (PageIndex = MaxPageNumber);
          Result := LinkList.Text;
        finally
          LinkList.Free;
        end;
      finally
        Free;
      end;
end;

function TLinkGrabber.GetNLLinks: string;
var
  Ini: TIniFile;
  UserName, Password: string;
  Params, ReplyData, LinkListData
  { , LinkListParams } : TStringStream;
  LinkList: TStringList;
begin
  Ini := TIniFile.Create(ExtractFilePath(GetModulePath) + ChangeFileExt(ExtractFileName(GetModulePath), '.ini'));
  try
    UserName := Ini.ReadString('Netload.in', 'USERNAME', '');
    Password := Ini.ReadString('Netload.in', 'PASSWORD', '');
  finally
    Ini.Free;
  end;

  if (Length(UserName) > 2) and (Length(Password) > 2) then
    with TIdHTTPHelper.Create(Self) do
      try
        RedirectMaximum := 1;
        Request.Referer := 'http://netload.in/';

        Params := TStringStream.Create('');
        ReplyData := TStringStream.Create('');
        try
          with Params do
          begin
            WriteString('txtuser=' + HTTPEncode(UserName) + '&');
            WriteString('txtpass=' + HTTPEncode(Password) + '&');
            WriteString('txtcheck=login&txtlogin=');
          end;

          Post('http://netload.in/', Params, ReplyData);
        finally
          ReplyData.Free;
          Params.Free;
        end;

        LinkList := TStringList.Create;
        try
          LinkListData := TStringStream.Create('');
          // LinkListParams := TStringStream.Create('search_name=' + AName + '&folder_id=0&search=suchen');
          try
            Get('http://netload.in/index.php?id=36&folder_id=0&search_name=&max_results=500&order_by=&order_type=&order_by=upload_timestamp&order_type=desc',
              { LinkListParams, } LinkListData);

            with TRegExpr.Create do
              try
                InputString := LinkListData.DataString;
                Expression := '<a class="Orange_Link" href="(.*?)"';

                if Exec(InputString) then
                begin
                  repeat
                    LinkList.Add(Match[1]);
                  until not ExecNext;
                end;
              finally
                Free;
              end;
          finally
            // LinkListParams.Free;
            LinkListData.Free;
          end;
          Result := LinkList.Text;
        finally
          LinkList.Free;
        end;
      finally
        Free;
      end;
end;

function TLinkGrabber.GetSOLinks: string;
var
  Ini: TIniFile;
  UserName, Password: string;
  { Params, ReplyData, } LinkListData, LinkListParams: TStringStream;
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
    with TIdHTTPHelper.Create(Self) do
      try
        RedirectMaximum := 1;
        Request.Referer := 'http://www.share-online.biz/';

        (*
          Params := TStringStream.Create('');
          ReplyData := TStringStream.Create('');
          try
          with Params do
          begin
          WriteString('user=' + (UserName) + '&');
          WriteString('pass=' + (Password) + '&');
          WriteString('l_rememberme=1');
          end;

          Post('https://www.share-online.biz/user/login', Params, ReplyData);
          finally
          ReplyData.Free;
          Params.Free;
          end;

          *)
        LinkList := TStringList.Create;
        try
          LinkListData := TStringStream.Create('');
          LinkListParams := TStringStream.Create
            ('username=' + UserName + '&password=' + Password + '&act=files&f_num=500&f_start=0&f_end_date=' + IntToStr(DateTimeToUnix(Tomorrow))
              + '&f_order_by=uploaded&f_order_dir=desc&f_abuse=0');
          try
            Post('http://api.share-online.biz/account.php', LinkListParams, LinkListData);

            // LinkListData.SaveToFile('LinkListData_ShareOnline.htm');

            with TRegExpr.Create do
              try
                InputString := LinkListData.DataString;
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

          finally
            LinkListParams.Free;
            LinkListData.Free;
          end;
          Result := LinkList.Text;
        finally
          LinkList.Free;
        end;
      finally
        Free;
      end;
end;

function TLinkGrabber.GetName: WideString;
begin
  Result := 'Link Grabber';
end;

function TLinkGrabber.Start(const AAppController: IAppController): Boolean;
begin
  FAppController := AAppController;

  Result := True;

  FNotifyEvent := TINotifyEvent.Create;
  FNotifyEvent.OnNotifyHandler := OnClick;
  with FAppController.MainMenu.GetMenuItems.GetItem(3) do
    FNewMenuItem := InsertMenuItem(GetMenuItems.GetCount, 'LinkGraber', 'This grabs links', 0, -1, 0, FNotifyEvent);
end;

procedure TLinkGrabber.Stop;
begin
  FAppController.MainMenu.GetMenuItems.GetItem(3).GetMenuItems.RemoveItem(FNewMenuItem);
  FNotifyEvent := nil;
end;

end.
