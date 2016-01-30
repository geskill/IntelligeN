unit uApiXml;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes, Forms, Controls, TypInfo, Variants, XMLDoc, XMLIntf, ActiveX, Generics.Collections,
  // RegEx
  RegExpr,
  // Common
  uBaseConst, uAppConst, uAppInterface, uFileInterface,
  // DLLs
  uExport,
  // Api
  uApiCodeTag, uApiFile,
  // Utils
  uFileUtils, uStringUtils, uVariantUtils, uURLUtils;

type
  TTemplateInfo = record
    TemplateType: TTypeID;
    TemplateFileName: TFileName;
    TemplateChecksum: string;
  end;

  TApiXml = class
  public
    class function GetControlsTemplateInfo(const AFileName: string): TTemplateInfo;
  end;

  TWebsiteTemplateHelper = class
  private const
    XML_ID = 'id';
    XML_NAME = 'name';
    XML_TYPE = 'type';
    XML_CHARSET = 'charset';
    XML_SETTINGS = 'settings';
    XML_FILTERS = 'filters';
  public
    class function Import(const AFileName: TFileName): IWebsiteConfigurationFile;
    class function Load(const AFileName: TFileName): IIntelligeNConfigurationFile;
    class procedure Save(const AFileName: TFileName; AWebsiteConfigurationFile: IWebsiteConfigurationFile; AForceSave: Boolean = False);
  end;

  TXMLProc = reference to procedure(AXMLNode: IXMLNode);

  THosterConfiguration = class
  private const
    HosterXML: string = 'hoster.xml';

    class procedure LoadXML(AXMLProc: TXMLProc);
    class function GetHoster(const AHoster, AAttribute: string): string;
  public
    class function GetHosters: string;
    class function GetCustomisedHoster(const AHoster: string; AShortName: Boolean = False): string;
  end;

  TCodeDefinition = record
    CodeTags: array [0 .. 18 - 1] of TCodeTag;
  end;

  TCodeDefinitions = class
  private const
    CodeDefinitionsXML: string = 'codedef.xml';

    class procedure LoadXML(AXMLProc: TXMLProc);
  public
    class function GetCodeDefinitions: TStrings;
    class function GetCodeDefinition(const ACodeDefinition: string): TCodeDefinition;
  end;

procedure GetControls(const AFileName: string; const AControlController: IControlController; const APageController: IPageController);

implementation

class function TApiXml.GetControlsTemplateInfo(const AFileName: string): TTemplateInfo;
var
  LNeedToUninitialize: Boolean;
  LXMLDoc: IXMLDocument;
  LTemplateInfo: TTemplateInfo;
begin
  LNeedToUninitialize := Succeeded(CoInitialize(nil));
  try
    LXMLDoc := NewXMLDocument;
    try
      with LXMLDoc do
      begin
        LoadFromFile(AFileName);
        Active := True;
      end;

      with LXMLDoc.DocumentElement do
        if HasChildNodes then
          with ChildNodes.Nodes['templatetype'] do
          begin
            with LTemplateInfo do
            begin
              TemplateType := StringToTypeID(VarToStr(NodeValue));
              TemplateFileName := VarToStr(Attributes['filename']);
              TemplateChecksum := VarToStr(Attributes['checksum']);
            end;
          end;

      Result := LTemplateInfo;
    finally
      LXMLDoc := nil;
    end;
  finally
    if LNeedToUninitialize then
      CoUninitialize;
  end;
end;

{ TWebsiteTemplateHelper }

class function TWebsiteTemplateHelper.Import;
type
  TMultiPosterVersion = (vMPU, vMP4);

  function GetForumType(AWebsiteType: string): string;
  const
    ForumType: array [0 .. 5, 0 .. 2] of string = (('ipb2', 'IPBv2', '2'), ('ipb3', 'IPBv3', '3'), ('MyBB', '', ''), ('phpbb2', 'phpbb2', '0'), ('phpbb3', 'phpbb3', '1'), ('vBulletin', 'vBulletin', '4'));

    function GetForumIndex(AWebsiteType: string): Integer;
    var
      I: Integer;
    begin
      Result := -1;
      for I := low(ForumType) to high(ForumType) do
        if SameText(AWebsiteType, ForumType[I, 0]) or SameText(AWebsiteType, ForumType[I, 1]) or SameText(AWebsiteType, ForumType[I, 2]) then
        begin
          Result := I;
          Break;
        end;
    end;

  begin
    try
      Result := ForumType[GetForumIndex(AWebsiteType), 0];
    except
      Result := '';
    end;
  end;

  function GetWebsiteNameCaption(AMultiPosterVersion: TMultiPosterVersion): string;
  begin
    case AMultiPosterVersion of
      vMPU:
        Result := 'ForumURL';
      vMP4:
        Result := 'siteURL';
    end;
  end;

  function GetWebsiteTypeCaption(AMultiPosterVersion: TMultiPosterVersion): string;
  begin
    case AMultiPosterVersion of
      vMPU:
        Result := 'ForumType';
      vMP4:
        Result := 'siteType';
    end;
  end;

  function CleanUP(AValue: Variant): string;
  begin
    Result := '';
    if AValue = Null then
      Exit;
    with TRegExpr.Create do
      try
        if ReplaceRegExpr('\s+', AValue, '', False) = '' then
          Exit;
      finally
        Free;
      end;
    Result := AValue;
  end;

var
  LNeedToUninitialize: Boolean;
  MultiPosterVersion: TMultiPosterVersion;
  XMLDoc: IXMLDocument;

  ID: IID;
  _Type: IType;
  _SubType: ISubType;
begin
  Result := TWebsiteConfigurationFile.Create;

  MultiPosterVersion := TMultiPosterVersion(IndexText(ExtractFileExt(AFileName), ['.mpu', '.ep4']));
  LNeedToUninitialize := Succeeded(CoInitialize(nil));
  try
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        Options := Options + [doNodeAutoIndent];
        ParseOptions := ParseOptions + [poPreserveWhiteSpace];
        NodeIndentStr := '  ';

        LoadFromFile(AFileName);
        Active := True;

        with DocumentElement do
          if HasChildNodes then
            with Result do
            begin
              WebsiteURL := IncludeTrailingUrlDelimiter(VarToStr(ChildNodes.Nodes[GetWebsiteNameCaption(MultiPosterVersion)].NodeValue));
              WebsiteType := GetForumType(VarToStr(ChildNodes.Nodes[GetWebsiteTypeCaption(MultiPosterVersion)].NodeValue));
              WebsiteCharset := '';
              if (MultiPosterVersion = vMPU) then
              begin
                ID := TID.Create;
                ID.Name := 'forums';

                if Assigned(ChildNodes.FindNode('MusicArea')) or Assigned(ChildNodes.FindNode('MusicAlbums')) then
                begin
                  _Type := TType.Create(cAudio);
                  if Assigned(ChildNodes.FindNode('MusicArea')) then
                    _Type.ID := CleanUP(ChildNodes.Nodes['MusicArea'].NodeValue)
                  else
                    _Type.ID := CleanUP(ChildNodes.Nodes['MusicAlbums'].NodeValue);
                  ID.Types.Add(_Type);
                end;

                _Type := TType.Create(cMovie, CleanUP(ChildNodes.Nodes['MoviesArea'].NodeValue));

                _SubType := TSubType.Create(cVideoCodec, 'MPEG-2', CleanUP(ChildNodes.Nodes['DvdMovies'].NodeValue));
                _Type.SubTypes.Add(_SubType);

                _SubType := TSubType.Create(cVideoCodec, 'VC-1', CleanUP(ChildNodes.Nodes['BluRayMovies'].NodeValue));
                _Type.SubTypes.Add(_SubType);

                _SubType := TSubType.Create(cVideoStream, 'BD', CleanUP(ChildNodes.Nodes['BluRayMovies'].NodeValue));
                _Type.SubTypes.Add(_SubType);

                _SubType := TSubType.Create(cVideoStream, 'CAM', CleanUP(ChildNodes.Nodes['CamMovies'].NodeValue));
                _Type.SubTypes.Add(_SubType);

                _SubType := TSubType.Create(cVideoStream, 'TS', CleanUP(ChildNodes.Nodes['TelesyncMovies'].NodeValue));
                _Type.SubTypes.Add(_SubType);

                _SubType := TSubType.Create(cVideoStream, 'SCR', CleanUP(ChildNodes.Nodes['ScreenerMovies'].NodeValue));
                _Type.SubTypes.Add(_SubType);

                _SubType := TSubType.Create(cVideoStream, 'R5', CleanUP(ChildNodes.Nodes['R5Movies'].NodeValue));
                _Type.SubTypes.Add(_SubType);

                ID.Types.Add(_Type);

                if Assigned(ChildNodes.FindNode('NintendoDsGames')) then
                  ID.Types.Add(TType.Create(cNintendoDS, CleanUP(ChildNodes.Nodes['NintendoDsGames'].NodeValue)));

                if Assigned(ChildNodes.FindNode('PcGames')) then
                  ID.Types.Add(TType.Create(cPCGames, CleanUP(ChildNodes.Nodes['PcGames'].NodeValue)));

                if Assigned(ChildNodes.FindNode('Ps2Games')) then
                  ID.Types.Add(TType.Create(cPlayStation3, CleanUP(ChildNodes.Nodes['Ps2Games'].NodeValue)));

                if Assigned(ChildNodes.FindNode('Ps3Games')) then
                  ID.Types.Add(TType.Create(cPlayStation3, CleanUP(ChildNodes.Nodes['Ps3Games'].NodeValue)));

                if Assigned(ChildNodes.FindNode('PspGames')) then
                  ID.Types.Add(TType.Create(cPlayStationVita, CleanUP(ChildNodes.Nodes['PspGames'].NodeValue)));

                if Assigned(ChildNodes.FindNode('Applications')) then
                  ID.Types.Add(TType.Create(cSoftware, CleanUP(ChildNodes.Nodes['Applications'].NodeValue)));

                if Assigned(ChildNodes.FindNode('NintendoWiiGames')) then
                  ID.Types.Add(TType.Create(cWii, CleanUP(ChildNodes.Nodes['NintendoWiiGames'].NodeValue)));

                if Assigned(ChildNodes.FindNode('XboxGames')) then
                  ID.Types.Add(TType.Create(cXbox360, CleanUP(ChildNodes.Nodes['XboxGames'].NodeValue)));

                if Assigned(ChildNodes.FindNode('Xbox360Games')) then
                  ID.Types.Add(TType.Create(cXbox360, CleanUP(ChildNodes.Nodes['Xbox360Games'].NodeValue)));

                _Type := TType.Create(cXXX, CleanUP(ChildNodes.Nodes['AdultVideos'].NodeValue));

                _SubType := TSubType.Create(cReleaseName, '%iMAGESET%', CleanUP(ChildNodes.Nodes['AdultPictureMegathreads'].NodeValue));
                _SubType.SubTypes.Add(TSubType.Create(cGenre, 'Amateur', CleanUP(ChildNodes.Nodes['AdultAmateurPictures'].NodeValue)));
                _SubType.SubTypes.Add(TSubType.Create(cGenre, 'Hentai', CleanUP(ChildNodes.Nodes['AdultHentaiPictures'].NodeValue)));
                _Type.SubTypes.Add(_SubType);

                _SubType := TSubType.Create(cReleaseName, '%XXX%', CleanUP(ChildNodes.Nodes['AdultMovies'].NodeValue));
                _SubType.SubTypes.Add(TSubType.Create(cGenre, 'Amateur', CleanUP(ChildNodes.Nodes['AdultHentaiMovies'].NodeValue)));
                _Type.SubTypes.Add(_SubType);

                ID.Types.Add(_Type);

                IDs.Add(ID);
              end;
              Changed := False;
            end;
      end;
    finally
      XMLDoc := nil;
    end;
  finally
    if LNeedToUninitialize then
      CoUninitialize;
  end;
end;

class function TWebsiteTemplateHelper.Load;
var
  LNeedToUninitialize: Boolean;
  XMLDoc: IXMLDocument;
  I: Integer;

  Control: IControl;
  HosterType: THosterType;
  Hoster: IHoster;
begin
  Result := TIntelligeNConfigurationFile.Create;

  if FileExists(AFileName) then
  begin
    LNeedToUninitialize := Succeeded(CoInitialize(nil));
    try
      XMLDoc := NewXMLDocument;
      try
        with XMLDoc do
        begin
          Options := Options + [doNodeAutoIndent];
          NodeIndentStr := #9;
          LoadFromFile(AFileName);
          Active := True;

          with DocumentElement do
            if HasChildNodes then
              with Result do
              begin
                WebsiteURL := VarToStr(ChildNodes.Nodes[XML_NAME].NodeValue);
                WebsiteType := VarToStr(ChildNodes.Nodes[XML_TYPE].NodeValue);
                WebsiteCharset := VarToStr(ChildNodes.Nodes[XML_CHARSET].NodeValue);

                // IDs

                if Assigned(ChildNodes.FindNode(XML_FILTERS)) then
                  with ChildNodes.Nodes[XML_FILTERS] do
                    with WebsiteFilter do
                    begin
                      Active := VarToBoolDef(Attributes['active'], False);

                      Categories := VarToStrDef(ChildNodes.Nodes['categories'].NodeValue, '');

                      if Assigned(ChildNodes.FindNode('controls')) then
                        with ChildNodes.Nodes['controls'] do
                          for I := 0 to ChildNodes.Count - 1 do
                          begin
                            Control := TControl.Create;
                            with Control do
                            begin
                              Category := VarToStr(ChildNodes.Nodes[I].Attributes['category']);
                              Name := VarToStr(ChildNodes.Nodes[I].Attributes[XML_NAME]);
                              Relation := VarToStr(ChildNodes.Nodes[I].Attributes['rel']);
                              Value := VarToStr(ChildNodes.Nodes[I].NodeValue);
                            end;
                            Controls.Add(Control);
                          end;

                      for HosterType := low(THosterType) to high(THosterType) do
                        if Assigned(ChildNodes.FindNode(GetHosterTypeName(HosterType))) then
                          with ChildNodes.Nodes[GetHosterTypeName(HosterType)] do
                          begin
                            Hoster := THoster.Create;
                            with Hoster do
                            begin
                              Name := GetHosterTypeName(HosterType);
                              Ranked := StrToBoolDef(VarToStr(Attributes['ranked']), False);

                              if not SameStr('', VarToStr(ChildNodes.Nodes['blacklist'].NodeValue)) then
                                with SplittString(';', VarToStr(ChildNodes.Nodes['blacklist'].NodeValue)) do
                                  try
                                    Blacklist.Text := Text;
                                  finally
                                    Free;
                                  end;

                              if not SameStr('', VarToStr(ChildNodes.Nodes['whitelist'].NodeValue)) then
                                with SplittString(';', VarToStr(ChildNodes.Nodes['whitelist'].NodeValue)) do
                                  try
                                    Whitelist.Text := Text;
                                  finally
                                    Free;
                                  end;
                            end;
                            Hosters.Add(Hoster);
                          end;
                    end;

                Changed := False;
              end;
        end;
      finally
        XMLDoc := nil;
      end;
    finally
    if LNeedToUninitialize then
      CoUninitialize;
    end;
  end;
end;

class procedure TWebsiteTemplateHelper.Save;

  procedure RecursiveSubItemAdd(ASubTypes: TList<ISubType>; AXMLNode: IXMLNode);
  var
    SubType: ISubType;
    XMLNode: IXMLNode;
  begin
    for SubType in ASubTypes do
    begin
      XMLNode := AXMLNode.AddChild('subtype');

      with XMLNode do
      begin
        Attributes[XML_TYPE] := SubType.ControlName;
        Attributes['value'] := SubType.ControlValue;
        Attributes[XML_ID] := SubType.ID;
      end;

      RecursiveSubItemAdd(SubType.SubTypes, XMLNode);
    end;
  end;

var
  LNeedToUninitialize: Boolean;
  XMLDoc: IXMLDocument;
  XMLNode: IXMLNode;

  ID: IID;
  _Type: IType;
  IntelligeNConfigurationFile: IIntelligeNConfigurationFile;

  Control: IControl;
  Hoster: IHoster;
begin
  LNeedToUninitialize := Succeeded(CoInitialize(nil));
  try
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        Options := Options + [doNodeAutoIndent];
        NodeIndentStr := #9;

        if not FileExists(AFileName) then
          DocumentElement := CreateElement('xml', '')
        else
          LoadFromFile(AFileName);

        Active := True;
        Encoding := 'utf-8';

        with DocumentElement do
        begin
          if ChildNodes.FindNode(XML_NAME) = nil then
            AddChild(XML_NAME);
          ChildNodes.Nodes[XML_NAME].NodeValue := AWebsiteConfigurationFile.WebsiteURL;

          if ChildNodes.FindNode(XML_TYPE) = nil then
            AddChild(XML_TYPE);
          ChildNodes.Nodes[XML_TYPE].NodeValue := AWebsiteConfigurationFile.WebsiteType;

          if ChildNodes.FindNode(XML_CHARSET) = nil then
            AddChild(XML_CHARSET);
          ChildNodes.Nodes[XML_CHARSET].NodeValue := AWebsiteConfigurationFile.WebsiteCharset;

          if ChildNodes.FindNode(XML_SETTINGS) = nil then
            AddChild(XML_SETTINGS);

          for ID in AWebsiteConfigurationFile.IDs do
          begin
            if ChildNodes.FindNode(ID.Name) = nil then
              AddChild(ID.Name);
            with ChildNodes.Nodes[ID.Name] do
            begin
              ChildNodes.Clear;

              for _Type in ID.Types do
              begin
                XMLNode := AddChild(XML_TYPE);
                with XMLNode do
                begin
                  Attributes[XML_NAME] := _Type.Name;
                  Attributes[XML_ID] := _Type.ID;
                end;
                RecursiveSubItemAdd(_Type.SubTypes, XMLNode);
              end;
            end;
          end;

          if AWebsiteConfigurationFile.QueryInterface(IIntelligeNConfigurationFile, IntelligeNConfigurationFile) = 0 then
            try
              if ChildNodes.FindNode(XML_FILTERS) = nil then
                AddChild(XML_FILTERS);
              with ChildNodes.Nodes[XML_FILTERS] do
              begin
                Attributes['active'] := IntelligeNConfigurationFile.WebsiteFilter.Active;

                if ChildNodes.FindNode('categories') = nil then
                  AddChild('categories');
                ChildNodes.Nodes['categories'].NodeValue := IntelligeNConfigurationFile.WebsiteFilter.Categories;

                if ChildNodes.FindNode('controls') = nil then
                  AddChild('controls');
                with ChildNodes.Nodes['controls'] do
                begin
                  ChildNodes.Clear;
                  for Control in IntelligeNConfigurationFile.WebsiteFilter.Controls do
                    with AddChild('control') do
                    begin
                      Attributes['category'] := Control.Category;
                      Attributes[XML_NAME] := Control.Name;
                      Attributes['rel'] := Control.Relation;
                      NodeValue := Control.Value;
                    end;
                end;

                for Hoster in IntelligeNConfigurationFile.WebsiteFilter.Hosters do
                begin
                  if ChildNodes.FindNode(Hoster.Name) = nil then
                    AddChild(Hoster.Name);
                  with ChildNodes.Nodes[Hoster.Name] do
                  begin
                    Attributes['ranked'] := Hoster.Ranked;

                    if ChildNodes.FindNode('blacklist') = nil then
                      AddChild('blacklist');

                    with TStringList.Create do
                      try
                        Text := Hoster.Blacklist.Text;
                        Delimiter := ';';
                        ChildNodes.Nodes['blacklist'].NodeValue := DelimitedText;
                      finally
                        Free;
                      end;

                    if ChildNodes.FindNode('whitelist') = nil then
                      AddChild('whitelist');
                    with TStringList.Create do
                      try
                        Text := Hoster.Whitelist.Text;
                        Delimiter := ';';
                        ChildNodes.Nodes['whitelist'].NodeValue := DelimitedText;
                      finally
                        Free;
                      end;
                  end;
                end;
              end;
            finally
              IntelligeNConfigurationFile := nil;
            end;
        end;

        SaveToFile(AFileName);
      end;
    finally
      XMLDoc := nil;
    end;
  finally
    if LNeedToUninitialize then
      CoUninitialize;
  end;
end;

{ THosterConfiguration }

class procedure THosterConfiguration.LoadXML(AXMLProc: TXMLProc);
var
  HosterIndex, HosterAlsoKnownAsIndex: Integer;
  LNeedToUninitialize, Found: Boolean;
  XMLDoc: IXMLDocument;
begin
  if not FileExists(GetConfigurationFolder + HosterXML) then
    raise Exception.Create(HosterXML + ' not found located at configuration\' + HosterXML)
  else
  begin
    LNeedToUninitialize := Succeeded(CoInitializeEx(nil, COINIT_MULTITHREADED));
    try
      XMLDoc := NewXMLDocument;
      try
        with XMLDoc do
        begin
          LoadFromFile(GetConfigurationFolder + HosterXML);
          Active := True;
        end;

        Found := False;

        AXMLProc(XMLDoc.DocumentElement);
      finally
        XMLDoc := nil;
      end;
    finally
      if LNeedToUninitialize then
        CoUninitialize;
    end;
  end;
end;

class function THosterConfiguration.GetHoster;
var
  LResult: string;
begin
  LResult := '';

  LoadXML(
    { } procedure(AXMLNode: IXMLNode)
    { } var
    { . } HosterIndex, HosterAlsoKnownAsIndex: Integer;
    { . } Found: Boolean;
    { } begin
    { . } Found := False;
    { . } with AXMLNode.ChildNodes do
    { . } begin
    { ... } HosterIndex := 0;
    { ... } while not Found and (HosterIndex < Count) do
    { ... } begin
    { ..... } Found := SameText(AHoster, VarToStr(Nodes[HosterIndex].Attributes['name'])) or SameText(AHoster, VarToStr(Nodes[HosterIndex].Attributes['short']));

    { ..... } HosterAlsoKnownAsIndex := 0;
    { ..... } if Nodes[HosterIndex].HasChildNodes then
    { ....... } while not Found and (HosterAlsoKnownAsIndex < Nodes[HosterIndex].ChildNodes.Count) do
    { ....... } begin
    { ......... } Found := SameText(AHoster, VarToStr(Nodes[HosterIndex].ChildNodes.Nodes[HosterAlsoKnownAsIndex].NodeValue));
    { ......... } Inc(HosterAlsoKnownAsIndex);
    { ....... } end;

    { ..... } if not Found then
    { ....... } Inc(HosterIndex);
    { ... } end;
    { ... } if Found then
    { ..... } LResult := Nodes[HosterIndex].Attributes[AAttribute];
    { . } end;
    { } end);

  Result := LResult;
end;

class function THosterConfiguration.GetHosters: string;
var
  LResult: string;
begin
  LResult := '';

  LoadXML(
    { } procedure(AXMLNode: IXMLNode)
    { } var
    { . } StringList: TStringList;
    { . } I: Integer;
    { } begin
    { . } StringList := TStringList.Create;
    { . } try
    { ... } with AXMLNode.ChildNodes do
    { ..... } for I := 0 to Count - 1 do
    { ....... } StringList.Add(VarToStr(Nodes[I].Attributes['name']));
    { ... } LResult := StringList.Text;
    { . } finally
    { ... } StringList.Free;
    { . } end;
    { } end);

  Result := LResult;
end;

class function THosterConfiguration.GetCustomisedHoster(const AHoster: string; AShortName: Boolean = False): string;
begin
  if AShortName then
    Result := GetHoster(AHoster, 'short')
  else
    Result := GetHoster(AHoster, 'name');
end;

procedure GetControls(const AFileName: string; const AControlController: IControlController; const APageController: IPageController);
var
  LNeedToUninitialize: Boolean;
  XMLDoc: IXMLDocument;
  I: Integer;
begin
  // Wenn man hier den ComponentCreator erstellt braucht man wegen dem WorkPanel ein Verweis auf uMain

  LNeedToUninitialize := Succeeded(CoInitialize(nil));
  try
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        LoadFromFile(AFileName);
        Active := True;
      end;

      with XMLDoc.DocumentElement do
        if HasChildNodes then
          with ChildNodes.Nodes['controls'] do
            if HasChildNodes then
              for I := 0 to ChildNodes.Count - 1 do
                with ChildNodes.Nodes[I] do
                  if HasChildNodes then
                  begin
                    AControlController.NewControl(StringToControlID(NodeName), VarToStr(ChildNodes.Nodes['title'].NodeValue), VarToStr(ChildNodes.Nodes['value'].NodeValue), VarToStr(ChildNodes.Nodes['title'].Attributes['hint']),
                      VarToStr(ChildNodes.Nodes['value'].Attributes['list']), VarToIntDef(ChildNodes.Nodes['position'].Attributes['left'], 0), VarToIntDef(ChildNodes.Nodes['position'].Attributes['top'], 0),
                      VarToIntDef(ChildNodes.Nodes['position'].Attributes['width'], 0), VarToIntDef(ChildNodes.Nodes['position'].Attributes['height'], 0));
                    APageController.CallControlAligner;
                  end;
    finally
      XMLDoc := nil;
    end;
  finally
    if LNeedToUninitialize then
      CoUninitialize;
  end;
end;

{ TCodeDefinitions }

class procedure TCodeDefinitions.LoadXML(AXMLProc: TXMLProc);
var
  LNeedToUninitialize: Boolean;
  XMLDoc: IXMLDocument;
begin
  if not FileExists(GetConfigurationFolder + CodeDefinitionsXML) then
    raise Exception.Create(CodeDefinitionsXML + ' not found located at configuration\bbcodedefinitions.xml')
  else
  begin
    LNeedToUninitialize := Succeeded(CoInitialize(nil));
    try
      XMLDoc := NewXMLDocument;
      try
        with XMLDoc do
        begin
          LoadFromFile(GetConfigurationFolder + CodeDefinitionsXML);
          Active := True;
        end;

        AXMLProc(XMLDoc.DocumentElement);
      finally
        XMLDoc := nil;
      end;
    finally
    if LNeedToUninitialize then
      CoUninitialize;
    end;
  end;
end;

class function TCodeDefinitions.GetCodeDefinitions: TStrings;
var
  _Result: TStringList;
begin
  _Result := TStringList.Create;
  LoadXML(
    { } procedure(AXMLNode: IXMLNode)
    { } var
    { . } I: Integer;
    { } begin
    { . } for I := 0 to AXMLNode.ChildNodes.Count - 1 do
    { ... } _Result.Add(AXMLNode.ChildNodes.Nodes[I].NodeName);
    { } end);
  Result := _Result;
end;

class function TCodeDefinitions.GetCodeDefinition(const ACodeDefinition: string): TCodeDefinition;
var
  _Result: TCodeDefinition;
begin
  LoadXML(
    { } procedure(AXMLNode: IXMLNode)
    { } var
    { . } I, J: Integer;
    { } begin
    { . } with AXMLNode.ChildNodes.Nodes[ACodeDefinition].ChildNodes do
    { ... } begin
    { ..... } for I := 0 to Count - 1 do
    { ..... } begin
    { ....... } _Result.CodeTags[I].Name := Nodes[I].NodeName;
    { ....... } _Result.CodeTags[I].Value := StringReplace(VarToStr(Nodes[I].NodeValue), #10, sLineBreak, [rfReplaceAll]);
    { ....... } _Result.CodeTags[I].Params := TStringList.Create;
    { ....... } _Result.CodeTags[I].ParamValues := TStringList.Create;
    { ....... } for J := 0 to Nodes[I].AttributeNodes.Count - 1 do
    { ....... } begin
    { ......... } if EndsText('value', Nodes[I].AttributeNodes.Nodes[J].NodeName) then
    { ........... } _Result.CodeTags[I].ParamValues.Add(Nodes[I].AttributeNodes.Nodes[J].NodeName + _Result.CodeTags[I].ParamValues.NameValueSeparator + VarToStr(Nodes[I].AttributeNodes.Nodes[J].NodeValue))
    { ......... } else
    { ........... } _Result.CodeTags[I].Params.Add(Nodes[I].AttributeNodes.Nodes[J].NodeName + _Result.CodeTags[I].ParamValues.NameValueSeparator + VarToStr(Nodes[I].AttributeNodes.Nodes[J].NodeValue));
    { ....... } end;
    { ..... } end;
    { ... } end;
    { } end);
  Result := _Result;
end;

end.
