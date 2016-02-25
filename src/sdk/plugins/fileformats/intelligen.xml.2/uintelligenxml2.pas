unit uintelligenxml2;

interface

uses
  // Delphi
  SysUtils, Variants, XMLDoc, XMLIntf, ActiveX,
  // Common
  uBaseConst, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Plugin system
  uPlugInFileFormatClass,
  // Utils
  uFileUtils, uStringUtils, uVariantUtils;

type
  TIntelligeNXML2 = class(TFileFormatPlugIn)
  public
    function GetName: WideString; override; safecall;

    function GetFileExtension: WideString; override; safecall;
    function GetFileFilter: WideString; override; safecall;

    function CanSaveFiles: WordBool; override; safecall;
    function SaveFile(const AFileName: WideString; const ATabSheetController: ITabSheetController): WordBool; override; safecall;

    function CanLoadFiles: WordBool; override; safecall;
    function LoadFile(const AFileName: WideString; const APageController: IPageController): Integer; override; safecall;
  end;

implementation

uses
  uSelectTemplateFileName;

resourcestring
  StrTheXMLFileIsNotCompatible = 'The XML file is not compatible to the intelligen.xml.2 standard';
  StrTheIntelligeNXML2StdReq = 'The intelligen.xml.2 standard requires a template file.';

function TIntelligeNXML2.GetName;
begin
  result := 'intelligen.xml.2';
end;

function TIntelligeNXML2.GetFileExtension;
begin
  result := '.xml';
end;

function TIntelligeNXML2.GetFileFilter;
begin
  result := 'IntelligeN 2009 %s (*.xml)|*.xml|';
end;

function TIntelligeNXML2.CanSaveFiles;
begin
  result := True;
end;

function TIntelligeNXML2.SaveFile;
var
  LNeedToUninitialize: Boolean;
  XMLDoc: IXMLDocument;
  Picture: IPicture;
  I, X: Integer;

  function RemoveSpecialChars(s: string): string;
  begin
    result := StringReplaceMultiple(s, [#$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$b, #$c, #$e, #$f, #$9, #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17, #$18, #$19, #$1a, #$1b, #$1c, #$1d, #$1e, #$1f],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '], False);
  end;

begin
  LNeedToUninitialize := Succeeded(CoInitialize(nil));
  try
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        Options := Options + [doNodeAutoIndent];
        DocumentElement := CreateElement('xml', '');
        Active := True;
        Encoding := 'utf-8';
      end;
      with XMLDoc.DocumentElement do
      begin
        with AddChild('templatetype') do
        begin
          if FileExists(ATabSheetController.TemplateFileName) then
          begin
            Attributes['filename'] := ExtractFileName(ChangeFileExt(ATabSheetController.TemplateFileName, ''));
            Attributes['checksum'] := GetMD5FromFile(ATabSheetController.TemplateFileName);
          end;
          NodeValue := TypeIDToString(ATabSheetController.ControlController.TypeID);
        end;

        with AddChild('controls') do
          with ATabSheetController.ControlController do
            for I := 0 to ControlCount - 1 do
              with AddChild(ControlIDToString(Control[I].ControlID)) do
              begin
                AddChild('title').NodeValue := Control[I].Title;
                with AddChild('value') do
                begin
                  Attributes['list'] := '#';
                  NodeValue := RemoveSpecialChars(Control[I].Value);
                end;
                if Control[I].QueryInterface(IPicture, Picture) = 0 then
                begin
                  with AddChild('hosters') do
                    for X := 0 to Picture.MirrorCount - 1 do
                      with AddChild('hoster') do
                      begin
                        Attributes['name'] := Picture.Mirror[X].Name;
                        NodeValue := Picture.Mirror[X].Value;
                      end;
                  Picture := nil;
                end;
                with AddChild('position') do
                begin
                  Attributes['left'] := Control[I].Left;
                  Attributes['top'] := Control[I].Top;
                  Attributes['width'] := Control[I].Width;
                  Attributes['height'] := Control[I].Height;
                end;
              end;
        with AddChild('mirrors') do
          with ATabSheetController.MirrorController do
            for I := 0 to MirrorCount - 1 do
              with AddChild('mirror') do
              begin
                for X := 0 to Mirror[I].DirectlinkCount - 1 do
                  with AddChild('directlink') do
                  begin
                    Attributes['status'] := ContentStatusToString(Mirror[I].Directlink[X].Status);
                    Attributes['size'] := Round(Mirror[I].Directlink[X].Size);
                    Attributes['partsize'] := Round(Mirror[I].Directlink[X].PartSize);
                    Attributes['hoster'] := Mirror[I].Directlink[X].Hoster;
                    Attributes['parts'] := Mirror[I].Directlink[X].Parts;
                    NodeValue := Mirror[I].Directlink[X].Value;
                  end;
                for X := 0 to Mirror[I].CrypterCount - 1 do
                  with AddChild('crypter') do
                  begin
                    Attributes['name'] := Mirror[I].Crypter[X].Name;
                    Attributes['status'] := ContentStatusToString(Mirror[I].Crypter[X].Status);
                    Attributes['size'] := Round(Mirror[I].Crypter[X].Size);
                    Attributes['partsize'] := Round(Mirror[I].Crypter[X].PartSize);
                    Attributes['hoster'] := Mirror[I].Crypter[X].Hoster;
                    Attributes['parts'] := Mirror[I].Crypter[X].Parts;
                    Attributes['statusimage'] := Mirror[I].Crypter[X].StatusImage;
                    Attributes['statusimagetext'] := Mirror[I].Crypter[X].StatusImageText;
                    NodeValue := Mirror[I].Crypter[X].Value;
                  end;
              end;
      end;

      XMLDoc.SaveToFile(AFileName);
    finally
      XMLDoc := nil;
    end;
  finally
    if LNeedToUninitialize then
      CoUninitialize;
  end;
end;

function TIntelligeNXML2.CanLoadFiles;
begin
  result := True;
end;

function TIntelligeNXML2.LoadFile;
var
  LNeedToUninitialize: Boolean;
  LXMLDoc: IXMLDocument;
  LBasicControl: IControlBasic;
  I, X, Y: Integer;
  Picture: IPicture;
  LType: TTypeID;
  LTemplateFile, LTemplateFileName: TFileName;
  LTemplateChecksum: string;
  _CrypterExists, _ImageMirrorExists: Boolean;
begin
  result := -1;
  try
    LNeedToUninitialize := Succeeded(CoInitialize(nil));
    try
      LXMLDoc := NewXMLDocument;
      try
        with LXMLDoc do
        begin
          LoadFromFile(AFileName);
          Active := True;
        end;

        // Read file header
        with LXMLDoc.DocumentElement do
        begin
          if HasChildNodes then
          begin
            with ChildNodes.Nodes['templatetype'] do
              if SameText('#', NodeValue) then
              begin
                with TSelectTemplateFileName.Create(nil) do
                  try
                    if Execute then
                    begin
                      LType := GetFileInfo(GetTemplatesTypeFolder + TemplateFileName + '.xml').TypeID;
                      LTemplateFileName := TemplateFileName + '.xml';
                      LTemplateChecksum := '#';
                    end
                    else
                    begin
                      ErrorMsg := StrTheIntelligeNXML2StdReq;
                      raise Exception.Create(ErrorMsg);
                    end;
                  finally
                    Free;
                  end;
              end
              else
              begin
                LType := StringToTypeID(VarToStr(NodeValue));
                LTemplateFileName := '';
                if HasAttribute('filename') then
                  LTemplateFileName := VarToStr(Attributes['filename']) + '.xml';
                LTemplateChecksum := '#';
                if HasAttribute('checksum') then
                  LTemplateChecksum := VarToStr(Attributes['checksum']);
              end;
          end
          else
          begin
            ErrorMsg := StrTheXMLFileIsNotCompatible;
            raise Exception.Create(ErrorMsg);
          end;
        end;

        if (FileExists(GetTemplatesTypeFolder + LTemplateFileName)) and (SameText(LTemplateChecksum, GetMD5FromFile(GetTemplatesTypeFolder + LTemplateFileName)) or SameText('#', LTemplateChecksum)) then
          LTemplateFile := GetTemplatesTypeFolder + LTemplateFileName
        else
          LTemplateFile := AFileName;

        with APageController.TabSheetController[APageController.CreateTabSheet(LTemplateFile, LType, True)] do
          with LXMLDoc.DocumentElement do
          begin
            for I := 0 to ChildNodes.Nodes['mirrors'].ChildNodes.Count - 1 do
              with MirrorController.Mirror[MirrorController.Add] do
              begin
                for X := 0 to ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Count - 1 do
                  if SameText('directlink', ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].NodeName) then
                  begin
                    with Directlink[GetDirectlink.Add(VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].NodeValue))] do
                    begin
                      Size := VarToIntDef(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['size'], 0);
                      PartSize := VarToIntDef(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['partsize'], 0);
                    end;

                    // Workaround for: http://www.devexpress.com/issue=B202502
                    APageController.CallControlAligner;
                  end
                  else if ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].NodeName = 'crypter' then
                  begin
                    if ForceAddCrypter then
                    begin
                      _CrypterExists := False;
                      for Y := 0 to CrypterCount - 1 do
                        if SameText(Crypter[Y].Name, VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['name'])) then
                        begin
                          _CrypterExists := True;
                          break;
                        end;
                      if not _CrypterExists then
                        AddCrypter(VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['name']));
                    end;
                    for Y := 0 to CrypterCount - 1 do
                      if SameText(Crypter[Y].name, VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['name'])) then
                      begin
                        Crypter[Y].Value := VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].NodeValue);
                        Crypter[Y].Size := VarToIntDef(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['size'], 0);
                        Crypter[Y].PartSize := VarToIntDef(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['partsize'], 0);
                        // Crypter[Y].Hoster := VarToStrDef(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['hoster'], '');
                        // Crypter[Y].Parts := VarToIntDef(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['parts'], 0);
                        Crypter[Y].StatusImage := VarToStrDef(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['statusimage'], '');
                        Crypter[Y].StatusImageText := VarToStrDef(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['statusimagetext'], '');

                        if not SameStr('', Crypter[Y].Value) then
                          Crypter[Y].CheckFolder;
                        break;
                      end;
                  end;
              end;
            for I := 0 to ChildNodes.Nodes['controls'].ChildNodes.Count - 1 do
              with ChildNodes.Nodes['controls'].ChildNodes.Nodes[I] do
              begin
                LBasicControl := ControlController.FindControl(StringToControlID(NodeName));
                if Assigned(LBasicControl) then
                begin
                  LBasicControl.Value := VarToStr(ChildNodes.Nodes['value'].NodeValue);

                  if LBasicControl.QueryInterface(IPicture, Picture) = 0 then
                  begin
                    for X := 0 to ChildNodes.Nodes['hosters'].ChildNodes.Count - 1 do
                    begin
                      if ForceAddImageMirror then
                      begin
                        _ImageMirrorExists := False;
                        for Y := 0 to Picture.MirrorCount - 1 do
                          if SameText(Picture.Mirror[Y].Name, VarToStr(ChildNodes.Nodes['hosters'].ChildNodes.Nodes[X].Attributes['name'])) then
                          begin
                            _ImageMirrorExists := True;
                            break;
                          end;
                        if not _ImageMirrorExists then
                          Picture.AddMirror(VarToStr(ChildNodes.Nodes['hosters'].ChildNodes.Nodes[X].Attributes['name']));
                      end;
                      for Y := 0 to Picture.MirrorCount - 1 do
                        if SameText(Picture.Mirror[Y].Name, VarToStr(ChildNodes.Nodes['hosters'].ChildNodes.Nodes[X].Attributes['name'])) then
                        begin
                          Picture.Mirror[Y].Value := VarToStr(ChildNodes.Nodes['hosters'].ChildNodes.Nodes[X].NodeValue);
                          break;
                        end;
                    end;
                  end;
                  Picture := nil;
                end;
                LBasicControl := nil;
              end;
            result := TabSheetIndex;
          end;
      finally
        LXMLDoc := nil;
      end;
    finally
      if LNeedToUninitialize then
        CoUninitialize;
    end;
  except
    result := -1;
  end;
end;

end.
