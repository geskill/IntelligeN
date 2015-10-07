unit uintelligenxml2;

interface

uses
  // Delphi
  SysUtils, Variants, XMLDoc, XMLIntf, ActiveX,
  // Common
  uBaseConst, uAppConst, uAppInterface,
  // DLLs
  uExport,
  // Utils
  uFileUtils, uStringUtils,
  // Plugin system
  uPlugInFileFormatClass;

type
  Tintelligenxml2 = class(TFileFormatPlugIn)
  public
    function GetName: WideString; override; safecall;
    function GetFileFormatName: WideString; override; safecall;
    function CanSaveControls: WordBool; override; safecall;
    procedure SaveControls(AFileName, ATemplateFileName: WideString; const ATabSheetController: ITabSheetController); override; safecall;
    function CanLoadControls: WordBool; override; safecall;
    function LoadControls(AFileName, ATemplateDirectory: WideString; const APageController: IPageController): Integer; override; safecall;
  end;

implementation

uses
  uSelectTemplateFileName;

function Tintelligenxml2.GetName;
begin
  result := 'intelligen.xml.2';
end;

function Tintelligenxml2.GetFileFormatName;
begin
  result := 'IntelligeN 2009 %s (*.xml)|*.xml|';
end;

function Tintelligenxml2.CanSaveControls;
begin
  result := True;
end;

procedure Tintelligenxml2.SaveControls;
var
  XMLDoc: IXMLDocument;
  Picture: IPicture;
  I, X: Integer;

  function RemoveSpecialChars(s: string): string;
  begin
    result := StringReplaceMultiple(s, [#$01, #$02, #$03, #$04, #$05, #$06, #$07, #$08, #$b, #$c, #$e, #$f, #$9, #$10, #$11, #$12, #$13, #$14, #$15, #$16,
      #$17, #$18, #$19, #$1a, #$1b, #$1c, #$1d, #$1e, #$1f], [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ',
      ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '], False);
  end;

begin
  OleInitialize(nil);
  try
    XMLDoc := NewXMLDocument;

    with XMLDoc do
    begin
      Options := Options + [doNodeAutoIndent];
      DocumentElement := CreateElement('xml', '');
      Active := True;
    end;
    with XMLDoc.DocumentElement do
    begin
      if FileExists(ATemplateFileName) then
      begin
        with AddChild('templatetype') do
        begin
          Attributes['filename'] := ExtractFileName(ChangeFileExt(ATemplateFileName, ''));
          Attributes['checksum'] := GetMD5FromFile(ATemplateFileName);
          NodeValue := TypeIDToString(ATabSheetController.ControlController.ATypeID);
        end;
      end
      else
        AddChild('templatetype').NodeValue := '#';

      with AddChild('controls') do
        with ATabSheetController.ControlController do
          for I := 0 to ControlCount - 1 do
            with AddChild(ControlIDToString(Control[I].AControlID)) do
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
              for X := 0 to Mirror[I].DirectlinksMirrorCount - 1 do
                with AddChild('directlink') do
                begin
                  Attributes['size'] := Mirror[I].Directlink.GetSize(X);
                  Attributes['partsize'] := Mirror[I].PartSize;
                  Attributes['hoster'] := Mirror[I].Hoster;
                  Attributes['parts'] := Mirror[I].Parts;
                  NodeValue := Mirror[I].DirectlinksMirror[X];
                end;
              for X := 0 to Mirror[I].CrypterCount - 1 do
                with AddChild('crypter') do
                begin
                  Attributes['name'] := Mirror[I].Crypter[X].name;
                  Attributes['status'] := Mirror[I].Crypter[X].Status;
                  Attributes['partsize'] := Mirror[I].Crypter[X].PartSize;
                  Attributes['size'] := Mirror[I].Crypter[X].Size;
                  Attributes['hoster'] := Mirror[I].Crypter[X].Hoster;
                  Attributes['parts'] := Mirror[I].Crypter[X].Parts;
                  Attributes['statusimage'] := Mirror[I].Crypter[X].StatusImage;
                  Attributes['statusimagetext'] := Mirror[I].Crypter[X].StatusImageText;
                  NodeValue := Mirror[I].Crypter[X].Link;
                end;
            end;
    end;

    XMLDoc.SaveToFile(AFileName);
  finally
    XMLDoc := nil;
    OleUninitialize;
  end;
end;

function Tintelligenxml2.CanLoadControls;
begin
  result := True;
end;

function Tintelligenxml2.LoadControls;
var
  XMLDoc: IXMLDocument;
  I, X, Y: Integer;
  Control: IBasic;
  Picture: IPicture;
  TemplateType: TTypeID;
  _TemplateFile, _TemplateFileName: TFileName;
  _TemplateChecksum: string;
  _CrypterExists, _ImageMirrorExists: Boolean;
begin
  OleInitialize(nil);
  try
    result := -1;
    try
      XMLDoc := NewXMLDocument;

      with XMLDoc do
      begin
        LoadFromFile(AFileName);
        Active := True;
      end;

      with XMLDoc.DocumentElement do
        if HasChildNodes then
        begin
          with ChildNodes.Nodes['templatetype'] do
            if SameText('#', NodeValue) then
            begin
              with TSelectTemplateFileName.Create(nil) do
                try
                  if Execute then
                  begin
                    TemplateType := GetFileInfo(ATemplateDirectory + TemplateFileName + '.xml').TemplateType;
                    _TemplateFileName := TemplateFileName + '.xml';
                    _TemplateChecksum := '#';
                  end
                  else
                    raise Exception.Create('');
                finally
                  Free;
                end;
            end
            else
            begin
              TemplateType := StringToTypeID(VarToStr(NodeValue));
              _TemplateFileName := '';
              if HasAttribute('filename') then
                _TemplateFileName := VarToStr(Attributes['filename']) + '.xml';
              _TemplateChecksum := VarToStr(Attributes['checksum']);
            end;
        end
        else
          raise Exception.Create('');

      if (FileExists(ATemplateDirectory + _TemplateFileName)) and (SameText(_TemplateChecksum, GetMD5FromFile(ATemplateDirectory + _TemplateFileName))
          or SameText('#', _TemplateChecksum)) then
        _TemplateFile := ATemplateDirectory + _TemplateFileName
      else
        _TemplateFile := AFileName;

      with APageController.TabSheetController[APageController.Add(_TemplateFile, TemplateType, True)] do
        with XMLDoc.DocumentElement do
        begin
          for I := 0 to ChildNodes.Nodes['mirrors'].ChildNodes.Count - 1 do
            with MirrorController.Mirror[MirrorController.Add] do
            begin
              for X := 0 to ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Count - 1 do
                if SameText('directlink', ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].NodeName) then
                begin
                  Directlink.Mirror[Directlink.Add(VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].NodeValue))].Size :=
                    StrToFloatDef(VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['size']), 0);

                  // Workaround for: http://www.devexpress.com/issue=B202502
                  APageController.CallComponentparser;
                end
                else if ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].NodeName = 'crypter' then
                begin
                  if ForceAddCrypter then
                  begin
                    _CrypterExists := False;
                    for Y := 0 to CrypterCount - 1 do
                      if SameText(Crypter[Y].name, VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].Attributes['name'])) then
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
                      Crypter[Y].Link := VarToStr(ChildNodes.Nodes['mirrors'].ChildNodes.Nodes[I].ChildNodes.Nodes[X].NodeValue);
                      Crypter[Y].CheckFolder;
                      break;
                    end;
                end;
            end;
          for I := 0 to ChildNodes.Nodes['controls'].ChildNodes.Count - 1 do
            with ChildNodes.Nodes['controls'].ChildNodes.Nodes[I] do
            begin
              Control := ControlController.FindControl(StringToControlID(NodeName));
              if Assigned(Control) then
              begin
                Control.Value := VarToStr(ChildNodes.Nodes['value'].NodeValue);

                if Control.QueryInterface(IPicture, Picture) = 0 then
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
              Control := nil;
            end;
          ResetDataChanged(AFileName, GetName);
          result := TabSheetIndex;
        end;
    except
      result := -1;
    end;
  finally
    XMLDoc := nil;
    OleUninitialize;
  end;
end;

end.
