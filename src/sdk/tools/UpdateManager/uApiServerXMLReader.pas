unit uApiServerXMLReader;

interface

uses
  // Delphi
  SysUtils, Classes, XMLDoc, XMLIntf, ActiveX, Variants,
  // Spring Framework
  Spring.SystemUtils, Spring.Collections.Lists,
  // Common
  uBase,
  // Api
  uApiServerInterface, uApiServerClasses, uApiUpdateModel, uApiUpdateInterface,
  // Utils
  uStringUtils;

type
  TServerXMLReader = class
  type
    TIBasicMeta = class of TIBasicServerResponse;
    TServerRequestType = (srtVersions, srtSystems, srtFTPServer);
  private
    class function GetClassType(AType: TServerRequestType): TIBasicMeta;
  protected
    class function Read(AXMLContent: string; AType: TServerRequestType): IBasicServerResponse;

  public
    class function ReadVersions(AXMLContent: string): IVersionsResponse;
    class function ReadSystems(AXMLContent: string): ISystemsResponse;
    class function ReadFTPServer(AXMLContent: string): IFTPServerResponse;
  end;

implementation

{ TServerXMLReader }

class function TServerXMLReader.GetClassType;
begin
  case AType of
    srtVersions:
      Result := TIVersionsResponse;
    srtSystems:
      Result := TISystemsResponse;
    srtFTPServer:
      Result := TIFTPServerResponse;
  end;
end;

class function TServerXMLReader.Read;
var
  LBasicServerResponse: TIBasicServerResponse;

  XMLDoc: IXMLDocument;

  XMLNodeIndex: Integer;

  LUpdateVersion: IUpdateManagerVersion;
  LUpdateSystemFileBase: IUpdateManagerSystemFileBase;
begin
  LBasicServerResponse := GetClassType(AType).Create;

  Result := LBasicServerResponse;

  OleInitialize(nil);
  try
    XMLDoc := NewXMLDocument;

    with XMLDoc do
    begin
      Options := Options + [doNodeAutoIndent];
      NodeIndentStr := #9;
      LoadFromXML(AXMLContent);
      Active := True;

      with DocumentElement do
        if HasChildNodes then
          with Result do
          begin
            Status := ChildNodes.Nodes['status'].NodeValue;
            Code := ChildNodes.Nodes['code'].NodeValue;
            Msg := VarToStr(ChildNodes.Nodes['msg'].NodeValue);

            case AType of
              srtVersions:
                with (Result as IVersionsResponse) do
                begin
                  if Assigned(ChildNodes.FindNode('versions')) then
                    with ChildNodes.Nodes['versions'] do
                      for XMLNodeIndex := 0 to ChildNodes.Count - 1 do
                      begin
                        LUpdateVersion := TIUpdateManagerVersion.Create;
                        with ChildNodes.Nodes[XMLNodeIndex], LUpdateVersion do
                        begin
                          ID := StrToIntDef(VarToStr(ChildNodes.Nodes['id'].NodeValue), 0);

                          if (IsNumber(ChildNodes.Nodes['active'].NodeValue)) then
                            Active := not(ChildNodes.Nodes['active'].NodeValue = 0)
                          else
                            Active := StrToBoolDef(VarToStr(ChildNodes.Nodes['major_version'].NodeValue), False);

                          MajorVersion := StrToIntDef(VarToStr(ChildNodes.Nodes['major_version'].NodeValue), 0);
                          MinorVersion := StrToIntDef(VarToStr(ChildNodes.Nodes['minor_version'].NodeValue), 0);
                          MajorBuild := StrToIntDef(VarToStr(ChildNodes.Nodes['major_build'].NodeValue), 0);
                          MinorBuild := StrToIntDef(VarToStr(ChildNodes.Nodes['minor_build'].NodeValue), 0);
                        end;
                        Versions.Add(LUpdateVersion);
                      end;
                end;
              srtSystems:
                with (Result as ISystemsResponse) do
                begin
                  if Assigned(ChildNodes.FindNode('systems')) then
                    with ChildNodes.Nodes['systems'] do
                      for XMLNodeIndex := 0 to ChildNodes.Count - 1 do
                      begin
                        LUpdateSystemFileBase := TIUpdateManagerSystemFileBase.Create;
                        with ChildNodes.Nodes[XMLNodeIndex], LUpdateSystemFileBase do
                        begin
                          ID := StrToIntDef(VarToStr(ChildNodes.Nodes['id'].NodeValue), 0);

                          FileName := VarToStr(ChildNodes.Nodes['name'].NodeValue);

                          if (IsNumber(ChildNodes.Nodes['filesystem_id'].NodeValue)) then
                            FileSystem := TFileSystem(StrToIntDef(VarToStr(ChildNodes.Nodes['filesystem_id'].NodeValue), 0))
                          else
                            FileSystem := TEnum.Parse<TFileSystem>(VarToStr(ChildNodes.Nodes['filesystem_id'].NodeValue));

                          FilePathAppendix := VarToStr(ChildNodes.Nodes['path_appendix'].NodeValue);
                        end;
                        Systems.Add(LUpdateSystemFileBase);
                      end;
                end;
              srtFTPServer:
                with (Result as IFTPServerResponse) do
                begin
                  if Assigned(ChildNodes.FindNode('server')) then
                    with ChildNodes.Nodes['server'], Server do
                      begin
                        Name := VarToStr(ChildNodes.Nodes['name'].NodeValue);
                        Port := VarToStr(ChildNodes.Nodes['port'].NodeValue);
                        Path := VarToStr(ChildNodes.Nodes['path'].NodeValue);
                        Username := VarToStr(ChildNodes.Nodes['username'].NodeValue);
                        Password := VarToStr(ChildNodes.Nodes['password'].NodeValue);
                      end;
                end;
            end;
          end;
    end;

    XMLDoc := nil;
  finally
    OleUninitialize;
  end;
end;

class function TServerXMLReader.ReadVersions(AXMLContent: string): IVersionsResponse;
begin
  Result := TServerXMLReader.Read(AXMLContent, srtVersions) as IVersionsResponse;
end;

class function TServerXMLReader.ReadSystems(AXMLContent: string): ISystemsResponse;
begin
  Result := TServerXMLReader.Read(AXMLContent, srtSystems) as ISystemsResponse;
end;

class function TServerXMLReader.ReadFTPServer(AXMLContent: string): IFTPServerResponse;
begin
  Result := TServerXMLReader.Read(AXMLContent, srtFTPServer) as IFTPServerResponse;
end;

end.
