unit uApiServerXMLReader;

interface

uses
  // Delphi
  SysUtils, Classes, XMLDoc, XMLIntf, ActiveX, Variants,
  // Common
  uServerInterface, uApiUpdateModel, uApiUpdateInterface,

  uServerClasses;

type
  TServerXMLReader = class
  type
    TIBasicMeta = class of TIBasicServerResponse;
    TServerRequestType = (srtVersions, srtFTPServer);
  private
    class function GetClassType(AType: TServerRequestType): TIBasicMeta;
  protected
    class function Read(AXMLContent: string; AType: TServerRequestType): IBasicServerResponse;

  public
    class function ReadVersions(AXMLContent: string): IVersionsResponse;
    class function ReadFTPServer(AXMLContent: string): IFTPServerResponse;
  end;

implementation

{ TServerXMLReader }

class function TServerXMLReader.GetClassType;
begin
  case AType of
    srtVersions:
      Result := TIVersionsResponse;
    srtFTPServer:
      Result := TIFTPServerResponse;
  end;
end;

class function TServerXMLReader.Read;
var
  LBasicServerResponse: TIBasicServerResponse;

  XMLDoc: IXMLDocument;

  XMLNodeIndex: Integer;


  LUpdateServerVersion: IUpdateServerVersion;
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
                        LUpdateServerVersion := TIUpdateServerVersion.Create;
                        with ChildNodes.Nodes[XMLNodeIndex], LUpdateServerVersion do
                        begin
                          ID := StrToIntDef(VarToStr(Attributes['id']), -1);
                          New := (StrToIntDef(VarToStr(Attributes['new']), 0) = 0);

                          MajorVersion := StrToIntDef(VarToStr(ChildNodes.Nodes['major_version'].NodeValue), -1);
                          MinorVersion := StrToIntDef(VarToStr(ChildNodes.Nodes['minor_version'].NodeValue), -1);
                          MajorBuild := StrToIntDef(VarToStr(ChildNodes.Nodes['major_build'].NodeValue), -1);
                          MinorBuild := StrToIntDef(VarToStr(ChildNodes.Nodes['minor_build'].NodeValue), -1);
                        end;
                        Versions.Add(LUpdateServerVersion);
                      end;
                end;
              srtFTPServer:
                with (Result as IFTPServerResponse) do
                begin
                  if Assigned(ChildNodes.FindNode('server')) then
                    with ChildNodes.Nodes['server'] do
                      with Server do
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

class function TServerXMLReader.ReadFTPServer(AXMLContent: string): IFTPServerResponse;
begin
  Result := TServerXMLReader.Read(AXMLContent, srtFTPServer) as IFTPServerResponse;
end;

end.
