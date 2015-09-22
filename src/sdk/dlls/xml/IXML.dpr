library IXML;

uses
  // Delphi
  Windows, SysUtils, Variants, XMLDoc, XMLIntf, ActiveX,
  // Common
  uConst;

{$R *.res}

  { ****************************************************************************** }

function GetFileInfo(AFileName: WideString): RTemplateFileInfo; stdcall; export;
var
  LXMLDoc: IXMLDocument;
  LTemplateInfo: RTemplateFileInfo;
begin
  OleInitialize(nil);
  try
    LXMLDoc := NewXMLDocument;

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
            TemplateType := StringToTTemplateTypeID(VarToStr(NodeValue));
            FileName := VarToStr(Attributes['filename']);
            Checksum := VarToStr(Attributes['checksum']);
          end;
        end;

    Result := LTemplateInfo;
  finally
    LXMLDoc := nil;
    OleUninitialize;
  end;
end;

exports { . }
  GetFileInfo name 'GetFileInfo';

begin

end.
