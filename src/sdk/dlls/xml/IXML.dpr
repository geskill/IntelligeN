library IXML;

uses
  // Delphi
  Windows, SysUtils, Variants, XMLDoc, XMLIntf, ActiveX,
  // Common
  uAppConst;
{$R *.res}
{ ****************************************************************************** }

function GetFileInfo(AFileName: WideString): RTemplateFileInfo; stdcall; export;
var
  LXMLDoc: IXMLDocument;
  LTemplateInfo: RTemplateFileInfo;
begin
  CoInitializeEx(nil, COINIT_MULTITHREADED);
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
              FileName := VarToStr(Attributes['filename']);
              Checksum := VarToStr(Attributes['checksum']);
            end;
          end;

      Result := LTemplateInfo;
    finally
      LXMLDoc := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

exports { . }
  GetFileInfo name 'GetFileInfo';

begin

end.
