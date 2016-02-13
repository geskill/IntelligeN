unit uSystemUtils;

interface

uses
  // Delphi
  Windows;

function GetModulePath: string;

implementation

function GetModulePath: string;
var
  LQueryRes: TMemoryBasicInformation;
  LBuffer: string;
begin
  VirtualQuery(@GetModulePath, LQueryRes, SizeOf(LQueryRes));
  SetLength(LBuffer, MAX_PATH);
  SetLength(LBuffer, GetModuleFileName(Cardinal(LQueryRes.AllocationBase), PChar(LBuffer), Length(LBuffer)));
  Result := LBuffer;
end;

end.
