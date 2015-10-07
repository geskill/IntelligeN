{ ******************************************************* }
{                                                         }
{ DownLoad Manager Files                                  }
{ Version 2.0.0.0                                         }
{ Copyright (c) 2009 - 2010 Sebastian Klatte              }
{                                                         }
{ ******************************************************* }
unit uApiDLMF;

interface

uses
  // Delphi
  Windows, SysUtils, StrUtils, Classes;

type
  TDLMF = class
  private type
    TRSDFToPlain = procedure(RSDFFile: Widestring; out Result: Widestring); stdcall;

  type
    TMSDNCFToPlain = procedure(NCFFile: Widestring; out Result: Widestring); stdcall;

  type
    TRSDNCFToPlain = procedure(NCFFile: Widestring; out Result: Widestring); stdcall;

  type
    TCCF07ToPlain = procedure(CCFFile: Widestring; out Result: Widestring); stdcall;

  type
    TCCF08ToPlain = procedure(CCFFile: Widestring; out Result: Widestring); stdcall;

  type
    TCCF10ToPlain = procedure(CCFFile: Widestring; out Result: Widestring); stdcall;

  type
    TCCF30ToPlain = procedure(CCFFile: Widestring; out Result: Widestring); stdcall;

  type
    TCCF50ToPlain = procedure(CCFFile: Widestring; out Result: Widestring); stdcall;

  type
    TDLCToPlain = procedure(DLCFile: Widestring; out Result: Widestring); stdcall;

  class function ValidContainerFile(s: string): Boolean;
  public
    { Returns the complete decrypted container file }
    class function ContainerFileToPlain(FileName: TFileName; DLMFFilePath: string): string;
    { Returns only the http links }
    class function ContainerFileToPlainEx(FileName: TFileName; DLMFFilePath: string): string;
  end;

implementation

function RemoveDuplicates(const s: string): string;
var
  I, X: Integer;
  Line: string;
  Exists: Boolean;
  StringList, ReturnList: TStringList;
begin
  ReturnList := TStringList.Create;
  StringList := TStringList.Create;
  try
    StringList.Text := s;

    for I := 0 to StringList.Count - 1 do
    begin
      Line := StringList.Strings[I];
      Exists := false;

      for X := 0 to ReturnList.Count - 1 do
        if (Line = ReturnList.Strings[X]) then
          Exists := true;

      if not Exists then
        ReturnList.Add(StringList.Strings[I]);
    end;

    Result := ReturnList.Text;
  finally
    StringList.Free;
    ReturnList.Free;
  end;
end;

function SearchHTTPLinks(s: string): string;
const
  LinkCharacters: array [0 .. 17] of string = (':', '.', '#', '{', ',', '|', '}', '!', '$', '%', '=', '?', '_', '~', '&', '+', '-', '/');
var
  I: Integer;
begin
  Result := '';
  I := 1;
  while (PosEx('http', s, I) > 0) do
  begin
    I := PosEx('http', s, I);
    while CharInSet(s[I], ['a' .. 'z', 'A' .. 'Z', '0' .. '9']) or (IndexText(s[I], LinkCharacters) >= 0) do
    begin
      Result := Result + s[I];
      Inc(I);
    end;
    Result := Result + #13;
  end;
end;

class function TDLMF.ValidContainerFile(s: string): Boolean;
begin
  Result := (Pos('http://', LowerCase(s)) > 0);
end;

class function TDLMF.ContainerFileToPlain(FileName: TFileName; DLMFFilePath: string): string;
var
  hLib: Cardinal;
  _result: Widestring;
  MRSDFToPlain: TRSDFToPlain;
  MMSDNCFToPlain: TMSDNCFToPlain;
  MRSDNCFToPlain: TRSDNCFToPlain;
  MCCF07ToPlain: TCCF07ToPlain;
  MCCF08ToPlain: TCCF08ToPlain;
  MCCF10ToPlain: TCCF10ToPlain;
  MCCF30ToPlain: TCCF30ToPlain;
  MCCF50ToPlain: TCCF50ToPlain;
  MDLCToPlain: TDLCToPlain;
begin
  hLib := LoadLibrary(PChar(DLMFFilePath + 'DLMF.dll'));
  try
    if not(hLib = 0) then
    begin
      @MRSDFToPlain := GetProcAddress(hLib, 'RSDFToPlain');
      try
        MRSDFToPlain(FileName, _result);
        Result := _result;
      except

      end;
      if not ValidContainerFile(_result) then
      begin
        @MMSDNCFToPlain := GetProcAddress(hLib, 'MSDNCFToPlain');
        try
          MMSDNCFToPlain(FileName, _result);
          Result := _result;
        except

        end;
        if not ValidContainerFile(_result) then
        begin
          @MRSDNCFToPlain := GetProcAddress(hLib, 'RSDNCFToPlain');
          try
            MRSDNCFToPlain(FileName, _result);
            Result := _result;
          except

          end;
          if not ValidContainerFile(_result) then
          begin
            @MCCF07ToPlain := GetProcAddress(hLib, 'CCF07ToPlain');
            try
              MCCF07ToPlain(FileName, _result);
              Result := _result;
            except

            end;
            if not ValidContainerFile(_result) then
            begin
              @MCCF08ToPlain := GetProcAddress(hLib, 'CCF08ToPlain');
              try
                MCCF08ToPlain(FileName, _result);
                Result := _result;
              except

              end;
              if not ValidContainerFile(_result) then
              begin
                @MCCF10ToPlain := GetProcAddress(hLib, 'CCF10ToPlain');
                try
                  MCCF10ToPlain(FileName, _result);
                  Result := _result;
                except

                end;
                if not ValidContainerFile(_result) then
                begin
                  @MCCF30ToPlain := GetProcAddress(hLib, 'CCF30ToPlain');
                  try
                    MCCF30ToPlain(FileName, _result);
                    Result := _result;
                  except

                  end;
                  if not ValidContainerFile(_result) then
                  begin
                    @MCCF50ToPlain := GetProcAddress(hLib, 'CCF50ToPlain');
                    try
                      MCCF50ToPlain(FileName, _result);
                      Result := _result;
                    except

                    end;
                    if not ValidContainerFile(_result) then
                    begin
                      @MDLCToPlain := GetProcAddress(hLib, 'DLCToPlain');
                      try
                        MDLCToPlain(FileName, _result);
                        Result := _result;
                      except

                      end;
                      if not ValidContainerFile(_result) then
                        Result := '';
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeLibrary(hLib);
  end;
end;

class function TDLMF.ContainerFileToPlainEx(FileName: TFileName; DLMFFilePath: string): string;
begin
  Result := RemoveDuplicates((SearchHTTPLinks(ContainerFileToPlain(FileName, DLMFFilePath))));
end;

end.
