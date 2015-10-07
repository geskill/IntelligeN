{ ********************************************************
  *                                                      *
  *  Artistic Style Delphi Wrapper                       *
  *  Version 1.0.0.0                                     *
  *  Copyright (c) 2013 Sebastian Klatte                 *
  *                                                      *
  *  Artistic Style                                      *
  *  Copyright (c) 2006-2011 by Jim Pattee               *
  *  Copyright (c) 1998-2002 by Tal Davidson             *
  ******************************************************** }
unit uApiIScriptFormatter;

interface

uses
  // Delphi
  Windows, SysUtils;

type
  TIScriptFormatter = class
  public
    class function Format(AInputStr: AnsiString): string;
  end;

implementation

type
  TErrorHandler = procedure(AErrorCode: Integer; const AErrorMsg: PAnsiChar); stdcall;
  TMemoryHandler = function(ASize: LongInt): PAnsiChar; stdcall;

  TStyleMain = function(const AInputStr: PAnsiChar; const AOptions: PAnsiChar; AErrorHandler: TErrorHandler; AMemoryHandler: TMemoryHandler): PAnsiChar;
    stdcall;
  TStyleGetVersion = function: PAnsiChar; stdcall;

const
  LibraryName = 'AStyle.dll';

procedure ErrorHandler(AErrorCode: Integer; const AErrorMsg: PAnsiChar); stdcall;
begin
  OutputDebugString(PChar(LibraryName + ' error code: ' + IntToStr(AErrorCode) + ' message: ' + AErrorMsg));
end;

function MemoryHandler(ASize: LongInt): PAnsiChar; stdcall;
begin
  GetMem(Result, ASize * 2);
end;

{ TIScriptFormatter }

class function TIScriptFormatter.Format(AInputStr: AnsiString): string;
var
  hLib: Cardinal;
  MStyleMain: TStyleMain;
  buf: PAnsiChar;
begin
  buf := '';
  hLib := LoadLibrary(PChar(LibraryName));
  try
    if not(hLib = 0) then
    begin
      @MStyleMain := GetProcAddress(hLib, 'AStyleMain');

      buf := MStyleMain(PAnsiChar(AInputStr), PAnsiChar( { }
          '--style=break' + AnsiChar(#10) + { }
          '--indent=spaces=2' + AnsiChar(#10) + { }
          '--indent-switches' + AnsiChar(#10) + { }
          '--break-blocks' + AnsiChar(#10) + { }
          '--pad-oper' + AnsiChar(#10) + { }
          '--pad-header' + AnsiChar(#10) + { }
          '--unpad-paren' + AnsiChar(#10) + { }
          '--delete-empty-lines' + AnsiChar(#10) + { }
          // '--break-elseifs' + AnsiChar(#10) +
            '--mode=java'), ErrorHandler, MemoryHandler);

    end;
  finally
    FreeLibrary(hLib);
  end;

  Result := buf;
  FreeMem(buf);
end;

end.

