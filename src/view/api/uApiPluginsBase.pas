unit uApiPluginsBase;

interface

uses
  // Delphi
  Windows, SysUtils,
  // Spring Framework
  Spring.Utils,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiConst, uApiLogManager, uApiMain,
  // Plugin system
  uPlugInConst, uPlugInInterface;

type
  TPluginErrorProc = reference to procedure(const AErrorMsg: string);
  TPluginProc = reference to procedure(var APlugin: IPlugIn);

  EIntelligeNPluginException = class(Exception)

  end;

  TPluginBase = class
  protected
    class procedure ReturnError(AErrorMsg: string; AErrorProc: TPluginErrorProc = nil);
  public
    class procedure LoadPluginBase(const APluginFile: string; out ALibraryHandle: Cardinal; APluginProc: TPluginProc; AUnLoad: Boolean = True; AErrorProc: TPluginErrorProc = nil);
    class procedure UnLoadPluginBase(const ALibraryHandle: Cardinal);
  end;

implementation

class procedure TPluginBase.ReturnError;
begin
  TLogManager.Instance().Add(AErrorMsg);

  if Assigned(AErrorProc) then
    AErrorProc(AErrorMsg)
  else
    raise EIntelligeNPluginException.Create(AErrorMsg);
end;

class procedure TPluginBase.LoadPluginBase;
var
  LPluginFileName: string;
  LPluginMinorVersion: Integer;
  LLoadPlugIn: TLoadPlugIn;
  LPlugIn: IPlugIn;
  LLoadPlugInStatus: Boolean;
begin
  LPluginFileName := ExtractFileName(APluginFile);
  LPluginMinorVersion := TFileVersionInfo.GetVersionInfo(APluginFile).FileVersionNumber.Minor;

  if FileExists(APluginFile) then
  begin
    if (MINOR_VERSION = LPluginMinorVersion) then
    begin
      try
        ALibraryHandle := LoadLibrary(PChar(APluginFile));
        try
          if not(ALibraryHandle = 0) then
          begin
            @LLoadPlugIn := GetProcAddress(ALibraryHandle, 'LoadPlugIn'); { do not localize }
            if not(@LLoadPlugIn = nil) then
            begin
              try
                LLoadPlugInStatus := LLoadPlugIn(LPlugIn);
                try
                  if LLoadPlugInStatus then
                  begin
                    APluginProc(LPlugIn);
                  end
                  else
                  begin
                    TPluginBase.ReturnError(Format(StrPluginStartupError, [LPluginFileName, LPlugIn.ErrorMsg]), AErrorProc);
                  end;
                finally
                  LPlugIn := nil;
                end;
              except
                TPluginBase.UnLoadPluginBase(ALibraryHandle);
                raise ;
              end;
            end
            else
            begin
              // (@LLoadPlugIn = nil)
              TPluginBase.ReturnError(Format(StrPluginUnknown, [LPluginFileName, SysErrorMessage(GetLastError())]), AErrorProc);
            end;
          end
          else
          begin
            // (LHandle = 0)
            TPluginBase.ReturnError(Format(StrPluginDefect, [LPluginFileName, SysErrorMessage(GetLastError())]), AErrorProc);
          end;
        finally
          if AUnLoad then
          begin
            TPluginBase.UnLoadPluginBase(ALibraryHandle);
          end;
        end;
      except
        TPluginBase.UnLoadPluginBase(ALibraryHandle);
        raise ;
      end;
    end
    else
    begin
      // not(MINOR_VERSION = LPluginMinorVersion)
      TPluginBase.ReturnError(Format(StrPluginIncompatible, [IntToStr(LPluginMinorVersion), IntToStr(MINOR_VERSION), LPluginFileName]), AErrorProc);
    end;
  end
  else
  begin
    // not(FileExists(PluginPath))
    TPluginBase.ReturnError(Format(StrPluginFileNotFound, [LPluginFileName]), AErrorProc);
  end;
end;

class procedure TPluginBase.UnLoadPluginBase;
begin
  if not(ALibraryHandle = 0) then
  begin
    FreeLibrary(ALibraryHandle);
  end;
end;

end.
