unit uCustomScriptEngine;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Variants,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // API
  uApiIScriptParser,
  // Utils
  uPathUtils, uSystemUtils,
  // CustomScript
  uCustomScriptSettings;

type
  TCustomScriptEngine = class
  public
    procedure Execute(const ATabSheetController: ITabSheetController; const ACustomScriptSettings: TCustomScriptSettings);
  end;

implementation

{ TCustomScriptEngine }

procedure TCustomScriptEngine.Execute(const ATabSheetController: ITabSheetController; const ACustomScriptSettings: TCustomScriptSettings);
var
  LScriptFileName: string;
  LConfigFile: TStringList;
  LControlIndex: Integer;
  LControl: IControlBasic;
  LControlID: TControlID;
  LParamArray: Variant;
  LParamIndex: Integer;
  LResult: Variant;
  LIScriptResult: RIScriptResult;
begin
  LScriptFileName := PathCombineEx(GetModulePath, ACustomScriptSettings.ScriptFileName);

  if FileExists(LScriptFileName) then
  begin
    LConfigFile := TStringList.Create;
    try
      LConfigFile.LoadFromFile(LScriptFileName);

      with TIScirptParser.Create(ATabSheetController, LConfigFile.Text) do
        try
          for LControlIndex := 0 to ATabSheetController.ControlController.ControlCount - 1 do
          begin
            LControl := ATabSheetController.ControlController.Control[LControlIndex];
            LControlID := LControl.ControlID;
            LParamArray := VarArrayCreate([0, LControl.ProposedValuesCount - 1], varVariant);
            for LParamIndex := 0 to LControl.ProposedValuesCount - 1 do
            begin
              LParamArray[LParamIndex] := LControl.GetProposedValue(LParamIndex);
            end;
            LIScriptResult := CallFunction3('Get' + ControlIDToString(LControlID), LParamArray, LResult);
            if not LIScriptResult.HasError then
            begin
              if not VarIsNull(LResult) and not VarIsOrdinal(LResult) then
              begin
                LControl.AddProposedValue('CustomScript', LResult);
                if ACustomScriptSettings.OverrideValue then
                  LControl.Value := LResult;
              end;
            end;
          end;
        finally
          Free;
        end;
    finally
      LConfigFile.Free;
    end;
  end;
end;

end.
