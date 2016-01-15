{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn content management system class              *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2015 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInCMSSettingsHelper;

interface

uses
  // Delphi
  Windows, SysUtils, Variants, XMLDoc, XMLIntf, ActiveX, TypInfo,
  // Common
  uBaseConst, uBaseInterface, uAppConst,
  // Utils,
  uStringUtils,
  // Plugin system
  uPlugInConst, uPlugInCMSClass;

type
  TIntegerArray = array of Integer;

function InArray(const AInteger: Integer; const AArray: TIntegerArray): Boolean;

type
  TXMLAccess = reference to procedure(AXMLNode: IXMLNode);

  TPlugInCMSSettingsHelper = class
  private
    class function AfterString(P: Pointer): Pointer;
    class function GetPropData(TypeData: PTypeData): PPropData;
    class function NextPropInfo(PropInfo: PPropInfo): PPropInfo;
  protected { . }
    // see: http://stackoverflow.com/questions/34397112/possible-to-loop-only-declared-properties-of-a-class
    class procedure GetDeclaredPropInfos(TypeInfo: PTypeInfo; PropList: PPropList);
    class function GetDeclaredPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer; overload;
    class function GetDeclaredPropList(AObject: TObject; out PropList: PPropList): Integer; overload;
  public
    class function GetID(const ANode: IXMLNode): string;
    class procedure SubSearch(const ANode: IXMLNode; const AData: ITabSheetData; var AID: Variant);
    class function LoadSettingsToClass(const AFileName: TFileName; const ASettings: TCMSPlugInSettings; const AData: ITabSheetData = nil; AXMLAccess: TXMLAccess = nil): TIntegerArray;
    class procedure LoadSettingsToWebsiteEditor(const AFileName: TFileName; const ASettings: TCMSPlugInSettings; const AWebsiteEditor: IWebsiteEditor);
  end;

implementation

function InArray(const AInteger: Integer; const AArray: TIntegerArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := low(AArray) to high(AArray) do
    if (AArray[I] = AInteger) then
      Exit(True);
end;

{ TPlugInCMSSettings }

class function TPlugInCMSSettingsHelper.AfterString(P: Pointer): Pointer;
begin
  Result := Pointer(NativeUInt(P) + (PByte(P)^ + 1));
end;

class function TPlugInCMSSettingsHelper.GetPropData(TypeData: PTypeData): PPropData;
begin
  Result := AfterString(@TypeData^.UnitName);
end;

class function TPlugInCMSSettingsHelper.NextPropInfo(PropInfo: PPropInfo): PPropInfo;
begin
  Result := AfterString(@PropInfo^.Name);
end;

class procedure TPlugInCMSSettingsHelper.GetDeclaredPropInfos(TypeInfo: PTypeInfo; PropList: PPropList);
var
  TypeData: PTypeData;
  PropData: PPropData;
  PropInfo: PPropInfo;
  I: Integer;
begin
  TypeData := GetTypeData(TypeInfo);
  PropData := GetPropData(TypeData);
  FillChar(PropList^, Sizeof(PPropInfo) * PropData^.PropCount, 0);
  PropInfo := PPropInfo(@PropData^.PropList);
  for I := 0 to PropData^.PropCount - 1 do
  begin
    PropList^[I] := PropInfo;
    PropInfo := NextPropInfo(PropInfo);
  end;
end;

class function TPlugInCMSSettingsHelper.GetDeclaredPropList(TypeInfo: PTypeInfo; out PropList: PPropList): Integer;
begin
  Result := GetPropData(GetTypeData(TypeInfo))^.PropCount;
  if Result > 0 then
  begin
    GetMem(PropList, Result * SizeOf(Pointer));
    GetDeclaredPropInfos(TypeInfo, PropList);
  end;
end;

class function TPlugInCMSSettingsHelper.GetDeclaredPropList(AObject: TObject; out PropList: PPropList): Integer;
begin
  Result := GetDeclaredPropList(PTypeInfo(AObject.ClassInfo), PropList);
end;

class function TPlugInCMSSettingsHelper.GetID(const ANode: IXMLNode): string;
const
  IDNames: array [0 .. 4] of string = ('id', 'f', 't', 'p', 'i');
var
  I: Integer;
begin
  Result := '';
  for I := 0 to length(IDNames) - 1 do
    if not(ANode.AttributeNodes.IndexOf(IDNames[I]) = -1) then
      Exit(VarToStr(ANode.Attributes[IDNames[I]]));
end;

class procedure TPlugInCMSSettingsHelper.SubSearch(const ANode: IXMLNode; const AData: ITabSheetData; var AID: Variant);
var
  z: Integer;
  LControlID: string;
  LControl: IControlData;
begin
  with ANode do
    if HasChildNodes then
      for z := 0 to ChildNodes.Count - 1 do
      begin
        LControlID := VarToStr(ChildNodes.Nodes[z].Attributes['type']);

        if not StringInControlID(LControlID) then
          raise Exception.Create('Unknown control id in subtype definition');

        LControl := AData.Control[LControlID]; // AControlController.FindControl(StringToControlID(controltype));
        if Assigned(LControl) then
          if MatchTextMask(VarToStr(ChildNodes.Nodes[z].Attributes['value']), LControl.Value) then
          begin
            if (ChildNodes.Nodes[z].AttributeNodes.Count = 0) then
              raise Exception.Create('no ID attribute');

            AID := GetID(ChildNodes.Nodes[z]);
            SubSearch(ChildNodes.Nodes[z], AData, AID);
            Break;
          end;
      end;
end;

class function TPlugInCMSSettingsHelper.LoadSettingsToClass(const AFileName: TFileName; const ASettings: TCMSPlugInSettings; const AData: ITabSheetData = nil; AXMLAccess: TXMLAccess = nil): TIntegerArray;

  procedure SubAllSearch(const ANode: IXMLNode; var AArray: TIntegerArray);
  var
    z: Integer;
  begin
    // search all defined ids used for intelligent_posting_boundedsearch
    with ANode do
      if HasChildNodes then
        for z := 0 to ChildNodes.Count - 1 do
        begin
          if (ChildNodes.Nodes[z].AttributeNodes.Count = 0) then
            raise Exception.Create('no ID attribute');

          if not InArray(StrToIntDef(GetID(ChildNodes.Nodes[z]), 0), AArray) then
          begin
            if IsNumber(GetID(ChildNodes.Nodes[z])) then
            begin
              SetLength(AArray, length(AArray) + 1);
              AArray[length(AArray) - 1] := StrToInt(GetID(ChildNodes.Nodes[z]));
            end;
            SubAllSearch(ChildNodes.Nodes[z], AArray);
          end;
        end;
  end;

var
  LXMLDoc: IXMLDocument;
  LXMLIndex: Integer;

  LPropIndex: Integer;
  LPropCount: Integer;
  LPropList: PPropList;
  LPropInfo: PPropInfo;

  LCMSCustomField: TCMSCustomField;

  LIDValue: Variant;
begin
  SetLength(Result, 0);
  CoInitializeEx(nil, COINIT_MULTITHREADED);
  try
    LXMLDoc := NewXMLDocument;
    try
      with LXMLDoc do
      begin
        Options := Options + [doNodeAutoIndent];
        NodeIndentStr := #9;

        LoadFromFile(AFileName);

        Active := True;
      end;

      if Assigned(AXMLAccess) then
        AXMLAccess(LXMLDoc.DocumentElement);

      with LXMLDoc.DocumentElement do
        if HasChildNodes then
          ASettings.Charset := VarToStr(ChildNodes.Nodes['charset'].NodeValue);

      // see: http://stackoverflow.com/questions/10188459/how-to-loop-all-properties-in-a-class
      LPropCount := GetDeclaredPropList(ASettings.ClassInfo, LPropList);
      try
        for LPropIndex := 0 to LPropCount - 1 do
        begin
          LPropInfo := LPropList^[LPropIndex];

          if (LPropInfo.PropType^.Kind = tkVariant) then
          begin
            LIDValue := null;

            if Assigned(AData) then
            begin
              with LXMLDoc.DocumentElement do
                if HasChildNodes then
                  with ChildNodes.Nodes[LPropInfo.Name] do
                    if HasChildNodes then
                      for LXMLIndex := 0 to ChildNodes.Count - 1 do
                        if SameText(VarToStr(ChildNodes.Nodes[LXMLIndex].Attributes['name']), TypeIDToString(AData.TypeID)) then
                        begin
                          if (ChildNodes.Nodes[LXMLIndex].AttributeNodes.Count = 0) then
                            raise Exception.Create('no ID attribute');

                          LIDValue := GetID(ChildNodes.Nodes[LXMLIndex]);

                          if SameText(LPropInfo.Name, 'forums') then
                          begin
                            if IsNumber(GetID(ChildNodes.Nodes[LXMLIndex])) then
                            begin
                              SetLength(Result, 1);
                              Result[0] := StrToInt(GetID(ChildNodes.Nodes[LXMLIndex]));
                            end;
                            SubAllSearch(ChildNodes.Nodes[LXMLIndex], Result);
                          end;

                          SubSearch(ChildNodes.Nodes[LXMLIndex], AData, LIDValue);
                          Break;
                        end;
            end;
            SetPropValue(ASettings, LPropInfo, LIDValue);
          end
          else if SameStr(LPropInfo.PropType^.Name, TCMSCustomFields.ClassName) then
          begin
            with LXMLDoc.DocumentElement do
              if HasChildNodes then
                with ChildNodes.Nodes[LPropInfo.Name] do
                  if HasChildNodes then
                    for LXMLIndex := 0 to ChildNodes.Count - 1 do
                    begin
                      if not ChildNodes.Nodes[LXMLIndex].HasAttribute('name') then
                        raise Exception.Create('no name attribute');

                      LCMSCustomField := TCMSCustomField.Create;
                      LCMSCustomField.Name := VarToStr(ChildNodes.Nodes[LXMLIndex].Attributes['name']);
                      LCMSCustomField.Value := VarToStr(ChildNodes.Nodes[LXMLIndex].NodeValue);

                      TCMSCustomFields(GetObjectProp(ASettings, LPropInfo)).Add(LCMSCustomField);
                    end;
          end
          else
          begin
            with LXMLDoc.DocumentElement do
              if HasChildNodes then
                with ChildNodes.Nodes['settings'] do
                begin
                  if Assigned(ChildNodes.FindNode(LPropInfo.Name)) then
                    SetPropValue(ASettings, LPropInfo, ChildNodes.Nodes[LPropInfo.Name].NodeValue);
                end;
          end;
        end;
      finally
        FreeMem(LPropList);
      end;
    finally
      LXMLDoc := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

class procedure TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(const AFileName: TFileName; const ASettings: TCMSPlugInSettings; const AWebsiteEditor: IWebsiteEditor);

  procedure AddWebsiteEditorComponent(AName: string; ATypeKind: TTypeKind; ADefaultValue: Variant);
  begin
    case ATypeKind of
      tkEnumeration:
        AWebsiteEditor.AddCheckbox(AName, ADefaultValue);
      // tkInteger:
      // ;
    else
      AWebsiteEditor.AddEdit(AName, ADefaultValue);
    end;
  end;

var
  LPropIndex: Integer;
  LPropCount: Integer;
  LPropList: PPropList;
  LPropInfo: PPropInfo;

  LDefaultValue: Variant;
begin
  // see: http://stackoverflow.com/questions/10188459/how-to-loop-all-properties-in-a-class
    LPropCount := GetDeclaredPropList(ASettings.ClassInfo, LPropList);
    try
      for LPropIndex := 0 to LPropCount - 1 do
      begin
        LPropInfo := LPropList^[LPropIndex];

        if (LPropInfo.PropType^.Kind = tkVariant) then
        begin
          AWebsiteEditor.AddCategoryTab(LPropInfo.Name);
        end
        else if SameStr(LPropInfo.PropType^.Name, TCMSCustomFields.ClassName) then
        begin
          AWebsiteEditor.CustomFields := True;
        end
        else
        begin
          LDefaultValue := GetPropValue(ASettings, LPropInfo);

          AddWebsiteEditorComponent(LPropInfo.Name, LPropInfo.PropType^.Kind, LDefaultValue);
        end;
      end;
    finally
      FreeMem(LPropList);
    end;
end;

end.
