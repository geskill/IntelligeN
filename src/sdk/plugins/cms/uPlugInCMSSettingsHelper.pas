unit uPlugInCMSSettingsHelper;

interface

uses
  // Delphi
  Windows, SysUtils, Variants, XMLDoc, XMLIntf, ActiveX, TypInfo, Rtti,
  // Common
  uBaseConst, uBaseInterface, uAppConst,
  // Utils,
  uStringUtils,
  // Plugin system
  uPlugInConst, uPlugInCMSClass;

type
  TIntegerArray = array of Integer;

function InArray(AInteger: Integer; AArray: TIntegerArray): Boolean;

type
  TXMLAccess = reference to procedure(AXMLNode: IXMLNode);

  TPlugInCMSSettingsHelper = class
  public
    class function GetID(ANode: IXMLNode): string;
    class procedure SubSearch(ANode: IXMLNode; const AData: ITabSheetData; var AID: Variant);
    class function LoadSettingsToClass(AFileName: TFileName; ASettings: TCMSPlugInSettings; const AData: ITabSheetData = nil; AXMLAccess: TXMLAccess = nil): TIntegerArray;
    class procedure LoadSettingsToWebsiteEditor(AFileName: TFileName; AClass: TClass; AWebsiteEditor: IWebsiteEditor);
  end;

implementation

function InArray(AInteger: Integer; AArray: TIntegerArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := low(AArray) to high(AArray) do
    if (AArray[I] = AInteger) then
      Exit(True);
end;

{ TPlugInCMSSettings }

class function TPlugInCMSSettingsHelper.GetID(ANode: IXMLNode): string;
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

class procedure TPlugInCMSSettingsHelper.SubSearch;
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

        LControl := AData.Control[LControlID]; // AControlController.FindControl(StringToControlID(controltype));
        if Assigned(LControl) then
          if MatchTextMask(VarToStr(ChildNodes.Nodes[z].Attributes['value']), LControl.Value) then
          begin
            if (ChildNodes.Nodes[z].AttributeNodes.Count = 0) then
              raise Exception.Create('no ID attribute');

            AID := TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[z]);
            TPlugInCMSSettingsHelper.SubSearch(ChildNodes.Nodes[z], AData, AID);
            Break;
          end;
      end;
end;

class function TPlugInCMSSettingsHelper.LoadSettingsToClass;

  function VariantFix(AVariant: Variant; ATypeKind: TTypeKind): Variant;
  begin
    if ATypeKind = tkEnumeration then
      Result := VarAsType(AVariant, varBoolean)
    else if ATypeKind = tkInteger then
      Result := VarAsType(AVariant, varInteger)
    else
      Result := VarToStr(AVariant);
  end;

  procedure SubAllSearch(ANode: IXMLNode; var AArray: TIntegerArray);
  var
    z: Integer;
  begin
    with ANode do
      if HasChildNodes then
        for z := 0 to ChildNodes.Count - 1 do
        begin
          if (ChildNodes.Nodes[z].AttributeNodes.Count = 0) then
            raise Exception.Create('no ID attribute');

          if not InArray(StrToIntDef(TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[z]), 0), AArray) then
          begin
            if IsNumber(TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[z])) then
            begin
              SetLength(AArray, length(AArray) + 1);
              AArray[length(AArray) - 1] := StrToInt(TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[z]));
            end;
            SubAllSearch(ChildNodes.Nodes[z], AArray);
          end;
        end;
  end;

var
  XMLDoc: IXMLDocument;

  rttiContext: TRttiContext;
  rttiProperty: TRttiProperty;
  rttiType: TRttiType;
  rttiAttribute: TCustomAttribute;

  I: Integer;

  CMSCustomField: TCMSCustomField;

  IsTopValue: Boolean;
  // HasDefaultValue: Boolean;
  IDValue, DefaultValue: Variant;

begin
  SetLength(Result, 0);
  CoInitializeEx(nil, COINIT_MULTITHREADED);
  try
    XMLDoc := NewXMLDocument;
    try
      with XMLDoc do
      begin
        Options := Options + [doNodeAutoIndent];
        NodeIndentStr := #9;

        LoadFromFile(AFileName);

        Active := True;
      end;

      if Assigned(AXMLAccess) then
        AXMLAccess(XMLDoc.DocumentElement);

      with XMLDoc.DocumentElement do
        if HasChildNodes then
          ASettings.Charset := VarToStr(ChildNodes.Nodes['charset'].NodeValue);

      rttiContext := TRttiContext.Create();
      try
        rttiType := rttiContext.GetType(ASettings.ClassType);

        for rttiProperty in rttiType.GetDeclaredProperties do
        begin
          if rttiProperty.PropertyType.TypeKind = tkVariant then
          begin
            IDValue := null;

            if Assigned(AData) then
            begin
              with XMLDoc.DocumentElement do
                if HasChildNodes then
                  with ChildNodes.Nodes[rttiProperty.Name] do
                    if HasChildNodes then
                      for I := 0 to ChildNodes.Count - 1 do
                        if SameText(VarToStr(ChildNodes.Nodes[I].Attributes['name']), TypeIDToString(AData.TypeID)) then
                        begin
                          if (ChildNodes.Nodes[I].AttributeNodes.Count = 0) then
                            raise Exception.Create('no ID attribute');

                          IDValue := TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[I]);

                          if SameText(rttiProperty.Name, 'forums') then
                          begin
                            if IsNumber(TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[I])) then
                            begin
                              SetLength(Result, 1);
                              Result[0] := StrToInt(TPlugInCMSSettingsHelper.GetID(ChildNodes.Nodes[I]));
                            end;
                            SubAllSearch(ChildNodes.Nodes[I], Result);
                          end;

                          TPlugInCMSSettingsHelper.SubSearch(ChildNodes.Nodes[I], AData, IDValue);
                          Break;
                        end;
            end;
            // workaround for: http://stackoverflow.com/questions/2206681/how-to-set-null-to-variant-field-using-rtti
            if VarIsNull(IDValue) then
              rttiProperty.SetValue(ASettings, TValue.From<Variant>(null))
            else
              rttiProperty.SetValue(ASettings, TValue.FromVariant(IDValue));
          end
          else if SameStr(rttiProperty.PropertyType.Name, TCMSCustomFields.ClassName) then
          begin
            with XMLDoc.DocumentElement do
              if HasChildNodes then
                with ChildNodes.Nodes[rttiProperty.Name] do
                  if HasChildNodes then
                    for I := 0 to ChildNodes.Count - 1 do
                    begin
                      if not ChildNodes.Nodes[I].HasAttribute('name') then
                        raise Exception.Create('no name attribute');

                      CMSCustomField := TCMSCustomField.Create;
                      CMSCustomField.Name := VarToStr(ChildNodes.Nodes[I].Attributes['name']);
                      CMSCustomField.Value := VarToStr(ChildNodes.Nodes[I].NodeValue);

                      TCMSCustomFields(rttiProperty.GetValue(ASettings).AsObject).Add(CMSCustomField);
                    end;
          end
          else
          begin
            IsTopValue := False;
            DefaultValue := Unassigned;
            // HasDefaultValue := False;
            for rttiAttribute in rttiProperty.GetAttributes() do
            begin
              if rttiAttribute is AttrTopValue then
              begin
                IsTopValue := True;
              end;
              if rttiAttribute is AttrDefaultValue then
              begin
                // HasDefaultValue := True;
                DefaultValue := AttrDefaultValue(rttiAttribute).Value;
              end;
            end;

            with XMLDoc.DocumentElement do
              if HasChildNodes then
              begin
                if IsTopValue then
                begin
                  with ChildNodes.Nodes['name'] do
                    if AttributeNodes.FindNode(rttiProperty.Name) = nil then
                      rttiProperty.SetValue(ASettings, TValue.FromVariant(DefaultValue))
                    else
                    begin
                      rttiProperty.SetValue(ASettings, TValue.FromVariant(VariantFix(Attributes[rttiProperty.Name], rttiProperty.PropertyType.TypeKind)));
                    end;
                end
                else
                begin
                  with ChildNodes.Nodes['settings'] do
                    if ChildNodes.FindNode(rttiProperty.Name) = nil then
                      rttiProperty.SetValue(ASettings, TValue.FromVariant(DefaultValue))
                    else
                    begin
                      rttiProperty.SetValue(ASettings, TValue.FromVariant(VariantFix(ChildNodes.Nodes[rttiProperty.Name].NodeValue, rttiProperty.PropertyType.TypeKind)));
                    end;
                end;
              end;
          end;
        end;

      finally
        rttiContext.Free;
      end;

    finally
      XMLDoc := nil;
    end;
  finally
    CoUninitialize;
  end;
end;

(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

class procedure TPlugInCMSSettingsHelper.LoadSettingsToWebsiteEditor(AFileName: TFileName; AClass: TClass; AWebsiteEditor: IWebsiteEditor);

  procedure AddWebsiteEditorComponent(AName: string; ATypeKind: TTypeKind; ADefaultValue: Variant; ATopValue: Boolean = False);
  begin
    case ATypeKind of
      tkEnumeration:
        AWebsiteEditor.AddCheckbox(AName, ADefaultValue, ATopValue);
      // tkInteger:
      // s  ;
    else
      AWebsiteEditor.AddEdit(AName, ADefaultValue, ATopValue);
    end;
  end;

var
  rttiContext: TRttiContext;
  rttiProperty: TRttiProperty;
  rttiType: TRttiType;
  rttiAttribute: TCustomAttribute;

  IsTopValue: Boolean;
  DefaultValue: Variant;
begin
  rttiContext := TRttiContext.Create();
  try
    rttiType := rttiContext.GetType(AClass);

    for rttiProperty in rttiType.GetDeclaredProperties do
    begin
      if rttiProperty.PropertyType.TypeKind = tkVariant then
      begin
        AWebsiteEditor.AddCategoryTab(rttiProperty.Name);
      end
      else if SameStr(rttiProperty.PropertyType.Name, TCMSCustomFields.ClassName) then
      begin
        AWebsiteEditor.CustomFields := True;
      end
      else
      begin
        IsTopValue := False;
        DefaultValue := Unassigned;
        // HasDefaultValue := False;
        for rttiAttribute in rttiProperty.GetAttributes() do
        begin
          if rttiAttribute is AttrTopValue then
          begin
            IsTopValue := True;
          end;
          if rttiAttribute is AttrDefaultValue then
          begin
            // HasDefaultValue := True;
            DefaultValue := AttrDefaultValue(rttiAttribute).Value;
          end;
        end;

        AddWebsiteEditorComponent(rttiProperty.Name, rttiProperty.PropertyType.TypeKind, DefaultValue, IsTopValue);
      end;
    end;
  finally
    rttiContext.Free;
  end;
end;

end.
