unit uApiXmlSettings;

interface

uses
  // Delphi
  Windows, SysUtils, Classes, Forms, Controls, TypInfo, Variants, XMLDoc, XMLIntf, ActiveX,
  // Common
  uConst, uAppInterface,
  // DLLs
  uExport,
  // Api
  uApiConst,
  // Utils
  uFileUtils, uStringUtils;

procedure LoadDefaultControlValues(AControlsTT: TCollection);

implementation

uses
  uApiSettings;

procedure LoadDefaultControlValues(AControlsTT: TCollection);

  procedure internal(Node: IXMLNode; var Title, HelpText, Value: string; Items: TCollection);
  var
    I, J: Integer;
  begin
    Title := VarToStr(Node.ChildNodes.FindNode('title').Attributes['name']);
    HelpText := VarToStr(Node.ChildNodes.FindNode('hint').Attributes['name']);
    Value := VarToStr(Node.ChildNodes.FindNode('value').Attributes['name']);

    for I := 0 to Node.ChildNodes.Count - 1 do
      if Node.ChildNodes.Nodes[I].NodeName = 'list' then
      begin
        with TControlItemsCollectionItem(Items.Add) do
        begin
          ItemName := Node.ChildNodes.Nodes[I].Attributes['name'];

          if Node.ChildNodes.Nodes[I].HasChildNodes then
            with TStringList.Create do
              try
                for J := 0 to Node.ChildNodes.Nodes[I].ChildNodes.Count - 1 do
                  Add(VarToStr(Node.ChildNodes.Nodes[I].ChildNodes.Nodes[J].NodeValue));
                AlsoKnownAs := Text;
              finally
                Free;
              end;
        end;
      end;
  end;

var
  I, X, Y: Integer;
  sl: TStrings;
  XMLDoc: IXMLDocument;
  Found: Boolean;
  bTitle, bHelpText, bValue: string;
begin
  if not FileExists(GetConfigurationFolder + 'controls.xml') then
    raise Exception.Create('controls.xml not found located at configuration\controls.xml')
  else
  begin
    OleInitialize(nil);
    try
      XMLDoc := NewXMLDocument;

      with XMLDoc do
      begin
        LoadFromFile(GetConfigurationFolder + 'controls.xml');
        Active := True;
      end;

      for I := 0 to length(TStringTemplateTypeID) - 1 do
        with TControlsCollectionItem(AControlsTT.Add) do
        begin
          TemplateTypeID := TTemplateTypeID(I);
          for X := 0 to length(TStringComponentID) - 1 do
            with TControlCollectionItem(Controls.Add) do
            begin
              ComponentID := TComponentID(X);

              with XMLDoc.DocumentElement.ChildNodes.Nodes[TComponentIDToString(TComponentID(X))].ChildNodes do
              begin
                Found := False;
                for Y := 0 to Count - 1 do
                begin
                  sl := SplittString('|', Nodes[Y].Attributes['name']);
                  try
                    if sl.IndexOf(TTemplateTypeIDToString(TTemplateTypeID(I))) <> -1 then
                    begin
                      internal(Nodes[Y], bTitle, bHelpText, bValue, Items);

                      Title := bTitle;
                      HelpText := bHelpText;
                      Value := bValue;

                      Found := True;
                      // break;
                    end;
                  finally
                    sl.Free;
                  end;
                end;
                if not Found then
                  for Y := 0 to Count - 1 do
                    if Nodes[Y].Attributes['name'] = '' then
                    begin
                      internal(Nodes[Y], bTitle, bHelpText, bValue, Items);

                      Title := bTitle;
                      HelpText := bHelpText;
                      Value := bValue;

                      // break;
                    end;
              end;
            end;
        end;
    finally
      XMLDoc := nil;
      OleUninitialize;
    end;
  end;
end;

end.
