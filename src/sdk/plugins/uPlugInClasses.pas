{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn classes                                      *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInClasses;

interface

uses
  // Delphi
  Classes,
  // Plugin
  uPlugInInterface, uPlugInConst;

type
  TPlugInInfoObject = class(TInterfacedObject)
  protected

  public
  end;

  TPlugInInfoObjectList = class(TInterfacedObject)
  private
    FList: TInterfaceList;
  protected
    function AddElement(const AElement: IInterface): Integer;
    function GetCount: Integer; safecall;
    function GetElement(const AIndex: Integer): IInterface; safecall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TPlugInInfoObjectList }

function TPlugInInfoObjectList.AddElement;
begin
  Result := FList.Add(AElement)
end;

function TPlugInInfoObjectList.GetCount;
begin
  Result := FList.Count;
end;

function TPlugInInfoObjectList.GetElement;
begin
  Result := FList[AIndex];
end;

constructor TPlugInInfoObjectList.Create;
begin
  inherited Create;
  FList := TInterfaceList.Create;
end;

destructor TPlugInInfoObjectList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

end.
