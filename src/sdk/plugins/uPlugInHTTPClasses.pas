{ ********************************************************
  *                            IntelligeN PLUGIN SYSTEM  *
  *  PlugIn HTTP bridge                                  *
  *  Version 2.5.0.0                                     *
  *  Copyright (c) 2016 Sebastian Klatte                 *
  *                                                      *
  ******************************************************** }
unit uPlugInHTTPClasses;

interface

uses
  // HTTPManager
  uHTTPClasses,
  // plugin system
  uPlugInClass;

type
  TPlugInHTTPOptions = class(THTTPOptions)
  public
    constructor Create(const APlugIn: TPlugIn); overload;
  end;

implementation

{ TPlugInHTTPOptions }

constructor TPlugInHTTPOptions.Create(const APlugIn: TPlugIn);
begin
  inherited Create(APlugIn.Proxy);

  ConnectTimeout := APlugIn.ConnectTimeout;
  ReadTimeout := APlugIn.ReadTimeout;
end;

end.
