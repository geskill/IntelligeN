unit uPlugInHTTPClasses;

interface

uses
  // HTTPManager
  uHTTPClasses,
  // plugin system
  uPlugInInterface;

type
  TPlugInHTTPOptions = class(THTTPOptions)
  public
    constructor Create(APlugIn: IPlugIn); overload;
  end;

implementation

{ TPlugInHTTPOptions }

constructor TPlugInHTTPOptions.Create(APlugIn: IPlugIn);
begin
  inherited Create(APlugIn.Proxy);

  ConnectTimeout := APlugIn.ConnectTimeout;
  ReadTimeout := APlugIn.ReadTimeout;
end;

end.
