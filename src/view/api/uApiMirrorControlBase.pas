unit uApiMirrorControlBase;

interface

uses
  // Common
  uBaseConst, uBaseInterface,
  // Api
  uApiControlsBase;

type
  TIMirrorData = class(TIValueItem, IMirrorData)

  end;

  TIMirrorContainer = class(TIMirrorData, IMirrorContainer)

  end;

implementation

end.
