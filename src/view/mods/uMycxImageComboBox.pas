unit uMycxImageComboBox;

interface

uses
  // Delphi
  Windows, Controls, ImgList,
  // Dev Express
  cxGraphics, cxControls, cxContainer, cxEdit, cxDropDownEdit;

type
  TMycxComboBoxViewData = class(TcxCustomComboBoxViewData)
  public
    function GetClientExtent(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo): TRect; override;
  end;

  TMycxComboBoxViewInfo = class(TcxCustomComboBoxViewInfo)
  protected
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  end;

  TMycxComboBoxProperties = class(TcxCustomComboBoxProperties)
  protected
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
  public
    class function GetContainerClass: TcxContainerClass; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
  end;

  TMycxImageComboBox = class(TcxComboBox)
  private
    FImageList: TImageList;
    procedure SetImageList(Value: TImageList);
  public
    destructor Destroy; override;

    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ImageList: TImageList read FImageList write SetImageList;
  end;

implementation

{ TMycxComboBoxViewData }

function TMycxComboBoxViewData.GetClientExtent(ACanvas: TcxCanvas; AViewInfo: TcxCustomEditViewInfo): TRect;
begin
  Result := inherited GetClientExtent(ACanvas, AViewInfo);
  with TMycxImageComboBox(Self.Edit) do
    if Assigned(ImageList) then
      Result.Left := Result.Left + ImageList.Width + 4;
end;

{ TMycxComboBoxViewInfo }

procedure TMycxComboBoxViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  LImageRect: TRect;
begin
  inherited InternalPaint(ACanvas);
  LImageRect := Bounds;
  LImageRect.Left := LImageRect.Left + 1;
  with Self.Edit as TMycxImageComboBox do
    LImageRect.Right := LImageRect.Left + ImageList.Width + 2;
  InflateRect(LImageRect, -1, -2);
  with Self.Edit as TMycxImageComboBox do
  begin
    if not(ItemIndex = -1) then
    begin
      cxDrawImage(ACanvas.Handle, LImageRect, LImageRect, nil, ImageList, Integer(Properties.Items.Objects[ItemIndex]), idmNormal, False, 0, ImageList.BkColor, False);
    end;
  end;
end;

{ TMycxComboBoxProperties }

class function TMycxComboBoxProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TMycxComboBoxViewData;
end;

class function TMycxComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TMycxImageComboBox;
end;

class function TMycxComboBoxProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TMycxComboBoxViewInfo;
end;

{ TMycxComboBox }

destructor TMycxImageComboBox.Destroy;
begin
  FImageList := nil;
  inherited Destroy;
end;

class function TMycxImageComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TMycxComboBoxProperties;
end;

procedure TMycxImageComboBox.SetImageList(Value: TImageList);
begin
  if Value <> FImageList then
    FImageList := Value;
end;

end.
