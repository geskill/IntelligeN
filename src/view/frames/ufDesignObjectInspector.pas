unit ufDesignObjectInspector;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, Mask;
// EControl Form Designer

type
  TfDesignObjectInspector = class(TFrame)
    pComponentSettings: TPanel;
    lComponentName: TLabel;
    eComponentName: TEdit;
    procedure pPositionResize(Sender: TObject);
    procedure pSizeResize(Sender: TObject);
  private
    FUNCTION GetComponentName: String;
    PROCEDURE SetComponentName(Value: String);
  public
    PROPERTY ComponentName: String READ GetComponentName WRITE SetComponentName;
  end;

implementation

{$R *.dfm}

FUNCTION TfDesignObjectInspector.GetComponentName: String;
BEGIN
  result := eComponentName.Text;
END;

PROCEDURE TfDesignObjectInspector.SetComponentName(Value: String);
BEGIN
  eComponentName.Text := Value;
END;

procedure TfDesignObjectInspector.pPositionResize(Sender: TObject);
// var
// size: Integer;
begin
  {
    size := (pPosition.Width - 6);
    size := (size DIV 2);
    cxSETop.Width := Size; cxSELeft.Width := Size;
    lLeft.Left := (pPosition.Width - Size);
    cxSELeft.Left := lLeft.Left;
    }
end;

procedure TfDesignObjectInspector.pSizeResize(Sender: TObject);
// var
// size: Integer;
begin
  {
    size := (pSize.Width - 6);
    size := (size DIV 2);
    cxSEHeight.Width := Size; cxSEWidth.Width := Size;
    lWidth.Left := (pSize.Width - Size);
    cxSEWidth.Left := lWidth.Left;
    }
end;

end.
