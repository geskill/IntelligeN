unit ufDesigner;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,

  ufDesignObjectInspector,

  uNewDesignWindow;

type
  TfDesigner = class(TFrame)
    procedure FrameResize(Sender: TObject);
  private
    NewDesignWindow: TNewDesignWindow;
  public
  end;

implementation

uses
  uMain;

{$R *.dfm}

procedure TfDesigner.FrameResize(Sender: TObject);
begin
  NewDesignWindow.Height := Height;
  NewDesignWindow.Width := Width;
end;

end.
