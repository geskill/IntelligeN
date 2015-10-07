unit uNewDesignWindow;

interface

uses
  // Delphi
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TNewDesignWindow = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
  private
    { Private-Deklarationen }
  public
    PROCEDURE Show;
    PROCEDURE Hide;
  end;

implementation

{$R *.dfm}

PROCEDURE TNewDesignWindow.Show;
BEGIN
  // Momentanes Design Laden
END;

PROCEDURE TNewDesignWindow.Hide;
BEGIN
  // Momentanes Design Verwerfen
END;

end.
