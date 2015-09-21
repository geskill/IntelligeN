unit uMyTMonospaceHint;

interface

uses
  // Delphi
  Controls, Classes;

type
  TMyMonospaceHint = class(THintWindow)
    public
      constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TMyMonospaceHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  with Canvas.Font do
  begin
    Name := 'Courier New';

    //

  end;  
end;

end.
