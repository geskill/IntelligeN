unit uMycxRichEdit;

interface

uses
  // Delphi
  Messages, Controls, StdCtrls, Dialogs,
  // Dev Express
  cxRichEdit;

type
  TMycxRichEdit = class(cxRichEdit.TcxRichEdit)
  protected
    fOnDropFiles: TWndMethod;
    procedure WMDROPFILES(var Msg: TMessage); message WM_DROPFILES;
  public
    property OnDropFiles: TWndMethod read fOnDropFiles write fOnDropFiles;
  end;

implementation

procedure TMycxRichEdit.WMDROPFILES(var Msg: TMessage);
begin
  if Assigned(fOnDropFiles) then
    fOnDropFiles(Msg)
  else
    inherited;
end;

end.
