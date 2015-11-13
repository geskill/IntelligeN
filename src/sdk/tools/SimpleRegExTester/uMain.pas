unit uMain;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ComCtrls, RegExpr, Generics.Collections;

type
  TMain = class(TForm)
    eRegularExpression: TEdit;
    lRegularExpression: TLabel;
    lTestString: TLabel;
    mTestString: TMemo;
    lSubstitution: TLabel;
    eSubstitution: TEdit;
    cbModifierI: TCheckBox;
    cbModifierR: TCheckBox;
    cbModifierS: TCheckBox;
    cbModifierG: TCheckBox;
    cbModifierM: TCheckBox;
    cbModifierX: TCheckBox;
    lMatch: TLabel;
    tbMatches: TTabControl;
    mMatch: TMemo;
    lClearTestString: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lClearTestStringClick(Sender: TObject);
    procedure mTestStringKeyPress(Sender: TObject; var Key: Char);
    procedure tbMatchesChange(Sender: TObject);

    //
    procedure OnChange(Sender: TObject);

  private
    FResultList: TList<string>;

  public
    procedure CleanUp();
    procedure UpdateResult();
    procedure Update();
    procedure AddMatch(AMatch: string);
  end;

var
  Main: TMain;

implementation

{$R *.dfm}
{ TMain }

procedure TMain.FormCreate(Sender: TObject);
begin
  FResultList := TList<string>.Create;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FResultList.Free;
end;

procedure TMain.lClearTestStringClick(Sender: TObject);
begin
  mTestString.Lines.Clear;
end;

procedure TMain.mTestStringKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^A then
  begin
    (Sender as TMemo).SelectAll;
    Key := #0;
  end;
end;

procedure TMain.tbMatchesChange(Sender: TObject);
begin
  UpdateResult;
end;

procedure TMain.OnChange(Sender: TObject);
begin
  Update;
end;

procedure TMain.CleanUp;
begin
  FResultList.Clear;
  tbMatches.Tabs.Clear;
end;

procedure TMain.UpdateResult;
begin
  mMatch.Enabled := (tbMatches.Tabs.Count > 0);

  if (tbMatches.Tabs.Count > 0) then
  begin
    mMatch.Lines.Text := FResultList[tbMatches.TabIndex];
  end
  else
  begin
    mMatch.Lines.Clear;
  end;
end;

procedure TMain.Update;
var
  LMatchCount: Integer;
  LStringList: TStringList;
begin
  CleanUp;

  if not SameStr('', eRegularExpression.Text) then
  begin
    try

      // Calculate new
      with TRegExpr.Create do
        try
          ModifierI := cbModifierI.Checked;
          ModifierR := cbModifierR.Checked;
          ModifierS := cbModifierS.Checked;
          ModifierG := cbModifierG.Checked;
          ModifierM := cbModifierM.Checked;
          ModifierX := cbModifierX.Checked;

          InputString := mTestString.Lines.Text;
          Expression := eRegularExpression.Text;

          if not SameStr('', eSubstitution.Text) then
          begin
            AddMatch(Replace(InputString, eSubstitution.Text, True));
          end
          else
          begin
            if Exec(InputString) then
            begin
              repeat

                LStringList := TStringList.Create;
                try
                  for LMatchCount := 0 to SubExprMatchCount { - 1 } do
                  begin
                    LStringList.Add('[' + IntToStr(LMatchCount) + '] ' + Match[LMatchCount]);
                  end;
                  AddMatch(LStringList.Text);
                finally
                  LStringList.Free;
                end;

              until not ExecNext;
            end;
          end;
        finally
          Free;
        end;

    finally
      tbMatches.TabIndex := 0;

      UpdateResult;
    end;
  end;
end;

procedure TMain.AddMatch(AMatch: string);
begin
  tbMatches.Tabs.Add(IntToStr(tbMatches.Tabs.Count + 1));
  FResultList.Add(AMatch);
end;

end.
