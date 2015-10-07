unit uApiComponentParser;

interface

uses
  // Delphi
  Types,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiSettings,
  // DevExpress
  cxProgressBar;

type
  TComponentParser = class
  private
    I: Integer;
    FWorkPanelWidth: Integer;
    FComponentController: IControlController;
    FMirrorController: IMirrorController;
    FProgressBar: TcxProgressBar;
    PreviousControl, CurrentControl: IBasic;
    PreviousMirrorControl, CurrentMirrorControl: IMirrorControl;
    procedure SetProgressBarPosition(ANewPosition: Extended);
    procedure MoveLeft;
    procedure MoveDown;
    procedure MoveUp;
    procedure MoveMirrors;
  public
    procedure Start;
    property WorkPanelWidth: Integer read FWorkPanelWidth write FWorkPanelWidth;
    function NextControlPosition: TPoint;
    function NextMirrorPosition: TPoint;
    property ControlController: IControlController read FComponentController write FComponentController;
    property MirrorController: IMirrorController read FMirrorController write FMirrorController;
    property ProgressBar: TcxProgressBar read FProgressBar write FProgressBar;
    destructor Destroy; override;
  end;

implementation

procedure TComponentParser.SetProgressBarPosition(ANewPosition: Extended);
begin
  with FProgressBar do
  begin
    Position := ANewPosition;
    Repaint;
    // Application.ProcessMessages;
  end;
end;

procedure TComponentParser.MoveLeft;
begin
  if (PreviousControl.Top = CurrentControl.Top) then
    CurrentControl.Left := (PreviousControl.Left + PreviousControl.Width + SettingsManager.Settings.ComponentParser.PaddingWidth)
  else
    CurrentControl.Left := SettingsManager.Settings.ComponentParser.PaddingWidth;
end;

procedure TComponentParser.MoveDown;
var
  Z, ZTop, NewTop: Integer;
begin
  NewTop := 0;
  for Z := 0 to I - 1 do
  begin
    ZTop := ControlController.Control[Z].Top + ControlController.Control[Z].Height;
    if (NewTop < ZTop) then
      NewTop := ZTop;
  end;
  CurrentControl.Top := NewTop + SettingsManager.Settings.ComponentParser.PaddingHeight;
end;

procedure TComponentParser.MoveUp;
begin
  CurrentControl.Top := PreviousControl.Top;
end;

function TComponentParser.NextControlPosition: TPoint;
begin
  //
end;

function TComponentParser.NextMirrorPosition: TPoint;
var
  _LastControl: IBasic;
  _PreviousMirrorControl: IMirrorControl;
begin
  with SettingsManager.Settings.ComponentParser do
    if MirrorController.MirrorCount = 0 then
    begin
      result.X := PaddingLeft;

      _LastControl := ControlController.Control[ControlController.ControlCount - 1];

      if (MirrorPosition = mpButtom) then
        result.Y := _LastControl.Top + _LastControl.Height + PaddingHeight
      else
        result.Y := 0;

      _LastControl := nil;
    end
    else
    begin
      _PreviousMirrorControl := MirrorController.Mirror[MirrorController.MirrorCount - 1];

      if (Frac((MirrorController.MirrorCount - 1) / MirrorColumns) = 0.0) then
        result.Y := (_PreviousMirrorControl.Top + MirrorHeight + PaddingHeight)
      else
        result.Y := _PreviousMirrorControl.Top;

      if _PreviousMirrorControl.Top = result.Y then
        result.X := _PreviousMirrorControl.Left + _PreviousMirrorControl.Width + PaddingLeft
      else
        result.X := PaddingLeft;

      _PreviousMirrorControl := nil;
    end;
end;

procedure TComponentParser.MoveMirrors;
var
  _MirrorControlIndex, _MirrorControlsWidth: Integer;
  _NewPBPosition: Extended;
begin
  if MirrorController.MirrorCount <= 0 then
    Exit;

  with SettingsManager.Settings.ComponentParser do
  begin
    // Alle MirrorControls sind gleich breit
    _MirrorControlsWidth := Trunc((WorkPanelWidth - (MirrorColumns * PaddingWidth)) / MirrorColumns);

    with MirrorController do
    begin
      // Das erste MirrorControl positionieren
      CurrentMirrorControl := Mirror[0];

      with CurrentMirrorControl do
      begin
        Left := PaddingLeft;
        Width := _MirrorControlsWidth;
        if (MirrorPosition = mpButtom) then
          Top := CurrentControl.Top + CurrentControl.Height + PaddingHeight
        else
          Top := 0;
      end;

      // Alle weiteren MirrorControls positionieren
      for _MirrorControlIndex := 1 to MirrorCount - 1 do
        with Mirror[_MirrorControlIndex] do
        begin
          PreviousMirrorControl := Mirror[_MirrorControlIndex - 1];
          CurrentMirrorControl := Mirror[_MirrorControlIndex];

          _NewPBPosition := (_MirrorControlIndex) * (50 / MirrorCount);
          if (MirrorPosition = mpButtom) then
            _NewPBPosition := _NewPBPosition + 50;
          SetProgressBarPosition(_NewPBPosition);

          with CurrentMirrorControl do
          begin
            if (Frac(_MirrorControlIndex / MirrorColumns) = 0.0) then
              Top := (PreviousMirrorControl.Top + MirrorHeight + PaddingHeight)
            else
              Top := PreviousMirrorControl.Top;

            if PreviousMirrorControl.Top = Top then
              Left := PreviousMirrorControl.Left + PreviousMirrorControl.Width + PaddingLeft
            else
              Left := PaddingLeft;

            Width := _MirrorControlsWidth;
          end;
        end;
    end;
  end;
end;

procedure TComponentParser.Start;
var
  _ControlIndex: Integer;
  NewPBPosition: Extended;
begin
  SetProgressBarPosition(0);

  with SettingsManager.Settings.ComponentParser do
  begin
    if (MirrorPosition = mpTop) then
    begin
      MoveMirrors;
      SetProgressBarPosition(50);
    end;

    // Das erste Control positionieren
    if (ControlController.ControlCount > 0) then
    begin
      CurrentControl := ControlController.Control[0];
      with CurrentControl do
      begin
        Left := PaddingLeft;

        if (MirrorPosition = mpTop) and (MirrorController.MirrorCount > 0) then
          Top := CurrentMirrorControl.Top + MirrorHeight + PaddingWidth
        else
          Top := 0;
      end;
    end;
  end;

  // Alle weiteren Controls positionieren
  for _ControlIndex := 1 to ControlController.ControlCount - 1 do
  begin
    I := _ControlIndex;

    PreviousControl := ControlController.Control[I - 1];
    CurrentControl := ControlController.Control[I];

    MoveLeft;

    NewPBPosition := (_ControlIndex) * (50 / ControlController.ControlCount);
    if SettingsManager.Settings.ComponentParser.MirrorPosition = mpTop then
      NewPBPosition := NewPBPosition + 50;
    SetProgressBarPosition(NewPBPosition);

    if ((CurrentControl.Left + CurrentControl.Width) > WorkPanelWidth) then
    begin
      MoveDown;
      MoveLeft;
    end
    else if not(PreviousControl.Top = CurrentControl.Top) then
    begin
      if not((PreviousControl.Left + PreviousControl.Width + SettingsManager.Settings.ComponentParser.PaddingWidth + CurrentControl.Width) > WorkPanelWidth)
        then
      begin
        MoveUp;
        MoveLeft;
      end
      else
      begin
        MoveDown;
        MoveLeft;
      end;
    end
  end;

  with SettingsManager.Settings.ComponentParser do
    if (MirrorPosition = mpButtom) then
    begin
      SetProgressBarPosition(50);

      MoveMirrors;
    end;

  SetProgressBarPosition(100);

  PreviousControl := nil;
  CurrentControl := nil;

  PreviousMirrorControl := nil;
  CurrentMirrorControl := nil;

  ControlController := nil;
  MirrorController := nil;
end;

destructor TComponentParser.Destroy;
begin
  PreviousControl := nil;
  CurrentControl := nil;

  PreviousMirrorControl := nil;
  CurrentMirrorControl := nil;

  ControlController := nil;
  MirrorController := nil;

  inherited Destroy;
end;

end.
