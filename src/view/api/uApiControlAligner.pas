unit uApiControlAligner;

interface

uses
  // Delphi
  Types,
  // DevExpress
  cxProgressBar,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface,
  // Api
  uApiSettings;

type
  // TODO: Replace with DevExpress TdxLayoutControl in the future
  // https://www.devexpress.com/Support/Center/Question/Details/Q258014
  TControlAligner = class
  private
    I: Integer;
    FWorkPanelWidth: Integer;
    FComponentController: IControlController;
    FMirrorController: IMirrorController;
    FProgressBar: TcxProgressBar;
    PreviousControl, CurrentControl: IControlBasic;
    PreviousMirrorControl, CurrentMirrorControl: IMirrorControl;
    procedure SetProgressBarPosition(ANewPosition: Extended);
    procedure MoveLeft;
    procedure MoveDown;
    procedure MoveUp;
    procedure MoveMirrors;
  public
    procedure Start;
    property WorkPanelWidth: Integer read FWorkPanelWidth write FWorkPanelWidth;
    function NextControlPosition(ANewControlHeight, ANewControlWidth: Integer): TPoint;
    function NextMirrorPosition: TPoint;
    property ControlController: IControlController read FComponentController write FComponentController;
    property MirrorController: IMirrorController read FMirrorController write FMirrorController;
    property ProgressBar: TcxProgressBar read FProgressBar write FProgressBar;
    destructor Destroy; override;
  end;

implementation

procedure TControlAligner.SetProgressBarPosition(ANewPosition: Extended);
begin
  // FUTURE: Remove the progress bar. Not really necessary
  with FProgressBar do
  begin
    Position := ANewPosition;
    Repaint;
    // Application.ProcessMessages;
  end;
end;

procedure TControlAligner.MoveLeft;
begin
  if (PreviousControl.Top = CurrentControl.Top) then
    CurrentControl.Left := (PreviousControl.Left + PreviousControl.Width + SettingsManager.Settings.ControlAligner.PaddingWidth)
  else
    CurrentControl.Left := SettingsManager.Settings.ControlAligner.PaddingWidth;
end;

procedure TControlAligner.MoveDown;
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
  CurrentControl.Top := NewTop + SettingsManager.Settings.ControlAligner.PaddingHeight;
end;

procedure TControlAligner.MoveUp;
begin
  CurrentControl.Top := PreviousControl.Top;
end;

function TControlAligner.NextControlPosition(ANewControlHeight, ANewControlWidth: Integer): TPoint;
var
  LPreviousMirrorControl: IMirrorControl;
  LPreviousControl: IControlBasic;
begin
  with SettingsManager.Settings.ControlAligner do
    if (ControlController.ControlCount = 0) then
    begin
      result.X := PaddingLeft;

      if (MirrorPosition = mpTop) then
      begin
        LPreviousMirrorControl := MirrorController.Mirror[MirrorController.MirrorCount - 1];

        result.Y := CurrentMirrorControl.Top + MirrorHeight + PaddingHeight;

        LPreviousMirrorControl := nil;
      end
      else
      begin
        result.Y := 0;
      end;
    end
    else
    begin
      LPreviousControl := ControlController.Control[ControlController.ControlCount - 1];

      if ( LPreviousControl.Left + LPreviousControl.Width + PaddingWidth + ANewControlWidth) > WorkPanelWidth then
      begin
        result.X := PaddingLeft;
        result.Y := LPreviousControl.Top + LPreviousControl.Height + PaddingHeight;
      end
      else
      begin
        result.X := LPreviousControl.Left + LPreviousControl.Width + PaddingWidth;
        result.Y := LPreviousControl.Top;
      end;

      LPreviousControl := nil;
    end;
end;

function TControlAligner.NextMirrorPosition: TPoint;
var
  LPreviousControl: IControlBasic;
  LPreviousMirrorControl: IMirrorControl;
begin
  with SettingsManager.Settings.ControlAligner do
    if (MirrorController.MirrorCount = 0) then
    begin
      result.X := PaddingLeft;

      if (MirrorPosition = mpBottom) then
      begin
        LPreviousControl := ControlController.Control[ControlController.ControlCount - 1];

        result.Y := LPreviousControl.Top + LPreviousControl.Height + PaddingHeight;

        LPreviousControl := nil;
      end
      else // (MirrorPosition = mpTop)
      begin
        result.Y := 0;
      end;
    end
    else
    begin
      LPreviousMirrorControl := MirrorController.Mirror[MirrorController.MirrorCount - 1];

      if (Frac((MirrorController.MirrorCount - 1) / MirrorColumns) = 0.0) then
        result.Y := (LPreviousMirrorControl.Top + MirrorHeight + PaddingHeight)
      else
        result.Y := LPreviousMirrorControl.Top;

      if LPreviousMirrorControl.Top = result.Y then
        result.X := LPreviousMirrorControl.Left + LPreviousMirrorControl.Width + PaddingLeft
      else
        result.X := PaddingLeft;

      LPreviousMirrorControl := nil;
    end;
end;

procedure TControlAligner.MoveMirrors;
var
  _MirrorControlIndex, _MirrorControlsWidth: Integer;
  _NewPBPosition: Extended;
begin
  if MirrorController.MirrorCount <= 0 then
    Exit;

  with SettingsManager.Settings.ControlAligner do
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
        if (MirrorPosition = mpBottom) then
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
          if (MirrorPosition = mpBottom) then
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

procedure TControlAligner.Start;
var
  _ControlIndex: Integer;
  NewPBPosition: Extended;
begin
  SetProgressBarPosition(0);

  with SettingsManager.Settings.ControlAligner do
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
          Top := CurrentMirrorControl.Top + MirrorHeight + PaddingHeight
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
    if SettingsManager.Settings.ControlAligner.MirrorPosition = mpTop then
      NewPBPosition := NewPBPosition + 50;
    SetProgressBarPosition(NewPBPosition);

    if ((CurrentControl.Left + CurrentControl.Width) > WorkPanelWidth) then
    begin
      MoveDown;
      MoveLeft;
    end
    else if not(PreviousControl.Top = CurrentControl.Top) then
    begin
      if not((PreviousControl.Left + PreviousControl.Width + SettingsManager.Settings.ControlAligner.PaddingWidth + CurrentControl.Width) > WorkPanelWidth) then
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

  with SettingsManager.Settings.ControlAligner do
    if (MirrorPosition = mpBottom) then
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

destructor TControlAligner.Destroy;
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
