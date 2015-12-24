unit uApiControlAligner;

interface

uses
  // Delphi
  Types,
  // DevExpress
  cxProgressBar,
  // Common
  uBaseConst, uBaseInterface, uAppConst, uAppInterface;

type
  // TODO: Replace with DevExpress TdxLayoutControl in the future
  // https://www.devexpress.com/Support/Center/Question/Details/Q258014
  TControlAligner = class
  private
    FIndex: Integer;
    FWorkPanelWidth: Integer;
    FComponentController: IControlController;
    FMirrorController: IMirrorController;
    FProgressBar: TcxProgressBar;
    FPreviousControl, FCurrentControl: IControlBasic;
    FPreviousMirrorControl, FCurrentMirrorControl: IMirrorControl;
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
    destructor Destroy; override;
  end;

implementation

uses
  // Api
  uApiSettings;

procedure TControlAligner.MoveLeft;
begin
  if (FPreviousControl.Top = FCurrentControl.Top) then
    FCurrentControl.Left := (FPreviousControl.Left + FPreviousControl.Width + SettingsManager.Settings.ControlAligner.PaddingWidth)
  else
    FCurrentControl.Left := SettingsManager.Settings.ControlAligner.PaddingWidth;
end;

procedure TControlAligner.MoveDown;
var
  Z, ZTop, NewTop: Integer;
begin
  NewTop := 0;
  for Z := 0 to FIndex - 1 do
  begin
    ZTop := ControlController.Control[Z].Top + ControlController.Control[Z].Height;
    if (NewTop < ZTop) then
      NewTop := ZTop;
  end;
  FCurrentControl.Top := NewTop + SettingsManager.Settings.ControlAligner.PaddingHeight;
end;

procedure TControlAligner.MoveUp;
begin
  FCurrentControl.Top := FPreviousControl.Top;
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

        result.Y := FCurrentMirrorControl.Top + MirrorHeight + PaddingHeight;

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
  LMirrorControlIndex, LMirrorControlsWidth: Integer;
begin
  if MirrorController.MirrorCount <= 0 then
    Exit;

  with SettingsManager.Settings.ControlAligner do
  begin
    // Alle MirrorControls sind gleich breit
    LMirrorControlsWidth := Trunc((WorkPanelWidth - (MirrorColumns * PaddingWidth)) / MirrorColumns);

    with MirrorController do
    begin
      // Das erste MirrorControl positionieren
      FCurrentMirrorControl := Mirror[0];

      with FCurrentMirrorControl do
      begin
        Left := PaddingLeft;
        Width := LMirrorControlsWidth;
        if (MirrorPosition = mpBottom) then
          Top := FCurrentControl.Top + FCurrentControl.Height + PaddingHeight
        else
          Top := 0;
      end;

      // Alle weiteren MirrorControls positionieren
      for LMirrorControlIndex := 1 to MirrorCount - 1 do
        with Mirror[LMirrorControlIndex] do
        begin
          FPreviousMirrorControl := Mirror[LMirrorControlIndex - 1];
          FCurrentMirrorControl := Mirror[LMirrorControlIndex];

          with FCurrentMirrorControl do
          begin
            if (Frac(LMirrorControlIndex / MirrorColumns) = 0.0) then
              Top := (FPreviousMirrorControl.Top + MirrorHeight + PaddingHeight)
            else
              Top := FPreviousMirrorControl.Top;

            if FPreviousMirrorControl.Top = Top then
              Left := FPreviousMirrorControl.Left + FPreviousMirrorControl.Width + PaddingLeft
            else
              Left := PaddingLeft;

            Width := LMirrorControlsWidth;
          end;
        end;
    end;
  end;
end;

procedure TControlAligner.Start;
var
  LControlIndex: Integer;
begin
  with SettingsManager.Settings.ControlAligner do
  begin
    if (MirrorPosition = mpTop) then
    begin
      MoveMirrors;
    end;

    // Das erste Control positionieren
    if (ControlController.ControlCount > 0) then
    begin
      FCurrentControl := ControlController.Control[0];
      with FCurrentControl do
      begin
        Left := PaddingLeft;

        if (MirrorPosition = mpTop) and (MirrorController.MirrorCount > 0) then
          Top := FCurrentMirrorControl.Top + MirrorHeight + PaddingHeight
        else
          Top := 0;
      end;
    end;
  end;

  // Alle weiteren Controls positionieren
  for LControlIndex := 1 to ControlController.ControlCount - 1 do
  begin
    FIndex := LControlIndex;

    FPreviousControl := ControlController.Control[FIndex - 1];
    FCurrentControl := ControlController.Control[FIndex];

    MoveLeft;

    if ((FCurrentControl.Left + FCurrentControl.Width) > WorkPanelWidth) then
    begin
      MoveDown;
      MoveLeft;
    end
    else if not(FPreviousControl.Top = FCurrentControl.Top) then
    begin
      if not((FPreviousControl.Left + FPreviousControl.Width + SettingsManager.Settings.ControlAligner.PaddingWidth + FCurrentControl.Width) > WorkPanelWidth) then
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
      MoveMirrors;
    end;

  FPreviousControl := nil;
  FCurrentControl := nil;

  FPreviousMirrorControl := nil;
  FCurrentMirrorControl := nil;

  ControlController := nil;
  MirrorController := nil;
end;

destructor TControlAligner.Destroy;
begin
  FPreviousControl := nil;
  FCurrentControl := nil;

  FPreviousMirrorControl := nil;
  FCurrentMirrorControl := nil;

  ControlController := nil;
  MirrorController := nil;

  inherited Destroy;
end;

end.
