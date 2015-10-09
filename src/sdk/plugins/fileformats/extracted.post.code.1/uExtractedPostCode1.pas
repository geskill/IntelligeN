unit uExtractedPostCode1;

interface

uses
  // Delphi
  SysUtils, Classes, Variants, StrUtils, XMLDoc, XMLIntf, ActiveX,
  // Common
  uBaseConst, uAppInterface,
  // Utils
  uStringUtils,
  // Plugin system
  uPlugInFileFormatClass;

type
  TExtractedPostCode = class(TFileFormatPlugIn)
  public
    function GetName: WideString; override; safecall;
    function GetFileFormatName: WideString; override; safecall;
    function CanSaveControls: WordBool; override; safecall;
    procedure SaveControls(AFileName, ATemplateFileName: WideString; const ATabSheetController: ITabSheetController); override; safecall;
    function CanLoadControls: WordBool; override; safecall;
    function LoadControls(AFileName, ATemplateDirectory: WideString; const APageController: IPageController): Integer; override; safecall;
  end;

implementation

{ TExtractedPostCode }

function TExtractedPostCode.GetName;
begin
  result := 'extracted.post.code.1';
end;

function TExtractedPostCode.GetFileFormatName;
begin
  result := 'extracted post code 1 %s (*.epc)|*.epc|';
end;

function TExtractedPostCode.CanSaveControls;
begin
  result := True;
end;

procedure TExtractedPostCode.SaveControls;
var
  XMLDoc: IXMLDocument;
  I: Integer;
  _hoster: string;
begin
  OleInitialize(nil);
  try
    XMLDoc := NewXMLDocument;

    with XMLDoc do
    begin
      Encoding := 'iso-8859-1';
      Options := Options + [doNodeAutoIndent];
      DocumentElement := CreateElement('Settings', '');
      Active := True;
    end;
    with XMLDoc.DocumentElement do
    begin
      with AddChild('Title') do
        NodeValue := ATabSheetController.ControlController.FindControl(cTitle).Value;
      with AddChild('SelectedArea') do
        case ATabSheetController.ControlController.TypeID of
          cAudio:
            NodeValue := '24';
          cGameCube:
            NodeValue := '4';
          cMovie:
            NodeValue := '14';
          cNintendoDS:
            NodeValue := '10';
          cPCGames:
            NodeValue := '4';
          cPlayStation2:
            NodeValue := '6';
          cPlayStation3:
            NodeValue := '7';
          cPlayStationPortable:
            NodeValue := '8';
          cSoftware:
            NodeValue := '0';
          cWii:
            NodeValue := '9';
          cXbox:
            NodeValue := '11';
          cXbox360:
            NodeValue := '12';
          cXXX:
            NodeValue := '31';
          cOther:
            NodeValue := '';
        end;
      with AddChild('Post') do
        with ATabSheetController do
          with TStringList.Create do
            try
              Add('.');
              Add('[center]');
              if Assigned(ControlController.FindControl(cPicture)) then
                Add('[img]' + ControlController.FindControl(cPicture).Value + '[/img]');

              Add('');

              if Assigned(ControlController.FindControl(cVideoSystem)) then
                Add('[b]Video System:[/b] ' + ControlController.FindControl(cVideoSystem).Value);
              if Assigned(ControlController.FindControl(cVideoStream)) then
                Add('[b]Video Stream:[/b] ' + ControlController.FindControl(cVideoStream).Value);
              if Assigned(ControlController.FindControl(cAudioStream)) then
                Add('[b]Audio Stream:[/b] ' + ControlController.FindControl(cAudioStream).Value);
              if Assigned(ControlController.FindControl(cAudioBitrate)) then
                Add('[b]Audio Bitrate:[/b] ' + ControlController.FindControl(cAudioBitrate).Value);
              if Assigned(ControlController.FindControl(cVideoCodec)) then
                Add('[b]Video Codec:[/b] ' + ControlController.FindControl(cVideoCodec).Value);
              if Assigned(ControlController.FindControl(cAudioEncoder)) then
                Add('[b]Audio Encoder:[/b] ' + ControlController.FindControl(cAudioEncoder).Value);
              if Assigned(ControlController.FindControl(cAudioSamplingRate)) then
                Add('[b]Audio Sampling Rate:[/b] ' + ControlController.FindControl(cAudioSamplingRate).Value);

              if Assigned(ControlController.FindControl(cGenre)) then
                Add('[b]Genre:[/b] ' + ControlController.FindControl(cGenre).Value);
              if Assigned(ControlController.FindControl(cLanguage)) then
                Add('[b]Language/s:[/b] ' + ControlController.FindControl(cLanguage).Value);
              Add('[b]Parts:[/b] ' + IntToStr(CharCount('http://', MirrorController.Mirror[0].Directlink[0].Value)));
              if Assigned(ControlController.FindControl(cPassword)) and (ControlController.FindControl(cPassword).Value <> '') then
                Add('[b]Password:[/b] ' + ControlController.FindControl(cPassword).Value);

              for I := 0 to MirrorController.MirrorCount - 1 do
                if MirrorController.Mirror[I].Size > 0 then
                begin
                  Add('[b]Size:[/b] ' + FloatToStr(MirrorController.Mirror[I].Size) + ' MB');
                  break;
                end;

              _hoster := '[b]Hoster:[/b]';
              for I := 0 to MirrorController.MirrorCount - 1 do
              begin
                _hoster := _hoster + ' ' + MirrorController.Mirror[I].Hoster;
                if not(I = MirrorController.MirrorCount - 1) then
                  _hoster := _hoster + ',';
              end;
              Add(_hoster);

              if Assigned(ControlController.FindControl(cDescription)) then
              begin
                Add('');
                Add(ControlController.FindControl(cDescription).Value);
              end;

              Add('');

              for I := 0 to MirrorController.MirrorCount - 1 do
              begin
                Add('[b]Mirror: ' + IntToStr(I + 1) + '[/b]');
                Add('');
                Add('[b]' + MirrorController.Mirror[I].Hoster + '[/b]');
                Add('[code]' + MirrorController.Mirror[I].Directlink[0].Value + '[/code]');
                if not(I = MirrorController.MirrorCount - 1) then
                  Add('');
              end;

              Add('[/center]');
              NodeValue := Text;
            finally
              Free;
            end;
    end;

    XMLDoc.SaveToFile(ChangeFileExt(AFileName, '.epc'));
  finally
    XMLDoc := nil;
    OleUninitialize;
  end;
end;

function TExtractedPostCode.CanLoadControls;
begin
  result := False;
end;

function TExtractedPostCode.LoadControls;
begin
  result := -1;
end;

end.
