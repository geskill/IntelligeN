unit uExtractedPostCode1;

interface

uses
  // Delphi
  SysUtils, Classes, Variants, StrUtils, XMLDoc, XMLIntf, ActiveX,
  // Common
  uConst, uAppInterface,
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
        NodeValue := ATabSheetController.ComponentController.FindControl(cTitle).Value;
      with AddChild('SelectedArea') do
        case ATabSheetController.ComponentController.TemplateTypeID of
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
              if Assigned(ComponentController.FindControl(cPicture)) then
                Add('[img]' + ComponentController.FindControl(cPicture).Value + '[/img]');

              Add('');

              if Assigned(ComponentController.FindControl(cVideoSystem)) then
                Add('[b]Video System:[/b] ' + ComponentController.FindControl(cVideoSystem).Value);
              if Assigned(ComponentController.FindControl(cVideoStream)) then
                Add('[b]Video Stream:[/b] ' + ComponentController.FindControl(cVideoStream).Value);
              if Assigned(ComponentController.FindControl(cAudioStream)) then
                Add('[b]Audio Stream:[/b] ' + ComponentController.FindControl(cAudioStream).Value);
              if Assigned(ComponentController.FindControl(cAudioBitrate)) then
                Add('[b]Audio Bitrate:[/b] ' + ComponentController.FindControl(cAudioBitrate).Value);
              if Assigned(ComponentController.FindControl(cVideoCodec)) then
                Add('[b]Video Codec:[/b] ' + ComponentController.FindControl(cVideoCodec).Value);
              if Assigned(ComponentController.FindControl(cAudioEncoder)) then
                Add('[b]Audio Encoder:[/b] ' + ComponentController.FindControl(cAudioEncoder).Value);
              if Assigned(ComponentController.FindControl(cAudioSamplingRate)) then
                Add('[b]Audio Sampling Rate:[/b] ' + ComponentController.FindControl(cAudioSamplingRate).Value);

              if Assigned(ComponentController.FindControl(cGenre)) then
                Add('[b]Genre:[/b] ' + ComponentController.FindControl(cGenre).Value);
              if Assigned(ComponentController.FindControl(cLanguage)) then
                Add('[b]Language/s:[/b] ' + ComponentController.FindControl(cLanguage).Value);
              Add('[b]Parts:[/b] ' + IntToStr(CharCount('http://', MirrorController.Mirror[0].DirectlinksMirror[0])));
              if Assigned(ComponentController.FindControl(cPassword)) and (ComponentController.FindControl(cPassword).Value <> '') then
                Add('[b]Password:[/b] ' + ComponentController.FindControl(cPassword).Value);

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

              if Assigned(ComponentController.FindControl(cDescription)) then
              begin
                Add('');
                Add(ComponentController.FindControl(cDescription).Value);
              end;

              Add('');

              for I := 0 to MirrorController.MirrorCount - 1 do
              begin
                Add('[b]Mirror: ' + IntToStr(I + 1) + '[/b]');
                Add('');
                Add('[b]' + MirrorController.Mirror[I].Hoster + '[/b]');
                Add('[code]' + MirrorController.Mirror[I].DirectlinksMirror[0] + '[/code]');
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
