unit uImagesGoogleDe;

interface

uses
  // Delphi
  SysUtils, Classes, HTTPApp,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  TImagesGoogleDe = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override; stdcall;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override; stdcall;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override; stdcall;
    function GetLimitDefaultValue: Integer; override; stdcall;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override; stdcall;
  end;

implementation

function TImagesGoogleDe.GetName;
begin
  result := 'images.google.de';
end;

function TImagesGoogleDe.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [ low(TTemplateTypeID) .. high(TTemplateTypeID)];
  result := Word(_TemplateTypeIDs);
end;

function TImagesGoogleDe.GetAvailableComponentIDs;
var
  _ComponentIDs: TComponentIDs;
begin
  _ComponentIDs := [cPicture];
  result := LongWord(_ComponentIDs);
end;

function TImagesGoogleDe.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TImagesGoogleDe.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TImagesGoogleDe.Exec;
const
  website = 'https://www.google.de/';
var
  _ComponentIDs: TComponentIDs;
  Title: string;
  ReplyData: TStringStream;
  Count: Integer;
begin
  LongWord(_ComponentIDs) := AComponentIDs;
  Title := AComponentController.FindControl(cTitle).Value;

  with TIdHTTPHelper.Create(Self) do
    try
      ReplyData := TStringStream.Create;
      try

        if (AComponentController.FindControl(cPicture) <> nil) and (cPicture in _ComponentIDs) then
        begin
          if TTemplateTypeID(ATemplateTypeID) = cMovie then
            Get(website + 'search?tbm=isch&q=' + HTTPEncode(Title + ' cover'), ReplyData)
          else
            Get(website + 'search?tbm=isch&q=' + HTTPEncode(TTemplateTypeIDToString(TTemplateTypeID(ATemplateTypeID)) + ' ' + Title + ' cover'), ReplyData);

          Count := 0;

          with TRegExpr.Create do
            try
              InputString := ReplyData.DataString;
              Expression := 'imgres\?imgurl=(.*?)&';

              if Exec(InputString) then
              begin
                repeat
                  AComponentController.FindControl(cPicture).AddValue(Match[1], GetName);
                  Inc(Count);
                until not(ExecNext and ((Count < ALimit) or (ALimit = 0)));
              end;
            finally
              Free;
            end;
        end;
      finally
        ReplyData.Free;
      end;

    finally
      Free;
    end;
end;

end.
