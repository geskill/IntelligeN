unit uWiiboxartCom;

interface

uses
  // Delphi
  SysUtils, Classes,
  // Indy
  IdMultipartFormData,
  // RegEx
  RegExpr,
  // Common
  uConst, uAppInterface,
  // Plugin system
  uPlugInCrawlerClass, uIdHTTPHelper;

type
  TWiiboxartCom = class(TCrawlerPlugIn)
  public
    function GetName: WideString; override;

    function GetAvailableTemplateTypeIDs: Integer; override; stdcall;
    function GetAvailableComponentIDs(const TemplateTypeID: Integer): Integer; override; stdcall;
    function GetComponentIDDefaultValue(const TemplateTypeID, ComponentID: Integer): Boolean; override; stdcall;
    function GetLimitDefaultValue: Integer; override; stdcall;

    procedure Exec(const ATemplateTypeID, AComponentIDs, ALimit: Integer; const AComponentController: IComponentController); override; stdcall;
  end;

implementation

function TWiiboxartCom.GetName;
begin
  result := 'wiiboxart.com';
end;

function TWiiboxartCom.GetAvailableTemplateTypeIDs;
var
  _TemplateTypeIDs: TTemplateTypeIDs;
begin
  _TemplateTypeIDs := [cWii];
  result := Word(_TemplateTypeIDs);
end;

function TWiiboxartCom.GetAvailableComponentIDs;
var
  // _TemplateTypeID: TTemplateTypeID;
  _ComponentIDs: TComponentIDs;
begin
  // _TemplateTypeID := TTemplateTypeID(TemplateTypeID);

  _ComponentIDs := [cPicture];

  result := LongWord(_ComponentIDs);
end;

function TWiiboxartCom.GetComponentIDDefaultValue;
begin
  result := True;
end;

function TWiiboxartCom.GetLimitDefaultValue;
begin
  result := 5;
end;

procedure TWiiboxartCom.Exec;
const
  website = 'http://www.wiiboxart.com/';
var
  _Title, s: string;
  Params: TIdMultiPartFormDataStream;
begin
  _Title := AComponentController.FindControl(cTitle).Value;

  with TIdHTTPHelper.Create(Self) do
    try
      Params := TIdMultiPartFormDataStream.Create;
      try
        Params.AddObject('search', '', '', TStringStream.Create(_Title, TEncoding.ASCII));
        Params.AddFormField('ie', 'ISO-8859-1');

        s := Post(website + 'search.php', Params);

        with TRegExpr.Create do
        begin
          try
            ModifierS := False;
            InputString := s;
            Expression := '"artwork\/cover\/(.*?)" target=';

            if Exec(InputString) then
            begin
              repeat
                AComponentController.FindControl(cPicture).AddValue(website + 'artwork/cover/' + Match[1], GetName);
              until not ExecNext;
            end;
          finally
            Free;
          end;
        end;
      finally
        Params.Free;
      end;
    finally
      Free;
    end;
end;

end.
