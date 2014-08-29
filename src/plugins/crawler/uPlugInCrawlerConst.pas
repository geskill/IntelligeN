unit uPlugInCrawlerConst;

interface

uses
  // Common
  uConst;

type
  TCrawlerContingent = record
    TemplateTypeID: TTemplateTypeID;
    ComponentID: TComponentID;
    Status: Boolean;
  end;

  TCrawlerContingentArray = array of TCrawlerContingent;

  function GetCrawlerContingentArrayStatus(ACrawlerContingentArray: TCrawlerContingentArray; AComponentID: TComponentID):Boolean;

implementation

function GetCrawlerContingentArrayStatus(ACrawlerContingentArray: TCrawlerContingentArray; AComponentID: TComponentID):Boolean;
var
  I:Integer;
begin
  Result := False;
  for I := 0 to length(ACrawlerContingentArray) - 1 do
    if ACrawlerContingentArray[I].ComponentID = AComponentID then
    begin
      Result := ACrawlerContingentArray[I].Status;
      break;
    end;
end;

end.
