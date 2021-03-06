unit uApiUpdateConst;

interface

type
  TUpdateCondition = (ucNew, ucFound, ucMissing);

  TUpdateAction = (uaAddnUpdate, uaEditnUpdate, uaDelete, uaIgnoreThisUpdate);

  TUpdateActions = set of TUpdateAction;

implementation

end.
