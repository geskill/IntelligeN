unit uApiUpdateConst;

interface

type
  TUpdateCondition = (ucNew, ucCondition, ucMissing);

  TUpdateAction = (uaAddnUpdate, uaEditnUpdate, uaDelete, uaIgnoreThisUpdate);

  TUpdateActions = set of TUpdateAction;

implementation

end.
