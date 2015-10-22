function StringReplace(S, OldPattern, NewPattern) // Result
{
  var SearchStr, Patt, NewStr, Offset;

  SearchStr = S;
  Patt = OldPattern;
  NewStr = S;
  Result  = "";

  while (SearchStr != "")
  {
    Offset = Pos(Patt, SearchStr);
    if (Offset == 0)
    {
      Result  = Result  + NewStr;
      break;
    }
    Result  = Result  + copy(NewStr, 1, Offset - 1) + NewPattern;

    NewStr = copy(NewStr, Offset + Length(OldPattern), 10000000);
    SearchStr = copy(SearchStr, Offset + Length(Patt), 10000000);
  }
}


function ReduceCapitals(s) // Result
{
  for(var i = length(s); i > 1; i--)
  {
    if( (s[i] in ["A".."Z"]) && (s[i -1] in ["A".."Z"]) )
    {
      s[i] = LowerCase(s[i]);
    }
  }
  Result = s;
}

function nfo_stripper(nfo) // Result
{
  s = ReplaceRegExpr("[^\\w\\s-\\\\\\/\\.:]", nfo, "", False);
  Result = Trim(s);
}