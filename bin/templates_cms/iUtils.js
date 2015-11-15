function nfo_stripper(nfo) // Result
{
  s = ReplaceRegExpr("[^\\w\\s-\\\\\\/\\.:]", nfo, "", False);
  Result = Trim(s);
}