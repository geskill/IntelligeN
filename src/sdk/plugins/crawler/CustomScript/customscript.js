/*
  You can create for every control a function

  i.e.: IGenre
  function IGenre() 

  the result can either be a simple string "abc", an integer value 123, a TStringList or false as boolean.
  if the result is false, the result will not be added to the crawler result
  for multiple values you can use a TStringList
*/


function IGenre() // Result
{
  if(MatchText("%Video2Brain%", IRELEASENAME))
    Result = "Education & Learning";
  else
    Result = false;
}

function IDescription() // Result
{
  Result = false;
}