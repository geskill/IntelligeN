function nfo_stripper(nfo) // Result
{
  s = ReplaceRegExpr("[^\\w\\s-\\\\\\/\\.:]", nfo, "", False);
  Result = Trim(s);
}

function html_spoiler(content, internalName, labelShow, labelHide) // Result
{
  Result = "<a id=\"show_id_" + internalName + "\" onclick=\"document.getElementById('spoiler_id_" + internalName + "').style.display=''; document.getElementById('show_id_" + internalName + "').style.display='none';\" style=\"cursor:pointer\">" + 
  
  labelShow + "</a><span id=\"spoiler_id_" + internalName + "\" style=\"display: none\">" +
  
  "<a onclick=\"document.getElementById('spoiler_id_" + internalName + "').style.display='none'; document.getElementById('show_id_" + internalName + "').style.display='';\" style=\"cursor:pointer\">" +
  
  labelHide + "</a><br />" + content + "</span>";
}