unit uHTMLUtils;

interface

uses
  // Delphi
  SysUtils, HTTPApp,
  // RegEx
  RegExpr,
  // Utils
  uStringUtils;

function SimpleHTMLDecode(const AHTML: string): string;
function HTML2Text(const AHTML: string; AWithURLLink: Boolean = True; AUseExternalHTMLDecode: Boolean = False): string;
function HTML2TextAndDecode(const AHTML: string; AWithURLLink: Boolean = True): string;

implementation

function SimpleHTMLDecode(const AHTML: string): string;
begin
  Result := StringReplaceMultiple(AHTML, ['&quot;', '&amp;', '&lt;', '&gt;'], ['"', '&', '<', '>'], True);
end;

function HTML2Text(const AHTML: string; AWithURLLink: Boolean = True; AUseExternalHTMLDecode: Boolean = False): string;
var
  LHTML: string;
begin
  with TRegExpr.Create do
    try
      LHTML := ReplaceRegExpr('<style(.*?)/style>', AHTML, '', False);

      if AWithURLLink then
        LHTML := ReplaceRegExpr('<a href="([^:\/].*?)".*?>(.*?)<\/a>', LHTML, '$2 ($1)', True);

      LHTML := ReplaceRegExpr('<a href="(.*?)".*?>(.*?)<\/a>', LHTML, '$2', True);

      LHTML := ReplaceRegExpr('<(.*?)>', LHTML, '', False);
    finally
      Free;
    end;
  if not AUseExternalHTMLDecode then
    LHTML := SimpleHTMLDecode(LHTML);

  LHTML := StringReplaceMultiple(LHTML, { }
    ['&nbsp;', '&lsquo;', '&rsquo;', '&sbquo;', '&ldquo;', '&rdquo;', '&bdquo;', '&iexcl;', '&cent;', '&pound;', '&curren;', '&yen;', '&brvbar;', '&sect;',
    '&uml;', '&copy;', '&ordf;', '&laquo;', '&not;', '&shy;', '&reg;', '&macr;', '&deg;', '&plusmn;', '&sup2;', '&sup3;', '&acute;', '&micro;', '&para;',
    '&middot;', '&cedil;', '&sup1;', '&ordm;', '&raquo;', '&frac14;', '&frac12;', '&frac34;', '&iquest;', '&Agrave;', '&Aacute;', '&Acirc;', '&Atilde;',
    '&Auml;', '&Aring;', '&Aelig;', '&Ccedil;', '&Egrave;', '&Eacute;', '&Ecirc;', '&Euml;', '&Igrave;', '&Iacute;', '&Icirc;', '&Iuml;', '&Eth;', '&Ntilde;',
    '&Ograve;', '&Oacute;', '&Ocirc;', '&Otilde;', '&Ouml;', '&times;', '&Oslash;', '&Ugrave;', '&Uacute;', '&Ucirc;', '&Uuml;', '&Yacute;', '&thorn;',
    '&szlig;', '&agrave;', '&aacute;', '&acirc;', '&atilde;', '&auml;', '&aring;', '&aelig;', '&ccedil;', '&egrave;', '&eacute;', '&ecirc;', '&euml;',
    '&igrave;', '&iacute;', '&icirc;', '&iuml;', '&eth;', '&ntilde;', '&ograve;', '&oacute;', '&ocirc;', '&otilde;', '&ouml;', '&divide;', '&oslash;',
    '&ugrave;', '&uacute;', '&ucirc;', '&uuml;', '&yacute;', '&thorn;', '&yuml;', '&hellip;', '&ndash;', '&mdash;', '&trade;', '&euro;'], { }
    [' ', '‘', '’', '‚', '„', '”', '“', '¡', '¢', '£', '¤', '¥', '¦', '§', '¨', '©', 'ª', '«', '¬', '#0', '®', '¯', '°', '±', '²', '³', '´', 'µ', '¶', '·',
    '¸', '¹', 'º', '»', '¼', '½', '¾', '¿', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï', 'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö',
    '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß', 'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï', 'ð', 'ñ', 'ò', 'ó', 'ô', 'õ',
    'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ', '…', '–', '—', '™', '€'], True);

  Result := LHTML;
end;

function HTML2TextAndDecode(const AHTML: string; AWithURLLink: Boolean = True): string;
var
  LHTML: string;
begin
  LHTML := HTML2Text(AHTML, AWithURLLink, True);
  try
    LHTML := HTMLDecode(LHTML);
  except
    LHTML := SimpleHTMLDecode(LHTML);
  end;
  Result := LHTML;
end;

end.
