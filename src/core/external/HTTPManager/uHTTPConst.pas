unit uHTTPConst;

interface

type
  TProxyType = (ptHTTP, ptSOCKS4, ptSOCKS4A, ptSOCKS5);

  TParamType = (ptList, ptData, ptMultipartFormData);

  THTTPMethod = (mGET, mPOST);

const
  HTTPConnection: string = 'Keep-Alive';

  HTTPRequestAccept: string = 'text/html, application/xml;q=0.9, application/xhtml+xml, image/png, image/jpeg, image/gif, image/x-xbitmap, */*;q=0.1';
  HTTPRequestAcceptCharSet: string = 'iso-8859-1, utf-8, utf-16, *;q=0.1';
  HTTPRequestAcceptEncoding: string = 'deflate, gzip, identity, *;q=0';
  HTTPRequestAcceptLanguage: string = 'de-DE,de;q=0.9,en;q=0.8';
  HTTPRequestUserAgent: string = 'Opera/9.80 (Windows NT 6.1; U; de) Presto/2.12.388 Version/12.15';

  HTTPRequestContentTypeList: string = 'application/x-www-form-urlencoded';
  HTTPRequestContentTypeMultipart: string = 'multipart/form-data';

implementation

end.
