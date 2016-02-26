unit IntelligeN_TLB;

// ************************************************************************ //
// WARNUNG
// -------
// Die in dieser Datei deklarierten Typen wurden aus Daten einer Typbibliothek
// generiert. Wenn diese Typbibliothek explizit oder indirekt (über eine
// andere Typbibliothek) reimportiert wird oder wenn der Befehl
// 'Aktualisieren' im Typbibliotheks-Editor während des Bearbeitens der
// Typbibliothek aktiviert ist, wird der Inhalt dieser Datei neu generiert und
// alle manuell vorgenommenen Änderungen gehen verloren.
// ************************************************************************ //

// $Rev: 17244 $
// Datei am 26.02.2016 21:37:38 erzeugt aus der unten beschriebenen Typbibliothek.

// ************************************************************************  //
// Typbib.: C:\Users\geskill\Documents\RAD Studio\Projekte\intelligen-2k9-master\src\view\ole\IntelligeN (1)
// LIBID: {DB0EEDEC-7A19-474C-8FCA-3C7CD25FAC03}
// LCID: 0
// Hilfedatei:
// Hilfe-String:
// Liste der Abhäng.:
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit muss ohne Typüberprüfung für Zeiger compiliert werden.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;

// *********************************************************************//
// In der Typbibliothek deklarierte GUIDS. Die folgenden Präfixe werden verwendet:
//   Typbibliotheken      : LIBID_xxxx
//   CoClasses            : CLASS_xxxx
//   DISPInterfaces       : DIID_xxxx
//   Nicht-DISP-Interfaces: IID_xxxx
// *********************************************************************//
const
  // Haupt- und Nebenversionen der Typbibliothek
  IntelligeNMajorVersion = 2;
  IntelligeNMinorVersion = 0;

  LIBID_IntelligeN: TGUID = '{DB0EEDEC-7A19-474C-8FCA-3C7CD25FAC03}';

  IID_IIntelligeN2009: TGUID = '{F9F52CDE-A58E-44CD-A70A-EC57381D9FE0}';
  CLASS_IntelligeN2009: TGUID = '{6BB30462-B804-4EDA-A9CD-906B64545E2B}';
type

// *********************************************************************//
// Forward-Deklaration von in der Typbibliothek definierten Typen
// *********************************************************************//
  IIntelligeN2009 = interface;
  IIntelligeN2009Disp = dispinterface;

// *********************************************************************//
// Deklaration von in der Typbibliothek definierten CoClasses
// (HINWEIS: Hier wird jede CoClass ihrem Standard-Interface zugewiesen)
// *********************************************************************//
  IntelligeN2009 = IIntelligeN2009;


// *********************************************************************//
// Interface: IIntelligeN2009
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F9F52CDE-A58E-44CD-A70A-EC57381D9FE0}
// *********************************************************************//
  IIntelligeN2009 = interface(IDispatch)
    ['{F9F52CDE-A58E-44CD-A70A-EC57381D9FE0}']
    function NewTabSheet(const ATemplateName: WideString): SYSINT; stdcall;
    function OpenTabSheet(const AFileName: WideString): SYSINT; stdcall;
    function SaveTabSheet(ATabIndex: SYSINT; const AFileName: WideString;
                          const AFileFormat: WideString; AForceDialog: WordBool): WordBool; stdcall;
    function CanCloseTabSheet(ATabIndex: SYSINT): WordBool; stdcall;
    function CloseTabSheet(ATabIndex: SYSINT): WordBool; stdcall;
    procedure CallPublish(ATabIndex: SYSINT); safecall;
    function IsPublishActive: WordBool; stdcall;
    procedure CallCrawler(ATabIndex: SYSINT); safecall;
    function IsCrawlerActive: WordBool; stdcall;
    procedure CallCrypterCrypt(ATabIndex: SYSINT); safecall;
    procedure CallCrypterCheck(ATabIndex: SYSINT); safecall;
    function IsCrypterActive: WordBool; stdcall;
    procedure CallFileHosterCheck(ATabIndex: SYSINT); safecall;
    function IsFileHosterActive: WordBool; stdcall;
    procedure CallImageHosterRemoteUpload(ATabIndex: SYSINT); safecall;
    function IsImageHosterActive: WordBool; stdcall;
    function ActiveTabSheetIndex: SYSINT; stdcall;
    function TabSheetCount: SYSINT; stdcall;
  end;

// *********************************************************************//
// DispIntf:  IIntelligeN2009Disp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {F9F52CDE-A58E-44CD-A70A-EC57381D9FE0}
// *********************************************************************//
  IIntelligeN2009Disp = dispinterface
    ['{F9F52CDE-A58E-44CD-A70A-EC57381D9FE0}']
    function NewTabSheet(const ATemplateName: WideString): SYSINT; dispid 201;
    function OpenTabSheet(const AFileName: WideString): SYSINT; dispid 202;
    function SaveTabSheet(ATabIndex: SYSINT; const AFileName: WideString;
                          const AFileFormat: WideString; AForceDialog: WordBool): WordBool; dispid 203;
    function CanCloseTabSheet(ATabIndex: SYSINT): WordBool; dispid 204;
    function CloseTabSheet(ATabIndex: SYSINT): WordBool; dispid 205;
    procedure CallPublish(ATabIndex: SYSINT); dispid 206;
    function IsPublishActive: WordBool; dispid 207;
    procedure CallCrawler(ATabIndex: SYSINT); dispid 208;
    function IsCrawlerActive: WordBool; dispid 209;
    procedure CallCrypterCrypt(ATabIndex: SYSINT); dispid 210;
    procedure CallCrypterCheck(ATabIndex: SYSINT); dispid 211;
    function IsCrypterActive: WordBool; dispid 212;
    procedure CallFileHosterCheck(ATabIndex: SYSINT); dispid 213;
    function IsFileHosterActive: WordBool; dispid 214;
    procedure CallImageHosterRemoteUpload(ATabIndex: SYSINT); dispid 215;
    function IsImageHosterActive: WordBool; dispid 216;
    function ActiveTabSheetIndex: SYSINT; dispid 217;
    function TabSheetCount: SYSINT; dispid 218;
  end;

// *********************************************************************//
// Die Klasse CoIntelligeN2009 stellt die Methoden Create und CreateRemote zur
// Verfügung, um Instanzen des Standard-Interface IIntelligeN2009, dargestellt
// von CoClass IntelligeN2009, zu erzeugen. Diese Funktionen können
// von einem Client verwendet werden, der die CoClasses automatisieren
// will, die von dieser Typbibliothek dargestellt werden.
// *********************************************************************//
  CoIntelligeN2009 = class
    class function Create: IIntelligeN2009;
    class function CreateRemote(const MachineName: string): IIntelligeN2009;
  end;

implementation

uses ComObj;

class function CoIntelligeN2009.Create: IIntelligeN2009;
begin
  Result := CreateComObject(CLASS_IntelligeN2009) as IIntelligeN2009;
end;

class function CoIntelligeN2009.CreateRemote(const MachineName: string): IIntelligeN2009;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_IntelligeN2009) as IIntelligeN2009;
end;

end.

