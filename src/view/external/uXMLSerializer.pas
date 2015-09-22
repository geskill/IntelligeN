unit uXMLSerializer;

//*****************************************************************************
//  DragonSoft Common Library version 5.x
//
//  Copyright (c) 2000-2009 DragonSoft. ALL RIGHTS RESERVED
//
//  The entire contents of this file is protected by U.S. and International
//  Copyright Laws. Unauthorized reproduction, reverse-engineering, and
//  distribution of all or any portion of the code contained in this file is
//  strictly prohibited and may result in severe civil and criminal penalties
//  and will be prosecuted to the maximum extent possible under the law.
//
//  RESTRICTIONS
//
//  THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED FILES OR ANY
//  PORTION OF ITS CONTENTS SHALL AT NO TIME BE COPIED, TRANSFERRED, SOLD,
//  DISTRIBUTED, OR OTHERWISE MADE AVAILABLE TO OTHER INDIVIDUALS WITHOUT
//  EXPRESS WRITTEN CONSENT AND PERMISSION FROM DRAGONSOFT.
//
//  CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON ADDITIONAL
//  RESTRICTIONS.
//*****************************************************************************

interface

uses
  Classes, Controls,
  XMLDoc, XMLIntf, TypInfo, Variants, SysUtils;

type
  TxsOnSetPropertyError = procedure (aSender: TObject; aPropertyName: string;
    aNode: IXMLNode; aE: Exception; var aHandled: boolean) of object;
  TxsOnStartObjectSave = procedure (aSender, aObject: TObject; aObjectName: string;
    aNode: IXMLNode; var aSkipObject: boolean) of object;
  TxsOnEndObjectSave = procedure (aSender, aObject: TObject; aObjectName: string;
    aNode: IXMLNode) of object;
  TxsOnStartObjectLoad = procedure (aSender, aObject: TObject;
    aObjectName: string; aNode: IXMLNode; var aSkipObject: boolean) of object;
  TxsOnEndObjectLoad = procedure (aSender, aObject: TObject;
    aObjectName: string; aNode: IXMLNode) of object;
  TxsOnStartPropertyLoad = procedure (aSender, aProperty: TObject;
    aObjectName, aPropertyName: string; aNode: IXMLNode; var aSkipProperty: boolean) of object;
  TxsOnStartPropertySave = procedure (aSender, aProperty: TObject;
    aObjectName, aPropertyName: string; var aSkipProperty: boolean) of object;

  TxsStandaloneType = (No, Yes);
  TxsSpecialClassTypes = (scTStrings, scTCollection, scTBitmap);
  TxsSpecialClassType = set of TxsSpecialClassTypes;

  // Specifies how properties are stored/restored
  TxsStorageOptionTypes = (
    soIncludeObjectLinks,         // Object Link properties need to be included in serialization
    soSortProperties,             // Sort properties by name when stored
    soIgnoreSetPropertyError,     // Ignore any errors on restore (missing properties)
    soStoreParentInfo,            // Store reference to parent
    soResetDefaultValues);        // Reset property value on restore to default if not present in serialization
  TxsStorageOptionType = set of TxsStorageOptionTypes;

const
  CONST_DEFAULT_STORAGETYPE = [soSortProperties, soStoreParentInfo, soResetDefaultValues];
  CONST_DEFAULT_ENCODING = 'ISO-8859-1';

type
  TXMLSerializer = class;

  TxsXMLSettings = class(TPersistent)
  private
    fOwner: TXMLSerializer;

    fXSNormalization: boolean;

    function GetEncoding: string;
    function GetStandalone: TxsStandaloneType;
    function GetWellFormatedXML: boolean;
    function IsDefaultEncoding: Boolean;
    procedure SetEncoding(const Value: string);
    procedure SetStandalone(const Value: TxsStandaloneType);
    procedure SetWellFormatedXML(const Value: boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
  published
    property Encoding: string read GetEncoding write SetEncoding stored IsDefaultEncoding;
    property Standalone: TxsStandaloneType read GetStandalone write SetStandalone default yes;
    property XSNormalization: boolean read fXSNormalization write fXSNormalization default True;
    property WellFormated: boolean read GetWellFormatedXML write SetWellFormatedXML default False;
  end;

  TXMLSerializer = class(TComponent)
  private
    fXMLDoc: IXMLDocument;
    fSpecialClasses: TxsSpecialClassType;
    fStorageOptions: TxsStorageOptionType;
    fXMLSettings: TxsXMLSettings;

    fOnXMLLoad: TNotifyEvent;
    fOnXMLSave: TNotifyEvent;
    fOnStartObjectSave: TxsOnStartObjectSave;
    fOnStartObjectLoad: TxsOnStartObjectLoad;
    fOnStartPropertyLoad: TxsOnStartPropertyLoad;
    fOnStartPropertySave: TxsOnStartPropertySave;
    fOnEndObjectLoad: TxsOnEndObjectLoad;
    fOnEndObjectSave: TxsOnEndObjectSave;
    fOnSetPropertyError: TxsOnSetPropertyError;

    function GetXMLData: IXMLDocument;

    procedure SetXML(Value: TStrings);
    function GetXML: TStrings;

    procedure SetXMLSettings(const Value: TxsXMLSettings);

    procedure LoadObjectProperties(aNode: IXMLNode; aPropertyInstance: TObject; const aPropertyName: string);
    procedure LoadChildObjects(aNode: IXMLNode);
    procedure LoadSpecialClasses(aNode: IXMLNode; aPropertyInstance: TObject);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function VarToString(const aValue: variant): string;
    function StringToVar(const aStr: string; const aVarType: TVarType): variant;

    procedure LoadSingleClass(aNode: IXMLNode; aPropertyInstance: TObject; const aPropertyName: string);
    function LoadClass(aParent: IXMLNode; aPropertyInstance: TObject; const aPropertyName: string): IXMLNode;

    function FindNode(const aName: string): IXMLNode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function LoadObject(const aInstance: TObject; const aNodeName: string = '';
      const aParentNode: IXMLNode = nil): IXMLNode;
    function SaveObject(const aInstance: TObject; const aNodeName: string = '';
      const aParentNode: IXMLNode = nil; const aResetXML: boolean = False): IXMLNode;

    function LoadComponent(const aComponent: TComponent; const aNodeName: string = '';
      const aParentNode: IXMLNode = nil): IXMLNode;
    function SaveComponent(const aComponent: TComponent; const aNodeName: string = '';
      const aParentNode: IXMLNode = nil; const aResetXML: boolean = False): IXMLNode;

    function LoadFromFile(const aFileName: string): Boolean;
    function SaveToFile(const aFileName: string): Boolean;

    procedure RestoreDefaultValues(aPropertyInstance: TObject; aIgnoreProperties: TStringList = nil);

    property XMLData: IXMLDocument read GetXMLData;
    property XMLText: TStrings read GetXML write SetXML;
  published
    property SpecialClasses: TxsSpecialClassType read fSpecialClasses write fSpecialClasses default [];
    property StorageOptions: TxsStorageOptionType read fStorageOptions write fStorageOptions default CONST_DEFAULT_STORAGETYPE;
    property XMLSettings: TxsXMLSettings read fXMLSettings write SetXMLSettings;

    property OnXMLLoad: TNotifyEvent read fOnXMLLoad write fOnXMLLoad;
    property OnXMLSave: TNotifyEvent read fOnXMLSave write fOnXMLSave;
    property OnStartObjectSave: TxsOnStartObjectSave read fOnStartObjectSave write fOnStartObjectSave;
    property OnEndObjectSave:   TxsOnEndObjectSave   read fOnEndObjectSave   write fOnEndObjectSave;
    property OnStartObjectLoad: TxsOnStartObjectLoad read fOnStartObjectLoad write fOnStartObjectLoad;
    property OnEndObjectLoad:   TxsOnEndObjectLoad   read fOnEndObjectLoad   write fOnEndObjectLoad;
    property OnSetPropertyError: TxsOnSetPropertyError read fOnSetPropertyError write fOnSetPropertyError;
    property OnStartPropertyLoad: TxsOnStartPropertyLoad read fOnStartPropertyLoad write fOnStartPropertyLoad;
    property OnStartPropertySave: TxsOnStartPropertySave read fOnStartPropertySave write fOnStartPropertySave;
  end;

  EXSDateTimeException = class(Exception);
  EXSSerializationException = class(Exception);
  EXSSetPropertyException = class(EXSSerializationException);

function  SerilializeObjectToXML(aObject: TObject; aSpecialClassSupport: TxsSpecialClassType = []; aOptions: TxsStorageOptionType = CONST_DEFAULT_STORAGETYPE): string;
procedure DeSerilializeObjectFromXML(aObject: TObject; const aXML: string; aSpecialClassSupport: TxsSpecialClassType = []; aIgnoreSetError: boolean = True);

function SerilializeComponentToXML(aComponent: TComponent; aSpecialClassSupport: TxsSpecialClassType = []; aOptions: TxsStorageOptionType = CONST_DEFAULT_STORAGETYPE): string;
procedure DeSerilializeComponentFromXML(aComponent: TComponent; const aXML: string; aSpecialClassSupport: TxsSpecialClassType = []; aIgnoreSetError: boolean = True);

function DateTimeToXMLTime(const aValue: TDateTime; const aApplyLocalBias: Boolean = True): WideString;
function XMLTimeToDateTime(const aXMLDateTime: WideString; const aAsUTCTime: Boolean = False): TDateTime;
function SoapFloatToStr(const aValue: Double): string;
function SoapStrToFloat(const aValue: string): double;

procedure Register;

implementation
uses
  Windows, SysConst, DateUtils, Graphics;

var
  cVarTypes: array[$0000..$4000] of string;

const
  SoapTimePrefix = 'T';
  XMLMilSecSeparator = '.';
  SoapDecimalSeparator = '.';
{$IFDEF VER140}
  MinsPerHour = 60;
{$ENDIF}

resourcestring
  MSG_ERR_NOOBJECT  = 'Object is not specified. Operation canceled';
  MSG_ERR_NOXMLDATA = 'No XML Data Provided. Operation canceled';

procedure Register;
begin
  RegisterComponents('Dragon Soft', [TXMLSerializer]);
end;

function SerilializeObjectToXML(aObject: TObject;
  aSpecialClassSupport: TxsSpecialClassType = [];
  aOptions: TxsStorageOptionType = CONST_DEFAULT_STORAGETYPE): string;
var
  lXMLS: TXMLSerializer;
begin
  if not Assigned(aObject) then
    raise EXSSerializationException.Create(MSG_ERR_NOOBJECT);

  lXMLS := TXMLSerializer.Create(nil);
  try
    lXMLS.SpecialClasses := aSpecialClassSupport;
    lXMLS.StorageOptions := aOptions;
    lXMLS.SaveObject(aObject, '', nil, True);
    Result := lXMLS.XMLText.Text;
  finally
    lXMLS.Free;
  end;
end;

procedure DeSerilializeObjectFromXML(aObject: TObject; const aXML: string;
  aSpecialClassSupport: TxsSpecialClassType = []; aIgnoreSetError: boolean = True);
var
  lXMLS: TXMLSerializer;
begin
  if not Assigned(aObject) then
    raise EXSSerializationException.Create(MSG_ERR_NOOBJECT);

  if aXML = '' then
    raise EXSSerializationException.Create(MSG_ERR_NOXMLDATA);

  lXMLS := TXMLSerializer.Create(nil);
  try
    lXMLS.SpecialClasses := aSpecialClassSupport;
    if aIgnoreSetError then
      lXMLS.StorageOptions := lXMLS.StorageOptions + [soIgnoreSetPropertyError]
    else
      lXMLS.StorageOptions := lXMLS.StorageOptions - [soIgnoreSetPropertyError];

    lXMLS.XMLText.Text := aXML;
    lXMLS.LoadObject(aObject);
  finally
    lXMLS.Free;
  end;
end;

function SerilializeComponentToXML(aComponent: TComponent;
  aSpecialClassSupport: TxsSpecialClassType = [];
  aOptions: TxsStorageOptionType = CONST_DEFAULT_STORAGETYPE): string;
var
  lXMLS: TXMLSerializer;
begin
  if not Assigned(aComponent) then
    raise EXSSerializationException.Create(MSG_ERR_NOOBJECT);

  lXMLS := TXMLSerializer.Create(nil);
  try
    lXMLS.SpecialClasses := aSpecialClassSupport;
    lXMLS.StorageOptions := aOptions;
    lXMLS.SaveComponent(aComponent, '', nil, True);
    Result := lXMLS.XMLText.Text;
  finally
    lXMLS.Free;
  end;
end;

procedure DeSerilializeComponentFromXML(aComponent: TComponent;
  const aXML: string; aSpecialClassSupport: TxsSpecialClassType = [];
  aIgnoreSetError: boolean = True);
var
  lXMLS: TXMLSerializer;
begin
  if not Assigned(aComponent) then
    raise EXSSerializationException.Create(MSG_ERR_NOOBJECT);

  if aXML = '' then
    raise EXSSerializationException.Create(MSG_ERR_NOXMLDATA);

  lXMLS := TXMLSerializer.Create(nil);
  try
    lXMLS.SpecialClasses := aSpecialClassSupport;
    if aIgnoreSetError then
      lXMLS.StorageOptions := lXMLS.StorageOptions + [soIgnoreSetPropertyError]
    else
      lXMLS.StorageOptions := lXMLS.StorageOptions - [soIgnoreSetPropertyError];

    lXMLS.XMLText.Text := aXML;
    lXMLS.LoadComponent(aComponent);
  finally
    lXMLS.Free;
  end;
end;

procedure InitVarTypes;
begin
  cVarTypes[varEmpty] := 'Empty';
  cVarTypes[varNull] := 'Null';
  cVarTypes[varSmallint] := 'Smallint';
  cVarTypes[varInteger] := 'Integer';
  cVarTypes[varSingle] := 'Single';
  cVarTypes[varDouble] := 'Double';
  cVarTypes[varCurrency] := 'Currency';
  cVarTypes[varDate] := 'Date';
  cVarTypes[varOleStr] := 'OleStr';
  cVarTypes[varDispatch] := 'Dispatch';
  cVarTypes[varError] := 'Error';
  cVarTypes[varBoolean] := 'Boolean';
  cVarTypes[varVariant] := 'Variant';
  cVarTypes[varUnknown] := 'Unknown';
  cVarTypes[varShortInt] := 'ShortInt';
  cVarTypes[varByte] := 'Byte';
  cVarTypes[varWord] := 'Word';
  cVarTypes[varLongWord] := 'LongWord';
  cVarTypes[varInt64] := 'Int64';
  cVarTypes[varStrArg] := 'StrArg';
  cVarTypes[varString] := 'String';
  cVarTypes[varAny] := 'Any';
  cVarTypes[varUString] := 'UnicodeString';
  cVarTypes[varTypeMask] := 'TypeMask';
  cVarTypes[varArray] := 'Array';
  cVarTypes[varByRef] := 'ByRef';
end;

function VarTypeToString(aVarType: TVarType): string;
begin
  Result := cVarTypes[aVarType];
end;

function StringToVarType(aVarTypeString: string): TVarType;
var
  i: WORD;
begin
  Result := 0;

  for i := $0000 to $4000 do
  begin
    if ANSISameText(cVarTypes[i], aVarTypeString) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

const
  EncodeTable: array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZ' +
    'abcdefghijklmnopqrstuvwxyz' +
    '0123456789+/';

  DecodeTable: array[#0..#127] of Integer = (
    Byte('='), 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 62, 64, 64, 64, 63,
    52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 64, 64, 64, 64, 64, 64,
    64,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
    15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 64, 64, 64, 64, 64,
    64, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 64, 64, 64, 64, 64);

type
  PPacket = ^TPacket;
  TPacket = packed record
    case Integer of
      0: (b0, b1, b2, b3: Byte);
      1: (i: Integer);
      2: (a: array[0..3] of Byte);
  end;

procedure EncodePacket(const Packet: TPacket; NumChars: Integer; OutBuf: PChar);
begin
  OutBuf[0] := EnCodeTable[Packet.a[0] shr 2];
  OutBuf[1] := EnCodeTable[((Packet.a[0] shl 4) or (Packet.a[1] shr 4)) and $0000003f];
  if NumChars < 2 then
    OutBuf[2] := '='
  else
    OutBuf[2] := EnCodeTable[((Packet.a[1] shl 2) or (Packet.a[2] shr 6)) and $0000003f];

  if NumChars < 3 then
    OutBuf[3] := '='
  else
    OutBuf[3] := EnCodeTable[Packet.a[2] and $0000003f];
end;

function DecodePacket(InBuf: PChar; var nChars: Integer): TPacket;
begin
  Result.a[0] := (DecodeTable[InBuf[0]] shl 2) or
    (DecodeTable[InBuf[1]] shr 4);
  NChars := 1;

  if InBuf[2] <> '=' then
  begin
    Inc(NChars);
    Result.a[1] := Byte((DecodeTable[InBuf[1]] shl 4) or (DecodeTable[InBuf[2]] shr 2));
  end;

  if InBuf[3] <> '=' then
  begin
    Inc(NChars);
    Result.a[2] := Byte((DecodeTable[InBuf[2]] shl 6) or DecodeTable[InBuf[3]]);
  end;
end;

procedure EncodeStream(Input, Output: TStream);
type
  PInteger = ^Integer;
var
  InBuf: array[0..509] of Byte;
  OutBuf: array[0..1023] of Char;
  BufPtr: PChar;
  I, J, K, BytesRead: Integer;
  Packet: TPacket;
begin
  K := 0;
  repeat
    BytesRead := Input.Read(InBuf, SizeOf(InBuf));
    I := 0;
    BufPtr := OutBuf;
    while I < BytesRead do
    begin
      if BytesRead - I < 3 then
        J := BytesRead - I
      else J := 3;
      Packet.i := 0;
      Packet.b0 := InBuf[I];
      if J > 1 then
        Packet.b1 := InBuf[I + 1];
      if J > 2 then
        Packet.b2 := InBuf[I + 2];
      EncodePacket(Packet, J, BufPtr);
      Inc(I, 3);
      Inc(BufPtr, 4);
      Inc(K, 4);
      if K > 75 then
      begin
        BufPtr[0] := #$0D;
        BufPtr[1] := #$0A;
        Inc(BufPtr, 2);
        K := 0;
      end;
    end;
    Output.Write(Outbuf, BufPtr - PChar(@OutBuf));
  until BytesRead = 0;
end;

function IsValidChar(C: char): boolean;
begin
  Result := IsCharAlphaNumeric(C) or (C = '+') or (C = '/') or (C = '=');
end;

function IsValidNumberChar(C: char): boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

procedure DecodeStream(Input, Output: TStream);
var
  InBuf: array[0..75] of Char;
  OutBuf: array[0..60] of Byte;
  InBufPtr, OutBufPtr: PChar;
  I, J, K, BytesRead: Integer;
  Packet: TPacket;

  procedure SkipWhite;
  var
    C: Char;
    NumRead: Integer;
  begin
    while True do
    begin
      NumRead := Input.Read(C, 1);
      if NumRead = 1 then
      begin
        if IsValidChar(C) then
        begin
          Input.Position := Input.Position - 1;     
          Break;
        end;
      end else Break;
    end;
  end;

  function ReadInput: Integer;
  var
    WhiteFound, EndReached : Boolean;
    CntRead, Idx, IdxEnd: Integer;
  begin
    IdxEnd:= 0;
    repeat
      WhiteFound := False;
      CntRead := Input.Read(InBuf[IdxEnd], (SizeOf(InBuf)-IdxEnd));
      EndReached := CntRead < (SizeOf(InBuf)-IdxEnd);
      Idx := IdxEnd;
      IdxEnd := CntRead + IdxEnd;
      while (Idx < IdxEnd) do
      begin
        if not IsValidChar(InBuf[Idx]) then
        begin
          Dec(IdxEnd);
          if Idx < IdxEnd then
            Move(InBuf[Idx+1], InBuf[Idx], IdxEnd-Idx);
          WhiteFound := True;
        end
        else
          Inc(Idx);
      end;
    until (not WhiteFound) or (EndReached);
    Result := IdxEnd;
  end;

begin
  repeat
    SkipWhite;

    BytesRead := ReadInput;
    InBufPtr := InBuf;
    OutBufPtr := @OutBuf;
    I := 0;

    while I < BytesRead do
    begin
      Packet := DecodePacket(InBufPtr, J);
      K := 0;
      while J > 0 do
      begin
        OutBufPtr^ := Char(Packet.a[K]);
        Inc(OutBufPtr);
        Dec(J);
        Inc(K);
      end;
      Inc(InBufPtr, 4);
      Inc(I, 4);
    end;

    Output.Write(OutBuf, OutBufPtr - PChar(@OutBuf));
  until BytesRead = 0;
end;

function EncodeString(const Input: string): string;
var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input);
  try
    OutStr := TStringStream.Create('');
    try
      EncodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

function DecodeString(const Input: string): string;
var
  InStr, OutStr: TStringStream;
begin
  InStr := TStringStream.Create(Input);
  try
    OutStr := TStringStream.Create('');
    try
      DecodeStream(InStr, OutStr);
      Result := OutStr.DataString;
    finally
      OutStr.Free;
    end;
  finally
    InStr.Free;
  end;
end;

function GetTimeZoneBias: Integer;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  case GetTimeZoneInformation(TimeZoneInfo) of
    TIME_ZONE_ID_STANDARD: Result := TimeZoneInfo.Bias + TimeZoneInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Result := TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias;
  else
    Result := 0;
  end;
end;

function DateTimeToXMLTime(const aValue: TDateTime; const aApplyLocalBias: Boolean = True): WideString;
const
  lcIsNeg: array[Boolean] of string=  ('+', '-');
var
  lBias: Integer;
begin
  Result := FormatDateTime('yyyy"-"mm"-"dd"' + SoapTimePrefix + '"hh":"nn":"ss"."zzz', aValue);
  lBias := GetTimeZoneBias;
  if (lBias <> 0) and aApplyLocalBias then
  begin
    Result := Format('%s%s%.2d:%.2d',
      [Result, lcIsNeg[lBias > 0], Abs(lBias) div MinsPerHour, Abs(lBias) mod MinsPerHour]);
  end
  else
    Result := Result + 'Z';
end;

function GetAdditionalYears(var aValue: string): Integer;
var
  lYear: String;
  lDigits: Integer;
begin
  Result := 0;
  if aValue[5] = '-' then
    Exit;
  if (aValue[1] = '-') and (aValue[6] = '-') then
    Exit;
  lYear := aValue;
  if lYear[1] = '-' then
    lYear := Copy(lYear, 2, Length(lYear));
  lDigits := Pos('-', lYear) - 5;
  if lDigits > 0 then
  begin
    try
      Result := StrToInt(Copy(lYear, 1, lDigits));
    except
      raise EXSDateTimeException.CreateFmt(SInvalidDate, [aValue]);
    end;
    lYear := Copy(lYear, lDigits + 1, 12);
  end;

  if aValue[1] = '-' then
    aValue := WideChar('-') + lYear
  else
    aValue := lYear;
end;

function XMLTimeToDateTime(const aXMLDateTime: WideString; const aAsUTCTime: Boolean = False): TDateTime;
var
  lTimePosition: Integer;
  lTimeString, lDateString: string;
  lYear, lMonth, lDay, lHour, lMinute, lSecond, lMilliSecond: integer;
  lNegativeTimeOffset: boolean;
  lHourOffset: integer;
  lMinuteOffset: integer;
  lStr: string;
  lBiasLocal, lBias: Integer;
  lHasTimeOffsetInfo: boolean;
begin
  lTimePosition := Pos(SoapTimePrefix, aXMLDateTime);

  if lTimePosition > 0 then
  begin
    lDateString := Copy(aXMLDateTime, 1, lTimePosition -1);
    lTimeString := Copy(aXMLDateTime, lTimePosition + 1, Length(aXMLDateTime));
  end
  else
  begin
    lDateString := aXMLDateTime;
    lTimeString := '00:00:00';
  end;

  lYear := -1;
  lMonth := -1;
  lDay := -1;
  lHour := -1;
  lMinute := -1;
  lSecond := -1;
  lMilliSecond := -1;
  lHasTimeOffsetInfo := False;
  lBias := 0;

  if lDateString<>'' then
  begin
    if lDateString[1] = '-' then
      lDateString := Copy(lDateString, 2, Length(lDateString));
    GetAdditionalYears(lDateString);
    lYear := StrToIntDef(Copy(lDateString, 1, 4), -1);
    lMonth := StrToIntDef(Copy(lDateString, 6, 2), -1);
    lDay := StrToIntDef(Copy(lDateString, 9, 2), -1);
  end;

  if lTimeString<>'' then
  begin
    if Length(lTimeString) < 8 then
      raise EXSDateTimeException.CreateFmt(SInvalidTime, [lTimeString]);
    lHour := StrToIntDef(Copy(lTimeString, 1, 2), -1);
    lMinute := StrToIntDef(Copy(lTimeString, 4, 2), -1);
    lSecond := StrToIntDef(Copy(lTimeString, 7, 2), -1);
    if Copy(lTimeString, 9, 1)=XMLMilSecSeparator then
    begin
      // millisecond portion is optional
      if Length(lTimeString) < 10 then
        raise EXSDateTimeException.CreateFmt(SInvalidTime, [lTimeString]);
      lStr := Copy(lTimeString, 10, Length(lTimeString));

      if not IsValidNumberChar(lStr[1]) then
        raise EXSDateTimeException.CreateFmt(SInvalidTime, [lTimeString]);

      lMilliSecond := 0;
      while (lStr<>'') and IsValidNumberChar(lStr[1]) do
      begin
        lMilliSecond := lMilliSecond * 10 + StrToInt(lStr[1]);
        Delete(lStr, 1, 1);
      end;

      if (lStr <> '') and ((lStr[1] = '-') or (lStr[1] = '+')) then
      begin
        lNegativeTimeOffset := lStr[1] = '-';
        Delete(lStr, 1, 1);
        lHourOffset := StrToIntDef(Copy(lStr, 1, 2), -1);
        lMinuteOffset := StrToIntDef(Copy(lStr, 4, 2), -1);
        lBias := (lHourOffset * MinsPerHour) + lMinuteOffset;
        if lNegativeTimeOffset then
          lBias := - lBias;
        lHasTimeOffsetInfo := True;
      end;
    end
    else
    begin
      lMilliSecond := 0;
    end;
  end;

  if (lYear = -1) or (lMonth = -1) or (lDay = -1) then
    if (lMinute = -1) or (lSecond = -1) or (lMilliSecond = -1) then
      raise EXSDateTimeException.CreateFmt(SInvalidDateTime, [lDateString + ' ' + lTimeString])
    else
      raise EXSDateTimeException.CreateFmt(SInvalidDate, [lDateString])
  else
    if (lMinute = -1) or (lSecond = -1) or (lMilliSecond = -1) then
      raise EXSDateTimeException.CreateFmt(SInvalidTime, [lTimeString])
    else
      Result := EncodeDateTime(lYear, lMonth, lDay, lHour, lMinute, lSecond, lMilliSecond);

  if lHasTimeOffsetInfo then
  begin
    lBiasLocal := GetTimeZoneBias;

    if not aAsUTCTime then
      lBias := lBias + lBiasLocal;

    Result := IncMinute(Result, lBias);
  end;
end;

function SoapFloatToStr(const aValue: double): string;
begin
  Result := StringReplace(FloatToStr(aValue), ThousandSeparator, '', [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, DecimalSeparator, SoapDecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
end;

function SoapStrToFloat(const aValue: string): double;
var
  lStr: string;
begin
  lStr := StringReplace(aValue, SoapDecimalSeparator, DecimalSeparator, [rfReplaceAll, rfIgnoreCase]);
  Result := StrToFloat(lStr);
end;

// =============================================================================

constructor TXMLSerializer.Create(AOwner: TComponent);
begin
  inherited;
  fXMLSettings := TxsXMLSettings.Create;
  fXMLSettings.fOwner := Self;
  fXMLDoc := nil;
  fSpecialClasses := [];
  fStorageOptions := CONST_DEFAULT_STORAGETYPE;
end;

function TXMLSerializer.LoadObject(const aInstance: TObject;
  const aNodeName: string = ''; const aParentNode: IXMLNode = nil): IXMLNode;
var
  lName: string;
  lNode: IXMLNode;
begin
  Result := nil;
  if not Assigned(aInstance) then
    Exit;

  if not XMLData.Active then
    XMLData.Active := True;

  if aNodeName='' then
  begin
    if (aInstance is TComponent) and ((aInstance as TComponent).Name<>'') then
      lName := (aInstance as TComponent).Name
    else
      lName := '';
  end
  else
    lName := aNodeName;

  if lName<>'' then
  begin
    if not Assigned(aParentNode) then
    begin
      lNode := FindNode(lName);
      if Assigned(lNode) then
        lNode := lNode.ParentNode
      else
        lNode := XMLData.DocumentElement;
    end
    else
      lNode := aParentNode;

    Result := LoadClass(lNode, aInstance, lName)
  end;
end;

function TXMLSerializer.LoadFromFile(const aFileName: string): Boolean;
//var
  //lStr: String;
  //lStream: TMemoryStream;
begin
  if not FileExists(aFilename) then
  begin
    Result := False;
    Exit;
  end;

  {
  lStream := TMemoryStream.Create;
  try
    lStream.LoadFromFile(aFilename);
    SetLength(lStr, lStream.Size);
    lStream.Read(lStr[1], Length(lStr));
  finally
    lStream.Free;
  end;
  }
  XMLData.LoadFromFile(aFilename);
  //XMLData.XML.Text := lStr;
  XMLData.Active := True;
  if Assigned(fOnXMLLoad) then
    fOnXMLLoad(Self);

  Result := True;
end;

function TXMLSerializer.SaveToFile(const aFileName: string): Boolean;
//var
  //lStr: string;
  //lStream: TMemoryStream;
begin
  try
    XMLData.SaveToFile(aFilename);
    {
    lStream := TMemoryStream.Create;
    try
      lStream.Write(lStr[1], Length(lStr));
      lStream.SaveToFile(aFilename);
    finally
      lStream.Free;
    end;
    }
    if Assigned(fOnXMLSave) then
      fOnXMLSave(Self);
    Result := True;
  except
    Result := False;
  end;
end;

function TXMLSerializer.SaveObject(const aInstance: TObject; const aNodeName: string = '';
  const aParentNode: IXMLNode = nil; const aResetXML: boolean = False): IXMLNode;

  procedure RemoveExistingInfo(aObjectName: string; aRootNode: IXMLNode);
  // remove class definition from existing document
  var
    lNode: IXMLNode;
    i: integer;
  begin
    if aResetXML then
    begin
      aRootNode.ChildNodes.Clear;
      Exit;
    end;

    if aObjectName = '' then
      Exit;

    for i := 0 to aRootNode.ChildNodes.Count - 1 do
    begin
      lNode := aRootNode.ChildNodes[i];
      if SameText(lNode.NodeName, 'class')
        and SameText(VarToString(lNode.Attributes['classname']), aInstance.ClassName)
        and SameText(VarToString(lNode.Attributes['name']), aObjectName) then
      begin
        aRootNode.ChildNodes.Remove(lNode);
        Break;
      end;
    end;
  end;

  function NewProperty(aRootNode: IXMLNode; aPropertyName, aPropertyType, aClassName: string; aTextNode: boolean = True): IXMLNode;
  begin
    if aTextNode then
    begin
      Result := aRootNode.AddChild('property');
      Result.Attributes['name'] := aPropertyName;
      Result.Attributes['type'] := aPropertyType;
      if aClassName<>'' then
        Result.Attributes['class'] := aClassName;
    end
    else
    begin
      Result := aRootNode.OwnerDocument.CreateNode('', ntCData);
      aRootNode.ChildNodes.Add(Result);
    end;
  end;

  function SaveClass(aParent: IXMLNode; aPropertyInstance: TObject; aPropertyName: string): IXMLNode;
  var
    lPropNodes: IXMLNode;
    lPropNode: IXMLNode;
    lPropCount: integer;
    lPropList: PPropList;
    lPropInfo: PPropInfo;
    lPropType: PPTypeInfo;
    lPropObj: TObject;
    lPropValue: variant;
    lPropDefaultValue: variant;
    lPropValueType: word;
    i: integer;
    lPropSameValue: boolean;
    lIsObjectLink: boolean;
    lSkipProperty: boolean;
    lStream: TStringStream;
    lPropName: string;
  begin
    Result := nil;
    lSkipProperty := False;
    if Assigned(fOnStartObjectSave) then
      fOnStartObjectSave(Self, aPropertyInstance, aPropertyName, Result, lSkipProperty);
    if lSkipProperty then
      Exit;

    Result := aParent.AddChild('class');
    Result.Attributes['classname'] := aPropertyInstance.ClassName;
    if (aPropertyInstance is TComponent) then
      Result.Attributes['name'] := (aPropertyInstance as TComponent).Name
    else
      Result.Attributes['name'] := aPropertyName;

    lPropCount := GetPropList(PTypeInfo(aPropertyInstance.ClassInfo), lPropList);
    if lPropCount>0 then
    begin
      if (soSortProperties in StorageOptions) and Assigned(lPropList) and (lPropCount > 1) then
        SortPropList(lPropList, lPropCount);

      lPropNodes := Result.AddChild('properties');
      if Assigned(lPropNodes) then
      begin
        try
          if (soStoreParentInfo in fStorageOptions)
            and (aPropertyInstance is TControl)
            and Assigned((aPropertyInstance as TControl).Parent) then
          begin
            lPropNode := NewProperty(lPropNodes, 'Parent', 'Special', 'TWinControl');
            lPropNode.NodeValue := (aPropertyInstance as TControl).Parent.Name;
          end;

          for i := 0 to lPropCount - 1 do
          begin
            lPropInfo := lPropList^[i];
            lPropType := lPropInfo^.PropType;

            if lPropType^.Kind <> tkMethod then
            begin
              if lPropType^.Kind in [tkLString, tkWString, {$IFDEF VER200}tkUString,{$ENDIF}{$IFDEF VER210}tkUString,{$ENDIF} tkString] then
                lPropDefaultValue := ''
              else
                lPropDefaultValue := lPropInfo^.Default;

              if Assigned(lPropInfo) then
              begin
                {$IFDEF VER210}
                lPropName := UnicodeString(lPropInfo^.Name);
                {$ELSE}
                lPropName := lPropInfo^.Name;
                {$ENDIF}

                if IsStoredProp(aPropertyInstance, lPropName)
                   and IsPublishedProp(aPropertyInstance, lPropName) then
                begin
                  if Assigned(fOnStartPropertySave) then
                  begin
                    lSkipProperty := False;
                    fOnStartPropertySave(Self, aPropertyInstance, aPropertyName, lPropName, lSkipProperty);
                    if lSkipProperty then
                      Continue;
                  end;

                  case lPropType^.Kind of
                    tkMethod: ;
                    tkClass:
                      begin
                        lPropObj := GetObjectProp(aPropertyInstance, lPropName);
                        if Assigned(lPropObj) and (lPropObj is TPersistent) then
                        begin
                          lIsObjectLink := False;

                          if (lPropObj is TComponent) then
                          begin
                            if (lPropObj as TComponent).Owner <> aPropertyInstance then
                            begin
                              if soIncludeObjectLinks in fStorageOptions then
                                lIsObjectLink := True
                              else
                                Continue;
                            end
                            else
                            begin
                              if (aPropertyInstance is TComponent) and Assigned((aPropertyInstance as TComponent).FindComponent((lPropObj as TComponent).Name)) then
                              begin
                                if soIncludeObjectLinks in fStorageOptions then
                                  lIsObjectLink := True
                                else
                                  Continue;
                              end
                            end;
                          end;

                          if lIsObjectLink then
                          begin
                            lPropNode := NewProperty(lPropNodes, lPropName, 'ObjectLink', lPropObj.ClassName);
                            lPropNode.NodeValue := (lPropObj as TComponent).Name;
                          end
                          else
                          begin
                            lPropNode := NewProperty(lPropNodes, lPropName, 'Object', lPropObj.ClassName);
                            SaveClass(lPropNode, lPropObj, lPropName);
                          end;
                        end;
                      end;
                    tkEnumeration:
                      begin
                        lPropValue := GetPropValue(aPropertyInstance, lPropName, False);
                        {
                        try
                          lPropSameValue := VarSameValue(VarAsType(lPropDefaultValue, VarType(lPropValue)), lPropValue);
                        except
                          lPropSameValue := False;
                        end;
                        }
                        //if not lPropSameValue then
                        //begin
                          lPropNode := NewProperty(lPropNodes, lPropName, 'Enumeration', '');
                          lPropNode.NodeValue := GetEnumName(lPropInfo^.PropType^, Ord(byte(lPropValue)));
                        //end;
                      end;
                  else
                    lPropValue := GetPropValue(aPropertyInstance, lPropName, False);
                    lPropValueType := VarType(lPropValue);

                    try
                      lPropSameValue := VarSameValue(VarAsType(lPropDefaultValue, VarType(lPropValue)), lPropValue);
                    except
                      lPropSameValue := False;
                    end;

                    if not lPropSameValue then
                    begin
                      lPropNode := NewProperty(lPropNodes, lPropName, VarTypeToString(lPropValueType), '');
                      lPropNode.NodeValue := VarToString(lPropValue);
                    end;
                  end;
                end;
              end;
            end;
          end;
        finally
          FreeMem(lPropList);
        end;
      end;
    end;

    if fSpecialClasses<>[] then
    begin
      if (scTCollection in fSpecialClasses) and aPropertyInstance.InheritsFrom(TCollection) then
      begin
        lPropNodes := Result.AddChild('customdata');
        if Assigned(lPropNodes) then
        begin
          for i := 0 to TCollection(aPropertyInstance).Count-1 do
          begin
            SaveClass(lPropNodes, TCollection(aPropertyInstance).Items[i], 'Item_' + IntToStr(i));
          end;
        end;
      end
      else
      if (scTStrings in fSpecialClasses) and aPropertyInstance.InheritsFrom(TStrings) then
      begin
        lPropNodes := Result.AddChild('customdata');
        if Assigned(lPropNodes) then
        begin
          lPropNode := NewProperty(lPropNodes, 'Text', VarTypeToString(varString), '');
          lPropNode.NodeValue := VarToString(TStrings(aPropertyInstance).Text);
        end;
      end
      else
      if (scTBitmap in fSpecialClasses) and aPropertyInstance.InheritsFrom(TPicture) then
      begin
        lPropNodes := Result.AddChild('customdata');
        if Assigned(lPropNodes) then
        begin
          lPropNode := NewProperty(lPropNodes, 'Text', VarTypeToString(varString), '', False);
          lStream:= TStringStream.Create('');
          try
            (aPropertyInstance as TPicture).Bitmap.SaveToStream(lStream);
            lPropNode.Text := EncodeString(lStream.DataString);
          finally
            lStream.Free;
          end;
        end;
      end;
    end;
    if Assigned(fOnEndObjectSave) then
      fOnEndObjectSave(Self, aPropertyInstance, aPropertyName, Result);
  end;

var
  lName: string;
  lRootNode: IXMLNode;
begin
  Result := nil;
  if not Assigned(aInstance) then
    Exit;

  if not XMLData.Active then
    XMLData.Active := True;

  if aNodeName='' then
  begin
    if (aInstance is TComponent) and ((aInstance as TComponent).Name<>'') then
      lName := (aInstance as TComponent).Name
    else
      lName := '';
  end
  else
    lName := aNodeName;

  if Assigned(aParentNode) then
    lRootNode := aParentNode
  else
    lRootNode := XMLData.DocumentElement;

  RemoveExistingInfo(lName, lRootNode);

  if lName <> '' then
    Result := SaveClass(lRootNode, aInstance, lName);
end;

procedure TXMLSerializer.SetXML(Value: TStrings);
begin
  XMLData.XML := Value;
  XMLData.Active := True;
end;

function TXMLSerializer.GetXML: TStrings;
begin
  Result := XMLData.XML;
end;

function TXMLSerializer.GetXMLData: IXMLDocument;
var
  lElement: IXMLNode;
begin
  if not Assigned(fXMLDoc) then
  begin
    fXMLDoc := NewXMLDocument;

    fXMLDoc.Encoding := fXMLSettings.Encoding;
    if fXMLSettings.Standalone = Yes then
      fXMLDoc.Standalone := 'yes';
    if fXMLSettings.WellFormated then
      fXMLDoc.Options := fXMLDoc.Options + [doNodeAutoIndent]
    else
      fXMLDoc.Options := fXMLDoc.Options - [doNodeAutoIndent];
    lElement := fXMLDoc.CreateElement('classes', '');
    fXMLDoc.DocumentElement := lElement;
    fXMLDoc.Active := True;
  end;

  Result := fXMLDoc;
end;

destructor TXMLSerializer.Destroy;
begin
  fXMLDoc := nil;
  fXMLSettings.Free;
  inherited;
end;

function TXMLSerializer.StringToVar(const aStr: string; const aVarType: TVarType): variant;
begin
  if XMLSettings.XSNormalization then
  begin
    case aVarType of
      varDate: Result := XMLTimeToDateTime(aStr);
      varDouble,
      varCurrency: Result := SoapStrToFloat(aStr);
    else
      Result := VarAsType(aStr, aVarType);
    end;
  end
  else
    Result := VarAsType(aStr, aVarType);
end;

function TXMLSerializer.VarToString(const aValue: variant): string;
begin
  if XMLSettings.XSNormalization then
  begin
    case VarType(aValue) of
      varDate: Result := DateTimeToXMLTime(aValue);
      varDouble,
      varCurrency: Result := SoapFloatToStr(aValue);
    else
      if VarIsNull(aValue) then
        Result := ''
      else
        Result := VarToStr(aValue);
    end;
  end
  else
  begin
    Result := VarToStr(aValue);
  end;
end;

function TXMLSerializer.LoadComponent(const aComponent: TComponent;
  const aNodeName: string; const aParentNode: IXMLNode): IXMLNode;
var
  i: integer;
  lNode: IXMLNode;
begin
  if Assigned(aComponent) then
  begin
    Result := LoadObject(aComponent, aNodeName, aParentNode);
    if Assigned(Result) and (aComponent.ComponentCount>0) then
    begin
      lNode := Result.ChildNodes.FindNode('components');
      if Assigned(lNode) then
      begin
        for i := 0 to aComponent.ComponentCount - 1 do
          LoadComponent(aComponent.Components[i], '', lNode);
      end;
    end;
  end
  else
    Result := nil;
end;

function TXMLSerializer.SaveComponent(const aComponent: TComponent;
  const aNodeName: string; const aParentNode: IXMLNode;
  const aResetXML: boolean): IXMLNode;
var
  i: integer;
  lNode: IXMLNode;
begin
  if not Assigned(aComponent) then
  begin
    Result := nil;
    Exit;
  end;

  Result := SaveObject(aComponent, aNodeName, aParentNode, aResetXML);

  if not Assigned(Result) then
    Exit;
  if aComponent.ComponentCount = 0 then
    Exit;

  lNode := Result.AddChild('components');
  if not Assigned(lNode) then
    Exit;

  for i := 0 to aComponent.ComponentCount-1 do
    SaveComponent(aComponent.Components[i], '', lNode, False);
end;

{ TxsXMLSettings }

procedure TxsXMLSettings.AssignTo(Dest: TPersistent);
begin
  if (Dest is TxsXMLSettings) then
  begin
    with Dest as TxsXMLSettings do
    begin
      Encoding := Self.Encoding;
      Standalone := Self.Standalone;
      XSNormalization := Self.XSNormalization;
      WellFormated := Self.WellFormated;
    end;
  end
  else
    inherited;
end;

constructor TxsXMLSettings.Create;
begin
  inherited Create;
  fXSNormalization := True;
end;

function TxsXMLSettings.GetEncoding: string;
begin
  fOwner.XMLData.Active := True;
  if fOwner.XMLData.Encoding='' then
    Result := CONST_DEFAULT_ENCODING
  else
    Result := fOwner.XMLData.Encoding;
end;

function TxsXMLSettings.GetStandalone: TxsStandaloneType;
begin
  fOwner.XMLData.Active := True;
  if ANSISameText(fOwner.XMLData.Standalone, 'yes') then
    Result := yes
  else
    Result := no;
end;

function TxsXMLSettings.GetWellFormatedXML: boolean;
begin
  Result := doNodeAutoIndent in fOwner.XMLData.Options;
end;

function TxsXMLSettings.IsDefaultEncoding: Boolean;
begin
  Result := not ANSISameText(Encoding, CONST_DEFAULT_ENCODING);
end;

procedure TxsXMLSettings.SetEncoding(const Value: string);
begin
  fOwner.XMLData.Active := True;
  if Trim(Value)='' then
    fOwner.XMLData.Encoding := CONST_DEFAULT_ENCODING
  else
    fOwner.XMLData.Encoding := Trim(Value);
end;

procedure TxsXMLSettings.SetStandalone(const Value: TxsStandaloneType);
begin
  fOwner.XMLData.Active := True;
  if Value=yes then
    fOwner.XMLData.Standalone := 'yes'
  else
    fOwner.XMLData.Standalone := 'no';
end;

procedure TxsXMLSettings.SetWellFormatedXML(const Value: boolean);
begin
  if Value then
    fOwner.XMLData.Options := fOwner.XMLData.Options + [doNodeAutoIndent]
  else
    fOwner.XMLData.Options := fOwner.XMLData.Options - [doNodeAutoIndent];
end;

procedure TXMLSerializer.SetXMLSettings(const Value: TxsXMLSettings);
begin
  fXMLSettings.Assign(Value);
end;

procedure TXMLSerializer.AssignTo(Dest: TPersistent);
begin
  if (Dest is TXMLSerializer) then
  begin
    with Dest as TXMLSerializer do
    begin
      SpecialClasses := Self.SpecialClasses;
      StorageOptions := Self.StorageOptions;
      XMLSettings.Assign(Self.XMLSettings);
      OnXMLLoad := Self.OnXMLLoad;
      OnXMLSave := Self.OnXMLSave;
      OnStartObjectSave := Self.OnStartObjectSave;
      OnEndObjectSave := Self.OnEndObjectSave;
      OnStartObjectLoad := Self.OnStartObjectLoad;
      OnEndObjectLoad := Self.OnEndObjectLoad;
      OnSetPropertyError := Self.OnSetPropertyError;
      OnStartPropertyLoad := Self.OnStartPropertyLoad;
      OnStartPropertySave := Self.OnStartPropertySave;
    end;
  end
  else
    inherited;
end;

procedure TXMLSerializer.LoadObjectProperties(aNode: IXMLNode;
  aPropertyInstance: TObject; const aPropertyName: string);

  function lfFindComponent(aPropObjName: string; aOwner: TComponent): TComponent;
  var
    j: Integer;
    lPropRootName: string;
    lRootCmp: TComponent;
  begin
    j := pos('.', aPropObjName);

    if j > 0 then
    begin
      lPropRootName := Copy(aPropObjName, 1, j - 1);
      Delete(aPropObjName, 1, j);
    end
    else
      lPropRootName := '';

    if lPropRootName <> '' then
    begin
      lRootCmp := FindGlobalComponent(lPropRootName);
      if Assigned(lRootCmp) then
        Result := FindNestedComponent(lRootCmp, aPropObjName)
      else
        Result := nil;
    end
    else
    begin
      if Assigned(aOwner) and Assigned(aOwner.Owner) then
        Result := aOwner.Owner.FindComponent(aPropObjName)
      else
        Result := nil;
    end;
  end;

  function lfCheckParentProperty(aPropNode: IXMLNode; const aPropName: string): boolean;
  var
    lPropObjName: string;
    lCmp: TComponent;
  begin
    // Process parent information
    if (aPropertyInstance is TWinControl)
       and ANSISameText(aPropName, 'Parent')
       and ANSISameText(aPropNode.Attributes['type'], 'Special') then
    begin
      lPropObjName := aPropNode.NodeValue;

      lCmp := lfFindComponent(lPropObjName, (aPropertyInstance as TComponent));
      if lCmp is TWinControl then
        TWinControl(aPropertyInstance).Parent := TWinControl(lCmp);
      Result := True;
    end
    else
      Result := False;
  end;

var
  lPropNodes: IXMLNode;
  lPropNode: IXMLNode;
  lPropName: OleVariant;
  lPropObj: TPersistent;
  lCmp: TComponent;
  lPropInfo: PPropInfo;
  lVarType: TVarType;
  lSkipProperty: boolean;
  lHandled: boolean;
  LProcessed: TStringList;
  i: integer;
begin
  if not Assigned(aNode) then
    Exit;

  lPropNodes := aNode.ChildNodes.FindNode('properties');
  if not Assigned(lPropNodes) then
    Exit;

  LProcessed := TStringList.Create;
  try
    LProcessed.Sorted := True;
    LProcessed.Duplicates := dupIgnore;
    LProcessed.CaseSensitive := False;

    for i := 0 to lPropNodes.ChildNodes.Count-1 do
    begin
      lPropNode := lPropNodes.ChildNodes.Nodes[i];
      if Trim(lPropNode.XML) = '' then
        Continue;

      lPropName := lPropNode.Attributes['name'];
      if Trim(lPropName) = '' then
        Continue;

      if lfCheckParentProperty(lPropNode, lPropName) then
      begin
        LProcessed.Add(lPropName);
        Continue;
      end;

      lPropInfo := GetPropInfo(aPropertyInstance, lPropName);
      // Only published properties are supported by serialization
      if not (IsPublishedProp(aPropertyInstance, lPropName) and Assigned(lPropInfo^.SetProc)) then
        Continue;

      // Custom code
      if Assigned(fOnStartPropertyLoad) then
      begin
        lSkipProperty := False;
        fOnStartPropertyLoad(Self, aPropertyInstance, aPropertyName, lPropName, lPropNode, lSkipProperty);
        if lSkipProperty then
          Continue;
      end;

      // Load Object property
      if ANSISameText(lPropNode.Attributes['type'], 'Object') then
      begin
        lPropObj := TPersistent(GetObjectProp(aPropertyInstance, lPropName));
        if Assigned(lPropObj) then
          LoadClass(lPropNode, lPropObj, lPropName)
       { --------------------------- }
        else
          if ANSISameText(lPropNode.Attributes['class'],'TCollection') then
          begin
            if not Assigned(lPropObj) then
              raise Exception.Create('Object of '+ lPropName +' is undefined');
            SetObjectProp(aPropertyInstance,lPropName,lPropObj);
            LoadClass(lPropNode, lPropObj, lPropName);
          end;
       { --------------------------- }
        LProcessed.Add(lPropName);
        Continue;
      end;

      // Try restore property value
      // If specified, errors would be ignored, otherwise error is raized
      try
        // If no value present then nothing is to be restored
        // Use default value if specified
        if VarIsNull(lPropNode.NodeValue) then
          Continue;

        if ANSISameText(lPropNode.Attributes['type'], 'ObjectLink') then
        begin
          lPropObj := TPersistent(GetObjectProp(aPropertyInstance, lPropName));
          if Assigned(lPropObj) and (lPropObj is TComponent) then
          begin
            lCmp := lfFindComponent(lPropNode.NodeValue, (lPropObj as TComponent));
            SetObjectProp(aPropertyInstance, lPropName, lCmp);
          end;
        end
        else
        if ANSISameText(lPropNode.Attributes['type'], 'Enumeration') then
          SetPropValue(aPropertyInstance, lPropName, GetEnumValue(lPropInfo^.PropType^, lPropNode.NodeValue))
        else
        begin
          lVarType := StringToVarType(lPropNode.Attributes['type']);
          SetPropValue(aPropertyInstance, lPropName, StringToVar(lPropNode.NodeValue, lVarType));
        end;

        LProcessed.Add(lPropName);
      except
        on E: Exception do
        begin
          if not (soIgnoreSetPropertyError in StorageOptions) then
          begin
            lHandled := False;
            if Assigned(fOnSetPropertyError) then
              fOnSetPropertyError(Self, lPropName, lPropNode, E, lHandled);

            if not lHandled then
              raise EXSSetPropertyException.Create('XML Serializer. Cannot set property value');
          end;
        end;
      end
    end;

    // Restore default values, ignore already processed
    if soResetDefaultValues in StorageOptions then
      RestoreDefaultValues(aPropertyInstance, LProcessed);
  finally
    LProcessed.Free;
  end;
end;

procedure TXMLSerializer.LoadChildObjects(aNode: IXMLNode);
var
  lPropNodes: IXMLNode;
  i: integer;
begin
  if not Assigned(aNode) then
    Exit;

  lPropNodes := aNode.ChildNodes.FindNode('components');
  if not Assigned(lPropNodes) then
    Exit;

  for i := 0 to lPropNodes.ChildNodes.Count - 1 do
    LoadClass(lPropNodes.ChildNodes[i], nil, '');
end;

procedure TXMLSerializer.LoadSpecialClasses(aNode: IXMLNode; aPropertyInstance: TObject);
var
  lPropNode: IXMLNode;
  lStream: TStringStream;
  lPropNodes: IXMLNode;
  i: Integer;
begin
  if fSpecialClasses = [] then
    Exit;

  if (scTCollection in fSpecialClasses) and aPropertyInstance.InheritsFrom(TCollection) then
  begin
    lPropNodes := aNode.ChildNodes.FindNode('customdata');
    { --------------------------- }
    for I := 0 to lPropNodes.ChildNodes.Count - 1 do
      if (lPropNodes.ChildNodes.Nodes[I].NodeName = 'class') then
        TCollection(aPropertyInstance).Add;
    { --------------------------- }
    if Assigned(lPropNodes) then
    begin
      for i := 0 to TCollection(aPropertyInstance).Count - 1 do
        LoadClass(lPropNodes, TCollection(aPropertyInstance).Items[i], 'Item_' + IntToStr(i));
    end;
  end
  else
  if (scTStrings in fSpecialClasses) and aPropertyInstance.InheritsFrom(TStrings) then
  begin
    lPropNodes := aNode.ChildNodes.FindNode('customdata');
    if Assigned(lPropNodes) then
    begin
      lPropNode := lPropNodes.ChildNodes.FindNode('property');
      while Assigned(lPropNode) do
      begin
        if ANSISameText(lPropNode.Attributes['name'], 'Text') then
        begin
          TStrings(aPropertyInstance).Text := VarToString(lPropNode.NodeValue);
          Break;
        end
        else
          lPropNode := lPropNode.NextSibling;
      end;
    end;
  end
  else
  if (scTBitmap in fSpecialClasses) and aPropertyInstance.InheritsFrom(TPicture) then
  begin
    lPropNodes := aNode.ChildNodes.FindNode('customdata');
    if Assigned(lPropNodes) and (lPropNodes.ChildNodes.Count > 0) then
    begin
      lPropNode := lPropNodes.ChildNodes.First;
      if Assigned(lPropNode) then
      begin
        lStream := TStringStream.Create(DecodeString(lPropNode.Text));
        try
          lStream.Position := 0;
          (aPropertyInstance as TPicture).Bitmap.LoadFromStream(lStream);
        finally
          lStream.Free;
        end;
      end;
    end;
  end;
end;

procedure TXMLSerializer.RestoreDefaultValues(aPropertyInstance: TObject;
  aIgnoreProperties: TStringList = nil);
const
  cNoDefault = - MaxLongint - 1;
  cPropsWithDefault = [tkInteger, tkChar, tkSet, tkEnumeration];
var
  lPropInfos: PPropList;
  lPropInfo: PPropInfo;
  lCount: integer;
  LIndex: integer;
  i: integer;
begin
  lCount := GetPropList(aPropertyInstance.ClassInfo, cPropsWithDefault, nil);

  // Allocate memory for RTTI data
  GetMem(lPropInfos, lCount * SizeOf(PPropInfo));
  try
    // Collect property info
    GetPropList(aPropertyInstance.ClassInfo, cPropsWithDefault, lPropInfos);

    // Restore defaults
    for i := 0 to lCount - 1 do
    begin
      lPropInfo := lPropInfos^[i];
      // Ignore processed properties
      {$IFDEF VER210}
      if Assigned(aIgnoreProperties) and (aIgnoreProperties.Find(UnicodeString(lPropInfo.Name), LIndex)) then
        Continue;
      {$ELSE}
      if Assigned(aIgnoreProperties) and (aIgnoreProperties.Find(lPropInfo.Name, LIndex)) then
        Continue;
      {$ENDIF}

      if lPropInfo^.Default <> cNoDefault then
        SetOrdProp(aPropertyInstance, lPropInfo, lPropInfo^.Default);
    end;
  finally
    FreeMem(lPropInfos, lCount * SizeOf(PPropInfo));
  end;
end;

procedure TXMLSerializer.LoadSingleClass(aNode: IXMLNode;
  aPropertyInstance: TObject; const aPropertyName: string);
var
  lSkipProperty: Boolean;
begin
  lSkipProperty := False;

  // Custom code
  if Assigned(fOnStartObjectLoad) then
    fOnStartObjectLoad(Self, aPropertyInstance, aPropertyName, aNode, lSkipProperty);

  if lSkipProperty then
    Exit;

  // process properties
  LoadObjectProperties(aNode, aPropertyInstance, aPropertyName);
  // Load child objects
  LoadChildObjects(aNode);
  // Load special classes
  LoadSpecialClasses(aNode, aPropertyInstance);

  // Custom code
  if Assigned(fOnEndObjectLoad) then
    fOnEndObjectLoad(Self, aPropertyInstance, aPropertyName, aNode);
end;

function TXMLSerializer.LoadClass(aParent: IXMLNode; aPropertyInstance: TObject;
  const aPropertyName: string): IXMLNode;
begin
  if aParent<>nil then
  begin
    Result := aParent.ChildNodes.FindNode('class');
    while Assigned(Result) do
    begin
      if ANSISameText(Result.NodeName, 'class')
        and ANSISameText(Result.Attributes['classname'], aPropertyInstance.ClassName)
        and ANSISameText(Result.Attributes['name'], aPropertyName) then
      begin
        LoadSingleClass(Result , aPropertyInstance, aPropertyName);
        Break;
      end
      else
        Result := Result.NextSibling;
    end;
  end;
end;

function TXMLSerializer.FindNode(const aName: string): IXMLNode;
  function lfFindNode(aRootNode: IXMLNode): IXMLNode;
  var
    i: Integer;
    lNode: IXMLNode;
  begin
    for i := 0 to aRootNode.ChildNodes.Count - 1 do
    begin
      lNode := aRootNode.ChildNodes[i];
      if (lNode.NodeName = 'class') and (lNode.Attributes['name']=aName) then
      begin
        Result := lNode;
        Break;
      end;
      Result := lfFindNode(lNode);
      if Assigned(Result) then
        Break;
    end;
  end;
begin
  Result := lfFindNode(XMLData.DocumentElement);
end;

initialization
  InitVarTypes;
finalization
end.
