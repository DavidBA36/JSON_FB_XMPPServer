unit uXMPPServerHelper;

interface

uses Classes, system.SysUtils, FireDAC.Comp.Client, system.Generics.Collections, DCPrijndael, IdHashMessageDigest, IdContext;

type
  XMPPNotLogged = class(Exception);
  XMPPContactMissing = class(Exception);
  XMPPLoggedForbidden = class(Exception);

  TMecanismoSASL = (mDigestMD5, mScramSHA1, mScramSHA256, mScramSHA512, mScramMD5, mPLAIN);

  TVisibilidad = (usOnline, usAway, usDNotDistrub, usFreeForChat, usUnavailable); // user status
  TStatus = (sNone, sOpenStream, sCriptoNego, sCryptoSucess, sSASLNego, sSASLSucess, sBind, sSession, sStanza);
  TRequestClass = (rcAdd, rcEdit, rcRemove);

  TPresencia = record
    Prioridad: integer;
    Visibilidad: TVisibilidad;
    Status: String;
  end;

  TCliente = class(TCollectionItem)
  private
    fFDConnection: TFDConnection;
    fFDTransaction: TFDTransaction;
    fUserName: string;
    fVServer: string;
    fSesionManager: integer;
    festado: TStatus;
    fdisconected: Boolean;
    fContexto: TIdContext;
    fID_Usuario: integer;
    fID_Session: string;
    fPresencia: TPresencia;
    fSASL: TMecanismoSASL;
    fRecurso: string;
    fClaveEmpresa: integer;
  published
    property Disconected: Boolean read fdisconected write fdisconected;
    property Estado: TStatus read festado write festado;
    property Contexto: TIdContext read fContexto write fContexto;
    property ID_Usuario: integer read fID_Usuario write fID_Usuario;
    property ID_Session: String read fID_Session write fID_Session;
    property Presencia: TPresencia read fPresencia write fPresencia;
    property SASL: TMecanismoSASL read fSASL write fSASL;
    property Recurso: string read fRecurso write fRecurso;
    property SesionManager: integer read fSesionManager write fSesionManager;
    property UserName: String read fUserName write fUserName;
    property VServer: string read fVServer write fVServer;
    property ClaveEmpresa: integer read fClaveEmpresa write fClaveEmpresa;
    property ConexionDB: TFDConnection read fFDConnection write fFDConnection;
    property TransaccionDB: TFDTransaction read fFDTransaction write fFDTransaction;
  end;

  TListaClientes = class(TCollection)
  private
    function GetItem(Index: integer): TCliente;
  public
    function Add: TCliente;
    property Item[Index: integer]: TCliente read GetItem;
    function IndexOfUser(Client: integer): integer;
  end;

  TInstancia = class(TCollectionItem)
  private
    fFDConnection: TFDConnection;
    fFDTransaction: TFDTransaction;
    fClaveInstancia: integer;
  published
    property ClaveInstancia: integer read fClaveInstancia write fClaveInstancia;
    property Conexion: TFDConnection read fFDConnection write fFDConnection;
    property Transaccion: TFDTransaction read fFDTransaction write fFDTransaction;
  end;

  TListaInstancias = class(TCollection)
  private
    function GetItem(Index: integer): TInstancia;
  public
    function Add: TInstancia;
    property Item[Index: integer]: TInstancia read GetItem;
    function IndexOfInstancia(Instancia: integer): integer;
  end;

  TEvent = procedure of Object;
  TOnRegistrationSucessfull = procedure(RequestClass: TRequestClass) of Object;
  TOnStatus = procedure(Status: TStatus; AStatus: String) of Object;
  TOnError = procedure(Tipo, Codigo, Condicion: String) of Object;
  TOnResponseWithStrData = procedure(AData: string) of Object; // event defining a response with a string paramter
  TonResponseWithBoolData = procedure(AStatus: Boolean) of object; // event defining a response with a boolean paramter
  // TOnInitConnection = procedure(AId: string; ACompression: String; AUseSASL: Boolean; AMechanism: TMechanism) of Object;
  TOnRoster = procedure(AName, Jid, ASubscription, AGroup: string) of object;
  TOnMessageReceived = procedure(AFrom, AMessage: string) of object; // On receive a message
  // TOnPresenceCallBack = procedure(AFrom, ATo, AStatus: String; APresence: TUserStatus) of Object; // On presence status for a contact change
  TOnContactAskToAdd = procedure(AJid: String; var AAccept: Boolean) of Object; // When someone add you as a contact
  TOnAddRequestStatus = procedure(AFrom: String; AAccepted: Boolean) of object; // Own request to add a contac
  // TOnGetRegistrationFiels = procedure(ARegistrationFiels: TList<TRegistrationField>) of object;
  TOnDebug = procedure(Tipo: Integer;OD: String; AMessage: string) of object;

Const
  // TABLAS

  TABLA_USUARIOS = 'PER$OPERARIO';
  TABLA_ROSTERS = 'SER$USUARIOS_DATOS_LISTA';
  TABLA_MENSAJES = 'PDA$MENSAJES';
  TABLA_VCARD_EMPRESA = 'SER$VCARD_DEMPRESA';
  TABLA_VCARD_PERSONAL = 'SER$VCARD_DPERSONALES';
  TABLA_DOMINIOS = 'INT$DOMINIO';
  TABLA_INSTANCIAS = 'INT$INSTANCIA';

  XML_HEADER = '<?xml version="1.0"?>';

  // features
  XMLNS_COMMANDS = 'http://jabber.org/protocol/commands';
  XMLNS_DISCOINFO = 'http://jabber.org/protocol/disco#info';
  XMLNS_DISCOITEMS = 'http://jabber.org/protocol/disco#items';
  XMLNS_PUBSUB = 'http://jabber.org/protocol/pubsub';
  XMLNS_LAST = 'jabber:iq:last';
  XMLNS_PRIVACY = 'jabber:iq:privacy';
  XMLNS_REGISTER = 'jabber:iq:register';
  XMLNS_VERSION = 'jabber:iq:version';
  XMLNS_MSOFFLINE = 'msgoffline';
  XMLNS_PRESENCE = 'presence';
  XMLNS_BLOCKING = 'urn:xmpp:blocking';
  XMLNS_PING = 'urn:xmpp:ping';
  XMLNS_VCARD = 'vcard-temp';
  XMLNS_IQ = 'iq';

  XMLNS_ROSTER = 'jabber:iq:roster';
  XMLNS_TIME = 'jabber:iq:time';
  XMLNS_IQOOB = 'jabber:iq:oob';
  XMLNS_BROWSE = 'jabber:iq:browse';
  XMLNS_AGENTS = 'jabber:iq:agents';
  XMLNS_SEARCH = 'jabber:iq:search';
  XMLNS_CONFERENCE = 'jabber:iq:conference';
  XMLNS_BM = 'storage:bookmarks';
  XMLNS_PREFS = 'storage:imprefs';
  XMLNS_CLIENT = 'jabber:client';
  XMLNS_XEVENT = 'jabber:x:event';
  XMLNS_DELAY = 'jabber:x:delay';
  XMLNS_XROSTER = 'jabber:x:roster';
  XMLNS_XCONFERENCE = 'jabber:x:conference';
  XMLNS_XDATA = 'jabber:x:data';
  XMLNS_XOOB = 'jabber:x:oob';
  XMLNS_MUC = 'http://jabber.org/protocol/muc';
  XMLNS_MUCOWNER = 'http://jabber.org/protocol/muc#owner';
  XMLNS_MUCADMIN = 'http://jabber.org/protocol/muc#admin';
  XMLNS_MUCUSER = 'http://jabber.org/protocol/muc#user';
  XMLNS_DISCO = 'http://jabber.org/protocol/disco';
  XMLNS_SI = 'http://jabber.org/protocol/si';
  XMLNS_FTPROFILE = 'http://jabber.org/protocol/si/profile/file-transfer';
  XMLNS_BYTESTREAMS = 'http://jabber.org/protocol/bytestreams';
  XMLNS_FEATNEG = 'http://jabber.org/protocol/feature-neg';
  XMLNS_CSI = 'urn:xmpp:csi:0';
  XMLNS_SM2 = 'urn:xmpp:sm:2';
  XMLNS_SM3 = 'urn:xmpp:sm:3';
  XMLNS_STANZAS = 'urn:ietf:params:xml:ns:xmpp-stanzas';
  XMLNS_XMPP_SASL = 'urn:ietf:params:xml:ns:xmpp-sasl';
  XMLNS_XMPP_BIND = 'urn:ietf:params:xml:ns:xmpp-bind';
  XMLNS_XMPP_SESSION = 'urn:ietf:params:xml:ns:xmpp-session';
  XMLNS_CAPS = 'http://jabber.org/protocol/caps';
  XMLNS_ADDRESS = 'http://jabber.org/protocol/address';
  XMLNS_STREAM = 'http://etherx.jabber.org/streams';
  XMLNS_IQ_REGISTER = 'http://jabber.org/features/iq-register';
  XMLNS_THIS_NODE = 'http://www.emite.net/es/EmiteTalkServer/';
  XMLNS_XHTMLIM = 'http://jabber.org/protocol/xhtml-im';
  XMLNS_XHTML = 'http://www.w3.org/1999/xhtml';
  XMLNS_SHIM = 'http://jabber.org/protocol/shim';

function GenRandomSeq(SLen: integer): string;
function ShaHASH(Fkey: String): String; // HASH function using indy
function Montar(S: string): string;
function Desmontar(S: string): string;

implementation

{ TListaClientes }

function TListaClientes.Add: TCliente;
begin
  result := inherited Add as TCliente;
end;

function TListaClientes.GetItem(Index: integer): TCliente;
begin
  result := inherited Items[Index] as TCliente;
end;

function TListaClientes.IndexOfUser(Client: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    if Item[i].ID_Usuario = Client then
    begin
      result := i;
      Exit;
    end;
  end;
end;

{ TListaInstancias }

function TListaInstancias.Add: TInstancia;
begin
  result := inherited Add as TInstancia;
end;

function TListaInstancias.GetItem(Index: integer): TInstancia;
begin
  result := inherited Items[Index] as TInstancia;
end;

function TListaInstancias.IndexOfInstancia(Instancia: integer): integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
  begin
    if Item[i].fClaveInstancia = Instancia then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function GenRandomSeq(SLen: integer): string;
var
  str: String;
begin
  Randomize;
  // string with all possible chars
  str := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
  result := '';
  repeat
    result := result + str[Random(Length(str)) + 1];
  until (Length(result) = SLen);
end;

function ShaHASH(Fkey: String): String;
var
  fdig, fdigest: string;
  _hasher: TIdHashMessageDigest5;
begin
  Fkey := Trim(Fkey);
  _hasher := TIdHashMessageDigest5.Create;
  fdigest := _hasher.HashStringAsHex(Fkey);
  FreeAndNil(_hasher);
  fdig := Lowercase(fdigest);
  result := fdig;
end;

function Montar(S: string): string;
const
  Churro: array [0 .. 15] of byte = ($20, $5A, $CB, $A5, $B6, $F4, $FD, $FE, $23, $04, $88, $99, $00, $8F, $4D, $1E);
var
  Buf: TBytes;
  Input: array [0 .. 15] of byte;
  Output: array [0 .. 15] of byte;
  Len, Nbytes, Resto, Lineas, i, j, k: integer;
  Cif: TDCP_rijndael;
begin
  Cif := TDCP_rijndael.Create(nil);
  try
    Resto := 0;
    Buf := TEncoding.ANSI.GetBytes(S);
    Len := Length(Buf);
    if Len < Length(Input) then
      Lineas := 1
    else
    begin
      Lineas := Len div Length(Input);
      Resto := Len mod Length(Input);
      if Resto <> 0 then
        Inc(Lineas);
    end;

    if Len > 0 then
    begin
      k := 0;
      for i := 0 to Lineas - 1 do
      begin
        FillChar(Input, Length(Input), #0);
        if Lineas = 1 then
          Nbytes := Len
        else if (Resto <> 0) and (i = Lineas - 1) then
          Nbytes := Resto
        else
          Nbytes := Length(Input);
        for j := 0 to Nbytes - 1 do
          Input[j] := Buf[k + j];
        Inc(k, Nbytes);
        Cif.Init(Churro, Sizeof(Churro) * 8, nil);
        Cif.EncryptECB(Input, Output);
        Cif.Burn;
        for j := 0 to Length(Output) - 1 do
          result := result + IntToHex(Output[j], 2);
      end;
    end;
  finally
    Cif.Free;
  end;
end;

function Desmontar(S: string): string;
const
  Churro: array [0 .. 15] of byte = ($20, $5A, $CB, $A5, $B6, $F4, $FD, $FE, $23, $04, $88, $99, $00, $8F, $4D, $1E);
var
  OutputSBuf, Buf: TBytes;
  Input: array [0 .. 15] of byte;
  Output: array [0 .. 15] of byte;
  Len, Lineas, i, j, k: integer;
  Cif: TDCP_rijndael;
begin
  Cif := TDCP_rijndael.Create(nil);
  try
    SetLength(Buf, Length(S) div 2);
    HexToBin(PChar(S), Buf[0], Length(Buf));
    Len := Length(Buf);
    SetLength(OutputSBuf, Len);
    if Len > 0 then
    begin
      Lineas := Len div Length(Input);
      k := 0;
      for i := 0 to Lineas - 1 do
      begin
        for j := 0 to Length(Input) - 1 do
          Input[j] := Buf[k + j];
        Cif.Init(Churro, Sizeof(Churro) * 8, nil);
        Cif.DecryptECB(Input, Output);
        Cif.Burn;
        for j := 0 to Length(Output) - 1 do
          OutputSBuf[k + j] := Output[j];
        Inc(k, 16);
      end;
      result := TEncoding.ASCII.GetString(OutputSBuf);
    end;
  finally
    Cif.Free;
  end;
end;

end.
