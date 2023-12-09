unit uXMLServerParser;

interface

uses system.Classes, uXMPPServerHelper, system.JSON, XMPPServerSASLCrypt, FireDAC.Comp.Client, FireDAC.Stan.Param, system.RegularExpressions, IdContext,
  system.Generics.Collections, system.SysUtils,
  system.Variants;

type

  TXMPPXml = class
  private
    FDMasterCon: TFDConnection;
    fClientes: TListaClientes;
    fInstancias: TListaInstancias;

    challengeSent: boolean;
    fUsername: String; // Connected username
    fPassword: String; // password
    fServer: String; // jabber server
    fSessionID: String;
    fAlreadyLogged: boolean; // Already logged flag
    // fOnInitConnection: TOnInitConnection; // On connection initialized event
    fOnCryptedSASL: TOnResponseWithStrData; // On decrypting SASL
    fOnSuccessSASL: TOnResponseWithStrData; // Decrypt successful
    fOnBindInit: TEvent; // On bind process
    fOnSessionInit: TEvent; // on session initialized
    fOnIqRequest: TOnResponseWithStrData; // On received an IQ request
    fOnGettingContacts: TOnResponseWithStrData; // On getting contacts list
    fOnFaillure: TOnResponseWithStrData; // On faillure from server
    fOnUpdateRoster: TOnRoster; // On contact informations updated
    fOnAddRoster: TOnRoster; // On contact informations updated
    fOnAskToAddRosterStatus: TOnAddRequestStatus; // On asked to add a contact
    fOnAskedForSubscription: TOnResponseWithStrData; // when a contact ask to add you
    fOnMessageReceived: TOnMessageReceived; // On received a message from a contact
    // fOnPresenceCallback: TOnPresenceCallBack; // On received a status change from a contact
    // fOnGetRegistrationFiels: TOnGetRegistrationFiels;
    fOnServerCloseStream: TEvent;
    fOnEditUser: TEvent;
    fOnAddUser: TEvent;
    fOnRemoveUser: TEvent;
    fOnError: TOnError;
    fOnOpenedStream: TEvent;
    fOnStatus: TOnStatus;
    FDebug: TOnDebug;
    fOnDeleteRoster: TEvent;
    // fEncriptMode: TEncriptMode;
    // fRegistrationFiels: Tlist<TRegistrationField>;
    fstatus: TStatus;
    // fStatusStr: string;



    // procesado de consultas///

    procedure ProcessOpenStreamRequest(ARequest: TJSONObject; Contexto: TIdContext); // Process StreamStream Response
    procedure ProcessAuthRequest(ARequest: TJSONObject; Contexto: TIdContext);
    procedure ProcessAuthResponseRequest(ARequest: TJSONObject; Contexto: TIdContext);
    procedure ProcessIQRequest(ARequest: TJSONObject; Contexto: TIdContext); // process IQ Request
    procedure ProcessPresenceResponse(ARequest: TJSONObject; Contexto: TIdContext); // process presence  Response
    procedure ProcessMessageResponse(ARequest: TJSONObject; Contexto: TIdContext); // process Message Response
    procedure ProcessEnableResponse(ARequest: TJSONObject; Contexto: TIdContext);
    procedure ProcessFaillureResponse(AResponse: TJSONObject); // process faillure Response
    procedure ProcessUnavailableServiceResponse(ARequest: TJSONObject; Contexto: TIdContext);
    procedure ProcessResumeResponse(ARequest: TJSONObject; Contexto: TIdContext);
    procedure ProcessVCardUpdateRequest(Vcard: TJSONObject; AId: String; Contexto: TIdContext);
    procedure SendOfflineMessages(Contexto: TIdContext);
    function ConfiguraConexion(dominio: string; Cliente: integer): boolean;

    // function GetMechanism(AMchanismsNode: PxmlNode): TMechanism; // Get mechanism to use for authentification from server response
    procedure ChangeStatus(NewStatus: TStatus; NewStatuStr: String);
    procedure Log(ATipo: integer; OD: String; AMessage: string);
  public
    Constructor Create(FDMaster: TFDConnection; Instancias: TListaInstancias; Clientes: TListaClientes);
    Destructor Destroy; Override;
    // Return requests as xml
    // function CreateInitRequest(AServer: string): String;
    // function CreateAuthRequest(Amechanism: TMechanism): string;
    // function CreateSASLRequest(ACrypted: String): String;
    // function CreateResourceRequest(AResource: String): string;
    // function CreateInitBindRequest(AResource: String): String;
    // function CreateInitSessionRequest(AUniqueID: String): String;
    // function CreateGetcontactsRequest: String;
    // function CreateAddContactRequest(AFrom, AUsername, AJID: string; GroupList: TStringlist): string;
    // function CreateSendMessageRequest(AFrom, ATo, AType, ABody: string): string;
    // function CreateDeleteRosterRequest(AFrom, ATo: String): string;
    // //  function CreateEditRosterRequest(ANewName, AFrom, ATo: String; GroupList: TStringlist): string;
    // function CreateAddRegistrationRequest(AServer, AUsername, APassword: string): string;
    /// /   function CreateEditRegistrationRequest(AServer, AUsername, APassword: string): string;
    // function CreateRemoveRegistrationRequest(AServer: string): string;
    // function CreateGetRegistrationFieldsRequest(AServer: string): string;
    // function CreateRegisterStreamRequest(AServer: string): String;

    /// /server /////

    function CreateOpenStreamResponse(AServer: string; Cliente: integer): string;
    function CreateAuthResponse(Mecanismo: TMecanismoSASL; Cliente: integer): string;
    function CreateSASLSuccessResponse(Cliente: integer): string;
    function CreateSASLErrorResponse(ErrorID: integer): string;
    function CreateSASLSuccessChallengeResponse(rspauth: string): string;
    function CreateBindIQResponse(AId, AUser, AServer, AResource: string): String;
    function CreateSessionIQResponse(SessionID: string; Cliente: integer): String;
    function CreateSessionManagementResponse(Version, Cliente: integer): String;
    function CreateUnavailableServiceResponse(AId, AFrom, ATo, AResource: string; Service: TJSONObject; Cliente: integer): String;
    function CreateRosterIQResponse(Roster: TFDQuery; AId, AFrom, ATo, AResource: string; Cliente: integer): String;
    function CreateMessageResponse(AFrom, ATo, AType, ABody: string; Cliente: integer; Stamp: TDateTime = 0; Delayed: boolean = False): string;
    function CreatePresenceResponse(ARequest: TJSONObject; Cliente: integer): string;
    function CreateDiscoItemsResponse(AId: string; Cliente: integer): string;
    function CreateDiscoInfoResponse(AId: string; Cliente: integer): string;
    // function CreatePresenceRequest(Astatus: TUserStatus; AMsg: String): string;

    // Parse server response
    procedure ParseServerResponse(AXmlResponse: string; Contexto: TIdContext);

    // public roperties
    // property OnInitConnection: TOnInitConnection read fOnInitConnection write fOnInitConnection;
    property OnCryptedSASL: TOnResponseWithStrData read fOnCryptedSASL write fOnCryptedSASL;
    property OnSuccessSASL: TOnResponseWithStrData read fOnSuccessSASL write fOnSuccessSASL;
    property OnBindInit: TEvent read fOnBindInit write fOnBindInit;
    property OnSessionInit: TEvent read fOnSessionInit write fOnSessionInit;
    property OnIqRequest: TOnResponseWithStrData read fOnIqRequest write fOnIqRequest;
    property OnGettingContacts: TOnResponseWithStrData read fOnGettingContacts write fOnGettingContacts;
    // property OnGettingRegistrationFields: TOnGetRegistrationFiels read fOnGetRegistrationFiels write fOnGetRegistrationFiels;
    property OnFaillure: TOnResponseWithStrData read fOnFaillure write fOnFaillure;
    property OnAddRoster: TOnRoster read fOnAddRoster write fOnAddRoster;
    property OnUpdateRoster: TOnRoster read fOnUpdateRoster write fOnUpdateRoster;
    property OnDeleteRoster: TEvent read fOnDeleteRoster write fOnDeleteRoster;
    property OnAskToAddRosterStatus: TOnAddRequestStatus read fOnAskToAddRosterStatus write fOnAskToAddRosterStatus;
    property OnAskedForSubscription: TOnResponseWithStrData read fOnAskedForSubscription write fOnAskedForSubscription;
    property OnMessageReceived: TOnMessageReceived read fOnMessageReceived write fOnMessageReceived;
    // property OnPresenceCallback: TOnPresenceCallBack read fOnPresenceCallback write fOnPresenceCallback;
    property OnError: TOnError read fOnError write fOnError;

    property OnEditUser: TEvent read fOnEditUser write fOnEditUser;
    property OnAddUser: TEvent read fOnAddUser write fOnAddUser;
    property OnRemoveUser: TEvent read fOnRemoveUser write fOnRemoveUser;
    property OnOpenedStream: TEvent read fOnOpenedStream write fOnOpenedStream;
    property OnServerCloseStream: TEvent read fOnServerCloseStream write fOnServerCloseStream;
    property OnStatus: TOnStatus read fOnStatus write fOnStatus;
    property Username: String read fUsername;
    property Password: String read fPassword;
    property Server: String read fServer;
    property OnDebug: TOnDebug read FDebug write FDebug;
    // property EncriptMode: TEncriptMode read fEncriptMode write fEncriptMode;
  end;

implementation

{ TXMPPXml }

constructor TXMPPXml.Create(FDMaster: TFDConnection; Instancias: TListaInstancias; Clientes: TListaClientes);
begin
  fInstancias := Instancias;
  fClientes := Clientes;
  FDMasterCon := FDMaster;
end;

destructor TXMPPXml.Destroy;
begin
  inherited;
end;

procedure TXMPPXml.Log(ATipo: integer; OD: String; AMessage: string);
begin
  if assigned(FDebug) then
    FDebug(ATipo, OD, AMessage);
end;

function TXMPPXml.ConfiguraConexion(dominio: string; Cliente: integer): boolean;
var
  FDDominio: TFDQuery;
  IDInstancia: integer;
begin
  Result := False;
  FDDominio := TFDQuery.Create(nil);
  try
    FDDominio.Connection := FDMasterCon;
    FDDominio.SQL.Text := 'SELECT * FROM ' + TABLA_DOMINIOS + ' WHERE ACTIVO = ''T'' AND NOMBRE=:PNOMBRE';
    try
      FDDominio.ParamByName('PNOMBRE').AsString := dominio;
      FDDominio.Open;
      if FDDominio.RecordCount > 0 then
      begin
        IDInstancia := fInstancias.IndexOfInstancia(FDDominio.FieldByName('CLAVE_INSTANCIA').AsInteger);
        if (IDInstancia <> -1) AND (IDInstancia < fInstancias.Count) then
        begin
          fClientes.Item[Cliente].ConexionDB := fInstancias.Item[IDInstancia].Conexion;
          fClientes.Item[Cliente].TransaccionDB := fInstancias.Item[IDInstancia].Transaccion;
          fClientes.Item[Cliente].ClaveEmpresa := FDDominio.FieldByName('CLAVE_EMPRESA').AsInteger;
          Result := True;
          Log(0, '', 'Configurada la conexion de: ' + dominio);
        end;
      end;
    except
      on E: Exception do
        Log(1, 'ConfiguraConexionException ', E.Message);
    end;
  finally
    FDDominio.Free;
  end;
end;

procedure TXMPPXml.ParseServerResponse(AXmlResponse: string; Contexto: TIdContext);
var
  JStanza, JAttributes: TJSONObject;
  JStanzaName: TJSONValue;
begin
  Log(2, Contexto.Connection.Socket.Binding.PeerIP + ':' + IntToStr(Contexto.Connection.Socket.Binding.PeerPort), AXmlResponse);
  JStanza := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(AXmlResponse), 0) as TJSONObject;
  try
    try
      if JStanza <> nil then
      begin
        JStanzaName := JStanza.GetValue('stanza-name');
        // JAttributes := JStanza.GetValue('stanza-attributes') as TJSONObject;
        // JNamespace := JAttributes.GetValue('xmlns');
        if (JStanzaName <> nil) then
        begin
          if JStanzaName.Value = 'stream' then
            ProcessOpenStreamRequest(JStanza, Contexto)
          else if JStanzaName.Value = 'auth' then
            ProcessAuthRequest(JStanza, Contexto)
          else if JStanzaName.Value = 'response' then
            ProcessAuthResponseRequest(JStanza, Contexto)
          else if JStanzaName.Value = 'iq' then
            ProcessIQRequest(JStanza, Contexto)
          else if JStanzaName.Value = 'presence' then
            ProcessPresenceResponse(JStanza, Contexto)
          else if JStanzaName.Value = 'message' then
            ProcessMessageResponse(JStanza, Contexto)

            { else if JStanzaName.Value = 'enable' then
              ProcessEnableResponse(wNode, Contexto)
              else if JStanzaName.Value = 'resume' then
              ProcessResumeResponse(wNode, Contexto) }

        end;
      end;
    except
      On E: Exception do
        Log(1, 'OnParseException', E.Message);
    end;
  finally
    if JStanza <> nil then
      JStanza.Free;
  end;
end;

procedure TXMPPXml.ChangeStatus(NewStatus: TStatus; NewStatuStr: String);
begin
  if assigned(fOnStatus) then
    fOnStatus(NewStatus, NewStatuStr);
  fstatus := NewStatus;
end;

function TXMPPXml.CreateOpenStreamResponse(AServer: string; Cliente: integer): string;
var
  JStanza: TJSONObject;
  JAttributes, JElement: TJSONObject;
  JChildrensFeat, JMecanismosChilds: TJSONArray;
begin
  if (fClientes.Count < Cliente) then
    raise Exception.Create('El Cliente no esta registrado');
  JStanza := TJSONObject.Create;
  JChildrensFeat := TJSONArray.Create;
  try
    JAttributes := TJSONObject.Create;
    with JAttributes do
    begin
      AddPair('xmlns', TJSONString.Create(XMLNS_CAPS));
      AddPair('hash', TJSONString.Create('sha-1'));
      AddPair('node', TJSONString.Create(XMLNS_THIS_NODE));
      AddPair('ver', TJSONString.Create('pYn5H6I0wyeMtWni4hReHOiHUPQ='));
    end;
    JElement := TJSONObject.Create;
    JElement.AddPair('element-name', TJSONString.Create('c'));
    JElement.AddPair('element-attributes', JAttributes);
    JChildrensFeat.AddElement(JElement);
    if fClientes.Item[Cliente].Estado = sSASLSucess then
    begin
      fClientes.Item[Cliente].Estado := sSession;
      JElement := TJSONObject.Create;
      JElement.AddPair('element-name', TJSONString.Create('bind'));
      JElement.AddPair('element-attributes', TJSONObject.Create(TJSONPair.Create('xmlns', TJSONString.Create(XMLNS_XMPP_BIND))));
      JChildrensFeat.AddElement(JElement);

      JElement := TJSONObject.Create;
      JElement.AddPair('element-name', TJSONString.Create('c'));
      JElement.AddPair('element-attributes', TJSONObject.Create(TJSONPair.Create('xmlns', TJSONString.Create(XMLNS_XMPP_SESSION))));
      JChildrensFeat.AddElement(JElement);

      // JChildrensFeat.AddElement(JChildrenSession); <sm xmlns="' + XMLNS_SM2 + '"/>
    end
    else if fClientes.Item[Cliente].Estado < sOpenStream then
    begin
      fClientes.Item[Cliente].Estado := sOpenStream;

      JElement := TJSONObject.Create;
      JElement.AddPair('element-name', TJSONString.Create('register'));
      JElement.AddPair('element-attributes', TJSONString.Create('DIGEST-MD5'));

      JChildrensFeat.AddElement(JElement);

      JMecanismosChilds := TJSONArray.Create;
      JElement := TJSONObject.Create;
      JElement.AddPair('element-name', TJSONString.Create('mechanism'));
      JElement.AddPair('element-value', TJSONString.Create('DIGEST-MD5'));
      JMecanismosChilds.AddElement(JElement);

      JElement := TJSONObject.Create;
      JElement.AddPair('element-name', TJSONString.Create('mechanisms'));
      JElement.AddPair('element-attributes', TJSONObject.Create(TJSONPair.Create('xmlns', TJSONString.Create(XMLNS_XMPP_SASL))));
      JElement.AddPair('element-children', JMecanismosChilds);

      JChildrensFeat.AddElement(JElement);
    end;

    JAttributes := TJSONObject.Create;
    JAttributes.AddPair('xmlns', TJSONString.Create(XMLNS_CLIENT));
    JAttributes.AddPair('id', TJSONString.Create(GenRandomSeq(10)));
    JAttributes.AddPair('from', TJSONString.Create(AServer));
    JAttributes.AddPair('version', TJSONString.Create('1.0'));
    JAttributes.AddPair('xmlns:stream', TJSONString.Create(XMLNS_STREAM));
    JAttributes.AddPair('lang', TJSONString.Create('es'));

    JElement := TJSONObject.Create;
    JElement.AddPair('element-name', TJSONString.Create('features'));
    JElement.AddPair('element-children', JChildrensFeat);

    JStanza.AddPair('stanza-name', TJSONString.Create('stream'));
    JStanza.AddPair('stanza-attributes', JAttributes);
    JStanza.AddPair('stanza-children', JElement);
    Result := JStanza.ToString;

  finally
    JStanza.Free;
  end;
end;

function TXMPPXml.CreateAuthResponse(Mecanismo: TMecanismoSASL; Cliente: integer): string;
var
  Challenge: string;
  JStanza: TJSONObject;
  JAtributes: TJSONObject;
begin
  if (fClientes.Count < Cliente) then
    raise Exception.Create('El Cliente no esta registrado');

  Challenge := CreateSASLChallenge(Mecanismo);
  fClientes.Item[Cliente].SASL := Mecanismo;

  JStanza := TJSONObject.Create;
  JAtributes := TJSONObject.Create;
  try
    JAtributes.AddPair('xmlns', TJSONString.Create(XMLNS_XMPP_SASL));
    JStanza.AddPair('stanza-name', TJSONString.Create('challenge'));
    JStanza.AddPair('stanza-attributes', JAtributes);
    JStanza.AddPair('stanza-value', TJSONString.Create(Challenge));
    Result := JStanza.ToString;
  finally
    JStanza.Free;
  end;
end;

function TXMPPXml.CreateSASLSuccessChallengeResponse(rspauth: string): string;
var
  Challenge: string;
  JStanza: TJSONObject;
  JAtributes: TJSONObject;
begin
  { ref doc http://wiki.xmpp.org/web/SASLandDIGEST-MD5 }
  Challenge := Encode64('rspauth=' + rspauth);

  JStanza := TJSONObject.Create;
  JAtributes := TJSONObject.Create;
  try
    JAtributes.AddPair('xmlns', TJSONString.Create(XMLNS_XMPP_SASL));
    JStanza.AddPair('stanza-name', TJSONString.Create('challenge'));
    JStanza.AddPair('stanza-attributes', JAtributes);
    JStanza.AddPair('stanza-value', TJSONString.Create(Challenge));
    Result := JStanza.ToString;
  finally
    JStanza.Free;
  end;
end;

function TXMPPXml.CreateSASLSuccessResponse(Cliente: integer): string;
var
  JStanza: TJSONObject;
  JAtributes: TJSONObject;
begin
  { ref doc https://tools.ietf.org/html/rfc6120#ref-SASL section 6.4.6 }

  if (fClientes.Count < Cliente) then
    raise Exception.Create('El Cliente no esta registrado');

  JStanza := TJSONObject.Create;
  JAtributes := TJSONObject.Create;
  try
    JAtributes.AddPair('xmlns', TJSONString.Create(XMLNS_XMPP_SASL));
    JStanza.AddPair('stanza-name', TJSONString.Create('success'));
    JStanza.AddPair('stanza-attributes', JAtributes);
    Result := JStanza.ToString;
  finally
    JStanza.Free;
  end;
end;

function TXMPPXml.CreateSASLErrorResponse(ErrorID: integer): string;
var
  FailureDetails: string;
begin
  { ref doc https://tools.ietf.org/html/rfc6120#ref-SASL section 6.5 }

  case ErrorID of
    1:
      FailureDetails := '<aborted/>';
    2:
      FailureDetails := '<account-disabled/>';
    3:
      FailureDetails := '<credentials-expired/>';
    4:
      FailureDetails := '<encryption-required/>';
    5:
      FailureDetails := '<incorrect-encoding/>';
    6:
      FailureDetails := '<invalid-authzid/>';
    7:
      FailureDetails := '<invalid-mechanism/>';
    8:
      FailureDetails := '<malformed-request/>';
    9:
      FailureDetails := '<mechanism-too-weak/>';
    10:
      FailureDetails := '<not-authorized/>';
    11:
      FailureDetails := '<temporary-auth-failure/>';
  end;
  Result := '<failure xmlns="' + XMLNS_XMPP_SASL + '"' + FailureDetails + '</failure>';
end;

// function TXMPPXml.CreateSASLUnautorizedResponse(ErrorID: integer): string;

function TXMPPXml.CreateBindIQResponse(AId, AUser, AServer, AResource: string): String;
var
  JStanza, JAtributes, JChildren, JBindChildren: TJSONObject;
begin
  JStanza := TJSONObject.Create;
  JAtributes := TJSONObject.Create;
  JChildren := TJSONObject.Create;
  JBindChildren := TJSONObject.Create;
  try
    JBindChildren.AddPair('element-name', TJSONString.Create('jid'));
    JBindChildren.AddPair('element-value', TJSONString.Create(AUser + '@' + AServer + '/' + AResource));

    JChildren.AddPair('element-name', TJSONString.Create('bind'));
    JChildren.AddPair('element-attributes', TJSONObject.Create(TJSONPair.Create('xmlns', TJSONString.Create(XMLNS_XMPP_BIND))));
    JChildren.AddPair('element-children', JBindChildren);

    JAtributes.AddPair('id', TJSONString.Create(AId));
    JAtributes.AddPair('type', TJSONString.Create('result'));

    JStanza.AddPair('stanza-name', TJSONString.Create('iq'));
    JStanza.AddPair('stanza-attributes', JAtributes);
    JStanza.AddPair('stanza-children', JChildren);
    Result := JStanza.ToString;
  finally
    JStanza.Free;
  end;
end;

function TXMPPXml.CreateSessionIQResponse(SessionID: string; Cliente: integer): String;
var
  JStanza, JAtributes, JChildren: TJSONObject;
begin
  { SM := '';
    case fClientes.Item[Cliente].SesionManager of
    2:
    SM := '<r xmlns="' + XMLNS_SM2 + '"/>';

    end; }

  JStanza := TJSONObject.Create;
  JAtributes := TJSONObject.Create;
  JChildren := TJSONObject.Create;

  try
    JChildren.AddPair('element-name', TJSONString.Create('session'));
    JChildren.AddPair('element-attributes', TJSONObject.Create(TJSONPair.Create('xmlns', TJSONString.Create(XMLNS_XMPP_SESSION))));

    JAtributes.AddPair('id', TJSONString.Create(SessionID));
    JAtributes.AddPair('type', TJSONString.Create('result'));

    JStanza.AddPair('stanza-name', TJSONString.Create('iq'));
    JStanza.AddPair('stanza-attributes', JAtributes);
    JStanza.AddPair('stanza-children', JChildren);
    Result := JStanza.ToString;
  finally
    JStanza.Free;
  end;

  { if SM = '' then
    Result := '<iq type="result" id="' + SessionID + '"><session xmlns="' + XMLNS_XMPP_SESSION + '"/></iq>'
    else
    Result := '<iq type="result" id="' + SessionID + '"/>' + SM; }
end;

function TXMPPXml.CreateSessionManagementResponse(Version, Cliente: integer): String;
begin
  case Version of
    2:
      Result := '<enabled xmlns="' + XMLNS_SM2 + '" id="' + GenRandomSeq(48) + '" resume="true" max="300"/>';
    3:
      Result := '<enabled xmlns="' + XMLNS_SM3 + '" id="' + GenRandomSeq(48) + '" resume="true" max="300"/>';
    4:
      Result := '<enabled xmlns="' + XMLNS_CSI + '" id="' + GenRandomSeq(48) + '" resume="true" max="300"/>';

  end;
  fClientes.Item[Cliente].SesionManager := Version;
end;

function TXMPPXml.CreateDiscoItemsResponse(AId: string; Cliente: integer): string;
var
  Serv, Usr, Res, SM: string;
begin
  Serv := fClientes.Item[Cliente].VServer;
  Usr := fClientes.Item[Cliente].Username;
  Res := fClientes.Item[Cliente].Recurso;

  SM := '';
  case fClientes.Item[Cliente].SesionManager of
    2:
      SM := '<r xmlns="' + XMLNS_SM2 + '"/>';

  end;

  Result := '<iq from="' + Serv + '" to="' + Usr + '@' + Serv + '/' + Res + '" id="' + AId + '" type="result"><query xmlns="' + XMLNS_DISCOITEMS + '"><item jid="conference.' + Serv
    + '"/><item jid="pubsub.' + Serv + '"/><item jid="vjud.' + Serv + '"/></query></iq>' + SM;
end;

function TXMPPXml.CreateDiscoInfoResponse(AId: string; Cliente: integer): string;
var
  Serv, Usr, Res, SM: string;

  SL: TStringList;

begin
  SL := TStringList.Create;
  try
    Serv := fClientes.Item[Cliente].VServer;
    Usr := fClientes.Item[Cliente].Username;
    Res := fClientes.Item[Cliente].Recurso;

    SM := '';
    case fClientes.Item[Cliente].SesionManager of
      2:
        SM := '<r xmlns="' + XMLNS_SM2 + '"/>';

    end;

    SL.Add('<iq from="' + Serv + '" to="' + Usr + '@' + Serv + '/' + Res + '" id="' + AId + '" type="result">');
    SL.Add('<query xmlns="' + XMLNS_DISCOINFO + '">');
    SL.Add('<identity category="pubsub" type="pep"/>');
    SL.Add('<identity category="server" type="im" name="EMensajeria"/>');
    SL.Add('<x xmlns="jabber:x:data" type="result">');
    SL.Add('<field var="FORM_TYPE" type="hidden">');
    SL.Add('<value>http://jabber.org/network/serverinfo</value>');
    SL.Add('</field>');
    SL.Add('</x>');
    SL.Add('<feature var="' + XMLNS_COMMANDS + '"/>');
    SL.Add('<feature var="' + XMLNS_DISCOINFO + '"/>');
    SL.Add('<feature var="' + XMLNS_DISCOITEMS + '"/>');
    SL.Add('<feature var="' + XMLNS_IQ + '"/>');
    SL.Add('<feature var="' + XMLNS_LAST + '"/>');
    SL.Add('<feature var="' + XMLNS_PRIVACY + '"/>');
    SL.Add('<feature var="' + XMLNS_REGISTER + '"/>');
    SL.Add('<feature var="' + XMLNS_VERSION + '"/>');
    SL.Add('<feature var="' + XMLNS_MSOFFLINE + '"/>');
    SL.Add('<feature var="' + XMLNS_PRESENCE + '"/>');
    SL.Add('<feature var="' + XMLNS_BLOCKING + '"/>');
    SL.Add('<feature var="' + XMLNS_PING + '"/>');
    SL.Add('<feature var="' + XMLNS_VCARD + '"/>');
    SL.Add('</query>');
    SL.Add('</iq>');
    SL.Add(SM);
    Result := SL.Text;
  finally
    SL.Free;
  end;

end;

procedure TXMPPXml.ProcessResumeResponse(ARequest: TJSONObject; Contexto: TIdContext);
begin
  // Contexto.Connection.IOHandler.WriteLn('<failed xmlns="' + XMLNS_SM2 + '"><item-not-found xmlns="' + XMLNS_STANZAS + '"/></failed>');
end;

function TXMPPXml.CreateUnavailableServiceResponse(AId, AFrom, ATo, AResource: string; Service: TJSONObject; Cliente: integer): String;
var
  SM: string;
begin
  SM := '';
  case fClientes.Item[Cliente].SesionManager of
    2:
      SM := '<r xmlns="' + XMLNS_SM2 + '"/>';

  end;

  // Result := '<iq from="' + AFrom + '" to="' + ATo + '/' + AResource + '" type="error" xml:lang="es" id="' + AId + '"><' + Service.ChildNodes[0].NodeName + ' xmlns="' +
  // Service.ChildNodes[0].Attributes['xmlns'] + '"/><error code="503" type="cancel"><service-unavailable xmlns="' + XMLNS_STANZAS + '"/></error></iq>' + SM;

end;

function TXMPPXml.CreateRosterIQResponse(Roster: TFDQuery; AId, AFrom, ATo, AResource: string; Cliente: integer): String;
var
  Items, SM: string;
begin
  while not Roster.Eof do
  begin
    Items := Items + '<item subscription="' + Roster.FieldByName('SUBSCRIPCION').AsString + '" name="' + Roster.FieldByName('NOMBRE').AsString + '" jid="' +
      Roster.FieldByName('NOMBRE').AsString + '@' + Roster.FieldByName('DOMINIO').AsString + '"/>';
    Roster.Next;
  end;
  SM := '';
  case fClientes.Item[Cliente].SesionManager of
    2:
      SM := '<r xmlns="' + XMLNS_SM2 + '"/>';

  end;
  Result := '<iq from="' + AFrom + '" to="' + ATo + '/' + AResource + '" type="result" id="' + AId + '"><query xmlns="' + XMLNS_ROSTER + '">' + Items + '</query></iq>' + SM;
end;

function TXMPPXml.CreateMessageResponse(AFrom, ATo, AType, ABody: string; Cliente: integer; Stamp: TDateTime = 0; Delayed: boolean = False): string;
var
  JStanza, JAtributes, JChildren: TJSONObject;
  JChildrens: TJSONArray;
begin
  { SM := '';
    case fClientes.Item[Cliente].SesionManager of
    2:
    SM := '<r xmlns="' + XMLNS_SM2 + '"/>';
    end; }

  JStanza := TJSONObject.Create;
  JChildrens := TJSONArray.Create;
  try
    if Delayed then
    begin
      JAtributes := TJSONObject.Create;
      JAtributes.AddPair('xmlns', TJSONString.Create(XMLNS_DELAY));
      JAtributes.AddPair('from', TJSONString.Create(AFrom));
      JAtributes.AddPair('stamp', TJSONString.Create(DateToStr(Stamp) + 'T' + TimeToStr(Stamp) + 'Z'));

      JChildren := TJSONObject.Create;
      JChildren.AddPair('element-name', TJSONString.Create('delay'));
      JChildren.AddPair('element-attributes', JAtributes);
      JChildren.AddPair('element-value', TJSONString.Create('Offline Storage'));
      JChildrens.AddElement(JChildren);
    end;

    JChildren := TJSONObject.Create;
    JChildren.AddPair('element-name', TJSONString.Create('body'));
    JChildren.AddPair('element-value', TJSONString.Create(Encode64(ABody)));
    JChildrens.AddElement(JChildren);

    JAtributes := TJSONObject.Create;
    JAtributes.AddPair('to', TJSONString.Create(ATo));
    JAtributes.AddPair('from', TJSONString.Create(AFrom));
    JAtributes.AddPair('type', TJSONString.Create(AType));
    JAtributes.AddPair('id', TJSONString.Create(GenRandomSeq(10)));

    JStanza.AddPair('stanza-name', TJSONString.Create('message'));
    JStanza.AddPair('stanza-attributes', JAtributes);
    JStanza.AddPair('stanza-children', JChildrens);
    Result := JStanza.ToString;
  finally
    JStanza.Free;
  end;
end;

function TXMPPXml.CreatePresenceResponse(ARequest: TJSONObject; Cliente: integer): string;
begin
  //
end;

procedure TXMPPXml.ProcessOpenStreamRequest(ARequest: TJSONObject; Contexto: TIdContext);
var
  AServer, Response: String;
  JAttributes: TJSONObject;
  JValue: TJSONValue;
begin
  JAttributes := ARequest.GetValue('stanza-attributes') as TJSONObject;
  if JAttributes <> nil then
  begin
    JValue := JAttributes.GetValue('to');
    if JValue <> nil then
    begin
      AServer := JValue.Value;
      if ConfiguraConexion(AServer, Contexto.Connection.Tag) then
      begin
        fClientes.Item[Contexto.Connection.Tag].VServer := AServer;

        Response := CreateOpenStreamResponse(AServer, Contexto.Connection.Tag);

        Contexto.Connection.IOHandler.WriteLn(Response);

        Log(3, Contexto.Connection.Socket.Binding.PeerIP + ':' + IntToStr(Contexto.Connection.Socket.Binding.PeerPort), Response);
      end;
    end;
  end;
end;

procedure TXMPPXml.ProcessAuthRequest(ARequest: TJSONObject; Contexto: TIdContext);
var
  Response: String;
  AMecanismo: TMecanismoSASL;
  JAttributes: TJSONObject;
  JValue: TJSONValue;
begin

  AMecanismo := TMecanismoSASL.mDigestMD5;
  if fClientes.Item[Contexto.Connection.Tag].Estado = sOpenStream then
  begin
    fClientes.Item[Contexto.Connection.Tag].Estado := sSASLNego;
    JAttributes := ARequest.GetValue('stanza-attributes') as TJSONObject;
    if JAttributes <> nil then
    begin
      JValue := JAttributes.GetValue('mechanism');
      if JValue <> nil then
      begin
        if JValue.Value = 'DIGEST-MD5' then
          AMecanismo := mDigestMD5;

        Response := CreateAuthResponse(AMecanismo, Contexto.Connection.Tag);

        Contexto.Connection.IOHandler.WriteLn(Response);

        Log(3, Contexto.Connection.Socket.Binding.PeerIP + ':' + IntToStr(Contexto.Connection.Socket.Binding.PeerPort), Response);
      end;
    end;
  end;
end;

procedure TXMPPXml.ProcessAuthResponseRequest(ARequest: TJSONObject; Contexto: TIdContext);
var
  Challenge, Response: String;
  ErrorID: integer;
  rspauth: string;
  ID: integer;
  JValue: TJSONValue;
begin
  ID := Contexto.Connection.Tag;
  if fClientes.Item[ID].Estado = sSASLNego then
  begin
    Challenge := '';
    JValue := ARequest.GetValue('stanza-value');
    if JValue <> nil then
      Challenge := JValue.Value;
    if Challenge <> '' then
    begin
      ErrorID := CheckSASLDigestMD5(Challenge, fClientes.Item[ID].ConexionDB, fClientes.Item[ID].TransaccionDB, fClientes, ID, rspauth);
      if ErrorID = 0 then
        Response := CreateSASLSuccessChallengeResponse(rspauth)
      else
        Response := CreateSASLErrorResponse(ErrorID);
    end
    else
    begin
      Response := CreateSASLSuccessResponse(ID);
      fClientes.Item[ID].Estado := sSASLSucess;
    end;
    Contexto.Connection.IOHandler.WriteLn(Response);

    Log(3, Contexto.Connection.Socket.Binding.PeerIP + ':' + IntToStr(Contexto.Connection.Socket.Binding.PeerPort), Response);
  end;
end;

procedure TXMPPXml.ProcessEnableResponse(ARequest: TJSONObject; Contexto: TIdContext);
var
  Response: string;
begin
  { if (fClientes.Item[Contexto.Connection.Tag].Estado >= sSession) and ARequest.HasAttribute('xmlns') then
    begin
    if ARequest.Attributes['xmlns'] = XMLNS_SM2 then
    Response := CreateSessionManagementResponse(2, Contexto.Connection.Tag)
    else if ARequest.Attributes['xmlns'] = XMLNS_SM3 then
    Response := CreateSessionManagementResponse(3, Contexto.Connection.Tag);
    Contexto.Connection.IOHandler.WriteLn(Response);
    end; }
end;

procedure TXMPPXml.ProcessIQRequest(ARequest: TJSONObject; Contexto: TIdContext);
var
  QUsuario, QListaContactos: TFDQuery;
  wFrom, wId, wType, Nombre, dominio, ChildrenName, Resource, Response: String;
  ID: integer;
  JAttributes, JChildren, JChild: TJSONObject;
  JValue, JName: TJSONValue;
begin
  ID := Contexto.Connection.Tag;
  wType := '';
  wId := '';
  if (fClientes.Count < ID) then
    raise Exception.Create('El Cliente no esta registrado');

  JAttributes := ARequest.GetValue('stanza-attributes') as TJSONObject;
  if JAttributes <> nil then
  begin
    JValue := JAttributes.GetValue('type');
    if JValue <> nil then
      wType := JValue.Value;

    JValue := JAttributes.GetValue('id');
    if JValue <> nil then
      wId := JValue.Value;
  end;
  if (wType <> '') and (wId <> '') then
  begin
    Nombre := fClientes.Item[ID].Username;
    dominio := fClientes.Item[ID].VServer;

    QUsuario := TFDQuery.Create(nil);
    QListaContactos := TFDQuery.Create(nil);
    try

      { with QUsuario do
        begin
        Connection := fClientes.Item[ID].ConexionDB;
        Transaction := fClientes.Item[ID].TransaccionDB;

        SQL.Clear;
        SQL.Add('SELECT U.NOMBRE,S.DOMINIO FROM ' + TABLA_USUARIOS + );
        SQL.Add('ON U.CLAVE_VSERVER=S.CLAVE WHERE U.CLAVE=:PCLAVE');
        ParamByName('PCLAVE').AsInteger := fClientes.Item[ID].ID_Usuario;
        Open;
        end; }
      ChildrenName := '';
      JChildren := ARequest.GetValue('stanza-children') as TJSONObject;
      if JChildren <> nil then
      begin
        JValue := JChildren.GetValue('element-name');
        if JValue <> nil then
          ChildrenName := JValue.Value;
      end;

      if (ChildrenName = 'bind') and (fClientes.Item[ID].Estado >= sSession) then
      begin
        if wType = 'set' then
        begin
          Resource := '';
          JChild := JChildren.GetValue('element-children') as TJSONObject;
          if JChild <> nil then
          begin
            JName := JChild.GetValue('element-name');
            JValue := JChild.GetValue('element-value');
            if (JName <> nil) and (JValue <> nil) then
            begin
              if JName.Value = 'resource' then
                Resource := JValue.Value;
              fClientes.Item[ID].Recurso := Resource;
              Response := CreateBindIQResponse(wId, Nombre, dominio, Resource);
            end;
          end;
        end;
      end
      else if (ChildrenName = 'session') and (fClientes.Item[ID].Estado = sSession) then
      begin
        fClientes.Item[ID].Estado := sStanza;
        if wType = 'set' then
        begin
          fClientes.Item[ID].ID_Session := wId;
          Response := CreateSessionIQResponse(wId, ID);
        end;
      end
      else if (ChildrenName = 'query') and (fClientes.Item[ID].Estado >= sStanza) then
      begin
        { if wChild.HasAttribute('xmlns') then
          begin
          if wChild.Attributes['xmlns'] = XMLNS_ROSTER then
          begin
          if wType = 'get' then
          begin
          { with QListaContactos do
          begin
          Connection := fClientes.Item[ID].ConexionDB;
          Transaction := fClientes.Item[ID].TransaccionDB;
          SQL.Add('SELECT U.*,S.DOMINIO ' + TABLA_ROSTERS + ' L');
          SQL.Add('LEFT OUTER JOIN ' + TABLA_USUARIOS + ' U ON U.CLAVE=L.CLAVE_USUARIO');
          SQL.Add('LEFT OUTER JOIN ' + TABLA_USUARIOS + ' S ON S.CLAVE=U.CLAVE_VSERVER');
          SQL.Add('WHERE CLAVE_LISTA=(SELECT CLAVE_CONTACTOS FROM ' + TABLA_USUARIOS + ' WHERE CLAVE=:PCLAVE)');
          ParamByName('PCLAVE').AsInteger := fClientes.Item[ID].ID_Usuario;
          Open;
          if RecordCount > 0 then
          begin
          wFrom := QUsuario.FieldByName('NOMBRE').AsString + '@' + QUsuario.FieldByName('DOMINIO').AsString;
          Response := CreateRosterIQResponse(QListaContactos, wId, wFrom, wFrom, fClientes.Item[ID].Recurso, ID);
          end;
          end;
          end;
          end
          else if (wChild.Attributes['xmlns'] = XMLNS_DISCOITEMS) and (fClientes.Item[ID].Estado >= sStanza) then
          begin
          if wType = 'get' then
          Response := CreateDiscoItemsResponse(wId, ID);
          end
          else if (wChild.Attributes['xmlns'] = XMLNS_DISCOINFO) and (fClientes.Item[ID].Estado >= sStanza) then
          begin
          if wType = 'get' then
          Response := CreateDiscoInfoResponse(wId, ID);
          end
          else
          begin
          wFrom := Nombre + '@' + dominio;
          Response := CreateUnavailableServiceResponse(wId, wFrom, wFrom, fClientes.Item[ID].Recurso, ARequest, ID);
          end;
          end; }
      end
      else if (ChildrenName = 'vCard') and (fClientes.Item[ID].Estado >= sStanza) then
      begin
        if wType = 'set' then
          // ProcessVCardUpdateRequest(wChild, wId, Contexto)
        else
        begin
          Response := CreateUnavailableServiceResponse(wId, wFrom, wFrom, fClientes.Item[ID].Recurso, ARequest, ID);
        end;
      end
      else if fClientes.Item[ID].Estado >= sStanza then
      begin
        wFrom := Nombre + '@' + dominio;
        Response := CreateUnavailableServiceResponse(wId, wFrom, wFrom, fClientes.Item[ID].Recurso, ARequest, ID);
      end;
      Contexto.Connection.IOHandler.WriteLn(Response);

      Log(3, Contexto.Connection.Socket.Binding.PeerIP + ':' + IntToStr(Contexto.Connection.Socket.Binding.PeerPort), Response);
    finally
      QUsuario.Free;
      QListaContactos.Free;
    end;
  end;
end;

procedure TXMPPXml.ProcessPresenceResponse(ARequest: TJSONObject; Contexto: TIdContext);
var
  aPresencia: TPresencia;
  AResponse: string;
  JChildren: TJSONArray;
  JChildData: TJSONObject;
  JValue, JChild, JName: TJSONValue;
begin
  if (fClientes.Count < Contexto.Connection.Tag) then
    raise Exception.Create('El Cliente no esta registrado');

  if fClientes.Item[Contexto.Connection.Tag].Estado >= sStanza then
  begin
    // aPresencia.Prioridad := 0;
    // if ARequest.SelectNode('priority', wPriority) then
    // aPresencia.Prioridad := StrToInt(wPriority.Text);

    // ARequest.SelectNode('x', x);
    // ARequest.SelectNode('c', c);
    aPresencia.Visibilidad := usOnline;
    aPresencia.Status := '';
    JChildren := ARequest.GetValue('stanza-children') as TJSONArray;
    if JChildren <> nil then
    begin
      for JChild in JChildren do
      begin
        JChildData := JChild as TJSONObject;
        if JChildData <> nil then
        begin
          JName := JChildData.GetValue('element-name');
          JValue := JChildData.GetValue('element-value');
          if (JName <> nil) and (JValue <> nil) then
          begin
            if JName.Value = 'show' then
            begin
              if JValue.Value = 'chat' then
                aPresencia.Visibilidad := usFreeForChat
              else if JValue.Value = 'away' then
                aPresencia.Visibilidad := usAway // ausente
              else if JValue.Value = 'xa' then
                aPresencia.Visibilidad := usUnavailable // ausente
              else if JValue.Value = 'dnd' then
                aPresencia.Visibilidad := usDNotDistrub // ausente
            end
            else if JName.Value = 'show' then
              aPresencia.Status := JValue.Value;
          end;
        end;
      end;
      fClientes.Item[Contexto.Connection.Tag].Presencia := aPresencia;
      if aPresencia.Visibilidad in [usOnline, usFreeForChat] then
        SendOfflineMessages(Contexto);
      // AResponse := fClientes.Item[IDDest].Contexto.Connection.IOHandler.WriteLn(AResponse);
      Log(3, Contexto.Connection.Socket.Binding.PeerIP + ':' + IntToStr(Contexto.Connection.Socket.Binding.PeerPort), AResponse);
    end;
  end;
end;

procedure TXMPPXml.SendOfflineMessages(Contexto: TIdContext);
var
  AResponse, AFrom, ATo, ATipo, ABody: string;
  ADelay: TDateTime;
  QMensaje, QUsuario, QMensajeEnviado: TFDQuery;
  Transaccion: TFDTransaction;
  ID: integer;
begin
  ID := Contexto.Connection.Tag;
  QMensaje := TFDQuery.Create(nil);
  QUsuario := TFDQuery.Create(nil);
  QMensajeEnviado := TFDQuery.Create(nil);
  Transaccion := TFDTransaction.Create(nil);
  try
    Transaccion.Connection := fClientes.Item[ID].ConexionDB;
    Transaccion.Options.AutoStart := False;
    Transaccion.Options.AutoCommit := False;
    QUsuario.Connection := fClientes.Item[ID].ConexionDB;
    QUsuario.Transaction := Transaccion;
    QMensaje.Connection := fClientes.Item[ID].ConexionDB;
    QMensaje.Transaction := Transaccion;
    QMensajeEnviado.Connection := fClientes.Item[ID].ConexionDB;
    QMensajeEnviado.Transaction := Transaccion;
    Transaccion.StartTransaction;
    QUsuario.SQL.Text := 'SELECT PDA_USUARIO FROM ' + TABLA_USUARIOS + ' WHERE CLAVE=:PCLAVE';
    QMensajeEnviado.SQL.Text := 'UPDATE ' + TABLA_MENSAJES + ' SET ENVIADO=''T'' WHERE CLAVE=:PCLAVE';
    ATo := fClientes.Item[ID].Username + '@' + fClientes.Item[Contexto.Connection.Tag].VServer;
    QMensaje.SQL.Add('SELECT * FROM ' + TABLA_MENSAJES + ' WHERE CLAVE_USUARIO_DESTINO=:PCLAVE_USUARIO_DESTINO AND ENVIADO=''F''');
    QMensaje.ParamByName('PCLAVE_USUARIO_DESTINO').AsInteger := fClientes.Item[ID].ID_Usuario;
    QMensaje.Open;
    if QMensaje.RecordCount > 0 then
    begin
      try
        while not QMensaje.Eof do
        begin
          QMensajeEnviado.ParamByName('PCLAVE').AsInteger := QMensaje.FieldByName('CLAVE').AsInteger;
          QMensajeEnviado.ExecSQL;
          QUsuario.Close;
          QUsuario.ParamByName('PCLAVE').AsInteger := QMensaje.FieldByName('CLAVE_USUARIO_ORIGEN').AsInteger;
          QUsuario.Open;
          AFrom := QUsuario.FieldByName('PDA_USUARIO').AsString + '@' + QMensaje.FieldByName('DOMINIO_DESTINO').AsString;
          ATipo := QMensaje.FieldByName('TIPO').AsString;
          ABody := QMensaje.FieldByName('MENSAJE').AsString;
          ADelay := QMensaje.FieldByName('FECHA_HORA').AsDateTime;
          AResponse := AResponse + CreateMessageResponse(AFrom, ATo, ATipo, ABody, ID, ADelay, True);
          QMensaje.Next;
        end;
        Contexto.Connection.IOHandler.WriteLn(AResponse);
        Log(3, Contexto.Connection.Socket.Binding.PeerIP + ':' + IntToStr(Contexto.Connection.Socket.Binding.PeerPort), AResponse);
        Transaccion.Commit;
      except
        Transaccion.Rollback;
      end;
    end;
  finally
    QMensaje.Free;
    QUsuario.Free;
    QMensajeEnviado.Free;
    Transaccion.Free;
  end;
end;

procedure TXMPPXml.ProcessMessageResponse(ARequest: TJSONObject; Contexto: TIdContext);
var
  JAttributes, JChildren: TJSONObject;
  JValue, JName: TJSONValue;

  wTo, wType, wId, wResponse, wFrom, wMensage: String;
  UserTo, ServerTo: string;
  QUsuario, FDDominio, QMensaje: TFDQuery;
  Arroba, IDInstancia, i, ID, IDDest: integer;
begin
  ID := Contexto.Connection.Tag;
  wTo := '';
  wType := '';
  wId := '';
  wFrom := '';
  wMensage := '';
  if (fClientes.Count < ID) then
    raise Exception.Create('El Cliente no esta registrado');

  if fClientes.Item[Contexto.Connection.Tag].Estado >= sStanza then
  begin
    QMensaje := TFDQuery.Create(nil);
    QUsuario := TFDQuery.Create(nil);
    FDDominio := TFDQuery.Create(nil);
    try
      FDDominio.Connection := FDMasterCon;
      FDDominio.SQL.Text := 'SELECT * FROM ' + TABLA_DOMINIOS + ' WHERE ACTIVO = ''T'' AND NOMBRE=:PNOMBRE';
      QMensaje.Connection := fClientes.Item[ID].ConexionDB;
      QMensaje.Transaction := fClientes.Item[ID].TransaccionDB;
      if not QMensaje.Transaction.Active then
        QMensaje.Transaction.StartTransaction;

      JAttributes := ARequest.GetValue('stanza-attributes') as TJSONObject;
      if JAttributes <> nil then
      begin
        JValue := JAttributes.GetValue('type');
        if JValue <> nil then
          wType := JValue.Value;

        JValue := JAttributes.GetValue('id');
        if JValue <> nil then
          wId := JValue.Value;

        { JValue := JAttributes.GetValue('from');
          if JValue <> nil then
          wFrom := JValue.Value; }

        JValue := JAttributes.GetValue('to');
        if JValue <> nil then
          wTo := JValue.Value;
      end;
      JChildren := ARequest.GetValue('stanza-children') as TJSONObject;
      if JChildren <> nil then
      begin
        JName := JChildren.GetValue('element-name');
        JValue := JChildren.GetValue('element-value');
        if (JName <> nil) and (JValue <> nil) then
          if JName.Value = 'body' then
            wMensage := Decode64(JValue.Value);
      end;
      if (wType <> '') and (wId <> '') and (wTo <> '') and (wMensage <> '') then
      begin
        Arroba := pos('@', wTo);
        UserTo := Copy(wTo, 1, Arroba - 1);
        ServerTo := Copy(wTo, Arroba + 1, Length(wTo) - Arroba);
        // a quien va el mensage ¿Si esta conectado, autorizado y presente se le envia el mensage si no se marca pendiente
        FDDominio.ParamByName('PNOMBRE').AsString := ServerTo;
        FDDominio.Open;
        if FDDominio.RecordCount > 0 then
        begin
          IDInstancia := fInstancias.IndexOfInstancia(FDDominio.FieldByName('CLAVE_INSTANCIA').AsInteger);
          if (IDInstancia <> -1) AND (IDInstancia < fInstancias.Count) then
          begin

            QUsuario.Connection := fInstancias.Item[IDInstancia].Conexion;
            QUsuario.Transaction := fInstancias.Item[IDInstancia].Transaccion;
            QUsuario.SQL.Text := 'SELECT CLAVE FROM ' + TABLA_USUARIOS + ' WHERE CLAVE_EMPRESA=:PEMPRESA AND PDA_USUARIO=:PPDA_USUARIO';
            QUsuario.ParamByName('PEMPRESA').AsInteger := FDDominio.FieldByName('CLAVE_EMPRESA').AsInteger;
            QUsuario.ParamByName('PPDA_USUARIO').AsString := UserTo;
            QUsuario.Open;
            if QUsuario.RecordCount <> 0 then
            begin
              with QMensaje do
              begin
                SQL.Add('INSERT INTO ' + TABLA_MENSAJES + '(DOMINIO_ORIGEN,DOMINIO_DESTINO,CLAVE_USUARIO_ORIGEN,CLAVE_USUARIO_DESTINO,MENSAJE,TIPO,ENVIADO,FECHA_HORA)');
                SQL.Add(' VALUES(:DOMINIO_ORIGEN,:DOMINIO_DESTINO,:PCLAVE_USUARIO_ORIGEN,:PCLAVE_USUARIO_DESTINO,:PMENSAJE,:PTIPO,:PENVIADO,:PFECHA_HORA)');
                ParamByName('DOMINIO_ORIGEN').AsString := fClientes.Item[ID].VServer;
                ParamByName('DOMINIO_DESTINO').AsString := ServerTo;
                ParamByName('PCLAVE_USUARIO_ORIGEN').AsInteger := fClientes.Item[ID].ID_Usuario;
                ParamByName('PCLAVE_USUARIO_DESTINO').AsInteger := QUsuario.FieldByName('CLAVE').AsInteger;
                ParamByName('PMENSAJE').AsString := wMensage;
                ParamByName('PFECHA_HORA').AsDateTime := Now;
                ParamByName('PENVIADO').AsString := 'F';
                ParamByName('PTIPO').AsString := wType;
                IDDest := fClientes.IndexOfUser(QUsuario.FieldByName('CLAVE').AsInteger);
                if (IDDest <> -1) and (fClientes.Item[IDDest].Estado >= sStanza) and (fClientes.Item[IDDest].Presencia.Visibilidad in [usOnline, usFreeForChat]) then
                begin
                  wFrom := fClientes.Item[ID].Username + '@' + fClientes.Item[ID].VServer;
                  wResponse := CreateMessageResponse(wFrom, wTo, wType, wMensage, IDDest);
                  try
                    fClientes.Item[IDDest].Contexto.Connection.IOHandler.WriteLn(wResponse);
                    Log(3, fClientes.Item[IDDest].Contexto.Connection.Socket.Binding.PeerIP + ':' + IntToStr(fClientes.Item[IDDest].Contexto.Connection.Socket.Binding.PeerPort),
                      wResponse);

                  except

                  end;
                  ParamByName('PENVIADO').AsString := 'T';
                end;
                ExecSQL;
              end;
            end
            else
            begin
              // ERROR

            end;
          end;
        end;
      end;
      if QMensaje.Transaction.Active then
        QMensaje.Transaction.Commit;
    finally
      QUsuario.Free;
      QMensaje.Free;
    end;
  end;
end;

procedure TXMPPXml.ProcessVCardUpdateRequest(Vcard: TJSONObject; AId: String; Contexto: TIdContext);
var
  Imagen: TMemoryStream;
  Temp: string;
  i: integer;
  QVCardTrabajo, QVCardPersonal, QDataSet: TFDQuery;

begin
  { QVCardTrabajo := TFDQuery.Create(nil);
    QVCardPersonal := TFDQuery.Create(nil);
    Imagen := TMemoryStream.Create;
    try
    /// QVCardTrabajo.Connection := fClientes.Item[ID].ConexionDB;
    // QVCardTrabajo.Transaction := fClientes.Item[ID].ConexionDB;
    // QVCardPersonal.Connection := fClientes.Item[ID].ConexionDB;
    // QVCardPersonal.Transaction := fClientes.Item[ID].ConexionDB;
    QVCardTrabajo.SQL.Add('UPDATE OR INSERT INTO ' + TABLA_VCARD_EMPRESA +
    ' (CLAVE_USUARIO, WEB, EMAIL, TELEFONO, EMPRESA, DEPARTAMENTO, POSICION, ROL, DIRECCION_2, LOCALIDAD, DIRECCION, CPOSTAL, PAIS, PROVINCIA)');
    QVCardTrabajo.SQL.Add(' VALUES(:CLAVE_USUARIO,:WEB,:EMAIL,:TELEFONO,:EMPRESA,:DEPARTAMENTO,:POSICION,:ROL,:DIRECCION_2,:LOCALIDAD,:DIRECCION,:CPOSTAL,:PAIS,:PROVINCIA)');
    QVCardTrabajo.SQL.Add(' MATCHING(CLAVE_USUARIO)');

    QVCardPersonal.SQL.Add('UPDATE OR INSERT INTO ' + TABLA_VCARD_PERSONAL +
    '(CLAVE_USUARIO,NOMBRE,ALIAS,FAMILIA,APODO,MEDIO,PREFIJO,SUFIJO,WEB,EMAIL,TELEFONO,NACIMIENTO,DIRECCION_2,LOCALIDAD,DIRECCION,CPOSTAL,PAIS,PROVINCIA,TIPO_AVATAR,AVATAR,DESCRIPCION)');
    QVCardPersonal.SQL.Add
    (' VALUES (:CLAVE_USUARIO,:NOMBRE,:ALIAS,:FAMILIA,:APODO,:MEDIO,:PREFIJO,:SUFIJO,:WEB,:EMAIL,:TELEFONO,:NACIMIENTO,:DIRECCION_2,:LOCALIDAD,:DIRECCION,:CPOSTAL,:PAIS,:PROVINCIA,:TIPO_AVATAR,:AVATAR,:DESCRIPCION)');
    QVCardPersonal.SQL.Add(' MATCHING(CLAVE_USUARIO)');
    QVCardPersonal.ParamByName('CLAVE_USUARIO').AsInteger := fClientes.Item[Contexto.Connection.Tag].ID_Usuario;
    QVCardTrabajo.ParamByName('CLAVE_USUARIO').AsInteger := fClientes.Item[Contexto.Connection.Tag].ID_Usuario;
    for i := 0 to Vcard.ChildCount - 1 do
    begin
    if Vcard.ChildNodes[i].NodeName = 'TEL' then
    begin
    if Vcard.ChildNodes[i].SelectNode('HOME') <> nil then
    QDataSet := QVCardPersonal
    else
    QDataSet := QVCardTrabajo;
    with QDataSet do
    begin
    if Vcard.ChildNodes[i].SelectNode('NUMBER', ValNode) then
    ParamByName('TELEFONO').AsString := ValNode.Text
    end;
    end
    else if Vcard.ChildNodes[i].NodeName = 'TITLE' then
    QVCardTrabajo.ParamByName('POSICION').AsString := Vcard.ChildNodes[i].NodeValue
    else if Vcard.ChildNodes[i].NodeName = 'URL' then
    QVCardPersonal.ParamByName('WEB').AsString := Vcard.ChildNodes[i].NodeValue
    else if Vcard.ChildNodes[i].NodeName = 'PHOTO' then
    begin
    if Vcard.ChildNodes[i].SelectNode('TYPE', ValNode) then
    QVCardPersonal.ParamByName('TIPO_AVATAR').AsString := ValNode.Text;
    if Vcard.ChildNodes[i].SelectNode('BINVAL', ValNode) then
    begin
    // Imagen.Write(ValNode.Text     )

    // QVCardPersonal.ParamByName('AVATAR').LoadFromStream();
    end;
    end
    else if Vcard.ChildNodes[i].NodeName = 'N' then
    begin
    // if Vcard.ChildNodes[i].SelectNode('MIDDLE', ValNode) then

    end
    else if Vcard.ChildNodes[i].NodeName = 'ROLE' then
    QVCardTrabajo.ParamByName('ROL').AsString := Vcard.ChildNodes[i].NodeValue
    else if Vcard.ChildNodes[i].NodeName = 'ORG' then
    begin
    if Vcard.ChildNodes[i].SelectNode('ORGNAME', ValNode) then
    QVCardTrabajo.ParamByName('EMPRESA').AsString := ValNode.Text;
    if Vcard.ChildNodes[i].SelectNode('ORGUNIT', ValNode) then
    QVCardTrabajo.ParamByName('DEPARTAMENTO').AsString := ValNode.Text;
    end
    else if Vcard.ChildNodes[i].NodeName = 'ADR' then
    begin
    if Vcard.ChildNodes[i].SelectNode('HOME') <> nil then
    QDataSet := QVCardPersonal
    else
    QDataSet := QVCardTrabajo;
    with QDataSet do
    begin
    if Vcard.ChildNodes[i].SelectNode('EXTADR', ValNode) then
    ParamByName('DIRECCION').AsString := ValNode.Text;
    if Vcard.ChildNodes[i].SelectNode('LOCALITY', ValNode) then
    ParamByName('LOCALIDAD').AsString := ValNode.Text;
    if Vcard.ChildNodes[i].SelectNode('REGION', ValNode) then
    ParamByName('PROVINCIA').AsString := ValNode.Text;
    if Vcard.ChildNodes[i].SelectNode('CTRY', ValNode) then
    ParamByName('PAIS').AsString := ValNode.Text;
    if Vcard.ChildNodes[i].SelectNode('PCODE', ValNode) then
    ParamByName('CPOSTAL').AsString := ValNode.Text;
    if Vcard.ChildNodes[i].SelectNode('STREET', ValNode) then
    ParamByName('DIRECCION_2').AsString := ValNode.Text;
    end;
    end
    else if Vcard.ChildNodes[i].NodeName = 'NICKNAME' then
    QVCardPersonal.ParamByName('ALIAS').AsString := Vcard.ChildNodes[i].NodeValue
    else if Vcard.ChildNodes[i].NodeName = 'EMAIL' then
    begin
    if Vcard.ChildNodes[i].SelectNode('HOME') <> nil then
    QDataSet := QVCardPersonal
    else
    QDataSet := QVCardTrabajo;
    with QDataSet do
    begin
    if Vcard.ChildNodes[i].SelectNode('USERID', ValNode) then
    ParamByName('EMAIL').AsString := ValNode.Text
    end;
    end
    else if Vcard.ChildNodes[i].NodeName = 'FN' then
    QVCardPersonal.ParamByName('NOMBRE').AsString := Vcard.ChildNodes[i].NodeValue;
    end;
    try
    QVCardPersonal.ExecSQL;
    QVCardTrabajo.ExecSQL;
    with fClientes.Item[Contexto.Connection.Tag] do
    Contexto.Connection.IOHandler.WriteLn('<iq id="' + AId + '" to="' + Username + '@' + VServer + '/' + Recurso + '" type="result"/>');
    Log(#13#10 + 'Sent to: ' + Contexto.Connection.Socket.Binding.PeerIP + 'Thru: ' + IntToStr(Contexto.Connection.Socket.Binding.PeerPort) + '->' + #13#10 + Response);
    except
    on E: Exception do
    Temp := E.Message;
    end;

    finally
    Imagen.Free;
    end; }
end;

procedure TXMPPXml.ProcessUnavailableServiceResponse(ARequest: TJSONObject; Contexto: TIdContext);
begin
  //
end;

procedure TXMPPXml.ProcessFaillureResponse(AResponse: TJSONObject);
begin
  if assigned(fOnFaillure) then
  begin
    { if AResponse.HasChildNodes then
      fOnFaillure(AResponse.ChildNodes[0].NodeName)
      else
      fOnFaillure(AResponse.NodeName); }
  end;
end;

end.
