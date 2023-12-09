unit XMPPServerSASLCrypt;

interface

uses system.Classes, uXMPPServerHelper, system.SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Param, IdUserPassProvider, IdSASLDigest, IdCoderMIME, IdGlobal, IdHashMessageDigest,
  system.IOUtils;

function GetSASLResponse(AStr: string; AUsername, APassword, AServer: string): string;
function CreateSASLChallenge(Mecanismo: TMecanismoSASL): string;
function CheckSASLDigestMD5(Challenge: String; FDConn: TFDConnection; FDTrans: TFDTransaction; var Clientes: TListaClientes; Cliente: integer; var RspAuth: String): integer;
function Encode64(const S: string; const ByteEncoding: IIdTextEncoding = nil): string;
function Decode64(const S: string; const ByteEncoding: IIdTextEncoding = nil): string;

implementation

function Encode64(const S: string; const ByteEncoding: IIdTextEncoding = nil): string;
begin
  result := TIdEncoderMIME.EncodeString(S, ByteEncoding);
end;

function Decode64(const S: string; const ByteEncoding: IIdTextEncoding = nil): string;
begin
  result := TIdDecoderMIME.DecodeString(S, ByteEncoding);
end;

function HashResult(const AStr: String): TIdBytes;
{$IFDEF USE_INLINE} inline; {$ENDIF}
var
  LMD5: TIdHashMessageDigest5;
begin
  LMD5 := TIdHashMessageDigest5.Create;
  try
    result := LMD5.HashString(AStr);
  finally
    LMD5.Free;
  end;
end;

function HashResultAsHex(const ABytes: TIdBytes): String; overload;
{$IFDEF USE_INLINE} inline; {$ENDIF}
var
  LMD5: TIdHashMessageDigest5;
begin
  LMD5 := TIdHashMessageDigest5.Create;
  try
    result := LowerCase(LMD5.HashBytesAsHex(ABytes));
  finally
    LMD5.Free;
  end;
end;

function HashResultAsHex(const AStr: String): String; overload;
{$IFDEF USE_INLINE} inline; {$ENDIF}
var
  LMD5: TIdHashMessageDigest5;
begin
  LMD5 := TIdHashMessageDigest5.Create;
  try
    result := LowerCase(LMD5.HashStringAsHex(AStr));
  finally
    LMD5.Free;
  end;
end;

function NCToStr(const AValue: integer): String;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  result := IntToHex(AValue, 8);
end;

function GetSASLResponse(AStr: string; AUsername, APassword, AServer: string): string;
var
  UserPassProvider: TIdUserPassProvider;
  SASLDigest: TIdSASLDigest;
  UnEncodedChallenge: String;
  AuthString: string;
begin
  SASLDigest := TIdSASLDigest.Create(nil);
  UserPassProvider := TIdUserPassProvider.Create(nil);
  SASLDigest.UserPassProvider := UserPassProvider;
  try
    UnEncodedChallenge := Decode64(AStr);
    SASLDigest.UserPassProvider.Username := AUsername;
    SASLDigest.UserPassProvider.Password := APassword;
    AuthString := SASLDigest.StartAuthenticate(UnEncodedChallenge, AServer, 'xmpp');

    result := Encode64(AuthString);
  finally
    SASLDigest.Free;
    UserPassProvider.Free;
  end;
end;

function CreateSASLChallenge(Mecanismo: TMecanismoSASL): string;
var
  Challenge: string;
begin

  Challenge := 'nonce="' + GenRandomSeq(20) + '",qop="auth",charset=utf-8,algorithm=md5-sess';
  result := Encode64(Challenge);
end;

function Unquote(var S: String): String;
{$IFDEF USE_INLINE} inline; {$ENDIF}
var
  I, Len: integer;
begin
  Len := Length(S);
  I := 2; // skip first quote
  while I <= Len do
  begin
    if S[I] = '"' then
    begin
      Break;
    end;
    if S[I] = '\' then
    begin
      Inc(I);
    end;
    Inc(I);
  end;
  result := Copy(S, 2, I - 2);
  S := Copy(S, I + 1, MaxInt);
end;

procedure ChallengeToSL(Challenge: String; var LChallange: TStringList);
var
  LBuf, LValue, LName: String;
begin
  LChallange.Clear;
  LBuf := Decode64(Challenge);
  while Length(LBuf) > 0 do
  begin
    LName := Trim(Fetch(LBuf, '=')); { do not localize }
    LBuf := TrimLeft(LBuf);
    if TextStartsWith(LBuf, '"') then
    begin { do not localize }
      LValue := Unquote(LBuf); { do not localize }
      Fetch(LBuf, ','); { do not localize }
    end
    else
    begin
      LValue := Trim(Fetch(LBuf, ','));
    end;
    LChallange.Add(LName + '=' + LValue);
    LBuf := TrimLeft(LBuf);
  end;
end;

function CalcRspAuth(const AUsername, APassword, ARealm, ANonce, ACNonce: String; const ANC: integer; const AQop, ADigestURI: String; const AAuthzid: String = ''): String;
var
  LA1: TIdBytes;
  LA2: TIdBytes;
  LA1_P: TIdBytes;
begin
  LA1_P := IdGlobal.ToBytes(':' + ANonce + ':' + ACNonce);
  LA1 := HashResult(AUsername + ':' + ARealm + ':' + APassword);
  IdGlobal.AppendBytes(LA1, LA1_P);
  If AAuthzid <> '' then
  begin
    IdGlobal.AppendBytes(LA1, IdGlobal.ToBytes(AAuthzid));
  end;
  if AQop = 'auth' then
  begin
    LA2 := ToBytes(':' + ADigestURI);
  end
  else if (AQop = 'auth-int') or (AQop = 'auth-conf') then
  begin
    LA2 := ToBytes(':' + ADigestURI + ':00000000000000000000000000000000');
  end
  else
  begin
    SetLength(LA2, 0);
  end;
  result := HashResultAsHex(HashResultAsHex(LA1) + ':' + ANonce + ':' + NCToStr(ANC) + ':' + ACNonce + ':' + AQop + ':' + HashResultAsHex(LA2));
end;

function CheckSASLDigestMD5(Challenge: String; FDConn: TFDConnection; FDTrans: TFDTransaction; var Clientes: TListaClientes; Cliente: integer; var RspAuth: String): integer;
var
  AName, ANonce, ARealm, ACNonce, AResponse, AQop, ADigestURI, APassword: string;
  LChallange: TStringList;
  CalcResponse: String;
  QUsuario: TFDQuery;
  Aerror: string;
begin
  if (Clientes.Count < Cliente) then
    raise Exception.Create('El Cliente no esta registrado');

  result := -1;
  LChallange := TStringList.Create;
  QUsuario := TFDQuery.Create(nil);
  try

    try

      QUsuario.Connection := FDConn;
      QUsuario.Transaction := FDTrans;

      if not QUsuario.Transaction.Active then
        QUsuario.Transaction.StartTransaction;

      ChallengeToSL(Challenge, LChallange);

      AName := LChallange.Values['username'];
      ARealm := LChallange.Values['realm'];
      ACNonce := LChallange.Values['cnonce'];
      AQop := LChallange.Values['qop'];
      ADigestURI := LChallange.Values['digest-uri'];
      ANonce := LChallange.Values['nonce'];
      AResponse := LChallange.Values['response'];
      QUsuario.SQL.Text := 'SELECT CLAVE,PDA_PASS FROM ' + TABLA_USUARIOS + ' WHERE PDA_USUARIO=:PPDA_USUARIO AND CLAVE_EMPRESA=:PCLAVE_EMPRESA';
      QUsuario.ParamByName('PPDA_USUARIO').AsString := AName;
      QUsuario.ParamByName('PCLAVE_EMPRESA').AsInteger := Clientes.Item[Cliente].ClaveEmpresa;
      QUsuario.Open;
      if QUsuario.RecordCount = 0 then
        Exit;
      APassword := QUsuario.FieldByName('PDA_PASS').AsString;
      // APassword :=Desmontar(QUsuario.FieldByName('PPASO').AsString);
      CalcResponse := CalcDigestResponse(AName, APassword, ARealm, ANonce, ACNonce, 1, AQop, ADigestURI, '');
      if AResponse <> CalcResponse then
        Exit;

      RspAuth := CalcRspAuth(AName, APassword, ARealm, ANonce, ACNonce, 1, AQop, ADigestURI);
      Clientes.Item[Cliente].ID_Usuario := QUsuario.FieldByName('CLAVE').AsInteger;
      Clientes.Item[Cliente].Username := AName;
      result := 0;
      if QUsuario.Transaction.Active then
        QUsuario.Transaction.commit;
    except
      on E: Exception do
      begin
        Aerror := E.Message;
        if QUsuario.Transaction.Active then
          QUsuario.Transaction.rollback;
      end;
    end;
  finally
    LChallange.Free;
    QUsuario.Free;
  end;
end;

end.
