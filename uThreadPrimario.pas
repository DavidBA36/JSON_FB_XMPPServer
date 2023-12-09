unit uThreadPrimario;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Data.DB, Vcl.Graphics, uXMLServerParser, System.RegularExpressions,
  System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdContext, Vcl.StdCtrls, IdCustomHTTPServer, IdHTTPServer, IdScheduler,
  IdSchedulerOfThread, IdSchedulerOfThreadPool, IdThreadComponent,
  IdServerIOHandler, IdSSL, IdSSLOpenSSL, IdGlobal, IdSASL, IdSASLUserPass, IdSASLDigest, IdSASL_CRAM_MD5, IdSASL_CRAMBase, IdSASL_CRAM_SHA1,
  uXMPPServerHelper, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.FB, FireDAC.Phys.FBDef, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.VCLUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.IBBase, FireDAC.Comp.Client, FireDAC.Comp.DataSet, IdTCPConnection, IdTCPClient,
  IdExplicitTLSClientServerBase, IdMessageClient, IdSMTPBase, IdSMTP,
  IdCmdTCPServer, IdSMTPServer, FDGenerator, IdSASLAnonymous, System.JSON, IdIntercept, IdLogBase, IdLogFile, JvScheduledEvents, IdServerInterceptLogBase, IdServerInterceptLogFile;

type

  TForm1 = class(TForm)
    XMPPServer: TIdTCPServer;
    TLS: TIdServerIOHandlerSSLOpenSSL;
    Scheduler: TIdSchedulerOfThreadPool;
    FDPhysFBDriverLink1: TFDPhysFBDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Button1: TButton;
    FDMasterCon: TFDConnection;
    JvScheduledEvents1: TJvScheduledEvents;
    LogFile: TIdServerInterceptLogFile;
    procedure XMPPServerExecute(AContext: TIdContext);
    procedure FormCreate(Sender: TObject);
    procedure XMPPServerConnect(AContext: TIdContext);
    procedure XMPPServerDisconnect(AContext: TIdContext);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure JvScheduledEvents1Events0Execute(Sender: TJvEventCollectionItem; const IsSnoozeEvent: Boolean);
  private
    ListaClientesOnline: TListaClientes;
    ListaInstancias: TListaInstancias;
    XmlParser: TXMPPXml;
    procedure OnXMPPLog(Tipo: Integer; OD: String; AMessage: string);
    procedure ConectarInstancias;
    procedure NotifyAllClients;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
  L: TList;
  Context: TIdContext;
begin
  NotifyAllClients;
  L := XMPPServer.Contexts.LockList;
  if XMPPServer.Active then
    with L do
      try
        for i := Count - 1 downto 0 do
        begin
          Context := Items[i];
          if Context = nil then
            Continue;
          Context.Connection.IOHandler.WriteBufferClear;
          Context.Connection.IOHandler.InputBuffer.Clear;
          Context.Connection.IOHandler.Close;
          if Context.Connection.Connected then
            Context.Connection.Disconnect;
        end;
      finally
        XMPPServer.Contexts.UnlockList;
      end;

  if XMPPServer.Active then
    XMPPServer.Active := False;

  FDMasterCon.Connected := False;
  XmlParser.Free;
  ListaClientesOnline.Free;
  for i := ListaInstancias.Count - 1 downto 0 do
  begin
    if Assigned(ListaInstancias.Item[i].Transaccion) then
      ListaInstancias.Item[i].Transaccion.Free;
    if Assigned(ListaInstancias.Item[i].Conexion) then
      ListaInstancias.Item[i].Conexion.Free;
  end;
  ListaInstancias.Free;
end;

procedure TForm1.NotifyAllClients;
var
  i: Integer;
  JStanza: TJSONObject;
begin
  JStanza := TJSONObject.Create;
  try
    JStanza.AddPair('stanza-name', TJSONString.Create('presence'));
    JStanza.AddPair('stanza-attributes', TJSONObject.Create(TJSONPair.Create('type', TJSONString.Create('unavailable'))));
    for i := 0 to ListaClientesOnline.Count - 1 do
      ListaClientesOnline.Item[i].Contexto.Connection.IOHandler.WriteLn(JStanza.ToString);
  finally
    JStanza.Free;
  end;
end;

procedure TForm1.OnXMPPLog(Tipo: Integer; OD: String; AMessage: string);
begin
  if (Tipo >= 0) and (Tipo <= 1) then
    // LogFile.DoLogWriteString(#13#10 + DateTimeToStr(Now) + OD + AMessage);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  RutaLog: string;
begin
  RutaLog := ExtractFilePath(ParamStr(0)) + 'Logs\';
  if not DirectoryExists(RutaLog) then
    ForceDirectories(RutaLog);
  LogFile.Filename := RutaLog + 'XMPPServer' + FormatDateTime('YYYYMMDD', date) + '.log';
  FDMasterCon.Connected := True;
  XMPPServer.Active := True;
  ListaClientesOnline := TListaClientes.Create(TCliente);
  ListaInstancias := TListaInstancias.Create(TInstancia);
  ReportMemoryLeaksOnShutdown := True;
  XmlParser := TXMPPXml.Create(FDMasterCon, ListaInstancias, ListaClientesOnline);
  XmlParser.OnDebug := OnXMPPLog;
  ConectarInstancias;
end;

procedure TForm1.JvScheduledEvents1Events0Execute(Sender: TJvEventCollectionItem; const IsSnoozeEvent: Boolean);
var
  RutaLog: string;
begin
  RutaLog := ExtractFilePath(ParamStr(0)) + 'Logs\';
  if not DirectoryExists(RutaLog) then
    ForceDirectories(RutaLog);
  if XMPPServer.Active then
    XMPPServer.Active := False;
  LogFile.Filename := RutaLog + 'XMPPServer' + FormatDateTime('YYYYMMDD', date) + '.log';
  if not XMPPServer.Active then
    XMPPServer.Active := True;
end;

procedure TForm1.XMPPServerConnect(AContext: TIdContext);
var
  i: Integer;
  Found: Boolean;
begin
  Found := False;
  for i := 0 to ListaClientesOnline.Count - 1 do
  begin
    if ListaClientesOnline.Item[i].Disconected then
    begin
      ListaClientesOnline.Item[i].Disconected := False;
      AContext.Connection.Tag := i;
      Found := True;
      Break;
    end;
  end;
  if not Found then
  begin
    AContext.Connection.Tag := ListaClientesOnline.Count;
    ListaClientesOnline.Add;
  end;
  ListaClientesOnline.Item[AContext.Connection.Tag].Estado := sNone;
  ListaClientesOnline.Item[AContext.Connection.Tag].Contexto := AContext;
  // LogFile.DoLogWriteString(OD + AMessage);
  // Memo1.Lines.Add('Connected Client IP:' + AContext.Binding.PeerIP + ' ID:' + IntToStr(AContext.Connection.Tag) + ' Socket port: ' + IntToStr(AContext.Binding.PeerPort));
end;

procedure TForm1.XMPPServerDisconnect(AContext: TIdContext);
begin
  if Assigned(ListaClientesOnline) and (ListaClientesOnline.Count > 0) then
    ListaClientesOnline.Item[AContext.Connection.Tag].Disconected := True;
  // if Assigned(Memo1) then
  // Memo1.Lines.Add('Disconnected Client IP:' + AContext.Binding.PeerIP + ' ID:' + IntToStr(AContext.Connection.Tag) + ' Socket port: ' + IntToStr(AContext.Binding.PeerPort));
end;

procedure TForm1.XMPPServerExecute(AContext: TIdContext);
begin
  if not AContext.Connection.IOHandler.InputBufferIsEmpty then
    XmlParser.ParseServerResponse(AContext.Connection.IOHandler.InputBufferAsString(IndyTextEncoding_UTF8), AContext);
  sleep(10);
end;

procedure TForm1.ConectarInstancias;
var
  FDInstancias: TFDQuery;
  FDRemoteCon: TFDConnection;
  FDRemoteTrans: TFDTransaction;
begin
  FDInstancias := TFDQuery.Create(nil);
  try
    FDInstancias.Connection := FDMasterCon;
    FDInstancias.SQL.Text := 'SELECT * FROM ' + TABLA_INSTANCIAS + ' WHERE ACTIVO = ''T''';
    try
      FDInstancias.Open;
      while not FDInstancias.Eof do
      begin
        FDRemoteCon := TFDConnection.Create(nil);
        FDRemoteTrans := TFDTransaction.Create(nil);
        with FDRemoteCon.Params do
        begin
          Values['Database'] := FDInstancias.FieldByName('FICHERO').AsString;
          Values['User_Name'] := FDInstancias.FieldByName('USUARIO').AsString;
          Values['Password'] := FDInstancias.FieldByName('PASS').AsString;
          Values['Protocol'] := 'TCPIP';
          Values['Server'] := FDInstancias.FieldByName('SERVIDOR').AsString;
          Values['CharacterSet'] := 'WIN1252';
          Values['DriverID'] := 'FB';
        end;
        FDRemoteCon.Connected := True;
        FDRemoteTrans.Connection := FDRemoteCon;
        FDRemoteTrans.Options.AutoStart := False;
        FDRemoteTrans.Options.AutoStop := False;
        FDRemoteTrans.Options.DisconnectAction := xdCommit;
        ListaInstancias.Add;
        ListaInstancias.Item[ListaInstancias.Count - 1].ClaveInstancia := FDInstancias.FieldByName('CLAVE').AsInteger;
        ListaInstancias.Item[ListaInstancias.Count - 1].Conexion := FDRemoteCon;
        ListaInstancias.Item[ListaInstancias.Count - 1].Transaccion := FDRemoteTrans;

        // LogFile.DoLogWriteString(#13#10 + DateTimeToStr(Now) + 'Conexion a base de datos ' + FDInstancias.FieldByName('NOMBRE').AsString + ' Realizada con exito');
        FDInstancias.Next;
      end;
    except
      on E: Exception do
        // LogFile.DoLogWriteString(#13#10 + DateTimeToStr(Now) + 'ConexionInstanciasException' + E.Message);
    end;
  finally
    FDInstancias.Free;
  end;
end;

end.
