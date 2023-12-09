object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 408
  ClientWidth = 780
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 288
    Top = 304
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
  end
  object XMPPServer: TIdTCPServer
    Bindings = <>
    DefaultPort = 5222
    Intercept = LogFile
    MaxConnections = 500
    OnConnect = XMPPServerConnect
    OnDisconnect = XMPPServerDisconnect
    ReuseSocket = rsTrue
    Scheduler = Scheduler
    OnExecute = XMPPServerExecute
    Left = 176
    Top = 56
  end
  object TLS: TIdServerIOHandlerSSLOpenSSL
    SSLOptions.Mode = sslmUnassigned
    SSLOptions.VerifyMode = []
    SSLOptions.VerifyDepth = 0
    Left = 120
    Top = 57
  end
  object Scheduler: TIdSchedulerOfThreadPool
    MaxThreads = 10
    Left = 64
    Top = 56
  end
  object FDPhysFBDriverLink1: TFDPhysFBDriverLink
    VendorHome = 'C:\Program Files (x86)\Firebird\Firebird_2_5\'
    VendorLib = 'fbclient.dll'
    Left = 688
    Top = 88
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'Forms'
    Left = 688
    Top = 32
  end
  object FDMasterCon: TFDConnection
    Params.Strings = (
      'Database=C:\Datos\GVena.Trio.FDB'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'Protocol=TCPIP'
      'Server=localhost'
      'CharacterSet=WIN1252'
      'DriverID=FB')
    Left = 464
    Top = 56
  end
  object JvScheduledEvents1: TJvScheduledEvents
    AutoSave = False
    Events = <
      item
        Name = 'Event1'
        OnExecute = JvScheduledEvents1Events0Execute
        StartDate = '2016/04/27 18:13:51.000'
        RecurringType = srkDaily
        EndType = sekNone
        Freq_StartTime = 0
        Freq_EndTime = 0
        Freq_Interval = 1
        Daily_EveryWeekDay = True
      end>
    Left = 352
    Top = 56
  end
  object LogFile: TIdServerInterceptLogFile
    Filename = 'log'
    Left = 240
    Top = 56
  end
end
