unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdBaseComponent, IdComponent,
  IdCustomTCPServer, IdTCPServer, Vcl.StdCtrls, Vcl.Imaging.pngimage,
  Vcl.ExtCtrls, IdContext, System.SyncObjs;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Memo1: TMemo;
    Memo2: TMemo;
    Image1: TImage;
    Button1: TButton;
    IdTCPServer1: TIdTCPServer;
    Panel7: TPanel;
    Button2: TButton;
    Memo3: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
  private
    { Private declarations }
    FEvent: TEvent;
    procedure Log(const ALine: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  IdIOHandler, IdGlobal;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Self.IdTCPServer1.Active := True;
  Self.Button1.Enabled     := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Self.FEvent.SetEvent;
  Self.FEvent.ResetEvent;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  Self.FEvent := TEvent.Create(nil, True, False, 'sinalizado');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Self.FEvent.Free;
end;

procedure TForm1.IdTCPServer1Connect(AContext: TIdContext);
begin
  Self.Log('CONECTOU');
end;

procedure TForm1.IdTCPServer1Disconnect(AContext: TIdContext);
begin
  Self.Log('DESCONECTOU');
end;

procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
var
  oHandler : TIdIOHandler;
  slPayload: TStringList;
  sLine    : string;
begin
  Self.Log('EXECUTANDO');

  slPayload := TStringList.Create;
  oHandler  := AContext.Connection.IOHandler;

  try
    repeat
      sLine := oHandler.ReadLn();
      slPayload.Add(sLine);
    until (sLine = EmptyStr);

    Self.Memo1.Text   := slPayload.Text;
    Self.Panel6.Color := clRed;

    Self.FEvent.WaitFor(INFINITE);

    for sLine in Self.Memo2.Lines do
    begin
      oHandler.WriteLn(sLine, IndyTextEncoding_UTF8);
    end;
  finally
    Self.Panel6.Color := clBtnFace;
    slPayload.Free;
    oHandler.Close;
  end;
end;

procedure TForm1.Log(const ALine: string);
var
  sLog: string;
  iTID: Cardinal;
begin
  iTID := GetCurrentThreadId;
  sLog := Format('%d - %s', [iTID, ALine]);
  Self.Memo3.Lines.Insert(0, sLog);
end;

end.
