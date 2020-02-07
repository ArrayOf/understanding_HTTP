unit Main;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.SyncObjs,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.Imaging.pngimage,
  Vcl.ExtCtrls,
  IdBaseComponent,
  IdComponent,
  IdCustomTCPServer,
  IdTCPServer,
  IdContext,
  Vcl.Samples.Spin;

type

  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    MemoRequisicao: TMemo;
    MemoResposta: TMemo;
    Image1: TImage;
    Button1: TButton;
    IdTCPServer1: TIdTCPServer;
    Panel7: TPanel;
    Button2: TButton;
    MemoLog: TMemo;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
  private
    { Private declarations }
    FEvent   : TEvent;
    FCritical: TCriticalSection;
    procedure Log(const ALine: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  IdIOHandler,
  IdGlobal;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
{
  Inicialização do servidor TCP/IP propriamente dito utilizando a porta
  especificada
}
begin
  Self.IdTCPServer1.Active := True;
  Self.Button1.Enabled     := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
{
  Aqui liberamos a aplicação para responder ao cliente com o conteúdo do
  Memo de resposta
}
begin
  Self.FEvent.SetEvent;
  Self.FEvent.ResetEvent;
end;

procedure TForm1.FormCreate(Sender: TObject);
{
  Criação dos objetos quando da inicialização da aplicação
}
begin
  ReportMemoryLeaksOnShutdown := True;

  Self.FEvent    := TEvent.Create(nil, True, False, 'sinalizador');
  Self.FCritical := TCriticalSection.Create;

  Self.MemoLog.Clear;
  Self.MemoRequisicao.Clear;
  Self.MemoResposta.Clear;
end;

procedure TForm1.FormDestroy(Sender: TObject);
{
  Liberação dos objetos quando do encerramento da aplicação
}
begin
  Self.FEvent.Free;
  Self.FCritical.Free;
end;

procedure TForm1.IdTCPServer1Connect(AContext: TIdContext);
{
  O evento `Connect` ocorre quando da conexão de um cliente ao nosso servidor
}
begin
  Self.Log('CONECTOU');
end;

procedure TForm1.IdTCPServer1Disconnect(AContext: TIdContext);
{
  O evento `Disconnect` ocorrer quando da desconexão seja por parte do cliente,
  seja por parte do servidor
}
begin
  Self.Log('DESCONECTOU');
end;

procedure TForm1.IdTCPServer1Execute(AContext: TIdContext);
{
  O evento `Execute` da classe `TidTCPServer` é acionado logo após a conexão.

  IMPORTANTE:
  ==========

  Aqui estamos em um contexto de thread portanto todos os cuidados
  relacionados à thread são necessários.

  É um código didático por isso abriu-se mão de alguns desses cuidados.

  * Assumimos que o conteúdo trafegado é sempre texto e em UTF-8
}
var
  oHandler      : TIdIOHandler;
  sLine         : string;
  iContentLength: Cardinal;
  iPos          : Integer;
  sContent      : string;
begin
  // Entrando em uma seção crítica para tratar cada conexão a uma só vez
  Self.FCritical.Enter;

  // Sinalização visual de qual requisição está sendo estudada
  Self.Log('EXECUTANDO');
  Self.Panel5.Caption := Format('THREAD #%d', [GetCurrentThreadId]);
  Self.Panel6.Color   := clRed;

  // Inicializando
  oHandler       := AContext.Connection.IOHandler;
  iContentLength := 0;
  Self.MemoRequisicao.Clear;

  try
    // Recuperando o cabeçalho da requisição HTTP
    repeat
      // O `ReadLn` recupera o conteúdo até encontrar uma quebra de linha
      sLine := oHandler.ReadLn();

      // Alimenta o Memo referente à requisição
      Self.MemoRequisicao.Lines.Add(sLine);

      // Recuperando o tamanho do conteúdo da mensagem
      sLine := LowerCase(sLine);
      iPos  := Pos('content-length', sLine);
      if iPos > 0 then
      begin
        iContentLength := StrToInt(Trim(Copy(sLine, Pos(':', sLine) + 1, MaxInt)));
      end;
    until (sLine = EmptyStr);

    // Recupera o conteúdo - caso haja
    if iContentLength > 0 then
    begin
      sContent := oHandler.ReadString(iContentLength, IndyTextEncoding_UTF8);
      Self.MemoRequisicao.Lines.Add(sContent);
    end;

    // Suspende a thread aguardando o envio da resposta
    Self.FEvent.WaitFor(INFINITE);

    // Escreve, linha a linha, o conteúdo da resposta
    for sLine in Self.MemoResposta.Lines do
    begin
      oHandler.WriteLn(sLine, IndyTextEncoding_UTF8);
    end;

  finally
    // Encerra a conexão TCP/IP
    oHandler.Close;

    // Sinalização visual de que encerrou o processamento
    Self.Panel5.Caption := 'REQUISIÇÃO';
    Self.Panel6.Color   := clBtnFace;

    // Libera a seção crítica permitindo o tratamento da próxima requisição
    Self.FCritical.Release;
  end;
end;

procedure TForm1.Log(const ALine: string);
{
  Geração de um log simples dos acontecimentos relevantes
}
var
  sLog: string;
  iTID: Cardinal;
begin
  iTID := GetCurrentThreadId;
  sLog := Format('%d - %s', [iTID, ALine]);
  Self.MemoLog.Lines.Insert(0, sLog);
end;

end.
