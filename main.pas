unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs, ComCtrls, ExtCtrls,
  ATSynEdit,
  raylib;

type
  // настройки потока Raylib
  TRaylibThreadSettings = record
    // Признак что настройки позиции были изменены
    IsPositionChanged: boolean;
    // Признак что настройки размера были изменены
    IsSizeChanged: boolean;
    // Признак что текст шейдера изменился
    IsShaderChanged: boolean;

    // Позиция
    Top, Left: integer;
    // Размеры
    Width, Height: integer;
    // Текст шейдера
    ShaderText: string;
  end;

  { TRaylibThread }

  TRaylibThread = class(TThread)
  private
    FShader: TShader;
    FWidthLoc, FHeightLoc, FSecondsLoc: integer;
    FScreenWidth, FScreenHeight: single;
    FSeconds: single;
    FSettings: TRaylibThreadSettings;
    // Применяет настройки
    procedure UpdateSettings();
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;

  { TTinterForm }

  TTinterForm = class(TForm)
    ShaderCodeEditor: TATSynEdit;
    MainImageList: TImageList;
    MainToolbar: TToolBar;
    NewShaderButton: TToolButton;
    OpenShaderButton: TToolButton;
    UpdateSettingsTimer: TTimer;
    UpdateShaderButton: TToolButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NewShaderButtonClick(Sender: TObject);
    procedure OpenShaderButtonClick(Sender: TObject);
    // Обработка таймера обновления настроек
    procedure UpdateSettingsTimerTimer(Sender: TObject);
    // Обрабатывает кнопку обновить шейдер
    procedure UpdateShaderButtonClick(Sender: TObject);
  private
  public

  end;

var
  TinterForm: TTinterForm;
  gRaylibThread: TRaylibThread;

implementation

{$R *.lfm}

{ TTinterForm }

procedure TTinterForm.FormCreate(Sender: TObject);
begin
  gRaylibThread := TRaylibThread.Create(True);
end;

procedure TTinterForm.NewShaderButtonClick(Sender: TObject);
begin

end;

procedure TTinterForm.OpenShaderButtonClick(Sender: TObject);
begin

end;

procedure TTinterForm.UpdateShaderButtonClick(Sender: TObject);
begin
  gRaylibThread.FSettings.ShaderText := string(ShaderCodeEditor.Text);
  gRaylibThread.FSettings.IsShaderChanged := True;
end;

procedure TTinterForm.UpdateSettingsTimerTimer(Sender: TObject);
const
  BOTTOM_PADDING = 26;
var
  rwidth, rheight, rtop, rleft: integer;
  rposchanged, rsizechanged: boolean;
begin
  rposchanged := False;
  rsizechanged := False;
  rwidth := trunc(self.Width / 2.2);
  rheight := trunc(self.Height / 2.2);

  if rwidth > rheight then
  begin
    rheight := rwidth;
  end;

  if rheight > Height then
  begin
    rheight := Height;
  end;

  rtop := (self.Top + self.Height) - rheight + BOTTOM_PADDING;
  rleft := (self.Left + self.Width) - rwidth;

  if gRaylibThread.FSettings.Top <> rtop then
  begin
    gRaylibThread.FSettings.Top := rtop;
    rposchanged := True;
  end;

  if gRaylibThread.FSettings.Left <> rleft then
  begin
    gRaylibThread.FSettings.Left := rleft;
    rposchanged := True;
  end;

  gRaylibThread.FSettings.IsPositionChanged := rposchanged;

  if gRaylibThread.FSettings.Width <> rwidth then
  begin
    gRaylibThread.FSettings.Width := rwidth;
    rsizechanged := True;
  end;

  if gRaylibThread.FSettings.Height <> rheight then
  begin
    gRaylibThread.FSettings.Height := rheight;
    rsizechanged := True;
  end;

  gRaylibThread.FSettings.IsSizeChanged := rsizechanged;
end;

procedure TTinterForm.FormActivate(Sender: TObject);
begin
  gRaylibThread.Start;
end;

{ TRaylibThread }

procedure TRaylibThread.UpdateSettings();
begin
  if FSettings.IsPositionChanged then
  begin
    SetWindowPosition(FSettings.Left, FSettings.Top);
  end;

  if FSettings.IsSizeChanged then
  begin
    SetWindowSize(FSettings.Width, FSettings.Height);
    FScreenWidth := FSettings.Width;
    FScreenHeight := FSettings.Height;
    SetShaderValue(FShader, FwidthLoc, @FScreenWidth, SHADER_UNIFORM_FLOAT);
    SetShaderValue(FShader, FheightLoc, @FScreenHeight, SHADER_UNIFORM_FLOAT);
  end;

  if FSettings.IsShaderChanged then
  begin
    FShader := LoadShaderFromMemory(nil, PChar(FSettings.ShaderText));
    FwidthLoc := GetShaderLocation(FShader, 'screenWidth');
    FheightLoc := GetShaderLocation(FShader, 'screenHeight');
    FsecondsLoc := GetShaderLocation(FShader, 'seconds');
    SetShaderValue(FShader, FwidthLoc, @FScreenWidth, SHADER_UNIFORM_FLOAT);
    SetShaderValue(FShader, FheightLoc, @FScreenHeight, SHADER_UNIFORM_FLOAT);
    FSeconds := 0;
    FSettings.IsShaderChanged := False;
  end;
end;

procedure TRaylibThread.Execute;
begin
  FScreenWidth := 400;
  FScreenHeight := 400;

  InitWindow(trunc(FScreenWidth), trunc(FScreenHeight), 'TinterRender');
  SetWindowState(FLAG_VSYNC_HINT or FLAG_WINDOW_UNDECORATED or FLAG_WINDOW_TOPMOST);

  FShader := LoadShader(nil, './assets/screenFrags.fs');
  FwidthLoc := GetShaderLocation(FShader, 'screenWidth');
  FheightLoc := GetShaderLocation(FShader, 'screenHeight');
  FsecondsLoc := GetShaderLocation(FShader, 'seconds');

  FSeconds := 0;

  SetTargetFPS(60);
  while (not Terminated) and (not WindowShouldClose) do
  begin
    UpdateSettings;

    FSeconds += GetFrameTime;
    SetShaderValue(FShader, FsecondsLoc, @FSeconds, SHADER_UNIFORM_FLOAT);

    BeginDrawing();
    ClearBackground(ColorCreate(17, 22, 44, 255));
    BeginShaderMode(FShader);
    DrawRectangle(0, 0, FSettings.Width, FSettings.Height, BLUE);
    EndShaderMode;
    //DrawFPS(4, 4);
    EndDrawing();
  end;

  UnloadShader(FShader);
  CloseWindow;
end;

constructor TRaylibThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

end.
