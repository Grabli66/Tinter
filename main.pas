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

    // Позиция
    Top, Left: integer;
    // Размеры
    Width, Height: integer;
  end;

  { TRaylibThread }

  TRaylibThread = class(TThread)
  private
    FSettings: TRaylibThreadSettings;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean);
  end;

  { TTinterForm }

  TTinterForm = class(TForm)
    ImageList1: TImageList;
    MainToolbar: TToolBar;
    NewShaderButton: TToolButton;
    OpenShaderButton: TToolButton;
    ShaderCodeEditor: TATSynEdit;
    UpdateSettingsTimer: TTimer;
    UpdateShaderButton: TToolButton;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure UpdateSettingsTimerTimer(Sender: TObject);
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

  if rheight > Height then begin
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

procedure TRaylibThread.Execute;
var
  shader: TShader;
  widthLoc, heightLoc, secondsLoc: integer;
  screenWidth, screenHeight, seconds: single;
begin
  screenWidth := 400;
  screenHeight := 400;

  InitWindow(trunc(screenWidth), trunc(screenHeight), 'TinterRender');
  SetWindowState(FLAG_VSYNC_HINT or FLAG_WINDOW_UNDECORATED or FLAG_WINDOW_TOPMOST);

  shader := LoadShader(nil, './assets/screenFrag.fs');
  widthLoc := GetShaderLocation(shader, 'screenWidth');
  heightLoc := GetShaderLocation(shader, 'screenHeight');
  secondsLoc := GetShaderLocation(shader, 'seconds');

  seconds := 0;

  SetTargetFPS(60);
  while (not Terminated) and (not WindowShouldClose) do
  begin
    seconds += GetFrameTime;

    SetShaderValue(shader, secondsLoc, @seconds, SHADER_UNIFORM_FLOAT);

    if FSettings.IsPositionChanged then
    begin
      SetWindowPosition(FSettings.Left, FSettings.Top);
    end;

    if FSettings.IsSizeChanged then
    begin
      SetWindowSize(FSettings.Width, FSettings.Height);
      screenWidth := FSettings.Width;
      screenHeight := FSettings.Height;
      SetShaderValue(shader, widthLoc, @screenWidth, SHADER_UNIFORM_FLOAT);
      SetShaderValue(shader, heightLoc, @screenHeight, SHADER_UNIFORM_FLOAT);
    end;

    BeginDrawing();
    ClearBackground(ColorCreate(17, 22, 44, 255));
    BeginShaderMode(shader);
    DrawRectangle(0, 0, FSettings.Width, FSettings.Height, BLUE);
    EndShaderMode;
    //DrawFPS(4, 4);
    EndDrawing();
  end;

  UnloadShader(shader);
  CloseWindow;
end;

constructor TRaylibThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

end.
