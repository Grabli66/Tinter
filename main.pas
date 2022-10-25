unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ATSynEdit,
  raylib;

type
  // Функция которая возвращает размеры
  TGetRectFunction = function(): TRect of object;

  { TRaylibThread }

  TRaylibThread = class(TThread)
  private
    // Функция возвращает размер для окна отображающего шейдер
    FGetWindowsRectFunc: TGetRectFunction;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; AGetWindowsRectFunc: TGetRectFunction);
  end;

  { TTinterForm }

  TTinterForm = class(TForm)
    ShaderCodeEditor: TATSynEdit;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FRaylibThread: TRaylibThread;
    function GetRaylibRectFunction(): TRect;
  public

  end;

var
  TinterForm: TTinterForm;

implementation

{$R *.lfm}

{ TTinterForm }

procedure TTinterForm.FormCreate(Sender: TObject);
begin
  FRaylibThread := TRaylibThread.Create(True, @GetRaylibRectFunction);
end;

function TTinterForm.GetRaylibRectFunction(): TRect;
const
  BOTTOM_PADDING = 26;
var
  rwidth, rheight: int64;
begin
  rwidth := trunc(self.Height / 2);
  rheight := trunc(self.Height / 2);

  Result.Top := (self.Top + self.Height) - rheight + BOTTOM_PADDING;
  Result.Left := (self.Left + self.Width) - rwidth;

  Result.Width := rwidth;
  Result.Height := rheight;
end;

procedure TTinterForm.FormActivate(Sender: TObject);
begin
  FRaylibThread.Start;
end;

{ TRaylibThread }

procedure TRaylibThread.Execute;
var
  lastRect, currentRect: TRect;
  time: single = 0;
begin
  lastRect := default(TRect);

  InitWindow(400, 400, 'TinterRender');
  SetWindowState(FLAG_VSYNC_HINT or FLAG_WINDOW_UNDECORATED or FLAG_WINDOW_TOPMOST);

  currentRect := FGetWindowsRectFunc();
  lastRect := currentRect;
  SetWindowPosition(currentRect.Left, currentRect.Top);
  SetWindowSize(currentRect.Width, currentRect.Height);

  SetTargetFPS(60);
  while (not Terminated) and (not WindowShouldClose) do
  begin
    time += GetFrameTime;

    if time > 0.2 then
    begin
      currentRect := FGetWindowsRectFunc();
      if currentRect <> lastRect then
      begin
        SetWindowPosition(currentRect.Left, currentRect.Top);
        SetWindowSize(currentRect.Width, currentRect.Height);
        lastRect := currentRect;
      end;
      time := 0;
    end;

    BeginDrawing();
    ClearBackground(ColorCreate(17, 22, 44, 255));
    //DrawFPS(4, 4);
    EndDrawing();
  end;

  CloseWindow;
end;

constructor TRaylibThread.Create(CreateSuspended: boolean;
  AGetWindowsRectFunc: TGetRectFunction);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  FGetWindowsRectFunc := AGetWindowsRectFunc;
end;

end.
