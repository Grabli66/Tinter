program tinter;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cmem,
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TTinterForm, TinterForm);
  Application.Run;
end.

