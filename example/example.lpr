program example;

{$Codepage UTF8}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  fafafa.Watch { you can add units after this };

var
  LWatch: IfafafaWatch;
  i: Integer;

  procedure test;
  var
    i, LTmp: integer;
  begin
    for i := 0 to Pred(999999999) do
      LTmp := i;
  end;

begin
  WriteLn('fafafa.fafafaWatch.Example');

  for i := 0 to 3 do
  begin
    LWatch := MakeWatch(True);
    test;
    LWatch.Stop;
    WriteLn('# IfafafaWatch Tick:', LWatch.ElapsedTicks, ' ns:', LWatch.ElapsedNanoSecond, '  microSec:', LWatch.ElapsedMicroSecond, '  ms:', LWatch.ElapsedMillisecond);

    LWatch := MakeHDWatch(True);
    test;
    LWatch.Stop;
    WriteLn('# IfafafaHDWatch Tick:', LWatch.ElapsedTicks, ' ns:', LWatch.ElapsedNanoSecond, '  microSec:', LWatch.ElapsedMicroSecond, '  ms:', LWatch.ElapsedMillisecond);

    LWatch := MakeTSCWatch(True);
    test;
    LWatch.Stop;
    WriteLn('# IfafafaTSCWatch Tick:', LWatch.ElapsedTicks, ' ns:', LWatch.ElapsedNanoSecond, '  microSec:', LWatch.ElapsedMicroSecond, '  ms:', LWatch.ElapsedMillisecond);
    WriteLn();
  end;

end.
