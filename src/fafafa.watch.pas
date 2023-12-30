unit fafafa.Watch;

{
  高精度的计时器 by fafafaStudio

  IfafafaWatch是毫秒精度
  IfafafaHDWatch在Linux下精确到纳秒,在Windows下精确到微秒

  注意:IfafafaWatch也提供了纳秒及微秒的接口,这是通过乘法插值计算的,不是精确的值
  Windows下的IfafafaHDWatch是微秒精度的,在windows下只能使用IfafafaTSCWatch获得纳秒精度,但它对CPU兼容性不够好,不支持ARM处理器

}

{$mode objfpc}{$H+}
{$Define ENABLED_TSC}
{$IfDef ENABLED_TSC}
{$AsmMode INTEL }
{$EndIf}


interface

uses
  Classes, SysUtils;

type

  { 纳秒,微秒,毫秒,秒,分,小时,天 }
  TTimeUnit = (TU_NanoSecond, TU_MicroSecond, TU_MilliSecond, TU_Second, TU_Minute, TU_Hour, TU_Day);

  TShortenElapsed = record
    TimeUnit: TTimeUnit;
    Elapsed: double;
  end;

  { IfafafaWatch }

  IfafafaWatch = interface
    ['{E96D8025-0671-454D-A8F0-063E31CA2A9D}']

    function GetElapsedTicks: uint64;
    function GetElapsedNanoSecond: uint64;
    function GetElapsedNanoSecondF: double;
    function GetElapsedMicroSecond: uint64;
    function GetElapsedMicroSecondF: double;
    function GetElapsedMillisecond: uint64;
    function GetElapsedMillisecondF: double;
    function GetElapsedSecond: uint64;
    function GetElapsedSecondF: double;
    function GetElapsedMinute: uint64;
    function GetElapsedMinuteF: double;
    function GetElapsedHour: uint64;
    function GetElapsedHourF: double;
    function GetElapsedDay: uint64;
    function GetElapsedDayF: double;

    function GetIsPause: boolean;
    function GetResolution: uint64;
    function GetIsRunning: boolean;

    { 开始计时 }
    procedure Start;

    { 暂停计时 }
    procedure Pause;

    { 恢复计时 }
    procedure Resume;

    { 停止计时 }
    procedure Stop;

    { 重置开始时间 }
    procedure Reset;

    { 获取消耗的短时间 }
    function ShortenElapsed: TShortenElapsed;

    { 获取消耗的短时间字符串 }
    function ShortenElapsedStr(const aFormat:string): string;

    { 是否在执行中 }
    property IsRunning: boolean read GetIsRunning;

    { 是否暂停计时 }
    property IsPause: boolean read GetIsPause;

    { Tick }
    property ElapsedTicks: uint64 read GetElapsedTicks;

    { 纳秒 }
    property ElapsedNanoSecond: uint64 read GetElapsedNanoSecond;
    property ElapsedNanoSecondF: double read GetElapsedNanoSecondF;

    { 微秒 }
    property ElapsedMicroSecond: uint64 read GetElapsedMicroSecond;
    property ElapsedMicroSecondF: double read GetElapsedMicroSecondF;

    { 毫秒 }
    property ElapsedMillisecond: uint64 read GetElapsedMillisecond;
    property ElapsedMillisecondF: double read GetElapsedMillisecondF;

    { 秒 }
    property ElapsedSecond: uint64 read GetElapsedSecond;
    property ElapsedSecondF: double read GetElapsedSecondF;

    { 分 }
    property ElapsedMinute: uint64 read GetElapsedMinute;
    property ElapsedMinuteF: double read GetElapsedMinuteF;

    { 小时 }
    property ElapsedHour: uint64 read GetElapsedHour;
    property ElapsedHourF: double read GetElapsedHourF;

    { 天 }
    property ElapsedDay: uint64 read GetElapsedDay;
    property ElapsedDayF: double read GetElapsedDayF;

    { 精度分辨率 计时器的分辨率代表每秒的Tick数量 }
    property Resolution: uint64 read GetResolution;
  end;

  { TfafafaWatch }

  TfafafaWatch = class(TInterfacedObject, IfafafaWatch)
  private
    FIsRunning, FIsPause: boolean;
    FStart, FStop, FPause: uint64;
    FResolution: uint64;
  private
    function GetElapsedTicks: uint64;
    function GetElapsedNanoSecond: uint64;
    function GetElapsedNanoSecondF: double;
    function GetElapsedMicroSecond: uint64;
    function GetElapsedMicroSecondF: double;
    function GetElapsedMillisecond: uint64;
    function GetElapsedMillisecondF: double;
    function GetElapsedSecond: uint64;
    function GetElapsedSecondF: double;
    function GetElapsedMinute: uint64;
    function GetElapsedMinuteF: double;
    function GetElapsedHour: uint64;
    function GetElapsedHourF: double;
    function GetElapsedDay: uint64;
    function GetElapsedDayF: double;

    function GetIsPause: boolean;
    function GetIsRunning: boolean;
  protected
    function GetResolution: uint64; virtual;
    function DoTick: uint64; virtual;
    procedure InitResolution(var aResolution: uint64); virtual;

    function CalcTicks(aStart, aElapsed: uint64): uint64; virtual;
  public
    constructor Create; virtual; overload;
    constructor Create(aRunNow: boolean); overload;

    procedure Start;
    procedure Pause;
    procedure Resume;
    procedure Stop;
    procedure Reset;
    function ShortenElapsed: TShortenElapsed;
    function ShortenElapsedStr(const aFormat:string): string;

    property IsRunning: boolean read GetIsRunning;
    property IsPause: boolean read GetIsPause;
    property ElapsedTicks: uint64 read GetElapsedTicks;
    property ElapsedNanoSecond: uint64 read GetElapsedNanoSecond;
    property ElapsedNanoSecondF: double read GetElapsedNanoSecondF;
    property ElapsedMicroSecond: uint64 read GetElapsedMicroSecond;
    property ElapsedMicroSecondF: double read GetElapsedMicroSecondF;
    property ElapsedMillisecond: uint64 read GetElapsedMillisecond;
    property ElapsedMillisecondF: double read GetElapsedMillisecondF;
    property ElapsedSecond: uint64 read GetElapsedSecond;
    property ElapsedSecondF: double read GetElapsedSecondF;
    property ElapsedMinute: uint64 read GetElapsedMinute;
    property ElapsedMinuteF: double read GetElapsedMinuteF;
    property ElapsedHour: uint64 read GetElapsedHour;
    property ElapsedHourF: double read GetElapsedHourF;
    property ElapsedDay: uint64 read GetElapsedDay;
    property ElapsedDayF: double read GetElapsedDayF;

    property Resolution: uint64 read GetResolution;
  end;


function MakeWatch(): IfafafaWatch; overload;
function MakeWatch(aRunNow: boolean): IfafafaWatch; overload;

type
  { TfafafaHDWatch 高精度计时器 }

  TfafafaHDWatch = class(TfafafaWatch)
  protected
    function DoTick: uint64; override;
    procedure InitResolution(var aResolution: uint64); override;
  end;

function MakeHDWatch(): IfafafaWatch; overload;
function MakeHDWatch(aRunNow: boolean): IfafafaWatch; overload;

{$IfDef ENABLED_TSC}

type

  { IfafafaTSCWatch }

  IfafafaTSCWatch = interface(IfafafaWatch)
    ['{23010A29-7AFD-4A2C-BDEF-DA45FE28D9B7}']

    { 校正,周期性的校正它获得精准度 }
    procedure calibrate;
  end;

  { TfafafaTSCWatch }

  TfafafaTSCWatch = class(TfafafaWatch)
  private
    FCPUClock: extended;
  protected
    procedure InitResolution(var aResolution: uint64); override;
    function DoTick: uint64; override;
  public
    constructor Create; override; overload;
    procedure calibrate;
  end;

function MakeTSCWatch: IfafafaWatch; overload;
function MakeTSCWatch(aRunNow: boolean): IfafafaWatch; overload;
{$EndIf}

const

  MS_PER_SEC = 1000;
  MICROSEC_PER_SEC = 1000000;
  NS_PER_SEC = 1000000000;
  PS_PER_SEC = 1000000000000;


function TickToNanoSecond(aTick, aResolution: uint64): double;
function TickToMicroSecond(aTick, aResolution: uint64): double;
function TickToMillisecond(aTick, aResolution: uint64): double;
function TickToSecond(aTick, aResolution: uint64): double;
function TickToMinute(aTick, aResolution: uint64): double;
function TickToHour(aTick, aResolution: uint64): double;
function TickToDay(aTick, aResolution: uint64): double;

{ 获取消耗的短时间 }
function CalcShortenElapsed(aTick, aResolution: uint64): TShortenElapsed;

{ 时间单位转为字符串 }
function TimeUnitToStr(aUnit: TTimeUnit): string;

implementation

{$IfDef MSWINDOWS}
type

  BOOL = longbool;

function QueryPerformanceCounter(var lpPerformanceCount: uint64): BOOL; external 'kernel32' Name 'QueryPerformanceCounter';
function QueryPerformanceFrequency(var lpFrequency: uint64): BOOL; external 'kernel32' Name 'QueryPerformanceFrequency';


{ TfafafaHDWatch }

procedure TfafafaHDWatch.InitResolution(var aResolution: uint64);
begin
  if not QueryPerformanceFrequency(aResolution) then
    raise Exception.Create('QueryPerformanceFrequency Failed!');
end;

function TfafafaHDWatch.DoTick: uint64;
begin
  if not QueryPerformanceCounter(Result) then
    raise Exception.Create('QueryPerformanceCounter Failed!');
end;

{$ELSE}
  uses
  BaseUnix,
  {$if DEFINED(LINUX)}
   linux
  {$ELSEIF DEFINED(FREEBSD)}
    FreeBSD
  {$ENDIF};

{ TfafafaHDWatch }

procedure TfafafaHDWatch.InitResolution(var aResolution: UInt64);
var
  LRes: timespec;
begin
  if clock_getres(CLOCK_MONOTONIC,@LRes)=0 then
  begin
    aResolution:=LRes.tv_nsec * NS_PER_SEC
  end
  else
    raise Exception.Create('clock_getres Failed!');
end;

function TfafafaHDWatch.DoTick:UInt64;
var
  LTP: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC,@LTP);
  Result:=(LTP.tv_sec*NS_PER_SEC)+LTP.tv_nsec;
end;

{$ENDIF}

function MakeWatch(): IfafafaWatch;
begin
  Result := MakeWatch(False);
end;

function MakeWatch(aRunNow: boolean): IfafafaWatch;
begin
  Result := TfafafaWatch.Create(aRunNow);
end;

function MakeHDWatch(): IfafafaWatch;
begin
  Result := MakeHDWatch(False);
end;

function MakeHDWatch(aRunNow: boolean): IfafafaWatch;
begin
  Result := TfafafaHDWatch.Create(aRunNow);
end;

{ TfafafaWatch }

constructor TfafafaWatch.Create;
begin
  inherited Create;
  InitResolution(FResolution);
  FIsRunning := False;
  FIsPause := False;
end;

constructor TfafafaWatch.Create(aRunNow: boolean);
begin
  Create;
  if aRunNow then
    Start;
end;

procedure TfafafaWatch.InitResolution(var aResolution: uint64);
begin
  aResolution := MS_PER_SEC;
end;

function TfafafaWatch.DoTick: uint64;
begin
  Result := GetTickCount64;
end;

procedure TfafafaWatch.Start;
begin
  if not IsRunning then
  begin
    if IsPause then
      Resume
    else
    begin
      FStart := DoTick;
      FIsRunning := True;
    end;
  end;
end;

procedure TfafafaWatch.Stop;
begin
  if IsRunning then
  begin
    FStop := DoTick;
    FIsRunning := False;
    FIsPause := False;
  end;
end;

procedure TfafafaWatch.Pause;
begin
  if IsRunning then
  begin
    FPause := DoTick;
    FIsPause := True;
    FIsRunning := False;
  end;
end;

procedure TfafafaWatch.Resume;
begin
  if (not IsRunning) and IsPause then
  begin
    Inc(FStart, DoTick - FPause);
    FIsRunning := True;
  end;
end;

procedure TfafafaWatch.Reset;
begin
  FIsRunning := False;
  FIsPause := False;
end;

function TfafafaWatch.ShortenElapsed: TShortenElapsed;
begin
  Result := CalcShortenElapsed(GetElapsedTicks, Resolution);
end;

function TfafafaWatch.ShortenElapsedStr(const aFormat: string): string;
var
  LElapsed: TShortenElapsed;
begin
  LElapsed := ShortenElapsed;
  Result := Concat(FormatFloat(aFormat, LElapsed.Elapsed), ' ' + TimeUnitToStr(LElapsed.TimeUnit));
end;

function TfafafaWatch.CalcTicks(aStart, aElapsed: uint64): uint64;
begin
  Result := aElapsed - aStart;
end;

function TfafafaWatch.GetElapsedSecond: uint64;
begin
  Result := Round(GetElapsedSecondF);
end;

function TfafafaWatch.GetElapsedMinute: uint64;
begin
  Result := round(GetElapsedMinuteF);
end;

function TfafafaWatch.GetElapsedHour: uint64;
begin
  Result := round(GetElapsedHourF);
end;

function TfafafaWatch.GetElapsedHourF: double;
begin
  Result := TickToHour(GetElapsedTicks, Resolution);
end;

function TfafafaWatch.GetElapsedMicroSecondF: double;
begin
  Result := TickToMicroSecond(GetElapsedTicks, Resolution);
end;

function TfafafaWatch.GetElapsedMillisecondF: double;
begin
  Result := TickToMillisecond(GetElapsedTicks, Resolution);
end;

function TfafafaWatch.GetElapsedMinuteF: double;
begin
  Result := TickToMinute(GetElapsedTicks, Resolution);
end;

function TfafafaWatch.GetElapsedNanoSecondF: double;
begin
  Result := TickToNanoSecond(GetElapsedTicks, Resolution);
end;

function TfafafaWatch.GetElapsedSecondF: double;
begin
  Result := TickToSecond(GetElapsedTicks, Resolution);
end;

function TfafafaWatch.GetElapsedDay: uint64;
begin
  Result := round(GetElapsedDayF);
end;

function TfafafaWatch.GetElapsedDayF: double;
begin
  Result := TickToDay(GetElapsedTicks, Resolution);
end;

function TfafafaWatch.GetElapsedTicks: uint64;
var
  LElapsed: uint64;
begin
  if IsRunning then
    LElapsed := DoTick
  else if IsPause then
    LElapsed := FPause
  else
    LElapsed := FStop;

  Result := LElapsed - FStart;
end;

function TfafafaWatch.GetElapsedMillisecond: uint64;
begin
  Result := round(GetElapsedMillisecondF);
end;

function TfafafaWatch.GetElapsedMicroSecond: uint64;
begin
  Result := round(GetElapsedMicroSecondF);
end;

function TfafafaWatch.GetElapsedNanoSecond: uint64;
begin
  Result := Round(GetElapsedNanoSecondF);
end;

function TfafafaWatch.GetIsPause: boolean;
begin
  Result := FIsPause;
end;

function TfafafaWatch.GetIsRunning: boolean;
begin
  Result := FIsRunning;
end;

function TfafafaWatch.GetResolution: uint64;
begin
  Result := FResolution;
end;

{$IFDEF ENABLED_TSC}
function ReadTSC: uint64; assembler;
asm
         {$IfDEF CPU64}
           XOR   RAX, RAX
           PUSH  RBX
           CPUID
           POP   RBX
           RDTSC
           SHL   RDX, 32
           OR    RAX, RDX
         {$else}
         XOR     EAX, EAX
         PUSH    EBX
         CPUID
         POP     EBX
         RDTSC
         {$endif}
end;

{ TfafafaTSCWatch }

procedure TfafafaTSCWatch.InitResolution(var aResolution: uint64);
begin
  aResolution := NS_PER_SEC;
end;

function TfafafaTSCWatch.DoTick: uint64;
begin
  Result := Round(ReadTSC / FCPUClock);
end;

constructor TfafafaTSCWatch.Create;
begin
  inherited Create;
  calibrate;
end;

procedure TfafafaTSCWatch.calibrate;
var
  LTick: QWord;
  LTSCTick: uint64;
begin
  LTick := GetTickCount64;
  while LTick = GetTickCount64 do ;

  LTSCTick := ReadTSC;

  while GetTickCount64 < (LTick + 400) do ;

  LTSCTick := ReadTSC - LTSCTick;
  FCPUClock := 2.5e-9 * LTSCTick;
end;

function MakeTSCWatch: IfafafaWatch;
begin
  Result := MakeTSCWatch(False);
end;

function MakeTSCWatch(aRunNow: boolean): IfafafaWatch;
begin
  Result := TfafafaTSCWatch.Create(aRunNow);
end;

{$ENDIF}

function TimeUnitToStr(aUnit: TTimeUnit): string;
begin
  case aUnit of
    TU_NanoSecond: Result := 'ns';
    TU_MicroSecond: Result :=('µs');//#166#204#115; // μs
    TU_MilliSecond: Result := 'ms';
    TU_Second: Result := 's';
    TU_Minute: Result := 'm';
    TU_Hour: Result := 'h';
    TU_Day: Result := 'd';
  end;
end;

function TickToNanoSecond(aTick, aResolution: uint64): double;
begin
  Result := (aTick * NS_PER_SEC) / aResolution;
end;

function TickToMicroSecond(aTick, aResolution: uint64): double;
begin
  Result := (aTick * MICROSEC_PER_SEC) / aResolution;
end;

function TickToMillisecond(aTick, aResolution: uint64): double;
begin
  Result := (aTick * MS_PER_SEC) / aResolution;
end;

function TickToSecond(aTick, aResolution: uint64): double;
begin
  Result := TickToMillisecond(aTick, aResolution) / 1000;
end;

function TickToMinute(aTick, aResolution: uint64): double;
begin
  Result := TickToSecond(aTick, aResolution) / 60;
end;

function TickToHour(aTick, aResolution: uint64): double;
begin
  Result := TickToMinute(aTick, aResolution) / 60;
end;

function TickToDay(aTick, aResolution: uint64): double;
begin
  Result := TickToHour(aTick, aResolution) / 24;
end;

function CalcShortenElapsed(aTick, aResolution: uint64): TShortenElapsed;
begin
  with Result do
  begin
    Elapsed := TickToNanoSecond(aTick, aResolution);
    if Elapsed < 1000 then
    begin
      TimeUnit := TU_NanoSecond;
      Exit;
    end;

    Elapsed := TickToMicroSecond(aTick, aResolution);
    if Elapsed < 1000 then
    begin
      TimeUnit := TU_MicroSecond;
      Exit;
    end;

    Elapsed := TickToMillisecond(aTick, aResolution);
    if Elapsed < 1000 then
    begin
      TimeUnit := TU_MilliSecond;
      Exit;
    end;

    Elapsed := TickToSecond(aTick, aResolution);
    if Elapsed < 60 then
    begin
      TimeUnit := TU_Second;
      Exit;
    end;

    Elapsed := TickToMinute(aTick, aResolution);
    if Elapsed < 60 then
    begin
      TimeUnit := TU_Minute;
      Exit;
    end;

    Elapsed := TickToHour(aTick, aResolution);
    if Elapsed < 24 then
    begin
      TimeUnit := TU_Hour;
      Exit;
    end;

    Elapsed := TickToDay(aTick, aResolution);
    TimeUnit := TU_Day;
  end;
end;

end.
