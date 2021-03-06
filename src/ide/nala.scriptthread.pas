unit nala.ScriptThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lpcompiler, lptypes, lpvartypes, lpeval, lpinterpreter, lpexceptions, lpparser,
  nala.LapeCompiler, nala.MessageList, nala.Thread;

type
  EMessageType = (mtMessage, mtError, mtNote);

  { TNalaScriptThread }

  TNalaScriptThread = class(TThread)
  private
    FCompileOnly: Boolean;
    FIncludePaths, FPluginPaths: TStringList;
    FScript: String;

    FCompiler: TLPCompiler;
    FRunning: TInitBool;

    FDebugBox: TMessageList;
    FDebugBuffer: String;

    FMessageBox: TMessageList;
    FMessageBuffer: String;

    FThreads: TNalaThreadList;

    procedure KillThreads;

    function getRunning: Boolean;
    procedure setRunning(AValue: Boolean);
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    property IncludePaths: TStringList read FIncludePaths;
    property PluginPaths: TStringList read FPluginPaths;
    property Script: String read FScript write FScript;
    property CompileOnly: Boolean write FCompileOnly;
    property Running: Boolean read getRunning write setRunning;

    // Debug tab
    property DebugBox: TMessageList read FDebugBox write FDebugBox;
    property DebugBuffer: String read FDebugBuffer write FDebugBuffer;

    procedure DebugWrite(constref Str: String);
    procedure DebugWriteLn(MessageType: EMessageType = mtMessage);
    procedure DebugWriteLn(constref Str: String; MessageType: EMessageType = mtMessage); overload;

    // Message tab
    property MessageBox: TMessageList read FMessageBox write FMessageBox;
    property MessageBuffer: String read FMessageBuffer write FDebugBuffer;

    procedure MessageWrite(constref Str: String);
    procedure MessageWriteLn(MessageType: EMessageType = mtMessage);
    procedure MessageWriteLn(constref Str: String; MessageType: EMessageType = mtMessage); overload;

    procedure AddThread(Thread: TNalaThread);

    constructor Create;
  end;

implementation

uses
  nala.OSUtils, nala.Time;

{ TNalaScriptThread }

procedure TNalaScriptThread.KillThreads;
var
  Thread: TNalaThread;
begin
  Thread := FThreads.Pop;
  if (Thread = nil) then
    Exit;

  Writeln('Some threads haven''t been terminated, we''re going to (unsafely) kill them');

  while (Thread <> nil) do
  begin
    if (KillThread(Thread.Handle) <> 0) then
      Writeln('Failed to kill thread: ', Thread.Handle);

    Thread := FThreads.Pop;
  end;
end;

function TNalaScriptThread.getRunning: Boolean;
begin
  Result := (FRunning = bTrue);
end;

procedure TNalaScriptThread.setRunning(AValue: Boolean);
begin
  if (AValue) then
    FRunning := bTrue
  else
    FRunning := bFalse;
end;

procedure TNalaScriptThread.Execute;

  function DoCreate: Boolean;
  begin
    Result := True;

    try
      FCompiler := TLPCompiler.Create(FScript, AllImports, Self);
    except
      on e: lpException do
      begin
        MessageWriteLn('Creating error (Lape): ' + e.Message, mtError);
        Result := False;
      end;
      on e: Exception do
      begin
        MessageWriteLn('Creating error (FPC): ' + e.Message, mtError);
        Result := False;
      end;
    end;
  end;

  function DoCompile: Boolean;
  var
    t: Double;
  begin
    t := OSUtils.MarkTime();

    with FCompiler do
    try
      Result := Compile();
      MessageWriteLn(Format('Succesfully compiled in %f ms', [OSUtils.MarkTime() - t]));
    except
      on e: lpException do
      begin
        MessageWriteLn('Compiling error (Lape): ' + e.Message, mtError);
        Result := False;
      end;
      on e: Exception do
      begin
        MessageWriteLn('Compiling error (FPC): ' + e.Message, mtError);
        Result := False;
      end;
    end;
  end;

var
  Failed: Boolean = True;
  StartTime: record Ticks: UInt64; Time: Double; end;
begin
  if (DoCreate) and (DoCompile) then
  begin
    if (FCompileOnly) then
      Exit;

    StartTime.Time := OSUtils.MarkTime;
    StartTime.Ticks := GetTickCount64;

    FRunning := bTrue;

    try
      RunCode(FCompiler.Emitter.Code, FRunning);
      Failed := False;
    except
      on e: lpException do
        MessageWriteLn('Runtime error (Lape): ' + e.Message, mtError);
      on e: Exception do
        MessageWriteLn('Runtime error (FPC): ' + e.Message, mtError);
    end;

    KillThreads;

    if (not Failed) then
    begin
      if ((GetTickCount64() - StartTime.Ticks) <= 60000) then
        MessageWriteLn('Succesfully executed (' + FormatFloat('0.000', OSUtils.MarkTime() - StartTime.Time) + ' ms)')
      else
        MessageWriteLn('Succesfully executed (' + ConvertMilliseconds(GetTickCount64() - StartTime.Ticks, mfLong, [mffSeconds]) + ')');
    end else
      MessageWriteLn('Failed execution', mtError);
  end;
end;

procedure TNalaScriptThread.DoTerminate;
begin
  Writeln('Script thread[', ThreadID, '] safely terminated');
  if (FCompiler <> nil) then
    FCompiler.Free;

  FThreads.Free;

  inherited DoTerminate;
end;

procedure TNalaScriptThread.DebugWrite(constref Str: String);
begin
  FDebugBuffer += Str;
end;

procedure TNalaScriptThread.DebugWriteLn(MessageType: EMessageType);
begin
  FDebugBox.Add(FDebugBuffer, Ord(MessageType), True);
  SetLength(FDebugBuffer, 0);
end;

procedure TNalaScriptThread.DebugWriteLn(constref Str: String; MessageType: EMessageType);
begin
  DebugWrite(Str);
  DebugWriteLn(Str, MessageType);
end;

procedure TNalaScriptThread.MessageWrite(constref Str: String);
begin
  FMessageBuffer += Str;
end;

procedure TNalaScriptThread.MessageWriteLn(MessageType: EMessageType);
begin
  FMessageBox.Add(FMessageBuffer, Ord(MessageType), True);
  SetLength(FMessageBuffer, 0);
end;

procedure TNalaScriptThread.MessageWriteLn(constref Str: String; MessageType: EMessageType);
begin
  MessageWrite(Str);
  MessageWriteLn(MessageType);
end;

procedure TNalaScriptThread.AddThread(Thread: TNalaThread);
begin
  FThreads.Add(Thread);
end;

constructor TNalaScriptThread.Create;
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FRunning := bFalse;
  FCompiler := nil;
  FScript := '';

  FThreads := TNalaThreadList.Create;
end;

end.

