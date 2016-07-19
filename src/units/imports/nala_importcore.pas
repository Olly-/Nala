unit nala_ImportCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lptypes, lpcompiler;

procedure Lape_ImportCore(Compiler: TLapeCompiler; Thread: TThread);

implementation

uses
  Types,
  nala_LapeThread, nala_Types;

type
  PThreadID = ^TThreadID;

procedure Lape_Write(Params: PParamArray);
begin
  with TNalaLapeThread(Params^[0]) do
    DebugBuffer := DebugBuffer + PlpString(Params^[1])^;
end;

procedure Lape_WriteLn(Params: PParamArray);
var
  Thread: TNalaLapeThread;
begin
  Thread := TNalaLapeThread(Params^[0]);
  Thread.Synchronize(Thread, Thread.DebugLnProc);
  Thread.DebugBuffer := '';
end;

procedure Lape_GetThreadID(Params: PParamArray; const Result: Pointer);
begin
  PThreadID(Result)^ := GetThreadID;
end;

procedure Lape_GetMainThreadID(Params: PParamArray; const Result: Pointer);
begin
  PThreadID(Result)^ := MainThreadID;
end;

procedure Lape_Wait(Params: PParamArray);
begin
  Sleep(PUInt32(Params^[0])^);
end;

procedure Lape_ImportCore(Compiler: TLapeCompiler; Thread: TThread);
begin
  with Compiler do
  begin
    addGlobalType('UInt32', 'THandle');
    addGlobalType('THandle', 'TThreadID');

    addGlobalMethod('procedure _Write(Str: String); override;', @Lape_Write, Thread as TNalaLapeThread);
    addGlobalMethod('procedure _WriteLn; override;', @Lape_Writeln, Thread as TNalaLapeThread);

    addGlobalFunc('function GetThreadID: TThreadID;', @Lape_GetThreadID);
    addGlobalFunc('function GetMainThreadID: TThreadID;', @Lape_GetMainThreadID);

    addGlobalFunc('procedure Wait(MilliSeconds: UInt32);', @Lape_Wait);
  end;
end;

end.

