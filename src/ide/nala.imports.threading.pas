unit nala.Imports.Threading;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpTypes, lpvartypes, nala.LapeCompiler, nala.Thread, ffi;

procedure Import_Threading(Compiler: TLPCompiler; Thread: TThread);

implementation

uses
  nala.ScriptThread;

procedure Lape_Threaded(Params: PParamArray);
var
  Method: TThreadedMethod;
begin
  Method := PThreadedMethod(Params^[1])^;
  if (Method <> nil) then
    TNalaScriptThread(Params^[0]).AddThread(TNalaThread.Create(Method, PPointer(Params^[2])^));
end;

procedure Import_Threading(Compiler: TLPCompiler; Thread: TThread);
begin
  with Compiler do
  begin
    Dump.Section := 'Threading';

    addGlobalType('procedure(constref Data: Pointer);', 'TThreadedMethod', FFI_DEFAULT_ABI);
    addGlobalMethod('procedure Threaded(Proc: TThreadedMethod; constref Data: Pointer = nil);', @Lape_Threaded, TNalaScriptThread(Thread));
  end;
end;

end.

