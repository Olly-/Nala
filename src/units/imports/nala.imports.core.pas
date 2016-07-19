unit nala.Imports.Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lpTypes, nala.LapeCompiler;

procedure Import_Core(Compiler: TLPCompiler; Thread: TThread);

implementation

uses
  nala.ScriptThread;

type
  PThreadID = ^TThreadID;

procedure Lape_Write(Params: PParamArray);
begin
  TNalaScriptThread(Params^[0]).DebugWrite(PlpString(Params^[1])^);
end;

procedure Lape_WriteLn(Params: PParamArray);
begin
  TNalaScriptThread(Params^[0]).DebugWriteln();
end;

procedure Lape_ScriptText(Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := TNalaScriptThread(Params^[0]).Script;
end;

procedure Lape_GetThreadID(Params: PParamArray; const Result: Pointer);
begin
  PThreadID(Result)^ := GetThreadID();
end;

procedure Lape_GetMainThreadID(Params: PParamArray; const Result: Pointer);
begin
  PThreadID(Result)^ := MainThreadID;
end;

procedure Lape_Wait(Params: PParamArray);
begin
  Sleep(PUInt32(Params^[0])^);
end;

procedure Import_Core(Compiler: TLPCompiler; Thread: TThread);
begin
  with Compiler do
  begin
    Dump.Section := 'Core';

    addGlobalType('array of Boolean', 'TBoolArray');
    addGlobalType('array of TBoolArray', 'T2DBoolArray');

    addGlobalType('array of String', 'TStringArray');
    addGlobalType('array of TStringArray', 'T2DStringArray');

    addGlobalType('array of Int32', 'TIntArray');
    addGlobalType('array of TIntArray', 'T2DIntArray');

    addGlobalType('array of Double', 'TDoubleArray');
    addGlobalType('array of TDoubleArray', 'T2DDoubleArray');

    addGlobalType('packed record X1, Y1, X2, Y2: Int32; end;', 'TBox');
    addGlobalType('array of TBox', 'TBoxArray');
    addGlobalType('array of TBoxArray', 'T2DBoxArray');

    addGlobalType('packed record X, Y: Int32; end;', 'TPoint');
    addGlobalType('array of TPoint', 'TPointArray');
    addGlobalType('array of TPointArray', 'T2DPointArray');

    addGlobalType('UInt32', 'THandle');
    addGlobalType('THandle', 'TThreadID');

    addGlobalMethod('procedure _Write(Str: String); override;', @Lape_Write, TNalaScriptThread(Thread));
    addGlobalMethod('procedure _WriteLn; override;', @Lape_Writeln, TNalaScriptThread(Thread));

    addGlobalMethod('function ScriptText: String;', @Lape_ScriptText, TNalaScriptThread(Thread));

    addGlobalFunc('function GetThreadID: TThreadID;', @Lape_GetThreadID);
    addGlobalFunc('function GetMainThreadID: TThreadID;', @Lape_GetMainThreadID);

    addGlobalFunc('procedure Wait(Milliseconds: UInt32);', @Lape_Wait);

    addDelayedCode('type                                                        ' + LineEnding +
                   '  TScript = record                                          ' + LineEnding +
                   '    StartMilliseconds: UInt64;                              ' + LineEnding +
                   '    StartDateTime: TDateTime;                               ' + LineEnding +
                   '  end;                                                      ' + LineEnding +
                   '                                                            ' + LineEnding +
                   'function TScript.Text: String;                              ' + LineEnding +
                   'begin                                                       ' + LineEnding +
                   '  Result := ScriptText();                                   ' + LineEnding +
                   'end;                                                        ' + LineEnding +
                   '                                                            ' + LineEnding +
                   'function TScript.RunTime: UInt64;                           ' + LineEnding +
                   'begin                                                       ' + LineEnding +
                   '  Result := GetTickCount() - StartMilliseconds;             ' + LineEnding +
                   'end;                                                        ' + LineEnding +
                   '                                                            ' + LineEnding +
                   'var                                                         ' + LineEnding +
                   '  Script: TScript = [' + IntToStr(GetTickCount64()) + ',    ' + LineEnding +
                   '                     ' + FloatToStr(Now()) + '];            '
                   );
  end;
end;

end.

