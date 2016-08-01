unit nala.imports.strings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lptypes, nala.LapeCompiler;

procedure Import_String(Compiler: TLPCompiler);

implementation

uses
  nala.Strings, nala.Types;

type
  PReplaceFlags = ^TReplaceFlags;

procedure Lape_String_Encode(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.Encode();
end;

procedure Lape_String_Decode(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.Decode();
end;

procedure Lape_String_After(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.After(PString(Params^[1])^);
end;

procedure Lape_String_Before(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.Before(PString(Params^[1])^);
end;

procedure Lape_String_PosEx(Params: PParamArray; const Result: Pointer);
begin
  PIntArray(Result)^ := PString(Params^[0])^.PosEx(PString(Params^[1])^);
end;

procedure Lape_String_Pos(Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := PString(Params^[0])^.Pos(PString(Params^[1])^);
end;

procedure Lape_String_PosR(Params: PParamArray; const Result: Pointer);
begin
  PInteger(Result)^ := PString(Params^[0])^.PosR(PString(Params^[1])^);
end;

procedure Lape_String_Replace(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.Replace(PString(Params^[1])^, PString(Params^[2])^, PReplaceFlags(Params^[3])^);
end;

procedure Lape_String_Explode(Params: PParamArray; const Result: Pointer);
begin
  PStringArray(Result)^ := PString(Params^[0])^.Explode(PString(Params^[1])^);
end;

procedure Lape_String_GetNumbers(Params: PParamArray; const Result: Pointer);
begin
  PIntArray(Result)^ := PString(Params^[0])^.GetNumbers;
end;

procedure Lape_String_GetLetters(Params: PParamArray; const Result: Pointer);
begin
  PStringArray(Result)^ := PString(Params^[0])^.GetLetters;
end;

procedure Import_String(Compiler: TLPCompiler);
begin
  with Compiler do
  begin
    Dump.Section := 'String';

    addGlobalFunc('function String.Encode: String;', @Lape_String_Encode);
    addGlobalFunc('function String.Decode: String;', @Lape_String_Decode);
    addGlobalFunc('function String.After(constref Delimiter: String): String;', @Lape_String_After);
    addGlobalFunc('function String.Before(constref Delimiter: String): String;', @Lape_String_Before);
    addGlobalFunc('function String.PosEx(constref SubStr: String): TIntArray;', @Lape_String_PosEx);
    addGlobalFunc('function String.Pos(constref SubStr: String): Integer;', @Lape_String_Pos);
    addGlobalFunc('function String.PosR(constref SubStr: String): Integer;', @Lape_String_PosR);
    addGlobalFunc('function String.Replace(constref SubStr, ReplaceStr: String; Flags: TReplaceFlags): String;', @Lape_String_Replace);
    addGlobalFunc('function String.Explode(constref Delimiter: String): TStringArray; ', @Lape_String_Encode);
    addGlobalFunc('function String.GetNumbers: TIntArray;', @Lape_String_GetNumbers);
    addGlobalFunc('function String.GetLetters: TStringArray;', @Lape_String_GetLetters);
  end;
end;

end.

