unit nala.imports.strings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lptypes, nala.LapeCompiler;

procedure Import_String(Compiler: TLPCompiler);

implementation

uses
  nala.Strings, nala.Types;

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
  PInt32(Result)^ := PString(Params^[0])^.Pos(PString(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_String_PosR(Params: PParamArray; const Result: Pointer);
begin
  PInt32(Result)^ := PString(Params^[0])^.PosR(PString(Params^[1])^);
end;

procedure Lape_String_Contains(Params: PParamArray; const Result: Pointer);
begin
  PBoolean(Result)^ := PString(Params^[0])^.Contains(PString(Params^[1])^);
end;

procedure Lape_String_Replace(Params: PParamArray; const Result: Pointer);
type
  PReplaceFlags = ^TReplaceFlags;
begin
  PString(Result)^ := PString(Params^[0])^.Replace(PString(Params^[1])^, PString(Params^[2])^, PReplaceFlags(Params^[3])^);
end;

procedure Lape_String_Explode(Params: PParamArray; const Result: Pointer);
begin
  PStringArray(Result)^ := PString(Params^[0])^.Explode(PString(Params^[1])^);
end;

procedure Lape_String_ExtractNumbers(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.ExtractNumbers;
end;

procedure Lape_String_ExtractLetters(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.ExtractLetters;
end;

procedure Lape_String_Extract(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.Extract(PCharArray(Params^[1])^);
end;

procedure Lape_String_UpperCase(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.UpperCase;
end;

procedure Lape_String_LowerCase(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.LowerCase;
end;

procedure Lape_String_Trim(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.Trim;
end;

procedure Lape_String_MD5(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.MD5;
end;

procedure Lape_String_SHA1(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PString(Params^[0])^.SHA1;
end;

procedure Import_String(Compiler: TLPCompiler);
begin
  with Compiler do
  begin
    Dump.Section := 'String';

    addGlobalFunc('function String.Encode: String;', 'Encodes and returns the string with Base64 encoding',  @Lape_String_Encode);
    addGlobalFunc('function String.Decode: String;', 'Decodes the string (containing Base64 encoded data) and returns the decoded data',  @Lape_String_Decode);
    addGlobalFunc('function String.After(constref Delimiter: String): String;', '',  @Lape_String_After);
    addGlobalFunc('function String.Before(constref Delimiter: String): String;', '',  @Lape_String_Before);
    addGlobalFunc('function String.PosEx(constref SubStr: String): TIntArray;', '',  @Lape_String_PosEx);
    addGlobalFunc('function String.Pos(constref SubStr: String; Offset: Int32 = 1): Int32;', '',  @Lape_String_Pos);
    addGlobalFunc('function String.PosR(constref SubStr: String): Int32;', '',  @Lape_String_PosR);
    addGlobalFunc('function String.Contains(constref SubStr: String): Boolean;', '',  @Lape_String_Contains);
    addGlobalFunc('function String.Replace(constref SubStr, ReplaceStr: String; Flags: TReplaceFlags): String;', '',  @Lape_String_Replace);
    addGlobalFunc('function String.Explode(constref Delimiter: String): TStringArray; ', '',  @Lape_String_Encode);
    addGlobalFunc('function String.ExtractNumbers: String;', '',  @Lape_String_ExtractNumbers);
    addGlobalFunc('function String.ExtractLetters: String;', '',  @Lape_String_ExtractLetters);
    addGlobalFunc('function String.Extract(constref Chars: TCharArray): String;', '',  @Lape_String_Extract);
    addGlobalFunc('function String.UpperCase: String;', '',  @Lape_String_UpperCase);
    addGlobalFunc('function String.LowerCase: String;', '',  @Lape_String_LowerCase);
    addGlobalFunc('function String.Trim: String;', 'Removes whitespace from the beginning and end of the string and returns the resulting string',  @Lape_String_Trim);
    addGlobalFunc('function String.MD5: String;', 'Returns a MD5 hash of the string',  @Lape_String_MD5);
    addGlobalFunc('function String.SHA1: String;', 'Returns a SHA1 hash of the string',  @Lape_String_SHA1);
  end;
end;

end.

