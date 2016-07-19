unit nala.Imports.Box;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.LapeCompiler, lptypes, dateutils;

procedure Import_Box(Compiler: TLPCompiler);

implementation

uses
  nala.CoreTypes, nala.TBox;

procedure Lape_TBox_Area(Params: PParamArray; const Result: Pointer);
begin
  PUInt32(Result)^ := PBox(Params^[0])^.Area;
end;

procedure Lape_TBox_Width(Params: PParamArray; const Result: Pointer);
begin
  PUInt32(Result)^ := PBox(Params^[0])^.Width;
end;

procedure Lape_TBox_Height(Params: PParamArray; const Result: Pointer);
begin
  PUInt32(Result)^ := PBox(Params^[0])^.Height;
end;

procedure Lape_TBox_Middle(Params: PParamArray; const Result: Pointer);
begin
  PPoint(Result)^ := PBox(Params^[0])^.Middle;
end;

procedure Lape_TBox_ToString(Params: PParamArray; const Result: Pointer);
begin
  PlpString(Result)^ := PBox(Params^[0])^.ToString;
end;

procedure Lape_TBox_Offset(Params: PParamArray; const Result: Pointer);
begin
  PBox(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

procedure Import_Box(Compiler: TLPCompiler);
begin
  with Compiler do
  begin
    Dump.Section := 'Box';

    addGlobalFunc('function TBox.Area: UInt32;', @Lape_TBox_Area);
    addGlobalFunc('function TBox.Width: UInt32;', @Lape_TBox_Width);
    addGlobalFunc('function TBox.Height: UInt32;', @Lape_TBox_Height);
    addGlobalFunc('function TBox.Middle: TPoint;', @Lape_TBox_Middle);
    addGlobalFunc('function TBox.ToString: String;', @Lape_TBox_ToString);
    addGlobalFunc('procedure TBox.Offset(pt: TPoint);', @Lape_TBox_Offset);
  end;
end;

end.

