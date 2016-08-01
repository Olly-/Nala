unit nala.Imports.TBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.LapeCompiler, lptypes;

procedure Import_TBox(Compiler: TLPCompiler);

implementation

uses
  nala.Types, nala.TBox;

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

procedure Lape_Box(Params: PParamArray; const Result: Pointer);
begin
  PBox(Result)^ := Box(PInt32(Params^[0])^, PInt32(Params^[1])^, PInt32(Params^[2])^, PInt32(Params^[3])^);
end;

procedure Import_TBox(Compiler: TLPCompiler);
begin
  with Compiler do
  begin
    Dump.Section := 'Box';

    addGlobalFunc('function Box(X1, Y1, X2, Y2: Int32): TBox;', @Lape_Box);

    addGlobalFunc('function TBox.Area: UInt32;', @Lape_TBox_Area);
    addGlobalFunc('function TBox.Width: UInt32;', @Lape_TBox_Width);
    addGlobalFunc('function TBox.Height: UInt32;', @Lape_TBox_Height);
    addGlobalFunc('function TBox.Middle: TPoint;', @Lape_TBox_Middle);
    addGlobalFunc('function TBox.ToString: String;', @Lape_TBox_ToString);
    addGlobalFunc('procedure TBox.Offset(Point: TPoint);', @Lape_TBox_Offset);
  end;
end;

end.

