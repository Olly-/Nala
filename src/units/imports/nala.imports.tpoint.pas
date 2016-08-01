unit nala.Imports.TPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.LapeCompiler, lptypes;

procedure Import_TPoint(Compiler: TLPCompiler);

implementation

uses
  nala.Types, nala.TPoint;

procedure Lape_TPoint_Offset(Params: PParamArray; const Result: Pointer);
begin
  PPoint(Result)^ :=  nala.Types.PPoint(Params^[0])^.Offset(PPoint(Params^[1])^);
end;

procedure Lape_TPoint_Inside(Params: PParamArray; const Result: Pointer);
begin
  PPoint(Params^[0])^.Inside(PBox(Params^[1])^);
end;

procedure Lape_TPoint_InsideEx(Params: PParamArray; const Result: Pointer);
begin
  PPoint(Params^[0])^.Inside(PPoint(Params^[1])^, PInt32(Params^[2])^);
end;

procedure Lape_TPoint_Random(Params: PParamArray; const Result: Pointer);
begin
  PPoint(Result)^ := PPoint(Params^[0])^.Random(PInt64(Params^[1])^, PInt64(Params^[2])^);
end;

procedure Lape_TPoint_DistanceTo(Params: PParamArray; const Result: Pointer);
begin
  PDouble(Result)^ := PPoint(Params^[0])^.DistanceTo(PPoint(Params^[1])^);
end;

procedure Lape_TPoint_ToString(Params: PParamArray; const Result: Pointer);
begin
  PString(Result)^ := PPoint(Params^[0])^.ToString;
end;

procedure Lape_Point(Params: PParamArray; const Result: Pointer);
begin
  PPoint(Result)^ := Point(PInt32(Params^[0])^, PInt32(Params^[1])^);
end;

procedure Import_TPoint(Compiler: TLPCompiler);
begin
  with Compiler do
  begin
    Dump.Section := 'Point';

    addGlobalFunc('function Point(X, Y: Int32): TPoint;', @Lape_Point);

    addGlobalFunc('function TPoint.Offset(Point: TPoint): TPoint;', @Lape_TPoint_Offset);
    addGlobalFunc('function TPoint.Inside(Box: TBox): Boolean;', @Lape_TPoint_Inside);
    addGlobalFunc('function TPoint.Inside(Middle: TPoint; Radius: Int32): Boolean; overload;', @Lape_TPoint_InsideEx);
    addGlobalFunc('function TPoint.Random(Min, Max: Int64): TPoint; ', @Lape_TPoint_Random);
    addGlobalFunc('function TPoint.DistanceTo(Point: TPoint): Double;', @Lape_TPoint_DistanceTo);
    addGlobalFunc('function TPoint.ToString: String;', @Lape_TPoint_ToString);
  end;
end;

end.

