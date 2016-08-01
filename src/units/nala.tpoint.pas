unit nala.TPoint;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$inline on}

interface

uses
  Classes, SysUtils, nala.Types;

type

  { TPointHelper }

  TPointHelper = record Helper for TPoint
  public
    function Offset(Point: TPoint): TPoint;
    function Inside(Box: TBox): Boolean;
    function Inside(Middle: TPoint; Radius: Int32): Boolean; overload;
    function Random(Min, Max: Int64): TPoint;
    function DistanceTo(Point: TPoint): Double;
    function ToString: String;
  end;

function Point(X, Y: Int32): TPoint;

implementation

uses
  Math;

function Point(X, Y: Int32): TPoint; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

{ TPointHelper }

function TPointHelper.Offset(Point: TPoint): TPoint;
begin
  Result.X := Self.X + Point.X;
  Result.Y := Self.Y + Point.Y;
end;

function TPointHelper.Inside(Box: TBox): Boolean;
begin
  Result := (Self.X > Box.X1) and (Self.Y > Box.Y1) and (Self.X < Box.X2) and (Self.Y < Box.Y2);
end;

function TPointHelper.Inside(Middle: TPoint; Radius: Int32): Boolean;
begin
  Result := Sqrt(Sqr(Self.X - Middle.X) + Sqr(Self.Y - Middle.Y)) < Radius;
end;

function TPointHelper.Random(Min, Max: Int64): TPoint;
begin
  Result.X += System.Random((Max - Min) + 1);
  Result.Y += System.Random((Max - Min) + 1);
end;

function TPointHelper.DistanceTo(Point: TPoint): Double;
begin
  Result := Sqrt(Sqr(Self.X - Point.X) + Sqr(Self.Y - Point.Y));
end;

function TPointHelper.ToString: String;
begin
  Result := '[' + IntToStr(Self.X) + ', ' + IntToStr(Self.Y) + ']';
end;

end.

