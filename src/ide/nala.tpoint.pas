unit nala.TPoint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  nala.Types;

function Point(X, Y: Int32): TPoint;

implementation

function Point(X, Y: Int32): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

end.

