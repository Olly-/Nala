unit nala.ColorMath;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, Graphics,
  nala.Types;

function RGBToBGR(Color: TColor): TRGB32; inline;
function BGRToRGB(BGR: TRGB32): TColor; inline;
function ColorToRGB(Color: TColor): TRGB; inline;
function ColorToHSB(Color: TColor): THSB; inline;

implementation

uses
  Math;

const
  ONE_DIV_THREE:     Single =  1.0 / 3.0;
  NEG_ONE_DIV_THREE: Single = -1.0 / 3.0;

function RGBToBGR(Color : TColor): TRGB32; inline;
begin
  Result.R := Color and $FF;
  Result.G := Color shr 8 and $FF;
  Result.B := Color shr 16 and $FF;
  Result.A := 0;
end;

function BGRToRGB(BGR: TRGB32): TColor; inline;
begin
  Result := BGR.R or BGR.G shl 8 or BGR.B shl 16;
end;

function ColorToRGB(Color: TColor): TRGB; inline;
begin
  Result.R := Color and $FF;
  Result.G := Color shr 8 and $FF;
  Result.B := Color shr 16 and $FF;
end;

procedure ColorToRGB(Color: TColor; out R, G, B: Integer); overload; inline;
begin
  R := Color and $FF;
  G := Color shr 8 and $FF;
  B := Color shr 16 and $FF;
end;

function ColorToHSB(Color: TColor): THSB; inline;
var
  Chroma, t: Single;
  R, G, B, K: Single;
begin
  R := (Color and $FF) / 255;
  G := (Color shr 8 and $FF) / 255;
  B := (Color shr 16 and $FF) / 255;
  K := 0.0;

  if (g < b) then
  begin
    t := b; b := g; g := t;
    K := -1.0;
  end;

  if (r < g) then
  begin
    t := r; r := g; g := t;
    K := NEG_ONE_DIV_THREE - K;
  end;

  Chroma := r - Min(g, b);
  Result.H := Abs(K + (g - b) / (6.0 * Chroma + 1.0e-20)) * 360;
  Result.S := Chroma / (r + 1.0e-20)  * 100;
  Result.B := r * 100;
end;

end.

