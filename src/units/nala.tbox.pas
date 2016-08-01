unit nala.TBox;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$inline on}

interface

uses
  Classes, SysUtils,
  nala.Types;

type

  { TBoxHelper }

  TBoxHelper = record Helper for TBox
  private
    function getWidth: Int32;
    function getHeight: Int32;
    function getMiddle: TPoint;
  public
    function ToString: String;
    function Area: UInt32;
    procedure Offset(pt: TPoint);

    property Middle: TPoint read getMiddle;
    property Width: Int32 read getWidth;
    property Height: Int32 read getHeight;
  end;

function Box(X1, Y1, X2, Y2: Int32): TBox;

implementation

function Box(X1, Y1, X2, Y2: Int32): TBox; inline;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

function TBoxHelper.ToString: String;
begin
  Result := Format('[%d, %d, %d, %d]', [Self.X1, Self.Y1, Self.X2, Self.Y2]);
end;

function TBoxHelper.Area: UInt32;
begin
  Result := Self.Width * Self.Height;
end;

procedure TBoxHelper.Offset(pt: TPoint);
begin
  Self.X1 += pt.X;
  Self.Y1 += pt.Y;
  Self.X2 += pt.X;
  Self.Y2 += pt.Y;
end;

function TBoxHelper.getMiddle: TPoint;
begin
  Result.X := (Self.X2 - Self.X1 + 1) div 2;
  Result.Y := (Self.Y2 - Self.Y1 + 1) div 2;
end;

function TBoxHelper.getWidth: Int32;
begin
  Result := Self.X2 - Self.X1 + 1;
end;

function TBoxHelper.getHeight: Int32;
begin
  Result := Self.Y2 - Self.Y1 + 1;
end;

end.

