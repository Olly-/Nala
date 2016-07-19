{%MainUnit nalaTypes}

function Point(X, Y: Int32): TPoint; inline;
begin
  Result.X := X;
  Result.Y := Y;
end;

function TPointHelper.toString: String;
begin

end;

procedure TPointHelper.Offset(p: TPoint);
begin

end;

function TPointHelper.InBox(b: TBox): Boolean;
begin

end;
