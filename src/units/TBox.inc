{%MainUnit nalaTypes}

function Box(X1, Y1, X2, Y2: Int32): TBox; inline;
begin
  Result.X1 := X1;
  Result.Y1 := Y1;
  Result.X2 := X2;
  Result.Y2 := Y2;
end;

function TBoxHelper.toString: String;
begin

end;

function TBoxHelper.getMiddle: TPoint;
begin
  Result.X := (Self.X2 - Self.X1 + 1) div 2;
  Result.Y := (Self.Y2 - Self.Y1 + 1) div 2;
end;

function TBoxHelper.getWidth: Integer;
begin
  Result := Self.X2 - Self.X1 + 1;
end;

function TBoxHelper.getHeight: Integer;
begin
  Result := Self.Y2 - Self.Y1 + 1;
end;

