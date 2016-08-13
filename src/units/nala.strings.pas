unit nala.Strings;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, base64, nala.Types, md5, sha1;

type

  { TNalaStringHelper }

  TNalaStringHelper = type Helper for String
  public
    function Encode: String;
    function Decode: String;

    function After(constref Delimiter: String): String;
    function Before(constref Delimiter: String): String;

    function PosEx(constref SubStr: String): TIntArray;
    function Pos(constref SubStr: String; Offset: Int32 = 1): Int32;
    function Pos(constref SubStrings: TStringArray; Offset: Int32 = 1): Int32; overload;
    function PosR(constref SubStr: String): Int32;

    function Explode(constref Delimiter: String): TStringArray;

    function Replace(constref SubStr, ReplaceStr: String; Flags: TReplaceFlags): String;

    function ExtractLetters: String;
    function ExtractNumbers: String;
    function Extract(constref Chars: TCharArray): String;

    function Upper: String;
    function Lower: String;

    function Trim: String;

    function MD5: String;
    function SHA1: String;
  end;

implementation

function TNalaStringHelper.Encode: String;
begin
  Result := EncodeStringBase64(Self);
end;

function TNalaStringHelper.Decode: String;
begin
  Result := DecodeStringBase64(Self);
end;

function TNalaStringHelper.After(constref Delimiter: String): String;
var
  p: UInt32;
begin
  p := Self.Pos(Delimiter);
  if (p = 0) then
    Exit('');
  Inc(p, Length(Delimiter));
  Result := Copy(Self, p, Length(Self) - p + 1);
end;

function TNalaStringHelper.Before(constref Delimiter: String): String;
var
  p: UInt32;
begin
  p := Self.Pos(Delimiter);
  if (p = 0) then
    Exit('');
  Result := Copy(Self, 1, p - 1);
end;

{*
  Returns all positions of the given pattern/substring.
*}
function TNalaStringHelper.PosEx(constref SubStr: String): TIntArray;
var
  HitPos,LenSub,h,q,i: UInt32;
begin
  LenSub := Length(SubStr);
  if LenSub = 0 then Exit;
  HitPos := 1;
  h := 0;
  q := 1;
  SetLength(Result, q);
  for i:=1 to Length(Self) do
    if Self[i] <> SubStr[HitPos] then
      HitPos := 1
    else begin
      if (HitPos <> LenSub) then
        Inc(HitPos)
      else begin
        if q <= h then
        begin
          q := q+q;
          SetLength(Result, q);
        end;
        Result[h] := (i - HitPos) + 1;
        Inc(h);
        HitPos := 1;
      end;
    end;
  SetLength(Result, h);
end;

function TNalaStringHelper.Pos(constref SubStrings: TStringArray; Offset: Int32): Int32;
var
  p, i: Int32;
begin
  for i := Offset to High(SubStrings) do
  begin
    p := Self.Pos(SubStrings[i], Offset);
    if (p > 0) then
      Exit(p);
  end;
  Exit(0);
end;

{*
 Returns first position of the given pattern/substring from left.
*}
function TNalaStringHelper.Pos(constref SubStr: String; Offset: Int32): Int32;
var
  HitPos, LenSub, i: UInt32;
begin
  LenSub := Length(SubStr);
  if (LenSub = 0) then Exit(0);
  HitPos := 1;
  for i := Offset to Length(Self) do
  begin
    if Self[i] = SubStr[HitPos] then
    begin
      if (HitPos = LenSub) then
        Exit(i - LenSub + 1);
      Inc(HitPos);
    end else
      HitPos := 1;
  end;
  Exit(0);
end;

{*
 Returns first position of the given pattern/substring from right.
*}
function TNalaStringHelper.PosR(constref SubStr: String): Int32;
var
  HitPos, LenSub, i: UInt32;
begin
  LenSub := Length(SubStr);
  if (LenSub = 0) then Exit(0);
  HitPos := LenSub;
  for i := Length(Self) downto 1 do
  begin
    if Self[i] = SubStr[HitPos] then
    begin
      if (HitPos = 1) then
        Exit(i);
      Dec(HitPos);
    end else
      HitPos := LenSub;
  end;
  Exit(0);
end;

function TNalaStringHelper.Replace(constref SubStr, ReplaceStr: String; Flags: TReplaceFlags): String;
var
  Hi,HiSub,HiRep,i,j,k: Int32;
  Prev,Curr:Integer;
  Subs: TIntArray;
begin
  Hi := Length(Self);
  if Hi = 0 then Exit;

  HiRep := Length(ReplaceStr);
  HiSub := Length(SubStr);

  k := 1;
  Prev := 1;
  case (rfReplaceAll in Flags) and True of
  True:
    begin
      //If substring and replacement string is only 1 char, then it's simpler:
      if (HiRep = 1) and (HiSub = 1) then begin
        Result := Copy(Self,1,hi);
        for i:=1 to Length(Self) do
          if Self[i] = SubStr[1] then
            Result[i] := Self[i];
        Exit();
      end;

      case (rfIgnoreCase in Flags) of
        True: Subs := LowerCase(Self).PosEx(LowerCase(SubStr));
        False:Subs := Self.PosEx(SubStr);
      end;
      SetLength(Result, Hi + (Length(Subs) * (HiRep-HiSub)));
      if Length(Subs) = 0 then Exit(Copy(Self, 1,Hi));
      for i:=0 to High(Subs) do
      begin
        Curr := Subs[i];
        j := (Curr-Prev);
        Move(Self[Prev], Result[k], j);
        k := k + j;
        Move(ReplaceStr[1], Result[k], HiRep);
        k := k+HiRep;
        Prev := Curr + HiSub;
      end;
      Move(Self[Prev], Result[k], Hi-Prev+1);

    end;
  False:
    begin
      SetLength(Result, Hi + (HiRep-HiSub));
      case (rfIgnoreCase in flags) of
        True: Curr := LowerCase(Self).Pos(LowerCase(SubStr));
        False:Curr := Self.Pos(SubStr);
      end;
      if Curr = 0 then Exit(Copy(Self, 1,Hi));

      Move(Self[1], Result[1], Curr-1);
      Move(ReplaceStr[1], Result[Curr], HiRep);
      Move(Self[(Curr+HiSub)], Result[Curr+HiRep], Hi-(Curr+HiSub)+1);
      SetLength(Result, Hi+(HiRep-HiSub));
    end;
  end;
end;

function TNalaStringHelper.ExtractLetters: String;
var
  i: Int32;
begin
  Result := '';

  for i := 1 to High(Self) do
    if (Self[i] in ['a'..'z', 'A'..'Z']) then
      Result += Self[i];
end;

function TNalaStringHelper.ExtractNumbers: String;
var
  i: Int32;
begin
  Result := '';

  for i := 1 to High(Self) do
    if (Self[i] in ['0'..'9']) then
      Result += Self[i];
end;

function TNalaStringHelper.Extract(constref Chars: TCharArray): String;
var
  CharSet: set of Char;
  i: Int32;
begin
  Result := '';

  for i := 0 to High(Chars) do
    Include(CharSet, Chars[i]);
  for i := 1 to Length(Self) do
    if (Self[i] in CharSet) then
      Result += Self[i];
end;

function TNalaStringHelper.Upper: String;
begin
  Result := UpperCase(Self);
end;

function TNalaStringHelper.Lower: String;
begin
  Result := LowerCase(Self);
end;

function TNalaStringHelper.Trim: String;
begin
  Result := SysUtils.Trim(Self);
end;

function TNalaStringHelper.MD5: String;
begin
  Result := MD5Print(MD5String(Self));
end;

function TNalaStringHelper.SHA1: String;
begin
  Result := SHA1Print(SHA1String(Self));
end;

{*
 StrExplode lets you take a string and blow it up into smaller pieces, at each
 occurance of the given seperator `Sep`.
*}
function TNalaStringHelper.Explode(constref Delimiter: String): TStringArray;
var
  Subs:TIntArray;
  Hi,i,Curr,Prev,HiSep: UInt32;
begin
  Hi := Length(Self);
  if Hi = 0 then Exit;

  Subs := Self.PosEx(Delimiter);
  if Length(Subs) = 0 then
  begin
    SetLength(Result, 1);
    Result[0] := Copy(Self, 1,Hi);
    Exit;
  end;
  HiSep := Length(Delimiter);
  Prev := 1;
  SetLength(Result, Length(Subs));
  for i:=0 to High(Subs) do
  begin
    Curr := Subs[i];
    Result[i] := Copy(Self, Prev, (Curr-Prev));
    Prev := Curr+HiSep;
  end;
  if Prev <= Hi then
  begin
    SetLength(Result, Length(Subs)+1);
    Result[Length(Subs)] := Copy(Self, Prev, Hi);
  end;
end;


end.

