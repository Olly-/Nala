unit nala.DrawString;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Graphics, nala.Types;

type
  TDrawString = type String;

  TDrawString_Helper = type Helper for TDrawString
  public
    function Draw(X, Y: Int32; Canvas: TCanvas; Paint: Boolean = True): TPoint; // Returns width and height of text
  end;

implementation

function TDrawString_Helper.Draw(X, Y: Int32; Canvas: TCanvas; Paint: Boolean): TPoint;
var
  i: Int32;
  Arr: TStringArray;
  Start: TPoint;

   function Split: TStringArray;

      procedure Push(var Str: String; Default: String = '');
      var
        Len: Int32;
      begin
        Len := Length(Result);
        SetLength(Result, Len + 1);
        Result[Len] := Str;

        Str := Default;
      end;

   var
     i: Int32;
     Str: String;
   begin
     Str := '';

     for i := 1 to Length(Self) do
      case Char(Self[i]) of
        '[':
          begin
            if (i > 1) then
              Push(Str);
            Str := '[';
          end;
        ']':
          begin
            Str := Str + ']';
            Push(Str);
          end;
        else
          if (Self[i] = #10) then
          begin
            Push(Str, '[/n]');
            Push(Str);
          end else
            Str := Str + Self[i];
      end;

     if (Str <> '') then
       Push(Str);
   end;

   function ParseOption(constref Str: String): Boolean;
   begin
     Result := False;
     if (Length(Str) < 4) then
       Exit;

     if (Str[1] = '[') and (Str[Length(Str)] = ']') then
     begin
       case LowerCase(Copy(Str, 2, 2)) of
         'c=':
           try
             Canvas.Font.Color := StringToColor(Copy(Str, 4, Length(Str) - 4));
           except
             Exit(False);
           end;
         's=':
           try
             Canvas.Font.Size := StrToInt(Copy(Str, 4, Length(Str) - 4));
           except
             Exit(False);
           end;
         'h=':
           try
             Canvas.Font.Height := StrToInt(Copy(Str, 4, Length(Str) - 4));
           except
             Exit(False);
           end;
         '/n':
           begin
             X := Start.X;
             Inc(Y, Canvas.TextHeight('Fj'));
           end;
         'b+':
           Canvas.Font.Bold := True;
         'b-':
           Canvas.Font.Bold := False;
         'i+':
           Canvas.Font.Italic := True;
         'i-':
           Canvas.Font.Italic := False;
         'u+':
           Canvas.Font.Underline := True;
         'u-':
           Canvas.Font.Underline := False;
         else
           Exit(False);
       end;

       Exit(True);
     end;
   end;

begin
  Result := Point(0, 0);
  Start := Point(X, Y);
  Arr := Split();

  if (Length(Arr) > 0) then
  begin
    for i := 0 to High(Arr) do
      if (not ParseOption(Arr[i])) then
      begin
        if (Paint) then
          Canvas.TextOut(X, Y, Arr[i]);

        Inc(X, Canvas.TextWidth(Arr[i]) + 1);
        if (X > Result.X) then
          Result.X := X;
        if (Y > Result.Y) then
          Result.Y := Y;
      end;

    Inc(Result.Y, Canvas.TextHeight('Fj'));
    Dec(Result.X, Start.X);
    Dec(Result.Y, Start.Y);
  end;
end;

end.

