unit nala.Helpers;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Graphics, Types,
  nala.Types;

type

  { TBitmapHelper }

  TBitmapHelper = class helper for TBitmap
  public
    function ToString: String;
    procedure LoadFromString(constref Str: String);
    procedure LoadFromPtr(Ptr: PRGB32; AWidth, AHeight: Integer);
  end;

  { TCanvasHelper }

  TCanvasHelper = class helper for TCanvas
  public
    procedure PrettyTextOut(X, Y: Integer; constref Str: String);
    function PrettyTextExtent(constref Str: String): TSize;
  end;

  TVariantHelper = type Helper for Variant
    function Assigned: Boolean;
  end;

implementation

uses
  fpjson, variants, GraphType, nala.Bitmap;

{ TCanvasHelper }

procedure TCanvasHelper.PrettyTextOut(X, Y: Integer; constref Str: String);
var
  i, w, Len, TextStart, Start: Integer;
  S: String;
begin
  Len := Length(Str);
  if (Len = 0) then
    Exit;

  w := 0;
  i := 1;
  TextStart := 1;
  while (i <= Len) do
  begin
    if (Str[i] = '{') then
    begin
      if (i > TextStart) then
      begin
        S := Copy(Str, TextStart, i - TextStart);
        TextOut(X + w, Y, S);
        w += TextWidth(S);
      end;

      Start := i;
      while (i <= Len) do
      begin
        if (Str[i] = '}') then
        begin
          if ((i - Start) > 1) then
            case Str[Start + 1] of
              'B': if (Str[Start + 2] = '+') then Font.Style := Font.Style + [fsBold] else Font.Style := Font.Style - [fsBold];
              'I': if (Str[Start + 2] = '+') then Font.Style := Font.Style + [fsItalic] else Font.Style := Font.Style - [fsItalic];
              'S': Font.Size := StrToInt(Copy(Str, Start + 3, (i - 3) - Start));
              'C': Font.Color := StringToColor(Copy(Str, Start + 3, (i - 3) - Start));
              'F': Font.Name := Copy(Str, Start + 3, (i - 3) - Start);
            end;
          Break;
        end;
        Inc(i);
      end;

      TextStart := i + 1;
    end;

    Inc(i);
  end;

  if (i > TextStart) then
  begin
    S := Copy(Str, TextStart, i - TextStart);
    TextOut(X + w, Y, S);
    w += TextWidth(S);
  end;
end;

function TCanvasHelper.PrettyTextExtent(constref Str: String): TSize;
var
  i, h, Len, TextStart, Start: Integer;
  S: String;
begin
  Result.cx := 0;
  Result.cy := 0;
  Len := Length(Str);
  if (Len = 0) then
    Exit;

  i := 1;
  TextStart := 1;
  while (i <= Len) do
  begin
    if (Str[i] = '{') then
    begin
      if (i > TextStart) then
      begin
        S := Copy(Str, TextStart, i - TextStart);
        //TextOut(X + w, Y, S);
        Result.cx += TextWidth(S);
        h := TextHeight(S);
        if (h > Result.cy) then
          Result.cy := h;
      end;

      Start := i;
      while (i <= Len) do
      begin
        if (Str[i] = '}') then
        begin
          if ((i - Start) > 1) then
            case Str[Start + 1] of
              'B': if (Str[Start + 2] = '+') then Font.Style := Font.Style + [fsBold] else Font.Style := Font.Style - [fsBold];
              'I': if (Str[Start + 2] = '+') then Font.Style := Font.Style + [fsItalic] else Font.Style := Font.Style - [fsItalic];
              'S': Font.Size := StrToInt(Copy(Str, Start + 3, (i - 3) - Start));
              'C': Font.Color := StringToColor(Copy(Str, Start + 3, (i - 3) - Start));
              'F': Font.Name := Copy(Str, Start + 3, (i - 3) - Start);
            end;
          Break;
        end;
        Inc(i);
      end;

      TextStart := i + 1;
    end;

    Inc(i);
  end;

  if (i > TextStart) then
  begin
    S := Copy(Str, TextStart, i - TextStart);
    //TextOut(X + w, Y, S);
    Result.cx += TextWidth(S);
    h := TextHeight(S);
    if (h > Result.cy) then
      Result.cy := h;
  end;
end;

{ TBitmapHelper }

function TBitmapHelper.ToString: String;
var
  Bitmap: TNalaBitmap;
begin
  Bitmap.Init;
  Bitmap.FromRawImage(Self.RawImage);
  Result := Bitmap.ToString;
end;

procedure TBitmapHelper.LoadFromString(constref Str: String);
var
  Bitmap: TNalaBitmap;
begin
  Bitmap.Init(Str);
  Self.LoadFromPtr(PRGB32(Bitmap.Data[0]), Bitmap.Width, Bitmap.Height);
end;

procedure TBitmapHelper.LoadFromPtr(Ptr: PRGB32; AWidth, AHeight: Integer);
var
  RawImage: TRawImage;
begin
  RawImage.Init;

  RawImage.Description.PaletteColorCount :=0;
  RawImage.Description.MaskBitsPerPixel :=0;
  RawImage.Description.Width := AWidth;
  RawImage.Description.Height := AHeight;

  RawImage.Description.Format := ricfRGBA;
  RawImage.Description.ByteOrder := riboLSBFirst;
  RawImage.Description.BitOrder := riboBitsInOrder;
  RawImage.Description.Depth := 24;
  RawImage.Description.BitsPerPixel := 32;
  RawImage.Description.LineOrder := riloTopToBottom;
  RawImage.Description.LineEnd := rileDWordBoundary;

  RawImage.Description.RedPrec := 8;
  RawImage.Description.GreenPrec := 8;
  RawImage.Description.BluePrec := 8;
  RawImage.Description.AlphaPrec := 0;

  RawImage.Description.RedShift := 16;
  RawImage.Description.GreenShift := 8;
  RawImage.Description.BlueShift := 0;

  RawImage.DataSize := RawImage.Description.Width * RawImage.Description.Height
                       * (RawImage.Description.BitsPerPixel shr 3);
  RawImage.Data := PByte(Ptr);

  LoadFromRawImage(RawImage, False);
end;

function TVariantHelper.Assigned: Boolean;
begin
  Result := TVarData(Self).VType <> varEmpty;
end;

end.

