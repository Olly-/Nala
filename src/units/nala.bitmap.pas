unit nala.Bitmap;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, GraphType, Graphics, FPWritePNG, FPReadPNG, FPimage, IntfGraphics, base64,
  nala.Types;

type

  { TNalaBitmap }

  TNalaBitmap = record
  private
    FData: TRGB32Array;
    FDataLength: UInt32;
    FWidth, FHeight: UInt32;
  public
    property Data: TRGB32Array read FData write FData;
    property Width: UInt32 read FWidth;
    property Height: UInt32 read FHeight;

    procedure SetSize(AWidth, AHeight: UInt32);

    function ToString: String;
    function ToRawImage: TRawImage;

    procedure FromWindow(NalaWindow: TObject);
    procedure FromWindow(NalaWindow: TObject; constref Area: TBox); overload;

    procedure FromString(AString: String);
    procedure FromRawImage(constref RawImage: TRawImage);
    procedure FromFile(APath: String);

    procedure Save(APath: String);

    function Thumbnail(NewWidth, NewHeight: UInt32): TNalaBitmap;

    procedure SetPixel(X, Y: UInt32; Color: TColor);
    function GetPixel(X, Y: UInt32): TColor;

    procedure Fill(Color: TColor);

    procedure Init;
    procedure Init(AWidth, AHeight: UInt32); overload;
    procedure Init(AString: String); overload;
  end;

implementation

uses
  Math,
  nala.ColorMath, nala.TBox, nala.Window;

{ TNalaBitmap }

function TNalaBitmap.ToRawImage: TRawImage;
begin
  Result.Init;
  Result.Description.Init_BPP32_B8G8R8_BIO_TTB(FWidth, FHeight);
  Result.DataSize := Result.Description.Width * Result.Description.Height * (Result.Description.BitsPerPixel shr 3);
  Result.Data := PByte(FData);
end;

procedure TNalaBitmap.FromRawImage(constref RawImage: TRawImage);
begin
  SetSize(RawImage.Description.Width, RawImage.Description.Height);
  Move(RawImage.Data[0], FData[0], FDataLength * SizeOf(TRGB32));
end;

procedure TNalaBitmap.SetSize(AWidth, AHeight: UInt32);
var
  NewLength: UInt32;
  NewData: TRGB32Array;
  Y, W, H: Integer;
begin
  NewLength := AWidth * AHeight;
  if (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    if (NewLength = 0) then
      SetLength(FData, 0)
    else
    begin
      SetLength(NewData, NewLength);

      // Move old data to new data
      if (FDataLength > 0) then
      begin
        W := Min(AWidth, FWidth);
        H := Min(AHeight, FHeight);

        for Y := 0 to H - 1 do
          Move(FData[Y * FWidth], NewData[Y * AWidth], W * SizeOf(TRGB32));

        SetLength(FData, 0);
      end;

      FData := NewData;
    end;

    FWidth := AWidth;
    FHeight := AHeight;
    FDataLength := NewLength;
  end;
end;

procedure TNalaBitmap.SetPixel(X, Y: UInt32; Color: TColor);
begin
  FData[Y * FWidth + X] := RGBToBGR(Color);
end;

function TNalaBitmap.GetPixel(X, Y: UInt32): TColor;
begin
  Result := BGRToRGB(FData[Y * FWidth + X]);
end;

procedure TNalaBitmap.Fill(Color: TColor);
begin
  FillDWord(FData[0], FDataLength, UInt32(RGBToBGR(Color)));
end;

procedure TNalaBitmap.FromWindow(NalaWindow: TObject);
begin
  Self.FromWindow(NalaWindow, Box(0, 0, (NalaWindow as TNalaWindow).Width, (NalaWindow as TNalaWindow).Height));
end;

procedure TNalaBitmap.FromWindow(NalaWindow: TObject; constref Area: TBox);
var
  WindowData: TWindowData;
  Y: Integer;
begin
  WindowData := (NalaWindow as TNalaWindow).Data(Area.X1, Area.Y1, Area.Width - 1, Area.Height - 1);
  SetSize(WindowData.Width, WindowData.Height);

  for Y := 0 to WindowData.Height -1  do
    Move(WindowData.BufferPtr[(Y + Area.Y1) * WindowData.BufferWidth + Area.X1], FData[Y * WindowData.Width], WindowData.Width * SizeOf(TRGB32));
end;

procedure TNalaBitmap.FromString(AString: String);
var
  Stream: TStringStream;
  LazImg: TLazIntfImage;
  RawImg: TRawImage;
  Reader: TFPCustomImageReader;
begin
  Stream := TStringStream.Create(DecodeStringBase64(AString));
  Reader := TFPReaderPNG.Create;
  RawImg := Self.ToRawImage; // Correct bitmap format etc

  LazImg := TLazIntfImage.Create(RawImg, True);
  LazImg.LoadFromStream(Stream, Reader);
  LazImg.GetRawImage(RawImg);

  Self.FromRawImage(RawImg);
  LazImg.Free;
  Reader.Free;
  Stream.Free;
end;

function TNalaBitmap.ToString: String;
var
  Stream: TStringStream;
  LazImg: TLazIntfImage;
  Writer: TFPCustomImageWriter;
begin
  try
    LazImg := TLazIntfImage.Create(Self.ToRawImage, False);
    Writer := TFPWriterPNG.Create;
    Stream := TStringStream.Create('');
    LazImg.SaveToStream(Stream, Writer);
    Result := EncodeStringBase64(Stream.DataString);
  finally
    LazImg.Free;
  end;
end;

procedure TNalaBitmap.FromFile(APath: String);
var
  DataDesc: TRawImageDescription;
  RawImage: TRawImage;
begin
  with TLazIntfImage.Create(0, 0) do
  try
    DataDesc.Init_BPP32_B8G8R8_BIO_TTB(0, 0);
    DataDescription := DataDesc;
    LoadFromFile(APath);
    GetRawImage(RawImage);

    Self.FromRawImage(RawImage);
  finally
    Free;
  end;
end;

// Magical right? any format ;)
procedure TNalaBitmap.Save(APath: String);
begin
  if (FDataLength = 0) then
    Exit;

  with TLazIntfImage.Create(ToRawImage, False) do
  try
    SaveToFile(APath);
  finally
    Free;
  end;
end;

procedure TNalaBitmap.Init;
begin
  FDataLength := 0;
  SetLength(FData, 0);
  FWidth := 0;
  FHeight := 0;
end;

procedure TNalaBitmap.Init(AWidth, AHeight: UInt32);
begin
  Init;
  SetSize(AWidth, AHeight);
end;

procedure TNalaBitmap.Init(AString: String);
begin
  Init;
  FromString(AString);
end;

function TNalaBitmap.Thumbnail(NewWidth, NewHeight: UInt32): TNalaBitmap;
var
  w, h, x, y, rx, ry, dx, dy: Int32;
  xx,r,g,b,n: Int32;
  wr, hr: Double;
  ResultData, rowPtr, Ptr: PRGB32;
  offset: PtrUInt;
begin
  Result.Init(NewWidth, NewHeight);
  ResultData := @Result.FData[0];

  wr := FWidth / NewWidth;
  hr := FHeight / NewHeight;
  w := FWidth - 1;
  h := FHeight - 1;

  for y:=0 to NewHeight - 1 do
  begin
    ry := Trunc(y*hr);
    dy := Min(ry+Trunc(hr), h);
    for x:=0 to NewWidth - 1 do
    begin
      rx := Trunc(x*wr);
      dx := Min(rx+Trunc(wr), w);
      r := 0;
      g := 0;
      b := 0;
      n := 0;
      rowPtr := @FData[ry * FWidth];
      offset := PtrUInt(@FData[dy * FWidth]);
      while PtrUInt(rowPtr) <= offset do
      begin
        for xx:=rx to dx do
        begin
          R += rowPtr[xx].R;
          G += rowPtr[xx].G;
          B += rowPtr[xx].B;
          Inc(n);
        end;
        Inc(rowPtr, FWidth);
      end;
      r := r div n;
      g := g div n;
      b := b div n;

      Ptr := @ResultData[y * NewWidth + x];
      Ptr^.R := r;
      Ptr^.G := g;
      Ptr^.B := b;
    end;
  end;
end;

end.

