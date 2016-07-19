unit nala.Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows,
  nala.WindowBase, nala.CoreTypes, nala.Bitmap;

type

  { TNalaWindow }

  TNalaWindow = class(TNalaWindowBase)
  private
    type
      TBuffer = class(TObject)
      public
        Ptr: PRGB32;
        Width, Height: UInt32;
        DC: HDC;
        Bitmap: HBitmap;

        constructor Create(AWidth, AHeight: Integer);
        destructor Destroy; override;
      end;
    var
      FBuffer: TBuffer;
  protected
    procedure setHandle(AValue: HWND); override;

    function getWidth: Int32; override;
    function getHeight: Int32; override;
    function getLeft: Int32; override;
    function getTop: Int32; override;

    function getCaption: String; override;
    procedure setCaption(AValue: String); override;
    function getShowing: Boolean; override;
    procedure setShowing(AValue: Boolean); override;
  private
    FCaptionSize, FBorderSize: UInt32;
    FWindowDC: HDC;

    procedure ApplyBorderOffset(var X, Y: Int32);
  public
    function Data(X, Y, AWidth, AHeight: Int32): TWindowData; override;
    procedure BringToFront; override;

    function HandleIsVaild: Boolean; override;

    constructor Create;
    constructor Create(AWindowHandle: HWND); overload;
    destructor Destroy; override;
  end;

implementation

uses
  nala.OSUtils;

constructor TNalaWindow.TBuffer.Create(AWidth, AHeight: Integer);
var
  bi: Windows.BITMAPINFO;
begin
  inherited Create;

  ZeroMemory(@bi, SizeOf(BITMAPINFO));
  bi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
  bi.bmiHeader.biWidth := AWidth;
  bi.bmiHeader.biHeight := -AHeight;
  bi.bmiHeader.biPlanes := 1;
  bi.bmiHeader.biBitCount := 32;
  bi.bmiHeader.biCompression := BI_RGB;
  bi.bmiHeader.biSizeImage := 0;
  bi.bmiHeader.biClrUsed := 0;

  DC := CreateCompatibleDC(0);
  Bitmap := Windows.CreateDIBSection(DC, bi, DIB_RGB_COLORS, Ptr, 0, 0);
  Width := AWidth;
  Height := AHeight;

  SelectObject(DC, Bitmap);
end;

destructor TNalaWindow.TBuffer.Destroy;
begin
  DeleteObject(DC);
  DeleteObject(Bitmap);

  inherited Destroy;
end;

{ TNalaWindow }

procedure TNalaWindow.setHandle(AValue: HWND);
var
  Style: LONG;
begin
  if (FHandle <> AValue) then
  begin
    FHandle := AValue;
    FWindowDC := GetWindowDC(FHandle);

    FCaptionSize := 0;
    FBorderSize := 0;

    Style := GetWindowLong(FHandle, GWL_STYLE);
    if ((Style and WS_CAPTION) = WS_CAPTION) then
      FCaptionSize := GetSystemMetrics(SM_CYCAPTION);
    if ((Style and WS_BORDER) = WS_BORDER) then
      FBorderSize := GetSystemMetrics(SM_CXFRAME);

    if (Assigned(FBuffer)) then
      FreeAndNil(FBuffer);
  end;
end;

function TNalaWindow.getWidth: Int32;
var
  r: TRect;
begin
  GetWindowRect(FHandle, r);
  Result := r.Right - r.Left;
  Result -= FBorderSize + FBorderSize; // Left and right
end;

function TNalaWindow.getHeight: Int32;
var
  r: TRect;
begin
  GetWindowRect(FHandle, r);
  Result := r.Bottom - r.Top;
  Result -= FBorderSize + FBorderSize; // Top and bottom
  Result -= FCaptionSize;
end;

function TNalaWindow.getLeft: Int32;
var
  r: TRect;
begin
  GetWindowRect(FHandle, r);
  Result := r.Left;
end;

function TNalaWindow.getTop: Int32;
var
  r: TRect;
begin
  GetWindowRect(FHandle, r);
  Result := r.Top;
end;

function TNalaWindow.getCaption: String;
var
  Buffer: array[0..2048] of Char;
begin
  Result := '';
  if (GetWindowText(FHandle, @Buffer[0], SizeOf(Buffer)) > 0) then
    Result := String(Buffer);
end;

procedure TNalaWindow.setCaption(AValue: String);
begin
  SetWindowText(FHandle, PChar(AValue));
end;

function TNalaWindow.getShowing: Boolean;
begin
  Result := (GetWindowLong(FHandle, GWL_STYLE) and WS_VISIBLE) = WS_VISIBLE;
end;

procedure TNalaWindow.setShowing(AValue: Boolean);
begin
  if (AValue) then
    ShowWindow(FHandle, SW_SHOW)
  else
    ShowWindow(FHandle, SW_HIDE);
end;

procedure TNalaWindow.BringToFront;
begin
  BringWindowToTop(FHandle);
  SetFocus(FHandle);
end;

function TNalaWindow.HandleIsVaild: Boolean;
begin
  Result := IsWindow(FHandle);
end;

procedure TNalaWindow.ApplyBorderOffset(var X, Y: Int32);
begin
  X += FBorderSize;
  Y += FCaptionSize + FBorderSize;
end;

constructor TNalaWindow.Create;
begin
  inherited Create;

  FBuffer := nil;
  FHandle := 0;
  FWindowDC := 0;
end;

constructor TNalaWindow.Create(AWindowHandle: HWND);
begin
  inherited Create;

  setHandle(AWindowHandle);
end;

destructor TNalaWindow.Destroy;
begin
  FBuffer.Free;

  inherited Destroy;
end;

function TNalaWindow.Data(X, Y, AWidth, AHeight: Int32): TWindowData;

  procedure FixAndWarn(W, H: Int32);
  const
    Message = '%s (%d) is outside the windows bounds!';
  begin
    if (X < 0) then
    begin
      X := 0;
      Writeln(Format(Message, ['X', X]));
    end;
    if (Y < 0) then
    begin
      Y := 0;
      Writeln(Format(Message, ['Y', Y]));
    end;
    if (AWidth > W) then
    begin
      AWidth := W;
      Writeln(Format(Message, ['Width', AWidth]));
    end;
    if (AHeight > H) then
    begin
      AHeight := H;
      Writeln(Format(Message, ['Height', AHeight]));
    end;
  end;

var
  W, H: UInt32;
begin
  W := Self.Width;
  H := Self.Height;

  if (not Assigned(FBuffer)) then
    FBuffer := TBuffer.Create(W, H);
  if (FBuffer.Width <> W) or (FBuffer.Height <> H) then
  begin
    FBuffer.Free;
    FBuffer := TBuffer.Create(W, H);
  end;

  FixAndWarn(W, H);
  ApplyBorderOffset(X, Y);

  BitBlt(FBuffer.DC, 0, 0, AWidth, AHeight, FWindowDC, X, Y, SRCCOPY);

  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.BufferPtr := FBuffer.Ptr;
  Result.BufferWidth := FBuffer.Width;
  Result.BufferHeight := FBuffer.Height;

  Writeln('woo');
end;

end.

