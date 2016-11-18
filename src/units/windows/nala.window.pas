unit nala.Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows,
  nala.WindowBase, nala.Types, nala.Bitmap, DwmApi;

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
  public
    function Data(X, Y, AWidth, AHeight: Int32): TWindowData; override;
    procedure BringToFront; override;

    procedure OffsetBorder(var X, Y: UInt32);

    function HandleIsVaild: Boolean; override;

    constructor Create;
    constructor Create(AWindowHandle: HWND); overload;
    destructor Destroy; override;
  end;

implementation

uses
  nala.OSUtils, JwaWindows;

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
  if (AValue <> FHandle) and (IsWindow(AValue)) then
  begin
    FHandle := AValue;
    FWindowDC := GetWindowDC(FHandle);
    GetWindowThreadProcessId(Handle, @FPID);

    FTopWindow := GetAncestor(FHandle, GA_ROOT);
    FIsTopWindow := FTopWindow = FHandle;

    if (FIsTopWindow) then
    begin
      Style := GetWindowLong(FHandle, GWL_STYLE);
      if ((Style and WS_CAPTION) = WS_CAPTION) then
        FCaptionSize := GetSystemMetrics(SM_CYCAPTION);
      if ((Style and WS_BORDER) = WS_BORDER) then
        FBorderSize := GetSystemMetrics(SM_CXFRAME);
    end;

    if (FBuffer <> nil) then
      FBuffer.Free;
  end;
end;

function TNalaWindow.getWidth: Int32;
begin
  Result := OSUtils.WindowWidth(FHandle);
  if (FIsTopWindow) then
    Result -= FBorderSize + FBorderSize;
end;

function TNalaWindow.getHeight: Int32;
begin
  Result := OSUtils.WindowHeight(FHandle);

  if (FIsTopWindow) then
  begin
    Result -= FBorderSize + FBorderSize;
    Result -= FCaptionSize;
  end;
end;

function TNalaWindow.getLeft: Int32;
var
  r: TRect;
begin
  if (Win32MajorVersion >= 6) and (DwmCompositionEnabled) then
    DwmGetWindowAttribute(FHandle, DWMWA_EXTENDED_FRAME_BOUNDS, @r, SizeOf(r))
  else
    GetWindowRect(FHandle, r);

  Result := r.Left;
end;

function TNalaWindow.getTop: Int32;
var
  r: TRect;
begin
  if (Win32MajorVersion >= 6) and (DwmCompositionEnabled) then
    DwmGetWindowAttribute(FHandle, DWMWA_EXTENDED_FRAME_BOUNDS, @r, SizeOf(r))
  else
    GetWindowRect(FHandle, r);

  Result := r.Top;
end;

function TNalaWindow.getCaption: String;
begin
  Result := OSUtils.WindowTitle(FHandle);
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

procedure TNalaWindow.OffsetBorder(var X, Y: UInt32);
begin
  X += FBorderSize;
  Y += FCaptionSize + FBorderSize;
end;

function TNalaWindow.HandleIsVaild: Boolean;
begin
  Result := IsWindow(FHandle);
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
var
  W, H: UInt32;
begin
  Result.Width := 0;
  Result.Height := 0;
  Result.BufferPtr := nil;
  Result.BufferWidth := 0;
  Result.BufferHeight := 0;

  if (not IsIconic(FHandle)) then
  begin
    W := Self.Width;
    H := Self.Height;

    if (FBuffer = nil) then
      FBuffer := TBuffer.Create(W, H);
    if (FBuffer.Width <> W) or (FBuffer.Height <> H) then
    begin
      FBuffer.Free;
      FBuffer := TBuffer.Create(W, H);
    end;

    if (FIsTopWindow) then
      BitBlt(FBuffer.DC, 0, 0, AWidth, AHeight, FWindowDC, FBorderSize + X, FCaptionSize + FBorderSize + Y, SRCCOPY)
    else
      BitBlt(FBuffer.DC, 0, 0, AWidth, AHeight, FWindowDC, X, Y, SRCCOPY);

    Result.Width := AWidth;
    Result.Height := AHeight;
    Result.BufferPtr := FBuffer.Ptr;
    Result.BufferWidth := FBuffer.Width;
    Result.BufferHeight := FBuffer.Height;
  end;
end;

end.

