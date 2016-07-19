unit nala.OSUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

{$i OSUtils_Header.inc}

implementation

uses
  Windows, JwaWindows, DwmApi;


{ TOSUtils }

function TOSUtils.GetConsoleHandle: HWND;
begin
  Result := JwaWindows.GetConsoleWindow;
end;

function TOSUtils.GetDesktopHandle: HWND;
begin
  Result := JwaWindows.GetDesktopWindow;
end;

function TOSUtils.WindowFromPoint(Point: nala.CoreTypes.TPoint): HWND;
var
  pt: JwaWindows.POINT;
begin
  pt.X := Point.X;
  pt.Y := Point.Y;
  Result := JwaWindows.WindowFromPoint(pt);
end;

function TOSUtils.WindowWidth(AWindow: HWND): UInt32;
var
  r: TRect;
begin
  if (Win32MajorVersion >= 6) and (DwmCompositionEnabled) then
    DwmGetWindowAttribute(AWindow, DWMWA_EXTENDED_FRAME_BOUNDS, @r, SizeOf(r))
  else
    GetWindowRect(AWindow, r);

  Result := r.Right - r.Left;
end;

function TOSUtils.WindowHeight(AWindow: HWND): UInt32;
var
  r: TRect;
begin
  if (Win32MajorVersion >= 6) and (DwmCompositionEnabled) then
    DwmGetWindowAttribute(AWindow, DWMWA_EXTENDED_FRAME_BOUNDS, @r, SizeOf(r))
  else
    GetWindowRect(AWindow, r);

  Result := r.Bottom - r.Top;
end;

function TOSUtils.WindowTitle(AWindow: HWND): String;
var
  Arr: array[0..2047] of Char;
begin
  GetWindowText(AWindow, @Arr[0], SizeOf(Arr));
  Result := String(Arr);
end;

function TOSUtils.WindowClass(AWindow: HWND): String;
var
  Arr: array[0..2047] of Char;
begin
  GetClassName(AWindow, @Arr[0], SizeOf(Arr));
  Result := String(Arr);
end;

function TOSUtils.MarkTime: Double;
var
  Frequency, Count: Int64;
begin
  Windows.QueryPerformanceFrequency(Frequency);
  Windows.QueryPerformanceCounter(Count);
  Result := Count / Frequency * 1000;
end;

end.

