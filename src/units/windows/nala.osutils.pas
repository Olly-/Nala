unit nala.OSUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

{$i OSUtils_Header.inc}

implementation

uses
  JwaWindows;


{ TOSUtils }

function TOSUtils.GetConsoleHandle: HWND;
begin
  Result := GetConsoleHandle;
end;

function TOSUtils.GetDesktopHandle: HWND;
begin
  Result := GetDesktopHandle;
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
  GetWindowRect(AWindow, r);
  Result := r.Right - r.Left;
end;

function TOSUtils.WindowHeight(AWindow: HWND): UInt32;
var
  r: TRect;
begin
  GetWindowRect(AWindow, r);
  Result := r.Bottom - r.Top;
end;

function TOSUtils.WindowTitle(AWindow: HWND): String;
var
  Arr: array[0..2047] of Char;
begin
  GetWindowText(AWindow, @Arr[0], 2048);
  Result := String(Arr);
end;

function TOSUtils.WindowClass(AWindow: HWND): String;
var
  Arr: array[0..2047] of Char;
begin
  GetClassName(AWindow, @Arr[0], 2048);
  Result := String(Arr);
end;

function TOSUtils.MarkTime: Double;
begin

end;

end.

