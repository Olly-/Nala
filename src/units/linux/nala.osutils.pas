unit nala.OSUtils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

{$i OSUtils_Header.inc}

implementation

uses
  BaseUnix, Unix, xlib, x, xutil,
  nala.XLib;

var
  Display: PDisplay;

{ TOSUtils }

function TOSUtils.GetConsoleHandle: HWND;
begin
  Result := 0;
end;

function TOSUtils.GetDesktopHandle: HWND;
begin
  Result := nala.XLib.GetDesktopHandle(Display);
end;

function TOSUtils.MarkTime: Double;
var
  TV: TTimeVal;
  TZ: TTimeZone;
begin
  fpGetTimeOfDay(@TV, @TZ);
  Result := (Int64(TV.TV_Sec) * 1000000 + Int64(TV.TV_USec)) / 1000;
end;

function TOSUtils.WindowFromPoint(Point: TPoint): HWND;
var
  Event: TXPointerEvent;
  Window: TWindow;
begin
  Event := Default(TXPointerEvent);
  XQueryPointer(Display, Self.GetDesktopHandle(), @Event.Root, @Result,
                @Event.Root_X, @Event.Root_Y, @Event.Win_X, @Event.Win_Y, @Event.Mask);

  Window := Result;
  while (Window <> 0) do
  begin
    Result := Window;
    XQueryPointer(Display, Result, @Event.Root, @Window,
                  @Event.Root_X, @Event.Root_Y, @Event.Win_X, @Event.Win_Y, @Event.Mask);
  end;
end;

function TOSUtils.WindowWidth(AWindow: HWND): UInt32;
var
  Attri: TXWindowAttributes;
  Child: TWindow;
begin
  Attri := Default(TXWindowAttributes);
  XGetWindowAttributes(Display, AWindow, @Attri);

  Result := Attri.Width;
end;

function TOSUtils.WindowHeight(AWindow: HWND): UInt32;
var
  Attri: TXWindowAttributes;
  Child: TWindow;
begin
  Attri := Default(TXWindowAttributes);
  XGetWindowAttributes(Display, AWindow, @Attri);

  Result := Attri.Height;
end;

function TOSUtils.WindowTitle(AWindow: HWND): String;
var
  Text: PChar = nil;
begin
  Result := '';
  XFetchName(Display, AWindow, @Text);
  Result := Text;
  if (Text <> nil) then
    XFree(Text);
end;

function TOSUtils.WindowClass(AWindow: HWND): String;
var
  Struct: TXClassHint;
begin
  Result := '';

  if (XGetClassHint(Display, AWindow, @Struct) <> 0) then
  begin
    Result := Struct.res_class;

    XFree(Struct.res_name);
    XFree(Struct.res_class);
  end;
end;

{
function TOSUtils.KeyPressed(Key: TVirtualKey): Boolean;
var
  Event: TXButtonEvent;
  Map: PCharArr32;
  Code: TKeyCode;
begin
  XQueryKeyMap(FDisplay, Map);
  Code := XKeysymToKeycode(FDisplay, VirtualKeyToXKey(Key));
  Result := Ord(Map[Code div 8]) and (1 shl (Code mod 8)) > 0;
end;

procedure TOSUtils.SetWindowTitle(Window: HWND; Title: String);
var
  Text: TXTextProperty;
begin
  Text.Value := PCUChar(Title);
  Text.Encoding := XA_STRING;
  Text.Format := 8;
  Text.NItems := Length(Title);

  XSetWMName(FDisplay, Window, @Text);
  XFlush(FDisplay);
end;
}

initialization
  Display := XOpenDisplay(nil);
  if (Display = nil) then
    raise Exception.Create('Failed to open display for OSUtils');

finalization
  XCloseDisplay(Display);

end.

