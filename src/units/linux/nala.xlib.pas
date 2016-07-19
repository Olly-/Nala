unit nala.XLib;

{$mode objfpc}{$H+}
{$linklib Xtst}

interface

uses
  Classes, SysUtils,
  x, xlib, ctypes, xatom;

type
  TXPointerEvent = record
    Display: PDisplay;
    Window: TWindow;
    Root: TWindow;
    Child: TWindow;
    Root_X: CInt;
    Root_Y: CInt;
    Win_X: CInt;
    Win_Y: CInt;
    Mask: CUint;
  end;

type
  TWindowArray = array[0..255] of TWindow;
  PWindowArray = ^TWindowArray;

function XTestFakeButtonEvent(Display: PDisplay; Button: DWord; IsPress: Boolean; Delay: DWord): LongInt; cdecl; external;
function XTestFakeMotionEvent(Display: PDisplay; Screen: LongInt; X: LongInt; Y: LongInt; Delay: DWord): LongInt; cdecl; external;

function GetDesktopHandle(Display: PDisplay): TWindow;
function GetWindowProperty(Display: PDisplay; AWindow: TWindow; Prop: TAtom; out Value: Int32): Boolean;
function HasWindowProperty(Display: PDisplay; AWindow: TWindow; AProperty: String): Boolean;

procedure ManageErrors(AWindow: TObject);
procedure UnManageErrors(AWindow: TObject);

implementation

uses
  nala.Dictionary, nala.Window;

function GetDesktopHandle(Display: PDisplay): TWindow;
begin
  Result := XDefaultRootWindow(Display);
end;

function GetWindowProperty(Display: PDisplay; AWindow: TWindow; Prop: TAtom; out Value: Int32): Boolean;
var
  Typ: TAtom;
  Format: Int32;
  Count, Bytes: UInt32;
  Ptr: PByte;
begin
  Result := (XGetWindowProperty(Display, AWindow, Prop, 0, 1, 0, AnyPropertyType, @Typ, @Format, @Count, @Bytes, @Ptr) = Success) and (Typ <> None) and (Ptr <> nil);
  if (Result) then
    Value := PInt32(Ptr)^
  else
    Value := -1;

  if (Ptr <> nil) then
    XFree(Ptr);
end;

function HasWindowProperty(Display: PDisplay; AWindow: TWindow; AProperty: String): Boolean;
var
  A: TAtom;
  Value: Int32;
begin
  A := XInternAtom(Display, PChar(AProperty), Boolean(1));
  Result := (A <> None) and (GetWindowProperty(Display, AWindow, A, Value)) and (Value > 0);
end;

{ Error Handling }

type
  TWindowDictionary = specialize TDictionary<PtrUInt, TNalaWindow>;

var
  WindowDictionary: TWindowDictionary;

procedure ManageErrors(AWindow: TObject);
begin
  WindowDictionary.Add(PtrUInt(TNalaWindow(AWindow).Display), TNalaWindow(AWindow));
end;

procedure UnManageErrors(AWindow: TObject);
begin
  WindowDictionary.Remove(PtrUInt(TNalaWindow(AWindow).Display));
end;

function XErrorHandler(Display: PDisplay; Event: PXErrorEvent): CInt; cdecl;

  function ErrorString(ErrorCode: Integer): String;
  const
    Arr: array[0..16] of String =
      ('Request', 'Value', 'Window', 'Pixmap', 'Atom', 'Cursor', 'Font', 'Match',
       'Drawable', 'Access', 'Alloc', 'Color', 'GC', 'IDChoice', 'Name', 'Length',
       'Implementation');
  begin
    if (ErrorCode >= 1) and (ErrorCode <= 17) then
      Exit('Bad ' + Arr[ErrorCode - 1])
    else
      Exit('Unknown: ' + IntToStr(ErrorCode));
  end;

var
  Window: TNalaWindow;
begin
  Result := 0;

  if (WindowDictionary.Get(PtrUInt(Display), Window)) then
  begin
    Window.ErrorCount := Window.ErrorCount + 1;
    Writeln('XError(', ErrorString(Event^.Error_Code), ') on Window: ', Window.Handle);
  end else
    Writeln('XError(', ErrorString(Event^.Error_Code), ') on Display: ', HexStr(Display));
end;

initialization
  WindowDictionary := TWindowDictionary.Create(@HashPointer);
  XSetErrorHandler(@XErrorHandler);

finalization
  XSetErrorHandler(nil);
  WindowDictionary.Free;

end.

