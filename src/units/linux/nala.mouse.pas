unit nala.Mouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xlib,
  nala.MouseBase, nala.Types, nala.XLib, nala.Window;

type

  { TNalaMouse }

  TNalaMouse = class(TNalaMouseBase)
  private
    FDisplay: PDisplay;
  public
    procedure SetPosition(NalaWindow: TNalaWindow; X, Y: Integer); override;
    function GetPosition(NalaWindow: TNalaWindow): TPoint; override;

    procedure Hold(Button: EMouseButton); override;
    procedure Release(Button: EMouseButton); override;

    function Pressed(Button: EMouseButton): Boolean; override;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  ctypes, x;

const
  MouseButtonToXButton: array[EMouseButton] of Int32 = (Button1, Button3, Button2, Button4, Button5);
  MouseButtonToXMask: array[EMouseButton] of Int32 = (Button1Mask, Button3Mask, Button2Mask, Button4Mask, Button5Mask);

{ TNalaMouse }

procedure TNalaMouse.SetPosition(NalaWindow: TNalaWindow; X, Y: Integer);
begin
  if (NalaWindow <> nil) then
  begin
    Inc(X, NalaWindow.Left);
    Inc(Y, NalaWindow.Top);
  end;

  XTestFakeMotionEvent(FDisplay, 0, X, Y, 0);
  XFlush(FDisplay);
end;

function TNalaMouse.GetPosition(NalaWindow: TNalaWindow): TPoint;
var
  Event: TXPointerEvent;
begin
  Event := Default(TXPointerEvent);
  XQueryPointer(FDisplay, GetDesktopHandle(FDisplay), @Event.Root, @Event.Window, @Event.Root_X,
                @Event.Root_Y, @Result.X, @Result.Y, @Event.Mask);

  if (NalaWindow <> nil) then
  begin
    Dec(Result.X, NalaWindow.Left);
    Dec(Result.Y, NalaWindow.Top);
  end;
end;

procedure TNalaMouse.Hold(Button: EMouseButton);
begin
  XTestFakeButtonEvent(FDisplay, MouseButtonToXButton[Button], True, 0);
  XFlush(FDisplay);
end;

procedure TNalaMouse.Release(Button: EMouseButton);
begin
  XTestFakeButtonEvent(FDisplay, MouseButtonToXButton[Button], False, 0);
  XFlush(FDisplay);
end;

function TNalaMouse.Pressed(Button: EMouseButton): Boolean;
var
  Event: TXPointerEvent;
begin
  Event := Default(TXPointerEvent);
  XQueryPointer(FDisplay, GetDesktopHandle(FDisplay), @Event.Root, @Event.Window,
                @Event.Root_X, @Event.Root_Y, @Event.Win_X, @Event.Win_Y, @Event.Mask);

  Result := (Event.Mask and MouseButtonToXMask[Button]) > 0;
end;

constructor TNalaMouse.Create;
begin
  FDisplay := XOpenDisplay(nil);
  if (FDisplay = nil) then
    raise Exception.Create('NalaMouse.Create: Failed to open display');
end;

destructor TNalaMouse.Destroy;
begin
  XCloseDisplay(FDisplay);

  inherited Destroy;
end;

end.

