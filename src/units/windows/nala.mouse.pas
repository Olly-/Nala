unit nala.Mouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.MouseBase, nala.Window, nala.CoreTypes;

type

  { TNalaMouse }

  TNalaMouse = class(TNalaMouseBase)
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
  Windows;

const
  // mbLeft, mbRight, mbMiddle, mbScrollUp, mbScrollDown
  MouseButtonToVirtualKey: array[EMouseButton] of Int32 = (VK_LBUTTON, VK_RBUTTON, VK_MBUTTON, -1, -1);

{ TNalaMouse }

procedure TNalaMouse.SetPosition(NalaWindow: TNalaWindow; X, Y: Integer);
begin

end;

function TNalaMouse.GetPosition(NalaWindow: TNalaWindow): nala.CoreTypes.TPoint;
var
  pt: Windows.TPOINT;
begin
  Windows.GetCursorPos(pt);
  Result.X := pt.X;
  Result.Y := pt.Y;

  if (NalaWindow <> nil) then
  begin
    Dec(Result.X, NalaWindow.Left);
    Dec(Result.Y, NalaWindow.Top);
  end;
end;

procedure TNalaMouse.Hold(Button: EMouseButton);
begin

end;

procedure TNalaMouse.Release(Button: EMouseButton);
begin

end;

function TNalaMouse.Pressed(Button: EMouseButton): Boolean;
begin
  Result := (GetASyncKeyState(MouseButtonToVirtualKey[Button]) and $8000) <> 0;
end;

constructor TNalaMouse.Create;
begin

end;

destructor TNalaMouse.Destroy;
begin
  inherited Destroy;
end;

end.

