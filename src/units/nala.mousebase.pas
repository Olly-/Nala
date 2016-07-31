unit nala.MouseBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.Types, nala.TPoint, nala.Window;

type
  EMouseButton = (mbLeft, mbRight, mbMiddle, mbScrollUp, mbScrollDown);

  { TNalaMouseBase }

  TNalaMouseBase = class(TObject)
  public
    procedure Hold(Button: EMouseButton); virtual;
    procedure Release(Button: EMouseButton); virtual;

    function Pressed(Button: EMouseButton): Boolean; virtual;

    procedure Click(Button: EMouseButton; ClickTime: UInt32 = 0);
    procedure Scroll(Down: Boolean; Scrolls: UInt32 = 1);

    procedure SetPosition(NalaWindow: TNalaWindow; X, Y: Integer); virtual;
    procedure SetPosition(NalaWindow: TNalaWindow; Point: TPoint); virtual; overload;
    function GetPosition(NalaWindow: TNalaWindow): TPoint; virtual;
  end;

implementation

{ TNalaMouse }

procedure TNalaMouseBase.Hold(Button: EMouseButton);
begin
  raise Exception.Create('TNalaMouse.Hold is not supported');
end;

procedure TNalaMouseBase.Release(Button: EMouseButton);
begin
  raise Exception.Create('TNalaMouse.Release is not supported');
end;

function TNalaMouseBase.Pressed(Button: EMouseButton): Boolean;
begin
  raise Exception.Create('TNalaMouse.Pressed is not supported');
end;

procedure TNalaMouseBase.Click(Button: EMouseButton; ClickTime: UInt32);
begin
  Hold(Button);
  Sleep(ClickTime);
  Release(Button);
end;

procedure TNalaMouseBase.Scroll(Down: Boolean; Scrolls: UInt32);
var
  i: Int32;
begin
  for i := 1 to Scrolls do
  begin
    if (Down) then
    begin
      Hold(mbScrollDown);
      Release(mbScrollDown);
    end else
    begin
      Hold(mbScrollUp);
      Release(mbScrollUp);
    end;

    Sleep(40 + Random(100));
  end;
end;

procedure TNalaMouseBase.SetPosition(NalaWindow: TNalaWindow; X, Y: Integer);
begin
  raise Exception.Create('TNalaMouse.SetPosition is not supported');
end;

procedure TNalaMouseBase.SetPosition(NalaWindow: TNalaWindow; Point: TPoint);
begin
  SetPosition(NalaWindow, Point.X, Point.Y);
end;

function TNalaMouseBase.GetPosition(NalaWindow: TNalaWindow): TPoint;
begin
  raise Exception.Create('TNalaMouse.GetPosition is not supported');
end;

end.

