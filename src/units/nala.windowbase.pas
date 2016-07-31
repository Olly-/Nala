unit nala.WindowBase;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, LCLType,
  nala.Types, fgl, nala.WindowInfo;

(*
  Based on how Lazarus forms work:

    - Client area is the inside area of window excluding the title bar and window frame

    Window.Left = The windows left point, including the window frame
    Window.Right = The windows right point, including the title bar
    Window.Width = The width of the client area
    Window.Height = the height of the client area
*)

type

  { TNalaWindowBase }

  TNalaWindowBase = class(TObject)
  protected
    FHandle: HWND;
    FPID: UInt32;
    FIsTopWindow: Boolean;
    FTopWindow: HWND;

    procedure setHandle(AValue: HWND); virtual;

    function getWidth: Int32; virtual;
    function getHeight: Int32; virtual;
    function getLeft: Int32; virtual;
    function getTop: Int32; virtual;

    function getCaption: String; virtual;
    procedure setCaption(AValue: String); virtual;

    function getShowing: Boolean; virtual;
    procedure setShowing(AValue: Boolean); virtual;
  public
    property Handle: HWND read FHandle write setHandle;
    property PID: UInt32 read FPID;

    property Width: Int32 read getWidth;
    property Height: Int32 read getHeight;
    property Left: Int32 read getLeft;
    property Top: Int32 read getTop;

    property Caption: String read getCaption write setCaption;
    property Showing: Boolean read getShowing write setShowing;

    property IsTopWindow: Boolean read FIsTopWindow;
    property TopWindow: HWND read FTopWindow;

    function HandleIsVaild: Boolean; virtual;

    procedure BringToFront; virtual;

    function Data(X, Y, AWidth, AHeight: Int32): TWindowData; virtual;
    function Data: TWindowData; overload;

    function GetChildren(Recursive: Boolean = False): TWindowList;
  end;

implementation

uses
  nala.TBox;

{ TNalaWindowBase }

procedure TNalaWindowBase.setCaption(AValue: String);
begin
  raise Exception.Create('TNalaWindow.setCaption: Not supported');
end;

procedure TNalaWindowBase.setShowing(AValue: Boolean);
begin
  raise Exception.Create('TNalaWindow.setShowing: Not supported');
end;

function TNalaWindowBase.HandleIsVaild: Boolean;
begin
  raise Exception.Create('TNalaWindow.HandleIsVaild: Not supported');
end;

function TNalaWindowBase.GetChildren(Recursive: Boolean): TWindowList;
begin
  raise Exception.Create('TNalaWindow.GetChildren: Not supported');
end;

procedure TNalaWindowBase.setHandle(AValue: HWND);
begin
  raise Exception.Create('TNalaWindow.setHandle: Not supported');
end;

function TNalaWindowBase.getWidth: Int32;
begin
  raise Exception.Create('TNalaWindow.setWidth: Not supported');
end;

function TNalaWindowBase.getHeight: Int32;
begin
  raise Exception.Create('TNalaWindow.setHeight: Not supported');
end;

function TNalaWindowBase.getLeft: Int32;
begin
  raise Exception.Create('TNalaWindow.setLeft: Not supported');
end;

function TNalaWindowBase.getTop: Int32;
begin
  raise Exception.Create('TNalaWindow.setTop: Not supported');
end;

function TNalaWindowBase.getCaption: String;
begin
  raise Exception.Create('TNalaWindow.getCaption: Not supported');
end;

function TNalaWindowBase.getShowing: Boolean;
begin
  raise Exception.Create('TNalaWindow.getShowing: Not supported');
end;

procedure TNalaWindowBase.BringToFront;
begin
  raise Exception.Create('TNalaWindow.BringToFront: Not supported');
end;

function TNalaWindowBase.Data(X, Y, AWidth, AHeight: Int32): TWindowData;
begin
  raise Exception.Create('TNalaWindow.Data: Not supported');
end;

function TNalaWindowBase.Data: TWindowData;
begin
  Result := Data(0, 0, Self.Width, Self.Height);
end;

end.

