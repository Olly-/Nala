uses
  Classes, SysUtils, nala.CoreTypes, LCLType;

type

  { TOSUtils }

  TOSUtils = record
  public
    function GetConsoleHandle: HWND;
    function GetDesktopHandle: HWND;
    function WindowFromPoint(Point: TPoint): HWND;
    function WindowWidth(AWindow: HWND): UInt32;
    function WindowHeight(AWindow: HWND): UInt32;
    function WindowTitle(AWindow: HWND): String;
    function WindowClass(AWindow: HWND): String;
    function MarkTime: Double;
  end;

var
  OSUtils: TOSUtils;
