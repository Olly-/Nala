unit nala.Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType,
  nala.WindowBase, nala.Types, nala.XLib, nala.Bitmap,
  x, xlib;

type

  { TNalaWindow }

  TNalaWindow = class(TNalaWindowBase)
  private
    FErrorCount: UInt32;
    FBuffer: TNalaBitmap;
    FDisplay: PDisplay;
  protected
    procedure setHandle(AValue: HWND); override;

    function getWidth: Int32; override;
    function getHeight: Int32; override;
    function getLeft: Int32; override;
    function getTop: Int32; override;

    procedure setShowing(AValue: Boolean); override;

    function getCaption: String; override;
  public
    property ErrorCount: UInt32 read FErrorCount write FErrorCount;
    property Display: PDisplay read FDisplay;

    function Data(X, Y, AWidth, AHeight: Int32): TWindowData; override;
    function HandleIsVaild: Boolean; override;

    //function GetChildren(Recursive: Boolean = False): TWindowList;

    constructor Create(AWindowHandle: HWND = 0);
    destructor Destroy; override;
  end;

implementation

uses
  nala.OSUtils, ctypes;

{ TNalaWindow }

procedure TNalaWindow.setHandle(AValue: HWND);

  // Check if first child is the client window, since that stores the caption and such...
  procedure CheckClientWindow(var Handle: HWND);
  var
    Parent, Root: TWindow;
    Children: PWindowArray;
    Count: CInt;
  begin
    if (XQueryTree(FDisplay, Handle, @Root, @Parent, PPWindow(@Children), @Count) > 0) and (Count > 0) and (Children <> nil) then
    begin
      if (HasWindowProperty(FDisplay, Children^[0], 'WM_STATE')) then
        Handle := Children^[0];

      if (Children <> nil) then
        XFree(Children);
    end;
  end;

  function GetTopWindow(Handle: HWND): HWND;

    function TraverseParents(AWindow: TWindow): TWindow;
    var
      Window, Parent, Root: TWindow;
      Children: PWindowArray;
      Count: CInt;
      Tree: array of TWindow;
    begin
      Result := 0;
      Root := 0;
      Parent := AWindow;
      Window := AWindow;

      while (Parent <> Root) do
      begin
        Window := Parent;

        if (XQueryTree(FDisplay, Window, @Root, @Parent, PPWindow(@Children), @Count) = 0) then
          Break;
        if (Children <> nil) then
          XFree(Children);

        SetLength(Tree, Length(Tree) + 1);
        Tree[High(Tree)] := Window;
      end;

      if (Length(Tree) > 0) then
        Result := Tree[High(Tree)];
    end;

  begin
    Result := TraverseParents(Handle);
  end;

var
  Last: HWND;
begin
  Last := FHandle;
  FHandle := AValue;

  if (not Self.HandleIsVaild()) then
    raise Exception.CreateFmt('TNalaWindow.setHandle: Handle %d isn''t a vaild window', [AValue]);

  CheckClientWindow(FHandle);
  if (Last <> FHandle) then
  begin
    FTopWindow := GetTopWindow(FHandle);
    CheckClientWindow(FTopWindow);
    FErrorCount := 0; // Incase getting the top window throws a error

    FIsTopWindow := FHandle = FTopWindow;
  end;
end;

function TNalaWindow.getWidth: Int32;
var
  Attri: TXWindowAttributes;
begin
  Attri := Default(TXWindowAttributes);
  XGetWindowAttributes(FDisplay, FHandle, @Attri);

  Result := Attri.Width;
end;

function TNalaWindow.getHeight: Int32;
var
  Attri: TXWindowAttributes;
begin
  Attri := Default(TXWindowAttributes);
  XGetWindowAttributes(FDisplay, FHandle, @Attri);

  Result := Attri.Height;
end;

function TNalaWindow.getLeft: Int32;
var
  Child: TWindow;
  Y: CInt;
begin
  XTranslateCoordinates(FDisplay, FHandle, DefaultRootWindow(FDisplay), 0, 0, @Result, @Y, @Child);
end;

function TNalaWindow.getTop: Int32;
var
  Child: TWindow;
  X: CInt;
begin
  XTranslateCoordinates(FDisplay, FHandle, DefaultRootWindow(FDisplay), 0, 0, @X, @Result, @Child);
end;

procedure TNalaWindow.setShowing(AValue: Boolean);
begin
  if (AValue) then
    XMapWindow(FDisplay, FHandle)
  else
    XUnmapWindow(FDisplay, FHandle);

  XFlush(FDisplay);
end;

function TNalaWindow.getCaption: String;
begin
  Result := OSUtils.WindowTitle(FHandle);
end;

function TNalaWindow.Data(X, Y, AWidth, AHeight: Int32): TWindowData;
var
  Image: PXImage;
  W, H, i: Integer;
  b: Byte;
  Ptr: PRGB32;
  Upper: PtrUInt;
begin
  W := Self.Width;
  H := Self.Height;

  if (Self.HandleIsVaild()) then
  begin
    if (FBuffer.Width <> W) or (FBuffer.Height <> H) then
      FBuffer.SetSize(W, H);

    Image := XGetImage(FDisplay, FHandle, X, Y, AWidth, AHeight, XAllPlanes(), ZPixmap);
    if (Image <> nil) and (Image^.Data <> nil) and (Image^.Depth >= 24) then
    begin
      for i := 0 to Image^.Height - 1 do
        Move(Image^.Data[i * Image^.Width * 4], FBuffer.Data[(i + Y) * FBuffer.Width + X], Image^.Width * SizeOf(TRGB32));

      // RGBA is most vnc's default format so let's check for this...
      // Is RGBA, needs to be BGRA
      if (Image^.Red_Mask = $0000FF) then
      begin
        Writeln('TNalaWindow.Data: Converting data to BGRA');

        for i := Y to (Y + AHeight) - 1 do
        begin
          Ptr := @Result.BufferPtr[i * FBuffer.Width + X];
          Upper := PtrUInt(Ptr) + (Image^.Width * SizeOf(TRGB32));

          while (PtrUInt(Ptr) <= Upper) do
          begin
            b := Ptr^.R;
            Ptr^.R := Ptr^.B;
            Ptr^.B := b;
            Inc(Ptr);
          end;
        end;
      end;

      XFree(Image);
    end else
    begin // There was a error, let's just return a black buffer rather than a exception (like windows does)
      if (Image = nil) or (Image^.Data = nil) then
        Writeln('TNalaWindow.Data: Image is nil')
      else
        Writeln('TNalaWindow.Data: Image Depth is < 24');

      FBuffer.Fill($000000);
    end;
  end;

  Result.Width := AWidth;
  Result.Height := AHeight;
  Result.BufferPtr := @FBuffer.Data[0];
  Result.BufferWidth := FBuffer.Width;
  Result.BufferHeight := FBuffer.Height;
end;

function TNalaWindow.HandleIsVaild: Boolean;
var
  Attri: TXWindowAttributes;
begin
  XSync(FDisplay, False);
  FErrorCount := 0;
  XGetWindowAttributes(FDisplay, FHandle, @Attri);
  Result := FErrorCount = 0;
end;

{
function TNalaWindow.GetChildren(Recursive: Boolean): TWindowList;
type
  TWindowArr = array of TWindow;

  procedure EnumChildren(StartWindow: TWindow; Recursive: Boolean; var Arr: TWindowArr);
  var
    Parent, Root: TWindow;
    Children: PWindowArray;
    Count: CInt;
    i: Integer;
  begin
    if (XQueryTree(FDisplay, StartWindow, @Root, @Parent, PPWindow(@Children), @Count) > 0) then
    begin
      for i := 0 to Count - 1 do
      begin
        SetLength(Arr, Length(Arr) + 1);
        Arr[High(Arr)] := Children^[i];

        if (Recursive) then
          EnumChildren(Children^[i], Recursive, Arr);
      end;

      if (Children <> nil) then
        XFree(Children);
    end;
  end;

var
  Arr: TWindowArr;
  i: Int32;
begin
  SetLength(Result, 0);
  SetLength(Arr, 0);
  EnumChildren(FHandle, Recursive, Arr);

  for i := 0 to High(Arr) do
    Result.Append(Arr[i]);
end;
}

constructor TNalaWindow.Create(AWindowHandle: HWND);
begin
  inherited Create;

  FHandle := 0;
  FDisplay := XOpenDisplay(nil);

  FErrorCount := 0;
  ManageErrors(Self);

  if (AWindowHandle > 0) then
    setHandle(AWindowHandle);
end;

destructor TNalaWindow.Destroy;
begin
  XCloseDisplay(FDisplay);
  UnManageErrors(Self);

  inherited Destroy;
end;

end.

