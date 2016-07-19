unit nala.WindowInfo;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, LCLType, nala.CoreTypes;

type

  TWindowInfo = record
    Handle: HWND;
    Title: String;
    ClassName: String;
    Width, Height: UInt32;
  end;

  TWindowList = array of TWindowInfo;

  { TWindowList_Helper }

  TWindowList_Helper = type Helper for TWindowList
    function Find(Title: String; Partial: Boolean; out Info: TWindowInfo): Boolean;
    function Find(Width, Height: UInt32; ClassName: String; out Info: TWindowInfo): Boolean; overload;
    function Fill: UInt32;
    procedure Append(Window: HWND);
  end;

implementation

uses
  nala.OSUtils;

{ TWindowList_Helper }

function TWindowList_Helper.Find(Title: String; Partial: Boolean; out Info: TWindowInfo): Boolean;
var
  i: Int32;
begin
  Result := False;
  Info := Default(TWindowInfo);

  for i := 0 to High(Self) do
    case Partial of
      True:
        if (Pos(Title, Self[i].Title) > 0) then
        begin
          Info := Self[i];
          Exit(True);
        end;

      False:
        if (SameText(Title, Self[i].Title)) then
        begin
          Info := Self[i];
          Exit(True);
        end;
    end;
end;

function TWindowList_Helper.Find(Width, Height: UInt32; ClassName: String; out Info: TWindowInfo): Boolean;
var
  i: Int32;
begin
  Result := False;
  Info := Default(TWindowInfo);

  for i := 0 to High(Self) do
    if (Self[i].Width = Width) and (Self[i].Height = Height) and (Self[i].ClassName = ClassName) then
    begin
      Info := Self[i];
      Exit(True);
    end;
end;

function TWindowList_Helper.Fill: UInt32;
begin

end;

procedure TWindowList_Helper.Append(Window: HWND);
begin
  SetLength(Self, Length(Self) + 1);
  with Self[High(Self)] do
  begin
    Handle := Window;
    Title := OSUtils.WindowTitle(Window);
    Width := OSUtils.WindowWidth(Window);
    Height := OSUtils.WindowHeight(Window);
    ClassName := OSUtils.WindowClass(Window);
  end;
end;


end.

