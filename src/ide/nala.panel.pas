unit nala.Panel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, nala.AnchorDocking, AnchorDockStorage;

type

  { TNalaPanel }

  TNalaPanel = class(TForm)
  private
    function GetSite: TAnchorDockHostSite;
  public
    property Site: TAnchorDockHostSite read GetSite;

    procedure ToggleVisible;
    procedure PaintDockHeader(Sender: TObject);
    procedure InitHeader;

    constructor Create(AOwner: TComponent; AName: String);
  end;

implementation

uses
  Types, Controls, Graphics;

{ TNalaPanel }

constructor TNalaPanel.Create(AOwner: TComponent; AName: String);
begin
  inherited CreateNew(AOwner);

  Name := AName;
  Caption := AName;
  ShowInTaskBar := stNever;

  DockMaster.MakeDockable(Self, False);
  Site.Header.Color := clForm;
end;

function TNalaPanel.GetSite: TAnchorDockHostSite;
begin
  Result := DockMaster.GetAnchorSite(Self);
end;

procedure TNalaPanel.ToggleVisible;
begin
  if (Site.Showing) then
  begin
    DockMaster.ManualFloat(Self);
    Site.Hide;
  end else
    Site.Show;
end;

procedure TNalaPanel.PaintDockHeader(Sender: TObject);

  procedure PaintGripper(ACanvas: TCanvas; Rect: TRect);

    procedure ResizeRect(var Rect: TRect);
    var
      b: Boolean = False;
    begin
      while (((Rect.Right - Rect.Left) mod 4) <> 1) do
      begin
        if (b) then Rect.Right += 1 else Rect.Left -= 1;
        b := not b;
      end;
    end;

  var
    x, y: Integer;
  begin
    ResizeRect(Rect);

    Rect.Left += 2;
    Rect.Right -= 2;
    x := Rect.Left;
    y := (Rect.Bottom - Rect.Top) div 2;

    ACanvas.Pixels[Rect.Left - 2, y] := clBtnShadow;
    while (x <= Rect.Right) do
    begin
      ACanvas.Pixels[x, y - 2] := clBtnShadow;
      ACanvas.Pixels[x, y + 2] := clBtnShadow;
      ACanvas.Pixels[x + 2, y] := clBtnShadow;
      Inc(x, 4);
    end;
  end;

const
  INDENT = 8;
var
  DockHeader: TAnchorDockHeader;
  r, TextRect: TRect;
  TextWidth: Integer;
  Style: TTextStyle;
  Mid: TPoint;
begin
  DockHeader := Sender as TAnchorDockHeader;
  if (DockHeader = nil) then
    Exit;

  with DockHeader do
  begin
    Canvas.Brush.Color := clForm;
    Canvas.FillRect(ClientRect);

    TextWidth := Canvas.TextWidth(Caption);
    Style := Canvas.TextStyle;
    Style.Alignment := taCenter;

    r := ClientRect;
    if (CloseButton.IsControlVisible) and (CloseButton.Parent = DockHeader) then
      r.Right := CloseButton.Left + 1;

    r.Left += INDENT;
    r.Right -= INDENT;
    Mid := Point((r.Right + r.Left) div 2, (r.Bottom - r.Top) div 2);

    TextRect.Top := r.Top;
    TextRect.Bottom := r.Bottom;
    TextRect.Left := Mid.X - (TextWidth div 2) - INDENT;
    TextRect.Right := Mid.X + (TextWidth div 2) + INDENT;

    Canvas.TextRect(TextRect, 0, 0, Caption, Style);

    PaintGripper(Canvas, Rect(r.Left, r.Top, TextRect.Left, r.Bottom)); // Left
    PaintGripper(Canvas, Rect(TextRect.Right, r.Top, r.Right, r.Bottom)); // Right
  end;
end;

procedure TNalaPanel.InitHeader;
begin
  with DockMaster.GetAnchorSite(Self).Header do
  begin
    HeaderPosition := adlhpTop;
    OnPaint := @Self.PaintDockHeader;
  end;
end;

end.

