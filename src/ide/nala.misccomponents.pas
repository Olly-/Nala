unit nala.MiscComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ValEdit, ExtCtrls, Grids, Buttons;

type

  { TNalaCustomButton }

  TNalaCustomButton = class(TSpeedButton)
  protected
    procedure PaintBackground(var PaintRect: TRect); override;
  end;

  TNalaValueList = class(TPanel)
  private
    FValEdit: TValueListEditor;

    function getCells(ACol, ARow: Integer): string;
    procedure setCells(ACol, ARow: Integer; const AValue: string);
    function getRowCount: Integer;
    procedure setRowCount(AValue: Integer);
    function getEditable: Boolean;
    procedure setEditable(AValue: Boolean);
    function getKeyColumnWidth: Integer;
    procedure setKeyColumnWidth(AValue: Integer);

    procedure BlockCellSelect(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  public
    property RowCount: Integer read getRowCount write setRowCount;
    property Cells[ACol, ARow: Integer]: string read getCells write setCells;
    property Editable: Boolean read getEditable write setEditable;
    property KeyColumnWidth: Integer read getKeyColumnWidth write setKeyColumnWidth;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TNalaGroupBox }

  TNalaGroupBox = class(TPanel)
  private
  const
    OFF_X_LEFT = 5;
    OFF_Y_TOP = 18;
    OFF_Y_BOTTOM = 5;
  private
    FInnerPanel: TPanel;

    function GetInnerHeight: Integer;
    function GetInnerWidth: Integer;
    procedure SetInnerHeight(AValue: Integer);

    procedure DoPaint(Sender: TObject);
  public
    property InnerHeight: Integer read GetInnerHeight write SetInnerHeight;
    property InnerWidth: Integer read GetInnerWidth;
    property InnerPanel: TPanel read FInnerPanel;

    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  nala.MainForm, agg_fpimage, Agg_LCL, Types;

var
  Button22, Button24: TBitmap;

{ TNalaCustomButton }

// Need a custom drawn button, some themed buttons just don't go with the layout
// uses aggpas for easy anti-aliasing (shipped with FPC). Would use BGRABitmap
// but it's a whole extra package to add for one method...
function CreateButton(AWidth, AHeight: Integer): TBitmap;

  // Hack? should find out why i need to do this and do it a 'real' way
  function ConvertColor(Color: TColor): TColor;
  begin
    with TBitmap.Create do
    try
      SetSize(1, 1);
      Canvas.Pixels[0, 0] := Color;
      Result := Canvas.Pixels[0, 0];
    finally
      Free;
    end;
  end;

var
  Agg: TAggLCLCanvas;
begin
  Agg := TAggLCLCanvas.Create;
  with Agg do
  try
    Image.PixelFormat := afpimRGBA32;
    Image.SetSize(AWidth, AHeight);

    Brush.Color := ConvertColor(clForm);
    FillRect(-1, -1, Width, Height);

    Brush.Color := ConvertColor(cl3DLight);
    Pen.Color := ConvertColor(cl3DShadow);
    RoundRect(0, 0, Width - 1, Height - 1, 6, 6);

    Result := TBitmap.Create;
    Result.LoadFromIntfImage(Agg.Image.IntfImg);
  finally
    Free;
  end;
end;

{ TNalaGroupBox }

function TNalaGroupBox.GetInnerHeight: Integer;
begin
  Result := Height - OFF_Y_TOP - OFF_Y_BOTTOM;
end;

function TNalaGroupBox.GetInnerWidth: Integer;
begin
  Result := Width - (OFF_X_LEFT * 2);
end;

procedure TNalaGroupBox.SetInnerHeight(AValue: Integer);
begin
  Height := AValue + OFF_Y_TOP + OFF_Y_BOTTOM;
end;

procedure TNalaGroupBox.DoPaint(Sender: TObject);
var
  FCaptionMiddle: Integer;
  ASize, lTextSize: TSize;
  lCaption: String;
begin
  ASize.cx := Width;
  ASize.cy := Height;

  with Canvas do
  begin
    FCaptionMiddle := Canvas.TextHeight('Fj') div 2;

    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    Pen.Color := cl3DLight;
    Rectangle(0, 7, ASize.cx, ASize.cy);

    lCaption := 'Image';
    lTextSize := TextExtent(lCaption);

    // Top of half text
    Brush.Style := bsSolid;
    Brush.Color := clForm;
    Pen.Style := psClear;
    Rectangle(FCaptionMiddle, 0, lTextSize.cx + 7, lTextSize.cy);

    // Bottom half of text
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Rectangle(FCaptionMiddle, 8, lTextSize.cx + 7, lTextSize.cy);

    Brush.Style := bsClear;
    TextOut(FCaptionMiddle + 3, 0, lCaption);
  end;
end;

constructor TNalaGroupBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Color := clForm;
  BevelOuter := bvNone;
  OnPaint := @DoPaint;

  FInnerPanel := TPanel.Create(Self);
  FInnerPanel.Parent := Self;
  FInnerPanel.Left := OFF_X_LEFT;
  FInnerPanel.Top := OFF_Y_TOP;
  FInnerPanel.AnchorParallel(akRight, OFF_X_LEFT, Self);
  FInnerPanel.AnchorParallel(akBottom, OFF_Y_BOTTOM, Self);
  FInnerPanel.BevelOuter := bvNone;
  FInnerPanel.Color := clWhite;
end;

procedure TNalaCustomButton.PaintBackground(var PaintRect: TRect);
begin
  if (Width = 24) then
    Canvas.Draw(PaintRect.Left, PaintRect.Top, Button24);
end;

function TNalaValueList.getCells(ACol, ARow: Integer): string;
begin
  Result := FValEdit.Cells[ACol, ARow];
end;

procedure TNalaValueList.setCells(ACol, ARow: Integer; const AValue: string);
begin
  FValEdit.Cells[ACol, ARow] := AValue;
end;

function TNalaValueList.getRowCount: Integer;
begin
  Result := FValEdit.RowCount;
end;

procedure TNalaValueList.setKeyColumnWidth(AValue: Integer);
begin
  FValEdit.ColWidths[0] := AValue;
end;

function TNalaValueList.getKeyColumnWidth: Integer;
begin
  Result := FValEdit.ColWidths[0];
end;

procedure TNalaValueList.setEditable(AValue: Boolean);
begin
  FValEdit.Enabled := AValue;
end;

function TNalaValueList.getEditable: Boolean;
begin
  Result := FValEdit.Enabled;
end;

procedure TNalaValueList.setRowCount(AValue: Integer);
begin
  FValEdit.RowCount := AValue;
  Height := FValEdit.RowCount * FValEdit.DefaultRowHeight + 1;
end;

procedure TNalaValueList.BlockCellSelect(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  if (aCol = 0) then
    CanSelect := False;
end;

constructor TNalaValueList.Create(AOwner: TComponent);
var
  bmp: TBitmap;
begin
  inherited Create(AOwner);

  Color := cl3DLight;
  BevelOuter := bvNone;

  FValEdit := TValueListEditor.Create(nil);
  FValEdit.Parent := Self;
  FValEdit.Align := alClient;
  FValEdit.BorderSpacing.Left := 1;
  FValEdit.BorderSpacing.Top := 1;
  FValEdit.GridLineColor := cl3DLight;
  FValEdit.BorderStyle := bsNone;
  FValEdit.DisplayOptions := FValEdit.DisplayOptions - [doColumnTitles];
  FValEdit.OnSelectCell := @BlockCellSelect;
  FValEdit.ExtendedSelect := False;
  FValEdit.FocusRectVisible := False;

  bmp := TBitmap.Create;
  bmp.Canvas.Font := Self.Font;
  FValEdit.DefaultRowHeight := bmp.Canvas.TextHeight('Fj') + 3;
  bmp.Free;
end;

destructor TNalaValueList.Destroy;
begin
  FValEdit.Free;

  inherited Destroy;
end;

initialization
  Button24 := CreateButton(24, 24);
  Button22 := CreateButton(22, 22);
finalization
  Button24.Free;
  Button22.Free;

end.

