unit nala.ColorPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Graphics, ComCtrls, ExtCtrls, Types,
  LCLType, Buttons, ImgList,
  nala.Panel, nala.MiscComponents;

type
  { TNalaColorPanel }

  TNalaColorPanel = class(TNalaPanel)
  private
    FTreeView: TTreeView;
    FTreeViewPanel: TPanel;
    btnLoad, btnSave: TNalaCustomButton;

    procedure DrawNode(Sender: TCustomTreeView; Node: TTreeNode; State: TCustomDrawState;
                       Stage: TCustomDrawStage; var PaintImages, DefaultDraw: Boolean);
    procedure DoResizePanel(Sender: TObject);
    procedure DoButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AName: String); reintroduce;
    destructor Destroy; override;

    procedure AddColor(AColor: TColor);
  end;

implementation

uses
  nala.ColorMath, nala.MainForm, nala.ColorPicker, nala.Types;

{ TNalaColorPanel }

procedure TNalaColorPanel.DrawNode(Sender: TCustomTreeView; Node: TTreeNode;
  State: TCustomDrawState; Stage: TCustomDrawStage; var PaintImages,
  DefaultDraw: Boolean);
var
  r: TRect;
begin
  if (PaintImages) and (Node.ImageIndex >= 0) and (not Node.Selected) then
  begin
    PaintImages := False;

    with FTreeView do
    begin
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := StrToInt(Node.Text);

      r := Node.DisplayRect(False);
      r := Rect(Node.DisplayIconLeft() + 3, r.Top + 3, Node.DisplayTextLeft() - 3, r.Bottom - 3);
      Canvas.Rectangle(r);
    end;
  end;
end;

procedure TNalaColorPanel.DoResizePanel(Sender: TObject);
begin
  FTreeViewPanel.Width := Self.Width - 15;
end;

procedure TNalaColorPanel.DoButtonClick(Sender: TObject);
begin
  if (Sender = btnLoad) then
    AddColor(PickColor())
  else if (Sender = btnSave) then
    FTreeView.Items.Clear;
end;

constructor TNalaColorPanel.Create(AOwner: TComponent; AName: String);
begin
  inherited Create(AOwner, AName);

  btnLoad := TNalaCustomButton.Create(Self);
  with btnLoad do
  begin
    Parent := Self;
    Top := 1;
    Left := 5;
    Width := 22;
    Height := 22;
    Hint := 'Pick Color';
    ShowHint := True;
    OnClick := @DoButtonClick;
    NalaForm.Images16x16.GetBitmap(8, Glyph);
  end;

  btnSave := TNalaCustomButton.Create(Self);
  with btnSave do
  begin
    Parent := Self;
    AnchorToNeighbour(akLeft, 5, btnLoad);
    Top := 1;
    Width := 22;
    Height := 22;
    Hint := 'Clear Colors';
    ShowHint := True;
    OnClick := @DoButtonClick;
    NalaForm.Images16x16.GetBitmap(7, Glyph);
  end;

  FTreeViewPanel := TPanel.Create(Self);
  with FTreeViewPanel do
  begin
    Parent := Self;
    BevelOuter := bvNone;
    Left := 5;
    Height := 180;
    Color := cl3DLight;
    AnchorToNeighbour(akTop, 4, btnLoad);
    AnchorParallel(akRight, 5, Self);
  end;

  OnResize := @DoResizePanel;

  FTreeView := TTreeView.Create(Self);
  FTreeView.Parent := FTreeViewPanel;
  FTreeView.Align := alClient;
  FTreeView.BorderSpacing.Around:= 1;
  FTreeView.Images := NalaForm.Images16x16;
  FTreeView.OnAdvancedCustomDrawItem := @DrawNode;
  FTreeView.BorderStyle := bsNone;
  FTreeView.ScrollBars := ssAutoVertical;
end;

destructor TNalaColorPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TNalaColorPanel.AddColor(AColor: TColor);
var
  Node: TTreeNode;
  RGB: TRGB;
  HSB: THSB;
begin
  Node := FTreeView.Items.Add(nil, IntToStr(AColor));
  Node.ImageIndex := 1;
  RGB := ColorToRGB(AColor);
  HSB := ColorToHSB(AColor);

  FTreeView.Items.AddChild(Node, Format('Hex: $%s', [IntToHex(AColor, 6)]));
  FTreeView.Items.AddChild(Node, Format('RGB: %d, %d, %d', [RGB.R, RGB.G, RGB.B]));
  FTreeView.Items.AddChild(Node, Format('HSB: %f, %f, %f', [HSB.H, HSB.S, HSB.B]));
end;

end.

