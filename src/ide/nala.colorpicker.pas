unit nala.ColorPicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, Types, Graphics, ExtCtrls, GraphType;

function PickColor: TColor;

implementation

uses
  nala.Bitmap, nala.OSUtils, LCLIntf, LCLType, StdCtrls,
  nala.Window;

type
  { TInfoForm }

  TInfoForm = class(TForm)
  private
    FImageArea: TImage;
    FImageColor: TImage;
    FImageDeskop: TImage;

    FTextColor: TLabel;
    FTextPos: TLabel;

    FMousePos: TPoint;

    procedure DoPaintForm(Sender: TObject);
    procedure DoPaintImage(Sender: TObject);
  public
    property DesktopImage: TImage write FImageDeskop;
    procedure Update(MousePos: TPoint);

    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

  { TPickerForm }

  TPickerForm = class(TForm)
  private
    FInfoForm: TInfoForm;
    FImage: TImage;
    FPickedColor: TColor;

    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoClick(Sender: TObject);
  protected
    procedure DoShow; override;
    procedure DoHide; override;
  public
    property PickedColor: TColor read FPickedColor;

    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  end;

{ TInfoForm }

procedure TInfoForm.DoPaintForm(Sender: TObject);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Frame(0, 0, Width, Height);
end;

procedure TInfoForm.DoPaintImage(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  with FImageArea do
  try
    Picture.Bitmap.BeginUpdate();

    try
      Bitmap := TBitmap.Create;
      Bitmap.SetSize(9, 9);
      with Bitmap.Canvas do
      begin
        CopyRect(Rect(0, 0, 9, 9), FImageDeskop.Picture.Bitmap.Canvas,
                 Rect(FMousePos.X - 4, FMousePos.Y - 4, FMousePos.X + 5, FMousePos.Y + 5));
      end;

      Canvas.Pen.Color := clBlack;
      Canvas.Frame(ClientRect);
      Canvas.StretchDraw(Rect(1, 1, ClientRect.Right - 1, ClientRect.Bottom - 1), Bitmap);
      Canvas.Frame(25, 25, 31, 31);
    finally
      Bitmap.Free;
    end;
  finally
    Picture.Bitmap.EndUpdate();
  end;

  with FImageColor.Canvas do
  begin
    Pen.Color := clBlack;
    Brush.Color := FImageDeskop.Picture.Bitmap.Canvas.Pixels[FMousePos.X, FMousePos.Y];
    Rectangle(0, 0, FImageColor.Width, FImageColor.Height);
  end;

  FTextColor.Caption := 'Color: ' + IntToStr(FImageDeskop.Picture.Bitmap.Canvas.Pixels[FMousePos.X, FMousePos.Y]);
  FTextPos.Caption := 'Position: ' + IntToStr(FMousePos.X) + ', ' + IntToStr(FMousePos.Y);
end;

procedure TInfoForm.Update(MousePos: TPoint);
begin
  FMousePos := MousePos;
  DoPaintImage(nil);
end;

constructor TInfoForm.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner, Num);

  ShowInTaskBar := stNever;
  Width := 72 + Canvas.TextWidth('Position: 9999, 9999') + 8;
  Height := 72;
  BorderStyle := bsNone;
  OnPaint := @DoPaintForm;

  FImageArea := TImage.Create(Self);
  with FImageArea do
  begin
    Parent := Self;
    Left := 8;
    Top := 8;
    Width := 56;
    Height := 56;
    OnPaint := @DoPaintImage;
    Picture.Bitmap.Width := Width;
    Picture.Bitmap.Height := Height;
  end;

  FImageColor := TImage.Create(Self);
  with FImageColor do
  begin
    Parent := Self;
    OnPaint := @DoPaintImage;
    Picture.Bitmap.Width := Width;
    Picture.Bitmap.Height := Height;

    Top := 8;
    Left := 56 + 16;
    Height := 20;
    AnchorParallel(akRight, 8, Self);
  end;

  FTextColor := TLabel.Create(Self);
  with FTextColor do
  begin
    Parent := Self;
    AnchorToNeighbour(akTop, 4, FImageColor);
    Left := FImageColor.Left;
  end;

  FTextPos := TLabel.Create(Self);
  with FTextPos do
  begin
    Parent := Self;
    AnchorToNeighbour(akTop, 1, FTextColor);
    Left := FImageColor.Left;
  end;
end;

{ TPickerForm }

procedure TPickerForm.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (Sender = FInfoForm) then
  begin
    X += FInfoForm.Left;
    Y += FInfoForm.Top;
  end else
  if (Sender = FInfoForm.FImageArea) then
  begin
    X += FInfoForm.Left + FInfoForm.FImageArea.Left;
    Y += FInfoForm.Top + FInfoFOrm.FImageArea.Top;
  end else
  if (Sender = FInfoForm.FImageColor) then
  begin
    X += FInfoForm.Left + FInfoForm.FImageColor.Left;
    Y += FInfoForm.Top + FInfoFOrm.FImageColor.Top;
  end;

  if ((X + 20 + FInfoForm.Width) > Self.Width) then
    FInfoForm.Left := X - 20 - FInfoForm.Width
  else
    FInfoForm.Left := X + 20;

  FInfoForm.Top := Y - FInfoForm.Height div 2;
  if (FInfoForm.Top < 0) then
    FInfoForm.Top := 0
  else
  if (FInfoForm.Top + FInfoForm.Height) > Height then
    FInfoForm.Top := Height - FInfoForm.Height;

  FInfoForm.Update(Point(X, Y));
end;

procedure TPickerForm.DoClick(Sender: TObject);
begin
  FPickedColor := FImage.Canvas.Pixels[FInfoForm.FMousePos.X, FInfoForm.FMousePos.Y];
  Self.Hide;
end;

procedure TPickerForm.DoShow;
begin
  inherited Show;

  FInfoForm.Show;
end;

procedure TPickerForm.DoHide;
begin
  FInfoForm.Hide;

  inherited DoHide;
end;

constructor TPickerForm.CreateNew(AOwner: TComponent; Num: Integer);

var
  Win: TNalaWindow;
  BMP: TNalaBitmap;
begin
  inherited CreateNew(AOwner, Num);

  Win := TNalaWindow.Create(OSUtils.GetDesktopHandle());
  BMP.FromWindow(Win);

  BorderStyle := bsNone;
  ShowInTaskBar := stNever;
  SetBounds(0, 0, Win.Width, Win.Height);

  FImage := TImage.Create(Self);
  with FImage do
  begin
    Parent := Self;
    Align := alClient;
    Cursor := crCross;
    OnMouseMove := @DoMouseMove;
    OnClick := @DoClick;

    Picture.Bitmap.LoadFromRawImage(BMP.ToRawImage, False);
  end;

  FInfoForm := TInfoForm.CreateNew(Self);
  FInfoForm.Parent := Self;
  FInfoForm.DesktopImage := FImage;
  FInfoForm.OnMouseMove := @DoMouseMove;
  FInfoForm.FImageDeskop.OnMouseMove := @DoMouseMove;
  FInfoForm.FImageColor.OnMouseMove := @DoMouseMove;
  FInfoForm.FImageArea.OnMouseMove := @DoMouseMove;

  Win.Free;
end;

function PickColor: TColor;
begin
  with TPickerForm.CreateNew(nil) do
  try
    Show;
    while Showing do
    begin
      Application.ProcessMessages;
      Sleep(25);
    end;

    Result := PickedColor;
  finally
    Free;
  end;
end;


end.

