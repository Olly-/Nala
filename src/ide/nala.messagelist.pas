unit nala.MessageList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Grids, Menus, syncobjs, fgl, Graphics,
  nala.Types;

type
  TMessageGrid = class(TDrawGrid)
  private
  type
    TLine = record
      Start, Len: Int32;
    end;
    TLineArray = array of TLine;

    TRowData = class
      Str: String;
      Image: Int8;
      Selected: Boolean;
      Lines: TLineArray;
    end;

    TRowList = specialize TFPGObjectList<TRowData>;
  private
    FImages: TImageList;
    FCapacity: Int32;
    FRowList: TRowList;
    FCharacterWidth: Int32;
    FCharacterHeight: Int32;
    FPaintLock: Int32;
    FPopupMenu: TPopupMenu;
    FPopupItems: array[0..3] of TMenuItem;

    procedure DoPopupClick(Sender: TObject);

    function WrapString(Data: TRowData): Int32;
  protected
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;
    procedure GridMouseWheel(Shift: TShiftState; Delta: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure FontChanged(Sender: TObject); override;
    procedure Resize; override;
  public
    property Images: TImageList read FImages write FImages;
    property Capacity: Int32 read FCapacity write FCapacity;

    procedure Add(constref S: String; Image: Int32 = -1);
    procedure Clear;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TMessageList = class(TComponent)
  private
    FGrid: TMessageGrid;
    FCricitalSection: TCriticalSection;
    FSyncInfo: record S: PString; Image: Int32; end;

    procedure _Add;
    procedure _Add(constref S: String; Image: Int32);

    function GetFont: TFont;
    function GetImages: TImageList;
    procedure SetFont(AValue: TFont);
    procedure SetImages(AValue: TImageList);
  public
    property Font: TFont read GetFont write SetFont;
    property Images: TImageList read GetImages write SetImages;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Add(constref S: String; Image: Int32 = -1; Synchronized: Boolean = False);
    procedure Clear(Synchronized: Boolean = False);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  LCLType, LCLIntf, Math, LMessages, Clipbrd;

procedure TMessageList._Add;
begin
  FGrid.Add(FSyncInfo.S^, FSyncInfo.Image);
end;

procedure TMessageList._Add(constref S: String; Image: Int32);
begin
  FCricitalSection.Enter();

  try
    FSyncInfo.S := @S;
    FSyncInfo.Image := Image;

    TThread.Synchronize(nil, @_Add);
  finally
    FCricitalSection.Leave();
  end;
end;

function TMessageList.GetFont: TFont;
begin
  Result := FGrid.Font;
end;

function TMessageList.GetImages: TImageList;
begin
  Result := FGrid.Images;
end;

procedure TMessageList.SetFont(AValue: TFont);
begin
  FGrid.Font := AValue;
end;

procedure TMessageList.SetImages(AValue: TImageList);
begin
  FGrid.Images := AValue;
end;

procedure TMessageList.BeginUpdate;
begin
  FGrid.BeginUpdate();
end;

procedure TMessageList.EndUpdate;
begin
  FGrid.EndUpdate();
end;

procedure TMessageList.Add(constref S: String; Image: Int32; Synchronized: Boolean);
begin
  if (Synchronized) then
    _Add(S, Image)
  else
    FGrid.Add(S, Image);
end;

procedure TMessageList.Clear(Synchronized: Boolean);
begin
  if (Synchronized) then
    TThread.Synchronize(nil, @FGrid.Clear)
  else
    FGrid.Clear();
end;

constructor TMessageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGrid := TMessageGrid.Create(Self);
  FGrid.Parent := TWinControl(AOwner);
  FGrid.Align := alClient;

  FCricitalSection := syncobjs.TCriticalSection.Create;
end;

destructor TMessageList.Destroy;
begin
  FGrid.Clear();
  FGrid.Free();

  FCricitalSection.Free();

  inherited Destroy;
end;

procedure TMessageGrid.GridMouseWheel(Shift: TShiftState; Delta: Integer);
var
  Scroll: TLMScroll;
begin
  Scroll := Default(TLMScroll);
  Scroll.ScrollCode := Max(Delta, 0);
  Scroll.ScrollBar := SB_THUMBTRACK;
  Scroll.Pos := GetScrollBarPosition(SB_Vert);

  WMVScroll(Scroll);
end;

procedure TMessageGrid.KeyDown(var Key: Word; Shift: TShiftState);
var
  i: Int32;
begin
  if (Key = VK_A) and (ssCtrl in Shift) then
  begin
    BeginUpdate();

    try
      for i := 0 to FRowList.Count - 1 do
        FRowList[i].Selected := True;
    finally
      EndUpdate();
    end;
  end;
end;

procedure TMessageGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if (Button = mbLeft) then
  begin
    P := MouseToCell(Point(X, Y));
    if (P.Y >= 0) then
    begin
      FRowList[P.Y].Selected := not FRowList[P.Y].Selected;
      InvalidateRow(P.Y);
    end;
  end;
end;

procedure TMessageGrid.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  with TBitmap.Create do
  try
    Canvas.Font := Self.Canvas.Font;

    FCharacterWidth := Canvas.TextWidth('F');
    FCharacterHeight := Canvas.TextHeight('Fj');
  finally
    Free();
  end;
end;

procedure TMessageGrid.Resize;
var
  i: Int32;
begin
  inherited Resize;

  if (ColCount > 0) then
  begin
    BeginUpdate();

    try
      ColWidths[0] := ClientWidth;

      if (ClientWidth <= 50) then
        FPaintLock := 1
      else
      begin
        FPaintLock := 0;

        for i := 0 to RowCount - 1 do
          RowHeights[i] := WrapString(FRowList[i]);
      end;
    finally
      EndUpdate();
    end;
  end;
end;

procedure TMessageGrid.DoPopupClick(Sender: TObject);
var
  i: Int32;
  S: String = '';
begin
  case TMenuItem(Sender).Caption of
    'Clear':
       Clear();

    'Copy Selected':
       begin
         for i := 0 to FRowList.Count - 1 do
           if (FRowList[i].Selected) then
             S += FRowList[i].Str + #10;

         Clipboard.AsText := TrimRight(S);
      end;

    'Copy All':
      begin
        for i := 0 to FRowList.Count - 1 do
          S += FRowList[i].Str + #10;

        Clipboard.AsText := TrimRight(S);
      end;
  end;
end;

function TMessageGrid.WrapString(Data: TRowData): Int32;
var
  Size, Space, Start, Len: Int32;

  procedure addLine(LineLength, NewStart, NewSpace, NewSize: Int32);
  begin
    SetLength(Data.Lines, Len + 1);
    Data.Lines[Len].Start := Start;
    Data.Lines[Len].Len := LineLength;

    Start := NewStart;
    Space := NewSpace;
    Size := NewSize;

    Inc(Len);
    Inc(Result, FCharacterHeight);
  end;

var
  Hi: PtrUInt;
  Ptr: PChar;
  LineWidth, Index: Int32;
begin
  Result := 0;
  SetLength(Data.Lines, 0);

  LineWidth := ClientWidth - 2;
  if (FImages <> nil) and (Data.Image >= 0) then
    Dec(LineWidth, FImages.Width + 2);

  Len := Length(Data.Str);
  if (Len = 0) or (LineWidth <= FCharacterWidth) then
    Exit;

  Ptr := PChar(@Data.Str[1]);
  Hi := PtrUInt(Ptr) + Len;

  Len := 0;

  Size := 0;
  Space := 0;
  Start := 1;
  Index := 1;

  while (PtrUInt(Ptr) < Hi) do
  begin
    if (Ptr^ = #32) then
      Space := Index
    else
    if (Ptr^ = #10) then
      addLine(Index - Start, Index + 1, 0, 0);

    if ((Size + FCharacterWidth) >= LineWidth) then
      if (Space > 0) then
        addLine(Space - Start, Space + 1, 0, (Index - Space) * FCharacterWidth)
      else
        addLine(Index - Start, Index, 0, 0);

    Inc(Size, FCharacterWidth);
    Inc(Ptr);
    Inc(Index);
  end;

  if (Index > Start) then
    addLine(Index - Start, 0, 0, 0);
end;

procedure TMessageGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  i, X: Int32;
  Data: TRowData;
begin
  if (FPaintLock > 0) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ARect);
  end else
  begin
    Data := FRowList[ARow];

    with Canvas do
    begin
      if (Data.Selected) then
        Brush.Color := clListSelected
      else if (Odd(ARow)) then
        Brush.Color := clListOdd
      else
        Brush.Color := clListEven;

      FillRect(ARect);
      X := ARect.Left + 2;

      for i := 0 to High(Data.Lines) do
      begin
        if (i = 0) and (Data.Image >= 0) and (FImages <> nil) then
        begin
          FImages.Draw(Canvas, X, ARect.Top + (FCharacterHeight - FImages.Height) - 1, Data.Image);
          X += FImages.Width + 2;
        end;

        TextOut(X, ARect.Top - 1, Copy(Data.Str, Data.Lines[i].Start, Data.Lines[i].Len));
        ARect.Top += FCharacterHeight;
      end;
    end;
  end;
end;

procedure TMessageGrid.Add(constref S: String; Image: Int32);
var
  Data: TRowData;
begin
  Data := TRowData.Create;
  Data.Image := Image;
  Data.Str := S;
  Data.Selected := False;

  try
    BeginUpdate();

    if (RowCount > FCapacity) then
    begin
      DeleteRow(0);
      FRowList.Delete(0);
    end;

    FRowList.Add(Data);

    RowCount := RowCount + 1;
    RowHeights[RowCount - 1] := WrapString(Data);

    if (ColWidths[0] <> ClientWidth) then
      ColWidths[0] := ClientWidth;
  finally
    EndUpdate();
  end;
end;

procedure TMessageGrid.Clear;
begin
  inherited Clear();

  FRowList.Clear();
end;

constructor TMessageGrid.Create(AOwner: TComponent);
const
  Captions: array[0..3] of String = ('Clear', '-', 'Copy Selected', 'Copy All');
var
  i: Int32;
begin
  inherited Create(AOwner);

  FPopupMenu := TPopupMenu.Create(Self);
  for i := 0 to High(FPopupItems) do
  begin
    FPopupItems[i] := TMenuItem.Create(FPopupMenu);
    FPopupItems[i].Caption := Captions[i];
    FPopupItems[i].OnClick := @DoPopupClick;

    FPopupMenu.Items.Add(FPopupItems[i]);
  end;

  FRowList := TRowList.Create(True);
  FCapacity := 5000;
  FImages := nil;
  FPaintLock := 0;

  PopupMenu := FPopupMenu;
  BorderStyle := bsNone;
  DefaultDrawing := False;
  ScrollBars := ssAutoVertical;
  Options := Options + [goThumbTracking] - [goEditing, goHorzLine, goVertLine];

  FixedCols := 0;
  FixedRows := 0;
  ColCount := 1;
  RowCount := 0;

  Font.Name := 'Consolas';
  Font.Size := 10;
end;

destructor TMessageGrid.Destroy;
begin
  FRowList.Free();

  inherited Destroy;
end;

end.

