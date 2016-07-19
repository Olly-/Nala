unit nala.ListBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Dialogs, Menus, LCLType, Forms, fgl, ExtCtrls;

type

  { TNalaListBox }

  TNalaListBox = class(TScrollBox)
  private
  const
    LINE_LIGHT = $FAF7EE;
    LINE_DARK = $F2EBD6;
    LINE_Y_BORDER = 2;
    LINE_X_BORDER = 4;
  private
  type
    TLine = record TextStart: Integer; Text: String; Top: Integer; end;
    TLineArray = array of TLine;

    TListItem = class(TObject)
    public
      Index: Integer;
      Text: String;
      Lines: TLineArray;
      Top: Integer;
      Height: Integer;
      CalculatedAt: Integer;
      Selected: Boolean;
      ImageIndex: Integer;
    end;

    TListItems = specialize TFPGObjectList<TListItem>;
  private
    FItems: TListItems;
    FShape: TShape;
    FUpdatingScroll: Boolean;
    FImageList: TImageList;

    FSelected: TListItem;
    FPopupItems: array[0..4] of TMenuItem;
    FPopupMenu: TPopupMenu;

    FLineHeight: Integer;
    FCharWidth: Integer;

    FSyncData: record Text: PChar; ImageIndex: Integer; end;
    procedure _AddSync;

    procedure WrapItem(Item: TListItem; AWidth: Integer);
    procedure UpdateScrollBar;

    procedure DoPopupItemClick(Sender: TObject);
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    property ImageList: TImageList read FImageList write FImageList;
    procedure Add(constref AText: String; AImageIndex: Integer = -1);
    procedure AddSync(constref AText: String; AImageIndex: Integer = -1);

    procedure Clear;
    procedure CopySelected;
    procedure CopyAllSelected;
    procedure CopyAll;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  LCLIntf, Types, Clipbrd;

var
  CriticalSection: TRTLCriticalSection;

{ TNalaListBox }

procedure TNalaListBox.WrapItem(Item: TListItem; AWidth: Integer);

  procedure AppendLine(constref Text: String; Start: Integer);
  var
    L: Integer;
  begin
    L := Length(Item.Lines);
    SetLength(Item.Lines, L + 1);
    Item.Lines[L].Text := Text;
    Item.Lines[L].TextStart := Start;
    if (L = 0) then
      Item.Lines[L].Top := 0
    else
      Item.Lines[L].Top := Item.Lines[L - 1].Top + FLineHeight;
    Inc(Item.Height, FLineHeight);
  end;

var
  i, Start, Len, LastSpace, TextWidth: Integer;
  Curr, Str: String;
begin
  SetLength(Item.Lines, 0);
  Item.Height := 0;
  Item.CalculatedAt := AWidth;

  Str := Item.Text;
  LastSpace := -1;
  i := 1;
  Start := 1;
  TextWidth := 0;
  Curr := '';

  while (i <= Length(Str)) do
  begin
    if (Char(Str[i]) = #32) then
      LastSpace := i;

    if (Str[i] <> LineEnding) then
    begin
      Curr := Curr + Str[i];
      TextWidth := TextWidth + FCharWidth;
    end;

    if (TextWidth >= AWidth) or (Str[i] = LineEnding) then
    begin
      if (LastSpace > -1) then
        Curr := Copy(Str, Start, LastSpace - Start)
      else
        Curr := Copy(Str, Start, (i - Start));

      AppendLine(Curr, Start);

      if (LastSpace > - 1) then
      begin
        Start := LastSpace + 1;
        LastSpace := - 1;
        Curr := Copy(Str, Start, (i + 1 - Start));
        TextWidth := Length(Curr) * FCharWidth;
      end else
      begin
        Start := i;
        Curr := '';
        TextWidth := 0;
      end;
    end;

    Inc(i);
  end;

  Curr := Copy(Str, Start, (i - Start) + 1);
  if (Length(Curr) > 0) then
    AppendLine(Curr, Start);
end;

procedure TNalaListBox.UpdateScrollBar;
begin
  if (FItems.Count = 0) then
    Exit;

  FUpdatingScroll := True;
  try
    FShape.Top := FItems.Last.Top + FItems.Last.Height;
  finally
    FUpdatingScroll := False;
  end;
end;

procedure TNalaListBox.Paint;

  procedure IncItems(Start, IncAmount: Integer);
  var
    i: Integer;
  begin
    for i := Start to FItems.Count - 1 do
      Inc(FItems[i].Top, IncAmount);
  end;

var
  Lo, Hi, AWidth: Integer;

  function RectVisible(constref r: TRect): Boolean;
  begin
    Result := ((r.Top >= Lo) and (r.Top <= Hi)) or
              ((r.Bottom >= Lo) and (r.Bottom <= Hi)) or
              ((Lo >= r.Top) and (Hi <= (r.Bottom)));
  end;

  function ItemRect(Item: TListItem): TRect;
  begin
    Result.Left := 0;
    Result.Top := Item.Top;
    Result.Right := AWidth;
    Result.Bottom := Item.Top + Item.Height;
  end;

var
  i, j, OldHeight, ImgWidth: Integer;
  Item: TListItem;
  r: TRect;
  Painted: Boolean = False;
begin
  if (FUpdatingScroll) then
    Exit;

  ImgWidth := 0;
  if (FImageList <> nil) then
    ImgWidth := FImageList.Width + LINE_X_BORDER;

  AWidth := ClientWidth;
  Lo := VertScrollBar.Position;
  Hi := Lo + ClientHeight;

  for i := 0 to FItems.Count - 1 do
  begin
    Item := FItems[i];
    r := ItemRect(Item);

    if (RectVisible(r)) then
    begin
      if (Item.CalculatedAt <> AWidth - ImgWidth) then
      begin
        OldHeight := Item.Height;
        WrapItem(Item, AWidth - ImgWidth - LINE_X_BORDER);

        if (Item.Height - OldHeight) <> 0 then
          IncItems(i + 1, Item.Height - OldHeight);
      end;

      if (Item.Selected) then
        Canvas.Brush.Color := clLtGray
      else
      if (Odd(i)) then
        Canvas.Brush.Color := LINE_DARK
      else
        Canvas.Brush.Color := LINE_LIGHT;

      Canvas.FillRect(r);

      // Only draw visible lines in the item
      for j := 0 to High(Item.Lines) do
      begin
        r := Rect(0, Item.Top + Item.Lines[j].Top, AWidth, Item.Top + Item.Lines[j].Top + FLineHeight);
        if (RectVisible(r)) then
        begin
          Canvas.TextOut(r.Left + ImgWidth + LINE_X_BORDER, r.Top, Item.Lines[j].Text);
          if (j = 0) then
            FImageList.Draw(Canvas, LINE_X_BORDER, r.Top + (FLineHeight div 2) - (FImageList.Height div 2) + 1, Item.ImageIndex);
        end;
      end;

      Painted := True;
    end else
      if (Painted) then
        Break;
  end;

  UpdateScrollBar;
end;

procedure TNalaListBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);

  if (Button = mbLeft) then
  begin
    Y := VertScrollBar.Position + Y;
    for i := 0 to FItems.Count - 1 do
      if (Y >= FItems[i].Top) and (Y <= (FItems[i].Top + FItems[i].Height)) then
      begin
        FItems[i].Selected := not FItems[i].Selected;
        Break;
      end;

    Refresh;
  end else
  if (Button = mbRight) then
  begin
    FSelected := nil;

    Y := VertScrollBar.Position + Y;
    for i := 0 to FItems.Count - 1 do
      if (Y >= FItems[i].Top) and (Y <= (FItems[i].Top + FItems[i].Height)) then
      begin
        FSelected := FItems[i];
        Break;
      end;

    FPopupMenu.PopUp;
  end;
end;

procedure TNalaListBox.Add(constref AText: String; AImageIndex: Integer);
var
  Item: TListItem;
begin
  Item := TListItem.Create;
  Item.Text := AText;
  Item.Selected := False;
  Item.ImageIndex := AImageIndex;

  WrapItem(Item, ClientWidth);

  if (FItems.Count = 0) then
    Item.Top := 0
  else
    Item.Top := FItems.Last.Top + FItems.Last.Height;

  Item.Index := FItems.Add(Item);

  UpdateScrollBar;
  Refresh;
end;

procedure TNalaListBox._AddSync;
begin
  Self.Add(String(FSyncData.Text), FSyncData.ImageIndex);
end;

procedure TNalaListBox.AddSync(constref AText: String; AImageIndex: Integer);
begin
  FSyncData.Text := PChar(AText);
  FSyncData.ImageIndex := AImageIndex;

  TThread.Synchronize(nil, @Self._AddSync);
end;

procedure TNalaListBox.DoPopupItemClick(Sender: TObject);
begin
  if (Sender = FPopupItems[0]) then // Clear
    Clear()
  else
  if (Sender = FPopupItems[2]) then // Copy selected
    CopySelected()
  else
  if (Sender = FPopupItems[3]) then // Copy all selected
    CopyAllSelected()
  else
  if (Sender = FPopupItems[4]) then // Copy all
    CopyAll();
end;

procedure TNalaListBox.Clear;
begin
  FItems.Clear;
  Refresh;
end;

procedure TNalaListBox.CopySelected;
begin
  if (FSelected <> nil) then
    Clipboard.AsText := FSelected.Text;
end;

procedure TNalaListBox.CopyAllSelected;
var
  i: Integer;
  Str: String = '';
begin
  for i := 0 to FItems.Count - 1 do
    if (FItems[i].Selected) then
      Str += FItems[i].Text + #10;

  Clipboard.AsText := Str;
end;

procedure TNalaListBox.CopyAll;
var
  i: Integer;
  Str: String = '';
begin
  for i := 0 to FItems.Count - 1 do
    Str += FItems[i].Text + #10;

  Clipboard.AsText := Str;
end;

constructor TNalaListBox.Create(AOwner: TComponent);

  procedure SetDefaults;
  begin
    with TBitmap.Create do
    try
      Canvas.Font := Self.Canvas.Font;

      FLineHeight := Canvas.TextHeight('Fj') + LINE_Y_BORDER;
      FCharWidth := Canvas.TextWidth('F');
    finally
      Free;
    end;
  end;

begin
  inherited Create(AOwner);

  Color := clWhite;

  Canvas.Font.Name := 'Consolas';
  Canvas.Font.Size := 10;

  FItems := TListItems.Create;
  FUpdatingScroll := False;

  // This shape gives a very easy way to set the scroll size
  FShape := TShape.Create(Self);
  FShape.Parent := Self;
  FShape.Height := 1;
  FShape.Width := 1;
  FShape.Pen.Color := LINE_LIGHT;

  FPopupItems[0] := TMenuItem.Create(Self);
  FPopupItems[0].Caption := 'Clear';
  FPopupItems[0].OnClick := @DoPopupItemClick;

  FPopupItems[1] := TMenuItem.Create(Self);
  FPopupItems[1].Caption := '-';

  FPopupItems[2] := TMenuItem.Create(Self);
  FPopupItems[2].Caption := 'Copy Selected';
  FPopupItems[2].OnClick := @DoPopupItemClick;

  FPopupItems[3] := TMenuItem.Create(Self);
  FPopupItems[3].Caption := 'Copy All Selected';
  FPopupItems[3].OnClick := @DoPopupItemClick;

  FPopupItems[4] := TMenuItem.Create(Self);
  FPopupItems[4].Caption := 'Copy All';
  FPopupItems[4].OnClick := @DoPopupItemClick;

  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.Items.Add(FPopupItems);

  SetDefaults;
end;

destructor TNalaListBox.Destroy;
begin
  FItems.Free;

  inherited Destroy;
end;

initialization
  InitCriticalSection(CriticalSection);
finalization
  DoneCriticalsection(CriticalSection);

end.

