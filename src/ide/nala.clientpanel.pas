unit nala.ClientPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Controls, ExtCtrls, Forms, Graphics,
  nala.Panel, nala.Bitmap, nala.MiscComponents;

type
  TUpdateThread = class(TThread)
  private
    FImage: TImage;
    FImageBox: TNalaGroupBox;
    FWindowHandle: HWND;
    FThumbnailBuffer: TNalaBitmap;

    FProperties: TNalaValueList;
    FStrings: array [0..4] of String;

    procedure UpdateImage;
    procedure UpdateProperties;
  protected
    procedure Execute; override;
  public
    property WindowHandle: HWND read FWindowHandle write FWindowHandle;

    constructor Create(AImage: TImage; AImageBox: TNalaGroupBox; AProperties: TNalaValueList);
  end;

  { TNalaClientPanel }

  TNalaClientPanel = class(TNalaPanel)
  private
    FProperties: TNalaValueList;

    FUpdateThread: TUpdateThread;

    FImageBox: TNalaGroupBox;
    FImage: TImage;

    FButtonPick: TNalaCustomButton;
    FButtonBringToFront: TNalaCustomButton;

    procedure DoResizeImage(Sender: TObject);
    procedure DoButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AName: String); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  nala.AnchorDocking, nala.MainForm, nala.ClientPicker, nala.Mouse, nala.OSUtils, nala.WindowInfo, nala.TBox,
  nala.Window;

{ TUpdateThread }

procedure TUpdateThread.UpdateImage;
begin
  FImageBox.InnerHeight := FThumbnailBuffer.Height;
  FImage.Picture.Bitmap.LoadFromRawImage(FThumbnailBuffer.ToRawImage, False);
end;

procedure TUpdateThread.UpdateProperties;
begin
  with FProperties do
  begin
    Cells[1, 0] := FStrings[0];
    Cells[1, 1] := FStrings[1];
    Cells[1, 2] := FStrings[2];
    Cells[1, 3] := FStrings[3];
    Cells[1, 4] := FStrings[4];
  end;
end;

type
  ESpecialException = class(Exception);

procedure TUpdateThread.Execute;
var
  W, H, i: Integer;
  Window: TNalaWindow;
  Buffer: TNalaBitmap;
  Mouse: TNalaMouse;

  procedure UpdateThumbnail;
  begin
    if (not Window.HandleIsVaild) then
      raise ESpecialException.Create('');

    if (FImage.Width > 0) and (FImage.Height > 0) then
    begin
      Buffer.FromWindow(Window);
      if (Buffer.Width > 0) and (Buffer.Height > 0) then
      begin
        H := Round(Buffer.Height / Buffer.Width * FImageBox.InnerWidth);
        if (H > Buffer.Height) then
          H := Buffer.Height;
        W := FImageBox.InnerWidth;
        if (W > Buffer.Width) then
          W := Buffer.Width;
        FThumbnailBuffer := Buffer.Thumbnail(W, H);

        Synchronize(@UpdateImage);
      end;
    end;
  end;

  procedure UpdateInfo;
  begin
    if (not Window.HandleIsVaild) then
      raise ESpecialException.Create('');

    FStrings[0] := IntToStr(Window.Handle);
    FStrings[1] := Window.Caption;
    FStrings[2] := IntToStr(Window.Width) + ', ' + IntToStr(Window.Height);
    FStrings[3] := IntToStr(Window.Left) + ', ' + IntToStr(Window.Top);
    FStrings[4] := IntToStr(Mouse.GetPosition(Window).x) + ', ' + IntToStr(Mouse.GetPosition(Window).y);

    Synchronize(@UpdateProperties);
  end;

  procedure Empty;
  var
    i: Integer;
  begin
    for i := 0 to High(FStrings) do
      FStrings[i] := '';
    FThumbnailBuffer.Fill($FFFFFF);

    Synchronize(@UpdateImage);
    Synchronize(@UpdateProperties);
  end;

begin
  Window := TNalaWindow.Create;
  Mouse := TNalaMouse.Create;
  Buffer.Init();

  while (not Terminated) do
  begin
    while (FWindowHandle > 0) and (not Terminated) do
    try
      try
        Window.Handle := FWindowHandle;
      except
        raise ESpecialException.Create(''); // Must have died
      end;

      UpdateThumbnail();
      Sleep(1000);

      for i := 1 to 4 do
      begin
        if (Terminated) then
          Break;

        UpdateInfo;
        Sleep(1000);
      end;
    except
      on e: ESpecialException do
      begin
        Writeln('TUpdateThread: Window died');
        FWindowHandle := 0;
        Empty;
        Break;
      end;

      on e: Exception do
      begin
        Writeln('TUpdateThread: Exception');
        FWindowHandle := 0;
        Empty;
        Break;
      end;
    end;

    Sleep(500);
  end;

  Window.Free;
  Mouse.Free;
end;

constructor TUpdateThread.Create(AImage: TImage; AImageBox: TNalaGroupBox; AProperties: TNalaValueList);
begin
  inherited Create(True);

  FImage := AImage;
  FImageBox := AImageBox;
  FProperties := AProperties;
end;

{ TNalaClientPanel }

procedure TNalaClientPanel.DoResizeImage(Sender: TObject);
var
  Splitter: TAnchorDockSplitter;
begin
  // For default layout
  if (not DockMaster.IsFloating(Self)) and (GetDockSplitter(DockMaster.GetSite(Self), akBottom, Splitter)) then
    Splitter.SetSplitterPosition(FImageBox.Top + FImageBox.Height + DockMaster.GetAnchorSite(Self).Header.Height + 5);
end;

procedure TNalaClientPanel.DoButtonClick(Sender: TObject);

  procedure Pick();
  begin
    try
      FUpdateThread.WindowHandle := PickClient();
    except
    end;
  end;

  procedure BringToFront();
  begin
    if (FUpdateThread.WindowHandle > 0) then
      with TNalaWindow.Create(FUpdateThread.WindowHandle) do
      try
        BringToFront();
      finally
        Free();
      end;
  end;

begin
  if (Sender = FButtonPick) then
    Pick()
  else
  if (Sender = FButtonBringToFront) then
    BringToFront();
end;

constructor TNalaClientPanel.Create(AOwner: TComponent; AName: String);
begin
  inherited Create(AOwner, AName);

  FButtonPick := TNalaCustomButton.Create(Self);
  with FButtonPick do
  begin
    Parent := Self;
    Top := 1;
    Left := 5;
    Width := 22;
    Height := 22;
    Hint := 'Pick Client';
    ShowHint := True;
    OnClick := @DoButtonClick;

    NalaForm.Images16x16.GetBitmap(9, Glyph);
  end;

  FButtonBringToFront := TNalaCustomButton.Create(Self);
  with FButtonBringToFront do
  begin
    Parent := Self;
    Top := 1;
    Width := 22;
    Height := 22;
    Hint := 'Bring To Front';
    ShowHint := True;
    OnClick := @DoButtonClick;
    AnchorToNeighbour(akLeft, 5, FButtonPick);

    NalaForm.Images16x16.GetBitmap(6, Glyph);
  end;

  FProperties := TNalaValueList.Create(Self);
  with FProperties do
  begin
    Parent := Self;
    Left := 5;
    RowCount := 5;
    KeyColumnWidth := 70;
    AnchorToNeighbour(akTop, 5, FButtonPick);
    AnchorParallel(akRight, 5, Self);

    Cells[0, 0] := 'Handle';
    Cells[0, 1] := 'Title';
    Cells[0, 2] := 'Size';
    Cells[0, 3] := 'Position';
    Cells[0, 4] := 'Cursor';
  end;

  FImageBox := TNalaGroupBox.Create(Self);
  with FImageBox do
  begin
    Parent := Self;
    Left := 5;
    AnchorToNeighbour(akTop, 1, FProperties);
    AnchorParallel(akRight, 5, Self);
    OnResize := @DoResizeImage;
  end;

  FImage := TImage.Create(Self);
  with FImage do
  begin
    Parent := FImageBox.InnerPanel;
    Align := alClient;
    Center := True;
  end;

  FUpdateThread := TUpdateThread.Create(FImage, FImageBox, FProperties);
  FUpdateThread.WindowHandle := 0;
  FUpdateThread.Start;
end;

destructor TNalaClientPanel.Destroy;
begin
  FUpdateThread.Terminate;
  FUpdateThread.Free;

  inherited Destroy;
end;

end.

