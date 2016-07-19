unit nala.MainForm;

{$DEFINE HEAPTRC} // + Enable in project options / debugging

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AnchorDockStorage, Types, XMLPropStorage, ComCtrls, ExtCtrls, Menus, LCLIntf, LCLType,
  nala.AnchorDocking, nala.Panel, nala.ScriptTab, nala.ClientPanel, nala.ColorPanel, nala.CoreTypes,
  nala.Window, nala.Bitmap, nala.CodeExplorer, nala.LapeCompiler
  {$IFDEF HEAPTRC},
    heaptrc
  {$ENDIF};

type
  EButtonState = (bsRunningScript, bsStoppingScript, bsStoppedScript);

  { TNalaForm }

  TNalaForm = class(TForm)
    Images12x12: TImageList;
    Images22x22: TImageList;
    Images16x16: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MI_Messages: TMenuItem;
    MI_Explorer: TMenuItem;
    ToolBar: TToolBar;
    btnNewScript: TToolButton;
    btnOpenScript: TToolButton;
    btnSaveScript: TToolButton;
    btnPlayScript: TToolButton;
    btnStopScript: TToolButton;
    btnCompile: TToolButton;
    tbSeperator1: TToolButton;
    tbSeperator2: TToolButton;
    tbSeperator3: TToolButton;

    procedure btnOpenScriptClick(Sender: TObject);
    procedure DoButtonClick(Sender: TObject);
    procedure DoMenuClick(Sender: TObject);
    procedure DoWindowShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FButtonState: EButtonState;
    FScriptTabs: TNalaTabs;
    FMinHeight, FMinWidth: UInt32;
    FExplorerPanel, FEditorPanel, FMessagePanel: TNalaPanel;
    FClientPanel: TNalaClientPanel;
    FColorPanel: TNalaColorPanel;
    FCodeExplorer: TNalaCodeExplorer;

    procedure setButtonState(AValue: EButtonState);

    procedure CreateDocking;
    procedure DefaultDocking(Reset: Boolean);
  public
    property ExplorerPanel: TNalaPanel read FExplorerPanel;
    property EditorPanel: TNalaPanel read FEditorPanel;
    property MessagePanel: TNalaPanel read FMessagePanel;
    property ClientPanel: TNalaClientPanel read FClientPanel;
    property ColorPanel: TNalaColorPanel read FColorPanel;

    property CodeExplorer: TNalaCodeExplorer read FCodeExplorer;

    property ButtonState: EButtonState read FButtonState write setButtonState;
  end;

var
  NalaForm: TNalaForm;

implementation

uses
  LazFileUtils, nala.OSUtils;

const
  BUILD_INFO = 'Built on : ' + {$I %DATE%} + LineEnding + 'with FPC version: ' + {$I %FPCVERSION%};

{$R *.lfm}

{ TNalaForm }

procedure TNalaForm.FormCreate(Sender: TObject);
begin
  Writeln('Create: Docking');
  CreateDocking();
  DefaultDocking(False);

  Writeln('Create: Explorer');
  FCodeExplorer := TNalaCodeExplorer.Create(Self);
  FCodeExplorer.Parent := FExplorerPanel;
  FCodeExplorer.Align := alClient;

  Writeln('Create: Tabs');
  FScriptTabs := TNalaTabs.Create(FEditorPanel);
  FScriptTabs.Add('Tab 1', DEFAULT_SCRIPT);
  FScriptTabs.Add('Tab 2', DEFAULT_SCRIPT);
end;

procedure TNalaForm.DoMenuClick(Sender: TObject);
begin
  if (Sender.Equals(MenuItem4)) then
    DefaultDocking(True);
end;

procedure TNalaForm.DoWindowShow(Sender: TObject);

  procedure DoShow(Panel: TNalaPanel);
  begin
    if (Panel.Showing) then
    begin
      DockMaster.ManualFloat(Panel);
      DockMaster.GetSite(Panel).Hide
    end
    else
      DockMaster.GetSite(Panel).Show;
  end;

begin

  if (Sender = MI_Explorer) then
    DoShow(FExplorerPanel);
end;

procedure TNalaForm.btnOpenScriptClick(Sender: TObject);
begin
  with TFileDialog.Create(nil) do
  try
    Filter := '*.(pas|pp|inc|nala)';
    if (Execute) then
      FScriptTabs.Add(FileName);
  finally
    Free;
  end;
end;

procedure TNalaForm.DoButtonClick(Sender: TObject);
begin
  if (Sender.Equals(btnPlayScript)) then
    FScriptTabs.ActiveTab.RunScript
  else
  if (Sender.Equals(btnCompile)) then
    FScriptTabs.ActiveTab.CompileScript
  else
  if (Sender.Equals(btnStopScript)) then
    FScriptTabs.ActiveTab.StopScript
  else
  if (Sender.Equals(btnNewScript)) then
    FScriptTabs.Add('default', DEFAULT_SCRIPT);
end;

procedure TNalaForm.setButtonState(AValue: EButtonState);

  procedure NormalStop;
  begin
    btnStopScript.ImageIndex := 13;
    btnStopScript.Hint := 'Terminate Script';
  end;

  procedure ForceStop;
  begin
    btnStopScript.ImageIndex := 15;
    btnStopScript.Hint := 'Force Terminate Script';
  end;

begin
  if (FButtonState = AValue) then
    Exit;
  FButtonState := AValue;

  case AValue of
    bsStoppedScript:
      begin
        btnPlayScript.Enabled := True;
        btnCompile.Enabled := True;
        btnStopScript.Enabled := False;

        NormalStop();
      end;
    bsRunningScript:
      begin
        btnPlayScript.Enabled := False;
        btnCompile.Enabled := False;
        btnStopScript.Enabled := True;

        NormalStop();
      end;
    bsStoppingScript:
      begin
        btnPlayScript.Enabled := False;
        btnCompile.Enabled := False;
        btnStopScript.Enabled := True;

        ForceStop();
      end;
  end;
end;

procedure TNalaForm.CreateDocking;
var
  m: TAnchorDockManager;
begin
  DockMaster.MakeDockSite(Self, [akBottom], admrpChild);
  DockMaster.HideHeaderCaptionFloatingControl := False;

  // Lazarus fix
  if (DockManager is TAnchorDockManager) then
  begin
    m := TAnchorDockManager(DockManager);
    m.PreferredSiteSizeAsSiteMinimum := False;
  end;

  FExplorerPanel := TNalaPanel.Create(Self, 'Explorer');
  FEditorPanel := TNalaPanel.Create(Self, 'Editor');
  FMessagePanel := TNalaPanel.Create(Self, 'Messages');
  FClientPanel := TNalaClientPanel.Create(Self, 'Client');
  FColorPanel := TNalaColorPanel.Create(Self, 'Colors');

  FMinWidth := 500;
  FMinHeight := 300;
end;

procedure TNalaForm.DefaultDocking(Reset: Boolean);
var
  Pos: TPoint;
  Control: TControl;
begin
  Pos.X := Self.Left;
  Pos.Y := Self.Top;

  if (Reset) then
  begin
    DockMaster.ManualFloat(FExplorerPanel);
    DockMaster.ManualFloat(FEditorPanel);
    DockMaster.ManualFloat(FMessagePanel);
    DockMaster.ManualFloat(FClientPanel);
    DockMaster.ManualFloat(FColorPanel);
  end;

  DockMaster.BeginUpdate;
  try
    Self.Width := 800;
    Self.Height := ToolBar.Height;

    // Dock stuff so we get some pretty default setup
    DockMaster.GetAnchorSite(FEditorPanel).Height := 525;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(FEditorPanel), DockMaster.GetSite(Self), alBottom);

    DockMaster.GetAnchorSite(FMessagePanel).Height := 240;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(FMessagePanel), DockMaster.GetSite(FEditorPanel), alBottom);

    DockMaster.GetAnchorSite(FExplorerPanel).Height := 240;
    DockMaster.GetAnchorSite(FExplorerPanel).Width := 160;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(FExplorerPanel), DockMaster.GetSite(FEditorPanel), alLeft);

    DockMaster.GetAnchorSite(FClientPanel).Height := 240;
    DockMaster.GetAnchorSite(FClientPanel).Width := 260;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(FClientPanel), DockMaster.GetSite(FEditorPanel), alRight);

    DockMaster.GetAnchorSite(FColorPanel).Width := 260;
    DockMaster.GetAnchorSite(FColorPanel).Height := 240;
    DockMaster.ManualDock(DockMaster.GetAnchorSite(FColorPanel), DockMaster.GetSite(FClientPanel), alBottom, FClientPanel);

    // Hide top scrollbar that connects all the other forms to the main form
    if (GetDockSplitterOrParent(DockMaster.GetSite(FEditorPanel), akTop, Control)) and (Control is TAnchorDockHostSite) then
      TAnchorDockHostSite(Control).BoundSplitter.Hide;

    FExplorerPanel.InitHeader;
    FEditorPanel.InitHeader;
    FMessagePanel.InitHeader;
    FClientPanel.InitHeader;
    FColorPanel.InitHeader;
  finally
    Self.Left := Pos.X;
    Self.Top := Pos.Y;

    DockMaster.EndUpdate;
  end;
end;

var
  Path: String;

initialization
  Writeln(BUILD_INFO);

  {$IFDEF HEAPTRC}
    Path := GetCurrentDirUTF8() + DirectorySeparator + 'ML.txt';
    Writeln('HEAPTRC defined; writing memory leaks to ', Path);

    if (FileExists(Path)) then
      with TStringList.Create do
      try
        LoadFromFile(Path);
        if (Text <> '') and (Pos('0 unfreed memory blocks', Text) = 0) then
          ShowMessage('Last run leaked memory, see: ' + LineEnding + Path);
        Clear;
        SaveToFile(Path);
      finally
        Free;
      end;

    SetHeapTraceOutput(Path);
  {$ENDIF}

end.
