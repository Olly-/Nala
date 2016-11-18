unit nala.MainForm;

{.$DEFINE HEAPTRC} { + Enable in project options / debugging }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  AnchorDockStorage, ComCtrls, ExtCtrls, Menus, LCLIntf, LCLType,
  nala.AnchorDocking, nala.Panel, nala.ScriptTab, nala.ClientPanel, nala.ColorPanel,
  nala.Types, nala.CodeExplorer, nala.Environment
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
    Menu_Tools_Templates: TMenuItem;
    Menu_Tools: TMenuItem;
    Menu_File: TMenuItem;
    Menu_Script: TMenuItem;
    Menu_View: TMenuItem;
    Menu_File_Open: TMenuItem;
    Menu_File_OpenRecent: TMenuItem;
    Menu_File_SaveAs: TMenuItem;
    Menu_File_Save: TMenuItem;
    Menu_File_Divider: TMenuItem;
    Menu_File_OpenTemplate: TMenuItem;
    Menu_View_Reset: TMenuItem;
    Menu_View_Open: TMenuItem;
    Menu_View_Save: TMenuItem;
    Menu_View_Divider: TMenuItem;
    Menu_View_Colors: TMenuItem;
    Menu_View_Client: TMenuItem;
    Menu_View_Messages: TMenuItem;
    Menu_View_Explorer: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
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

    procedure DoButtonClick(Sender: TObject);
    procedure MenuViewClick(Sender: TObject);
    procedure MenuFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuToolsClick(Sender: TObject);
    procedure Menu_FileClick(Sender: TObject);
  private
    FButtonState: EButtonState;
    FScriptTabs: TNalaTabs;
    FExplorerPanel, FEditorPanel, FMessagePanel: TNalaPanel;
    FClientPanel: TNalaClientPanel;
    FColorPanel: TNalaColorPanel;
    FCodeExplorer: TNalaCodeExplorer;

    procedure SetButtonState(AValue: EButtonState);

    procedure CreateDocking;
    procedure DefaultDocking(Reset: Boolean);
    procedure DoDockingChange(Sender: TAnchorDockHostSite; AShowing: Boolean);
  public
    property ExplorerPanel: TNalaPanel read FExplorerPanel;
    property EditorPanel: TNalaPanel read FEditorPanel;
    property MessagePanel: TNalaPanel read FMessagePanel;
    property ClientPanel: TNalaClientPanel read FClientPanel;
    property ColorPanel: TNalaColorPanel read FColorPanel;

    property CodeExplorer: TNalaCodeExplorer read FCodeExplorer;
    property ScriptTabs: TNalaTabs read FScriptTabs;

    property ButtonState: EButtonState read FButtonState write SetButtonState;
  end;

var
  NalaForm: TNalaForm;

implementation

uses
  LazFileUtils, nala.TemplateForm, nala.Parser.Script;

{$R *.lfm}

{ TNalaForm }

procedure TNalaForm.FormCreate(Sender: TObject);
begin
  CreateDocking;
  DefaultDocking(False);

  FCodeExplorer := TNalaCodeExplorer.Create(Self);
  FCodeExplorer.Parent := FExplorerPanel;
  FCodeExplorer.Align := alClient;

  FScriptTabs := TNalaTabs.Create(FEditorPanel);
  FScriptTabs.AddTab();

  SetButtonState(bsStoppedScript);
end;

procedure TNalaForm.MenuToolsClick(Sender: TObject);
begin
  TemplateForm.ShowModal;
end;

procedure TNalaForm.Menu_FileClick(Sender: TObject);
var
  i: Int32;
  Files: TStringList;
  Item: TMenuItem;
begin
  Menu_File_OpenTemplate.Clear;
  Files := FindAllFiles(NalaEnvironment.Paths['Templates'], '*.nala');

  try
    for i := 0 to Files.Count - 1 do
    begin
      Item := TMenuItem.Create(Menu_File_OpenTemplate);
      Item.Caption := ExtractFileNameWithoutExt(ExtractFileName(Files[i]));
      Item.OnClick := @MenuFileClick;

      Menu_File_OpenTemplate.Add(Item);
    end;
  finally
    Files.Free;
  end;
end;

procedure TNalaForm.MenuViewClick(Sender: TObject);
begin
  if (Sender = Menu_View_Reset) then
    DefaultDocking(True)
  else
  if (Sender = Menu_View_Open) then
    FScriptTabs.LoadTab
  else
  if (Sender = Menu_View_Save) then
    FScriptTabs.ActiveTab.Save
  else
  if (Sender = Menu_View_Explorer) then
    FExplorerPanel.ToggleVisible
  else
  if (Sender = Menu_View_Client) then
    FClientPanel.ToggleVisible
  else
  if (Sender = Menu_View_Messages) then
    FMessagePanel.ToggleVisible
  else
  if (Sender = Menu_View_Colors) then
    FColorPanel.ToggleVisible;
end;

procedure TNalaForm.MenuFileClick(Sender: TObject);
begin
  if (Sender = Menu_File_Open) then
    FScriptTabs.LoadTab
  else
  if (Sender = Menu_File_OpenRecent) then
    { nothing }
  else
  if (Sender = Menu_File_OpenTemplate) then
    { nothing }
  else
  if (Sender = Menu_File_SaveAs) then
    FScriptTabs.ActiveTab.SaveAs
  else
  if (Sender = Menu_File_Save) then
    FScriptTabs.ActiveTab.Save
  else
    FScriptTabs.LoadTab(NalaEnvironment.Paths['Templates'] + TMenuItem(Sender).Caption + '.nala');
end;

procedure TNalaForm.DoButtonClick(Sender: TObject);
begin
  if (Sender = btnPlayScript) then
    FScriptTabs.ActiveTab.RunScript
  else
  if (Sender = btnCompile) then
    FScriptTabs.ActiveTab.CompileScript
  else
  if (Sender = btnStopScript) then
    FScriptTabs.ActiveTab.StopScript
  else
  if (Sender = btnNewScript) then
    FScriptTabs.AddTab
  else
  if (Sender = btnOpenScript) then
    FScriptTabs.LoadTab
  else
  if (Sender = btnSaveScript) then
    FScriptTabs.ActiveTab.Save;
end;

procedure TNalaForm.SetButtonState(AValue: EButtonState);

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
  DockMaster.OnSiteVisibilityChange := @DoDockingChange;

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

    DockMaster.MakeVisible(FExplorerPanel, True);
    DockMaster.MakeVisible(FEditorPanel, True);
    DockMaster.MakeVisible(FMessagePanel, True);
    DockMaster.MakeVisible(FClientPanel, True);
    DockMaster.MakeVisible(FColorPanel, True);
  finally
    Self.Left := Pos.X;
    Self.Top := Pos.Y;

    DockMaster.EndUpdate;
  end;
end;

procedure TNalaForm.DoDockingChange(Sender: TAnchorDockHostSite; AShowing: Boolean);
begin
  if (Sender = FExplorerPanel.Site) then
    Menu_View_Explorer.Checked := AShowing
  else
  if (Sender = FColorPanel.Site) then
    Menu_View_Colors.Checked := AShowing
  else
  if (Sender = FClientPanel.Site) then
    Menu_View_Client.Checked := AShowing
  else
  if (Sender = FMessagePanel.Site) then
    Menu_View_Messages.Checked := AShowing;
end;

{$IFDEF HEAPTRC}
procedure TraceMemoryLeaks;
var
  Path: String;
begin
  Path := NalaEnvironment.Files['MemoryLeaks'];
  Writeln('Writing memory leaks to: ', Path);

  if (FileExists(Path)) then
  begin
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
  end else
    FileCreateUTF8(Path);

  SetHeapTraceOutput(Path);
end;
{$ENDIF}

initialization
  Writeln('Build date: ' + {$I %DATE%});
  Writeln('FPC version: ' + {$I %FPCVERSION%});

  {$IFDEF HEAPTRC}
    TraceMemoryLeaks;
  {$ENDIF}

end.
