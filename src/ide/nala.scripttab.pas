unit nala.ScriptTab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ExtCtrls, Buttons, Dialogs,
  nala.Messages, nala.SynEdit, nala.ScriptThread;

type

  { TNalaTab }

  TNalaTab = class(TTabSheet)
  private
    FSynEdit: TNalaSynEdit;
    FMessages: TNalaMessages;
    FThread: TNalaScriptThread;
    FScriptRunning: Boolean;
    FFilePath: String;

    procedure InitThread;
    procedure OnThreadTerminate(Sender: TObject);

    procedure HideComponents(Sender: TObject);
    procedure ShowComponents(Sender: TObject);
  public
    property Messages: TNalaMessages read FMessages;
    property ScriptRunning: Boolean read FScriptRunning;
    property SynEdit: TNalaSynEdit read FSynEdit;
    property FilePath: String read FFilePath write FFilePath;

    procedure RunScript;
    procedure CompileScript;
    procedure StopScript;

    procedure Save;
    procedure SaveAs;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TNalaTabs }

  TNalaTabs = class(TComponent)
  private
    FPageControl: TPageControl;
    FLastTab: TNalaTab;

    function getActiveTab: TNalaTab;
  public
    property ActiveTab: TNalaTab read getActiveTab;

    function AddTab: TNalaTab;
    procedure LoadTab;
    procedure LoadTab(Path: String); overload;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  nala.MainForm, Graphics, FileUtil;

{ TNalaTabs }

function TNalaTabs.GetActiveTab: TNalaTab;
begin
  Result := TNalaTab(FPageControl.ActivePage);
end;

procedure TNalaTabs.LoadTab;
var
  i: Int32;
begin
  if (NalaForm.OpenDialog.Execute) then
    for i := 0 to NalaForm.OpenDialog.Files.Count - 1 do
      Self.LoadTab(NalaForm.OpenDialog.Files[i]);
end;

procedure TNalaTabs.LoadTab(Path: String);
begin
 with Self.AddTab do
 begin
   SynEdit.Load(Path);
   Caption := ExtractFileNameWithoutExt(ExtractFileName(Path));
 end;
end;

function TNalaTabs.AddTab: TNalaTab;
begin
  Result := TNalaTab.Create(Self);
  with Result do
  begin
    Parent := FPageControl;
    Caption := 'New';

    SynEdit.Lines.AddStrings(['program new;', 'begin', 'end;']);
  end;

  FPageControl.ActivePageIndex := FPageControl.PageCount - 1;
end;

constructor TNalaTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := TWinControl(Owner);
  FPageControl.Align := alClient;

  FLastTab := nil;
end;

destructor TNalaTabs.Destroy;
begin
  FPageControl.Free;

  inherited Destroy;
end;

{ TNalaTab }

procedure TNalaTab.InitThread;
begin
  FThread := TNalaScriptThread.Create;
  with FThread do
  begin
    Script := FSynEdit.Text;
    OnTerminate := @Self.OnThreadTerminate;
    MessageBox := Self.Messages.Messages;
    DebugBox := Self.Messages.Debug;
  end;

  NalaForm.ButtonState := bsRunningScript;
  FScriptRunning := True;
end;

procedure TNalaTab.OnThreadTerminate(Sender: TObject);
begin
  FScriptRunning := False;

  if (PageControl.ActivePage = Self) then
    NalaForm.ButtonState := bsStoppedScript;
end;

procedure TNalaTab.HideComponents(Sender: TObject);
begin
  FMessages.Hide;
end;

procedure TNalaTab.ShowComponents(Sender: TObject);
begin
  FMessages.Show;

  case FScriptRunning of
    True: NalaForm.ButtonState := bsRunningScript;
    False: NalaForm.ButtonState := bsStoppedScript;
  end;
end;

procedure TNalaTab.RunScript;
begin
  InitThread;

  FThread.CompileOnly := False;
  FThread.Start;
end;

procedure TNalaTab.CompileScript;
begin
  InitThread;

  FThread.CompileOnly := True;
  FThread.Start;
end;

procedure TNalaTab.StopScript;
begin
  if (FScriptRunning) then
    if (NalaForm.ButtonState = bsStoppingScript) then // If the button is already pressed force terminate it (will mem leak)
    begin
      Sleep(500); // Give it some time to safely terminate
      if (FScriptRunning) and (KillThread(FThread.Handle) = 0) then // Force kill, and welcome the memory leak!
      begin
        Writeln('Script thread[', FThread.ThreadID, '] force terminated');
        FThread.Free;
        OnThreadTerminate(nil);
      end;
    end else
    begin
      NalaForm.ButtonState := bsStoppingScript;
      FThread.Running := False;
    end;
end;

procedure TNalaTab.SaveAs;
begin
  if (NalaForm.SaveDialog.Execute) then
    FSynEdit.Save(NalaForm.SaveDialog.FileName);
end;

procedure TNalaTab.Save;
begin
  if (FFilePath = '') or (not FileExists(FFilePath)) then
    Self.SaveAs
  else
    FSynEdit.Save(FFilePath);
end;

constructor TNalaTab.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FMessages := TNalaMessages.Create(NalaForm.MessagePanel);
  FSynEdit := TNalaSynEdit.Create(Self);
  FFilePath := '';

  OnHide := @HideComponents;
  OnShow := @ShowComponents;
end;

destructor TNalaTab.Destroy;
begin
  FMessages.Free;

  inherited Destroy;
end;

end.
