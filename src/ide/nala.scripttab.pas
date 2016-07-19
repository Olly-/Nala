unit nala.ScriptTab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ExtCtrls, Buttons,
  nala.Messages, nala.CodeExplorer, nala.SynEdit, nala.ScriptThread, nala.ListBox, nala.ScriptTree;

type

  { TNalaTab }

  TNalaTab = class(TTabSheet)
  private
    FSynEdit: TNalaSynEdit;
    FMessages: TNalaMessages;
    FThread: TNalaScriptThread;
    FScriptRunning: Boolean;
    FScriptTree: TNalaScriptTree;

    procedure InitThread;

    procedure OnThreadTerminate(Sender: TObject);
  public
    property Messages: TNalaMessages read FMessages;
    property ScriptRunning: Boolean read FScriptRunning;
    property SynEdit: TNalaSynEdit read FSynEdit;

    procedure HideComponents;
    procedure ShowComponents;

    procedure RunScript;
    procedure CompileScript;
    procedure StopScript;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TNalaTabs }

  TNalaTabs = class(TComponent)
  private
    FPageControl: TPageControl;
    FLastTab: TNalaTab;

    procedure DoChange(Sender: TObject);
    function getActiveTab: TNalaTab;
  public
    property ActiveTab: TNalaTab read getActiveTab;

    procedure Add(ACaption: String; AScript: String);
    procedure Add(APath: String);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  DEFAULT_SCRIPT = 'program new; ' + LineEnding +
                   'begin        ' + LineEnding +
                   'end.';

implementation

uses
  nala.MainForm, Graphics, FileUtil;

{ TNalaTabs }

procedure TNalaTabs.DoChange(Sender: TObject);
begin
  if (FLastTab <> nil) then
    FLastTab.HideComponents;
  FLastTab := FPageControl.ActivePage as TNalaTab;

  with ActiveTab do
  begin
    ShowComponents();
    if (ScriptRunning) then
      NalaForm.ButtonState := bsRunningScript
    else
      NalaForm.ButtonState := bsStoppedScript;
  end;
end;

function TNalaTabs.getActiveTab: TNalaTab;
begin
  Result := FPageControl.ActivePage as TNalaTab
end;

procedure TNalaTabs.Add(ACaption: String; AScript: String);
begin
  with TNalaTab.Create(Self) do
  begin
    Parent := FPageControl;
    Caption := ACaption;

    FSynEdit.Lines.Text := AScript;
  end;

  FPageControl.ActivePageIndex := FPageControl.PageCount - 1;
end;

procedure TNalaTabs.Add(APath: String);
begin
  with TNalaTab.Create(Self) do
  begin
    Parent := FPageControl;
    Caption := ExtractFileName(APath);

    FSynEdit.Lines.LoadFromFile(APath);
  end;

  FPageControl.ActivePageIndex := FPageControl.PageCount - 1;
end;

constructor TNalaTabs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := AOwner as TWinControl;
  FPageControl.OnChange := @DoChange;
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

procedure TNalaTab.HideComponents;
begin
  FScriptTree.Running := False;
  FMessages.Hide;
end;

procedure TNalaTab.ShowComponents;
begin
  FSynEdit.LastModified := Now(); // To force a update
  FScriptTree.Running := True;
  FMessages.Show;
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

constructor TNalaTab.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FMessages := TNalaMessages.Create(NalaForm.MessagePanel);
  FSynEdit := TNalaSynEdit.Create(Self);
  FScriptTree := TNalaScriptTree.Create(FSynEdit);

  HideComponents();
end;

destructor TNalaTab.Destroy;
begin
  FScriptTree.Terminate;
  FScriptTree.WaitFor;

  FMessages.Free;

  inherited Destroy;
end;

end.
