unit nala_Tab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, ExtCtrls, Buttons,
  nala.Messages, nala.CodeExplorer, nala.SynEdit, nala_LapeThread, nala.ListBox;

type

  { TNalaTab }

  TNalaTab = class(TTabSheet)
  private
    FSynEdit: TNalaSynEdit;
    FExplorer: TNalaCodeExplorer;
    FMessages: TNalaMessages;
    FThread: TNalaLapeThread;
    FScriptRunning: Boolean;

    procedure InitThread;

    procedure WriteMessage;
    procedure WriteDebug;

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
  nala_Form, Graphics, FileUtil;

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

  DoChange(nil);
end;

procedure TNalaTabs.Add(APath: String);
begin
  with TNalaTab.Create(Self) do
  begin
    Parent := FPageControl;
    Caption := ExtractFileNameWithoutExt(APath);

    FSynEdit.Lines.LoadFromFile(APath);
  end;

  DoChange(nil);
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
  FThread := TNalaLapeThread.Create;
  with FThread do
  begin
    Script := FSynEdit.Text;
    MessageLnProc := @Self.WriteMessage;
    DebugLnProc := @Self.WriteDebug;
    OnTerminate := @Self.OnThreadTerminate;
    FScriptRunning := True;
    NalaForm.ButtonState := bsRunningScript;
  end;
end;

procedure TNalaTab.WriteMessage;

  function isError: Boolean;
  begin
    Result := (Copy(FThread.MessageBuffer, 2, 5) = 'error');
    if (Result) then
      Messages.Messages.Add(Copy(FThread.MessageBuffer, 7, Length(FThread.MessageBuffer)), mtError, True);
  end;

  function isNote: Boolean;
  begin
    Result := (Copy(FThread.MessageBuffer, 2, 4) = 'note');
    if (Result) then
      Messages.Messages.Add(Copy(FThread.MessageBuffer, 6, Length(FThread.MessageBuffer)), mtNote, True);
  end;

begin
  Messages.ShowMessages;
  if (Length(FThread.MessageBuffer) > 0) and (FThread.MessageBuffer[1] = '!') then
    if (isError()) or (isNote()) then
      Exit;

  Messages.Messages.Add(FThread.MessageBuffer, mtMessage, True);
end;

procedure TNalaTab.WriteDebug;
begin
  Messages.Debug.Add(FThread.DebugBuffer);
end;

procedure TNalaTab.OnThreadTerminate(Sender: TObject);
begin
  FScriptRunning := False;

  if (Self.PageControl.ActivePage = Self) then
    NalaForm.ButtonState := bsStoppedScript;
end;

procedure TNalaTab.HideComponents;
begin
  FExplorer.Hide;
  FMessages.Hide;
end;

procedure TNalaTab.ShowComponents;
begin
  FExplorer.Show;
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
  FExplorer := TNalaCodeExplorer.Create(Self);

  HideComponents();
end;

destructor TNalaTab.Destroy;
begin
  FExplorer.Free;
  FMessages.Free;
  FSynEdit.Free;

  inherited Destroy;
end;

end.

