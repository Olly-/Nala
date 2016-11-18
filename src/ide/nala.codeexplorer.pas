unit nala.CodeExplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, nala.Parser.Code, nala.SynEdit,
  nala.CodeTree;

type
  TNalaCodeExplorer = class;

  { TNalaScriptTreeUpdater }

  TNalaScriptTreeUpdater = class(TThread)
  private
    FTree: TNalaCodeTree;
    FTreeBuffer: TNalaCodeTree;
    FVirtualSynEdit: record
      Text: String;
      LastModified: TDateTime;
      Pos: Int32;
    end;

    procedure UpdateInfo;
    procedure UpdateTree;
    procedure UpdateBuffer(Parser: TCodeParser; Pos: Int32);
  protected
    procedure Execute; override;
    procedure DoTerminate; override;

    constructor Create(ATree: TNalaCodeTree);
  end;

  { TNalaCodeExplorer }

  TNalaCodeExplorer = class(TPageControl)
  private
    FNalaTab: TTabSheet;
    FNalaTree: TNalaCodeTree;

    FScriptTab: TTabSheet;
    FScriptTree: TNalaCodeTree;

    FScriptTreeUpdater: TNalaScriptTreeUpdater;

    procedure AddSection(AName: String; constref SectionDump: String);
    procedure FillNala;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  nala.LapeCompiler, nala.MainForm;

procedure TNalaScriptTreeUpdater.UpdateInfo;
var
  SynEdit: TNalaSynEdit;
begin
  SynEdit := NalaForm.ScriptTabs.ActiveTab.SynEdit;

  FVirtualSynEdit.LastModified := SynEdit.LastModified;
  FVirtualSynEdit.Pos := SynEdit.SelStart;
  FVirtualSynEdit.Text := SynEdit.Text;
end;

procedure TNalaScriptTreeUpdater.UpdateTree;
begin
  FTree.Assign(FTreeBuffer);
end;

procedure TNalaScriptTreeUpdater.UpdateBuffer(Parser: TCodeParser; Pos: Int32);
var
  i: Int32;
begin
  {
  FTreeBuffer.BeginUpdate;
  FTreeBuffer.Items.Clear;

  for i := 0 to Parser.Items.Count - 1 do
    if (Parser.Items[i].ClassType = TCPMethod) then
      FTreeBuffer.AddMethod(TCPMethod(Parser.Items[i]), (Pos > Parser.Items[i].StartPos) and (Pos < (Parser.Items[i].EndPos - 1)))
    else
    if (Parser.Items[i].ClassType = TDeclVariable) then
      FTreeBuffer.AddVar(TDeclVariable(Parser.Items[i]))
    else
    if (Parser.Items[i].ClassType = TDeclConstant) then
      FTreeBuffer.AddConst(TDeclConstant(Parser.Items[i]))
    else
    if (Parser.Items[i].ClassType = TCPTypeDeclaration) then
      FTreeBuffer.AddType(TCPTypeDeclaration(Parser.Items[i]));

  FTreeBuffer.EndUpdate;
  }
end;

procedure TNalaScriptTreeUpdater.Execute;
var
  LastUpdate: TDateTime = 0;
  Parser: TCodeParser;
begin
 {
  try
    while (not Terminated) do
    begin
      Synchronize(@UpdateInfo);

      if (FVirtualSynEdit.LastModified <> LastUpdate) then
      begin
        Parser := TCodeParser.Create;
        try
          if (Parser.Run(FVirtualSynEdit.Text)) then
            UpdateBuffer(Parser, FVirtualSynEdit.Pos);
        finally
          Parser.Free;
        end;

        Synchronize(@UpdateTree);

        LastUpdate := FVirtualSynEdit.LastModified;
      end;

      Sleep(1000);
    end;
  except
    on e: Exception do
      Writeln('Exception on TNalaScriptTreeUpdater.Execute: ' + e.ClassName + ': ' + e.Message);
  end;
  }
end;

procedure TNalaScriptTreeUpdater.DoTerminate;
begin
  FTreeBuffer.Free;

  inherited;
end;

constructor TNalaScriptTreeUpdater.Create(ATree: TNalaCodeTree);
begin
  inherited Create(False);

  FreeOnTerminate := True;

  FTree := ATree;
  FTreeBuffer := TNalaCodeTree.Create(nil);
end;

procedure TNalaCodeExplorer.AddSection(AName: String; constref SectionDump: String);
var
  Node: TTreeNode;
begin
  with TCodeParser.Create do
  try
    Run(SectionDump);

    Node := FNalaTree.Items.Add(nil, AName);
    Node.ImageIndex := 5;
    Node.SelectedIndex := 5;

    FNalaTree.LoadFromDecls(Items, Node);
  finally
    Free;
  end;
end;

procedure TNalaCodeExplorer.FillNala;
var
  Compiler: TLPCompiler;
begin
  Compiler := TLPCompiler.Create('', AllImports, nil, True);
  Compiler.Dump.TraverseSections(@AddSection);
  Compiler.Free;
end;

constructor TNalaCodeExplorer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FNalaTab := Self.AddTabSheet;
  FNalaTab.Caption := 'Nala';

  FNalaTree := TNalaCodeTree.Create(FNalaTab);
  FNalaTree.Parent := FNalaTab;
  FNalaTree.Align := alClient;
  FNalaTree.BorderStyle := bsNone;
  FNalaTree.Images := NalaForm.Images16x16;

  FScriptTab := Self.AddTabSheet;
  FScriptTab.Caption := 'Script';

  FScriptTree := TNalaCodeTree.Create(FNalaTab);
  FScriptTree.Parent := FScriptTab;
  FScriptTree.Align := alClient;
  FScriptTree.BorderStyle := bsNone;
  FScriptTree.Images := NalaForm.Images16x16;
  FScriptTree.DoubleBuffered := True;

  FScriptTreeUpdater := TNalaScriptTreeUpdater.Create(FScriptTree);

  FillNala;
end;

destructor TNalaCodeExplorer.Destroy;
begin
  //FScriptTreeUpdater.Terminate;
  //FScriptTreeUpdater.WaitFor;

  inherited Destroy;
end;

end.

