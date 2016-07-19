unit nala.CodeExplorer;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Controls, ComCtrls, nala.CodeParser, nala.SynEdit, CastaliaPasLexTypes,
  nala.CodeTree;

type

  { TNalaCodeExplorer }

  TNalaCodeExplorer = class(TPageControl)
  private
    FNalaTab: TTabSheet;
    FNalaTree: TNalaCodeTree;

    FScriptTab: TTabSheet;
    FScriptTree: TNalaCodeTree;

    procedure FillNala;
  public
    procedure FillScript(Items: TTreeNodes);

    constructor Create(TheOwner: TComponent); override;
  end;

implementation

uses
  nala.LapeCompiler, nala.MainForm;

var
  CriticalSection: TRTLCriticalSection;

procedure TNalaCodeExplorer.FillNala;

   procedure AddSection(constref Name, Text: String);
   var
     Node: TTreeNode;
   begin
     with TCodeParser.Create do
     try
       Run(Text);

       Node := FNalaTree.Items.Add(nil, Name);
       Node.ImageIndex := 5;
       Node.SelectedIndex := 5;

       FNalaTree.LoadFromDecls(Items, Node);
     finally
       Free;
     end;
   end;

var
  Compiler: TLPCompiler;
  Section: String;
begin
  Compiler := TLPCompiler.Create('', True);

  try
    Compiler.Import([lpiCore, lpiBox, lpiString, lpiTime]);
    for Section in Compiler.Dump.Sections do
      AddSection(Section, Compiler.Dump[Section].Text);
  except
    on e: Exception do
      Writeln('TNalaCodeExplorer.FillNala: Exception: ' + e.Message);
  end;

  Compiler.Free;
end;

procedure TNalaCodeExplorer.FillScript(Items: TTreeNodes);
var
  i: Int32;
begin
  EnterCriticalSection(CriticalSection);

  try
    FScriptTree.BeginUpdate;
    FScriptTree.Items.Clear;
    FScriptTree.Items.Assign(Items);

    for i := 0 to Items.Count - 1 do
    begin
      if (Items[i].Data = nil) then
        Continue;

      FScriptTree.Items[i].Data := GetMem(SizeOf(TNodeData));
      Move(Items[i].Data^, FScriptTree.Items[i].Data^, SizeOf(TNodeData));
    end;

    FScriptTree.EndUpdate;
  finally
    LeaveCriticalsection(CriticalSection);
  end;
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

  FillNala();
end;

initialization
  InitCriticalSection(CriticalSection);

finalization
  DoneCriticalsection(CriticalSection);

end.

