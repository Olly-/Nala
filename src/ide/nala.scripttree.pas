unit nala.ScriptTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.SynEdit, nala.CodeParser, nala.CodeTree;

type

  { TNalaScriptTree }

  TNalaScriptTree = class(TThread)
  private
    FSynEdit: TNalaSynEdit;
    FTreeBuffer: TNalaCodeTree;
    FRunning: Boolean;

    procedure UpdateTree;
    procedure UpdateBuffer(Parser: TCodeParser; Pos: Int32);
  protected
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(ASynEdit: TNalaSynEdit);

    property Running: Boolean read FRunning write FRunning;
  end;

implementation

uses
  nala.MainForm;

{ TNalaScriptTree }

procedure TNalaScriptTree.UpdateTree;
begin
  NalaForm.CodeExplorer.FillScript(FTreeBuffer.Items);
end;

procedure TNalaScriptTree.UpdateBuffer(Parser: TCodeParser; Pos: Int32);
var
  i: Int32;
begin
  FTreeBuffer.BeginUpdate;
  FTreeBuffer.Items.Clear;

  for i := 0 to Parser.Items.Count - 1 do
    if (Parser.Items[i].ClassType = TDeclMethod) then
      FTreeBuffer.AddMethod(TDeclMethod(Parser.Items[i]), (Pos > Parser.Items[i].StartPos) and (Pos < (Parser.Items[i].EndPos - 1)))
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
end;

procedure TNalaScriptTree.Execute;
var
  LastUpdate: TDateTime = 0;
  Parser: TCodeParser;
begin
  try
    while (not Terminated) do
    begin
      Sleep(500);

      if (FRunning) and (FSynEdit.LastModified <> LastUpdate) then
      begin
        Parser := TCodeParser.Create;
        try
          if (Parser.Run(FSynEdit.Lines.Text)) then
            UpdateBuffer(Parser, FSynEdit.SelStart);
        finally
          Parser.Free;
        end;

        Synchronize(@UpdateTree);
        LastUpdate := FSynEdit.LastModified;
      end;

      Sleep(500);
    end;
  except
    on e: Exception do
      Writeln('Exception on TNalaScriptTree.Execute: ' + e.ClassName + ': ' + e.Message);
  end;
end;

procedure TNalaScriptTree.DoTerminate;
begin
  FTreeBuffer.Free;

  inherited;
end;

constructor TNalaScriptTree.Create(ASynEdit: TNalaSynEdit);
begin
  inherited Create(True);

  FreeOnTerminate := True;

  FSynEdit := ASynEdit;
  FTreeBuffer := TNalaCodeTree.Create(nil);
  FRunning := True;

  Start;
end;

end.

