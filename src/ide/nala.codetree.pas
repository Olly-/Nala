unit nala.CodeTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, StdCtrls,
  nala.CodeParser;

type
  PNodeData = ^TNodeData;
  TNodeData = record
    StartPos, EndPos: Integer;
    Typ: TClass;
  end;

  { TNalaCodeTree }

  TNalaCodeTree = class(TTreeView)
  private
    function AddNode(ADecl: TDeclaration; AText: String; ImageIndex: Int32 = -1; AParent: TTreeNode = nil): TTreeNode;
    procedure OnDeleteNode(Sender: TObject; Node: TTreeNode);

    function AddRecord(Decl: TCPTypeDeclaration; ParentNode: TTreeNode = nil): TTreeNode;
    function AddType(AText: String; ParentNode: TTreeNode = nil): TTreeNode;
  public
    function FindNode(Str: String; Typ: TClass): TTreeNode;

    function AddConst(Decl: TDeclConstant; ParentNode: TTreeNode = nil): TTreeNode;
    function AddVar(Decl: TDeclVariable; ParentNode: TTreeNode = nil): TTreeNode;
    function AddMethod(Decl: TDeclMethod; Locals: Boolean = False; ParentNode: TTreeNode = nil): TTreeNode;
    function AddType(Decl: TCPTypeDeclaration; ParentNode: TTreeNode = nil): TTreeNode; overload;

    procedure LoadFromDecls(List: TDeclarationList; ParentNode: TTreeNode);
    procedure Assign(From: TNalaCodeTree);

    constructor Create(AOwner: TComponent); override;
  end;

implementation

function TNalaCodeTree.AddNode(ADecl: TDeclaration; AText: String; ImageIndex: Int32; AParent: TTreeNode): TTreeNode;
begin
  if (AParent = nil) then
    Result := Items.Add(nil, AText)
  else
    Result := Items.AddChild(AParent, AText);

  Result.ImageIndex := ImageIndex;
  Result.SelectedIndex := ImageIndex;
  Result.Data := GetMem(SizeOf(TNodeData));

  with TNodeData(Result.Data^) do
  begin
    Typ := ADecl.ClassType;
    StartPos := ADecl.StartPos;
    EndPos := ADecl.EndPos;
  end;
end;

function TNalaCodeTree.FindNode(Str: String; Typ: TClass): TTreeNode;
begin
  Result := Items.GetFirstNode;
  while (Result <> nil) and ((Result.Text <> Str) and (Result.Data <> nil) and (TNodeData(Result.Data^).Typ <> Typ)) do
    Result := Result.GetNext;
end;

procedure TNalaCodeTree.OnDeleteNode(Sender: TObject; Node: TTreeNode);
begin
  if (Node.Data <> nil) then
  begin
    FreeMem(Node.Data);
    Node.Data := nil;
  end;
end;

function TNalaCodeTree.AddVar(Decl: TDeclVariable; ParentNode: TTreeNode): TTreeNode;
var
  Str: String;
begin
  Str := Decl.Name;
  if (Decl.Typ <> '') then
    Str += ': ' + Decl.Typ;
  if (Decl.Def <> '') then
    Str += ' = ' + Decl.Def;

  Result := AddNode(Decl, Str, 1, ParentNode);
end;

function TNalaCodeTree.AddMethod(Decl: TDeclMethod; Locals: Boolean; ParentNode: TTreeNode): TTreeNode;

  function StrBefore(constref Delimiter, Text: String): String;
  var
    p: UInt32;
  begin
    p := Pos(Delimiter, Text);
    if (p = 0) then
      Exit(Text);
    Result := Copy(Text, 1, p - 1);
  end;

var
  Str: String;
  i: Integer;
  Node: TTreeNode;
begin
  if (Decl.ObjectName <> '') then
  begin
    // Easy to check top level nodes
    if (ParentNode = nil) then
    begin
      if (Items.TopLvlCount > 0) and (Items.TopLvlItems[Items.TopLvlCount - 1].ImageIndex = 4) and
         (StrBefore(' ', Items.TopLvlItems[Items.TopLvlCount - 1].Text) = Decl.ObjectName) then
        ParentNode := Items.TopLvlItems[Items.TopLvlCount - 1]
      else
        ParentNode := AddType(Decl.ObjectName, ParentNode);
    end else
    begin
      if (ParentNode.Count > 0) then
      begin
        Node := ParentNode.Items[ParentNode.Count - 1];
        if (Node.ImageIndex = 4) and (StrBefore(' ', Node.Text) = Decl.ObjectName) then
          ParentNode := Node
        else
          ParentNode := AddType(Decl.ObjectName, ParentNode);
      end else
        ParentNode := AddType(Decl.ObjectName, ParentNode);
    end;
  end;

  Str := Decl.Name + Decl.ParameterText;
  if (Decl.ReturnType <> '') then
    Result := AddNode(Decl, Str + ': ' + Decl.ReturnType, 2, ParentNode)
  else
    Result := AddNode(Decl, Str, 3, ParentNode);

  if (Locals) then
  begin
    for i := 0 to Decl.Items.Count - 1 do
      if (Decl.Items[i].ClassType = TDeclVariable) then
        AddVar(TDeclVariable(Decl.Items[i]), Result)
      else
      if (Decl.Items[i].ClassType = TDeclConstant) then
        AddConst(TDeclConstant(Decl.Items[i]), Result)
      else
      if (Decl.Items[i].ClassType = TCPTypeDeclaration) then
        AddType(TCPTypeDeclaration(Decl.Items[i]), Result);

    Result.Expanded := True;
  end;

  if (ParentNode <> nil) then
    ParentNode.Expanded := True;
end;

function TNalaCodeTree.AddRecord(Decl: TCPTypeDeclaration; ParentNode: TTreeNode): TTreeNode;
var
  i: Integer;
  Node: TTreeNode;
begin
  Result := AddNode(Decl, Decl.Format, 4, ParentNode);

  with Decl.Kind.GetRecord do
    for i := 0 to FieldCount - 1 do
      begin
        Node := Self.Items.AddChild(Result, Fields[i].Name + ': ' + Fields[i].Typ);
        Node.SelectedIndex := 1;
        Node.ImageIndex := 1;
      end;

  Result.Expanded := True;
end;

function TNalaCodeTree.AddType(Decl: TCPTypeDeclaration; ParentNode: TTreeNode): TTreeNode;
begin
  if (Decl.Kind.Typ = tkRecord) then
    Result := AddRecord(Decl, ParentNode)
  else
    Result := AddNode(Decl, Decl.Format, 4, ParentNode);
end;

procedure TNalaCodeTree.LoadFromDecls(List: TDeclarationList; ParentNode: TTreeNode);

  function AddSection(Text: String): TTreeNode;
  begin
    Result := Items.AddChild(ParentNode, Text);
    Result.ImageIndex := 19;
    Result.SelectedIndex := 19;
  end;

var
  i: Integer;
  Types, Constants, Variables, Methods: TTreeNode;
begin
  Items.BeginUpdate;

  Types := AddSection('Types');
  Constants := AddSection('Constants');
  Variables := AddSection('Variables');
  Methods := AddSection('Methods');

  for i := 0 to List.Count - 1 do
    if (List[i].ClassType = TDeclMethod) and (TDeclMethod(List[i]).ObjectName <> '') then
      AddMethod(TDeclMethod(List[i]), False, Types)
    else
    if (List[i].ClassType = TDeclMethod) then
      AddMethod(TDeclMethod(List[i]), False, Methods)
    else
    if (List[i].ClassType = TCPTypeDeclaration) then
      AddType(TCPTypeDeclaration(List[i]), Types)
    else
    if (List[i].ClassType = TDeclVariable) then
      AddVar(TDeclVariable(List[i]),Variables)
    else
    if (List[i].ClassType = TDeclConstant) then
      AddConst(TDeclConstant(List[i]), Constants);

  if (Types.Count = 0) then
    Types.Delete;
  if (Constants.Count = 0) then
    Constants.Delete;
  if (Variables.Count = 0) then
    Variables.Delete;
  if (Methods.Count = 0) then
    Methods.Delete
  else
    Methods.Expanded := False;

  ParentNode.Expanded := False;

  Items.EndUpdate;
end;

procedure TNalaCodeTree.Assign(From: TNalaCodeTree);
var
  i: Int32;
begin
  BeginUpdate;
  Items.Assign(From.Items);

  for i := 0 to From.Items.Count - 1 do
  begin
    if (From.Items[i].Data = nil) then
      Continue;

    Items[i].Data := GetMem(SizeOf(TNodeData));
    Move(From.Items[i].Data^, Items[i].Data^, SizeOf(TNodeData));
  end;

  EndUpdate;
end;

function TNalaCodeTree.AddType(AText: String; ParentNode: TTreeNode): TTreeNode;
begin
  if (ParentNode = nil) then
    Result := Items.Add(nil, AText)
  else
    Result := Items.AddChild(ParentNode, AText);

  Result.ImageIndex := 4;
end;

function TNalaCodeTree.AddConst(Decl: TDeclConstant; ParentNode: TTreeNode): TTreeNode;
var
  Str: String;
begin
  Str := Decl.Name;
  if (Decl.Typ <> '') then
    Str += ': ' + Decl.Typ;
  Str += ' = ' + Decl.Def;

  Result := AddNode(Decl, Str, 0, ParentNode);
end;

constructor TNalaCodeTree.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  OnDeletion := @OnDeleteNode;
  ReadOnly := True;
  ScrollBars := ssAutoBoth;
  Font.Size := 10;
end;

end.

