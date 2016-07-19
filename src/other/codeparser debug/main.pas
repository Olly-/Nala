unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs, typinfo,
  ComCtrls, StdCtrls, CastaliaPasLex, CastaliaSimplePasPar, CastaliaPasLexTypes,
  nala.CodeParser, nala.CodeTree;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ImageList1: TImageList;
    Memo: TMemo;
    SynEdit: TSynEdit;
    TreeView: TTreeView;
    CodeTree: TNalaCodeTree;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    Parser: TCodeParser;

    procedure WriteMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
    procedure DebugMethod(Decl: TDeclMethod; ANode: TTreeNode = nil);
    procedure DebugVariable(Decl: TDeclVariable; ANode: TTreeNode = nil);
    procedure DebugConstant(Decl: TDeclConstant; ANode: TTreeNode = nil);
    procedure DebugType(Decl: TCPTypeDeclaration; ANode: TTreeNode = nil);
    procedure Debug;
  public
    property m: TMemo read Memo;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  CodeTree.Items.Clear;
  TreeView.Items.Clear;
  Memo.Clear;

  Parser := TCodeParser.Create;
  Parser.OnMessage := @WriteMessage;
  Parser.Run(SynEdit.Lines.Text);

  Debug;

  Parser.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CodeTree := TNalaCodeTree.Create(Self);
  CodeTree.Parent := Self;
  CodeTree.AnchorToNeighbour(akLeft, 0, TreeView);
  CodeTree.AnchorParallel(akRight, 5, Self);
  CodeTree.Height := TreeView.Height;
  CodeTree.Images := ImageList1;
  CodeTree.Font.Height:= 13;
end;

procedure TForm1.WriteMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
begin
  Memo.Append('(' + IntToStr(Y + 1) + '): ' + Msg);
end;

procedure TForm1.DebugMethod(Decl: TDeclMethod; ANode: TTreeNode);

  procedure DebugParameters(Node: TTreeNode);
  var
    i: Integer;
    Str: String;
  begin
    for i := 0 to Decl.ParameterCount - 1 do
    begin
      Str := Decl.Parameter[i].Name + ': ' + Decl.Parameter[i].Typ;
      if (Decl.Parameter[i].Def <> '') then
        Str += Decl.Parameter[i].Def;
      TreeView.Items.AddChild(Node, Str);
    end;
  end;

  procedure DebugLocalMethods(Node: TTreeNode);
  var
    Decls: TDeclarationArray;
    i: Integer;
  begin
    Decls := Decl.Items.FindItemsByClass(TDeclMethod, True);
    for i := 0 to High(Decls) do
      if (Decls[i].Owner = Decl) then
        DebugMethod(TDeclMethod(Decls[i]), Node);
  end;

var
  d: TDeclarationArray;
  i: Integer;
  Node: TTreeNode;
begin
  if (ANode <> nil) then
    Node := TreeView.Items.AddChild(ANode, Decl.ClassName)
  else
    Node := TreeView.Items.Add(nil, Decl.ClassName);

  TreeView.Items.AddChild(Node, 'Name: ' + Decl.Name);
  TreeView.Items.AddChild(Node, 'Object Name: ' + Decl.ObjectName);
  TreeView.Items.AddChild(Node, 'Return Type: ' + Decl.ReturnType);
  TreeView.Items.AddChild(Node, 'Directives: ' + GetEnumName(TypeInfo(TMethodDirective), Ord(Decl.Directive)));
  TreeView.Items.AddChild(Node, 'Depth: ' + IntToStr(Decl.Depth));

  DebugParameters(TreeView.Items.AddChild(Node, 'Parameter Count: ' + IntToStr(Decl.ParameterCount)));
  DebugLocalMethods(Node);
end;

procedure TForm1.DebugVariable(Decl: TDeclVariable; ANode: TTreeNode);
var
  Node: TTreeNode;
begin
  if (ANode <> nil) then
    Node := TreeView.Items.AddChild(ANode, Decl.ClassName)
  else
    Node := TreeView.Items.Add(nil, Decl.ClassName);

  TreeView.Items.AddChild(Node, 'Name: ' + Decl.Name);
  TreeView.Items.AddChild(Node, 'Typ: ' + Decl.Typ);
  if (Decl.Def <> '') then
   TreeView.Items.AddChild(Node, 'Def: ' + Decl.Def);
end;

procedure TForm1.DebugConstant(Decl: TDeclConstant; ANode: TTreeNode);
var
  Node: TTreeNode;
begin
  if (ANode <> nil) then
    Node := TreeView.Items.AddChild(ANode, Decl.ClassName)
  else
    Node := TreeView.Items.Add(nil, Decl.ClassName);

  TreeView.Items.AddChild(Node, 'Name: ' + Decl.Name);
  if (Decl.Typ <> '') then
    TreeView.Items.AddChild(Node, 'Typ: ' + Decl.Typ);
  TreeView.Items.AddChild(Node, 'Def: ' + Decl.Def);
end;

procedure TForm1.DebugType(Decl: TCPTypeDeclaration; ANode: TTreeNode);

  procedure DebugRecord(Node: TTreeNode);
  var
    i: Integer;
    n: TTreeNode;
  begin
    with Decl.Kind.GetRecord do
    begin
      TreeView.Items.AddChild(Node, 'Ancestor: ' + Ancestor);
      n := TreeView.Items.AddChild(Node, 'Fields(' + IntToStr(FieldCount) + ')');
      for i := 0 to FieldCount - 1 do
        TreeView.Items.AddChild(n, Fields[i].Name + ': ' + Fields[i].Typ);
      n.Expanded := True;
    end;
  end;

  procedure DebugEnum(Node: TTreeNode);
  var
    i: Integer;
    Str: String;
    n: TTreeNode;
  begin
    with Decl.Kind.GetEnum do
    begin
      n := TreeView.Items.AddChild(Node, 'Enums(' + IntToStr(EnumCount) + ')');
      for i := 0 to EnumCount - 1 do
      begin
        Str := Enums[i].Name;
        if (Enums[i].Value <> '') then
          Str += ' = ' + Enums[i].Value;
        TreeView.Items.AddChild(n, Str);
      end;
      n.Expanded := True;
    end;
  end;

  procedure DebugArray(Node: TTreeNode);
  var
    n: TTreeNode;
  begin
    n := TreeView.Items.AddChild(Node, 'Array');
    with Decl.Kind.GetArray do
    begin
      TreeView.Items.AddChild(n, 'Of: ' + Typ);
      TreeView.Items.AddChild(n, 'Dims: ' + IntToStr(Dimensions));
      if (Kind <> nil) then
        TreeView.Items.AddChild(n, 'Typ: ' + GetEnumName(TypeInfo(ETypeKind), Ord(Kind.Typ)));
    end;
    n.Expanded := True;
  end;

  procedure DebugCopy(Node: TTreeNode);
  begin
    TreeView.Items.AddChild(Node, 'Type: ' + Decl.Kind.GetCopy.CopyType);
  end;

  procedure DebugAlias(Node: TTreeNode);
  begin
    TreeView.Items.AddChild(Node, 'Type: ' + Decl.Kind.GetAlias.AliasType);
  end;

  procedure DebugSet(Node: TTreeNode);
  begin
    TreeView.Items.AddChild(Node, 'Type: ' + Decl.Kind.GetSet.SetType);
  end;

var
  Node: TTreeNode;
begin
  if (ANode <> nil) then
    Node := TreeView.Items.AddChild(ANode, Decl.ClassName)
  else
    Node := TreeView.Items.Add(nil, Decl.ClassName);

  TreeView.Items.AddChild(Node, 'Name: ' + Decl.Name);
  TreeView.Items.AddChild(Node, 'Kind: ' + GetEnumName(TypeInfo(ETypeKind), Ord(Decl.Kind.Typ)));
  TreeView.Items.AddChild(Node, 'Formatted: ' + Decl.Format(True));

  case Decl.Kind.Typ of
    tkRecord: DebugRecord(Node);
    tkEnum: DebugEnum(Node);
    tkArray: DebugArray(Node);
    tkCopy: DebugCopy(Node);
    tkSet: DebugSet(Node);
    tkAlias: DebugAlias(Node);
  end;
end;

procedure TForm1.Debug;
var
  i: Integer;
begin
  for i := 0 to Parser.Items.Count - 1 do
  begin
    if (Parser.Items[i].ClassType = TCPTypeDeclaration) then
    begin
      CodeTree.AddType(TCPTypeDeclaration(Parser.Items[i]));
      DebugType(TCPTypeDeclaration(Parser.Items[i]));
    end else
    if (Parser.Items[i].ClassType = TDeclMethod) then
    begin
      CodeTree.AddMethod(TDeclMethod(Parser.Items[i]), True);
      DebugMethod(TDeclMethod(Parser.Items[i]));
    end else
    if (Parser.Items[i].ClassType = TDeclVariable) then
    begin
      CodeTree.AddVar(TDeclVariable(Parser.Items[i]));
      DebugVariable(TDeclVariable(Parser.Items[i]));
    end else
    if (Parser.Items[i].ClassType = TDeclConstant) then
    begin
      CodeTree.AddConst(TDeclConstant(Parser.Items[i]));
      DebugConstant(TDeclConstant(Parser.Items[i]));
    end;
  end;
end;

end.

