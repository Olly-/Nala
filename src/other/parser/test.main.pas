unit test.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, nala.Parser.Script{, heaptrc};

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    PageControl1: TPageControl;
    Splitter1: TSplitter;
    SynEdit: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TreeView1: TTreeView;

    procedure Button1Click(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  nala.Parser.Code, typinfo, base64;

procedure TForm1.Button1Click(Sender: TObject);

  procedure DebugDictonaries(Parser: TScriptParser);
  var
    Typ: TDictionaryItem_Type;
    Method: TDictionaryItem_Method;
    Node, ItemNode: TTreeNode;
    i: Int32;
  begin
    with TreeView1 do
    begin
      Items.Clear;

      Node := Items.Add(nil, 'Types');
      for Typ in Parser.Dictionary.Types do
      begin
        ItemNode := Items.AddChild(Node, Typ.Decl.Name);
        for i := 0 to High(Typ.Methods) do
          Items.AddChild(ItemNode, Typ.Methods[i].Name);
      end;

      Node := Items.Add(nil, 'Methods');
      for Method in Parser.Dictionary.Methods do
      begin
        ItemNode := Items.AddChild(Node, Method.Decl.Header);
        for i := 0 to High(Method.Overloads) do
          Items.AddChild(ItemNode, Method.Overloads[i].Header);
      end;
    end;
  end;

  procedure DebugMethodAtCaret(Parser: TScriptParser);
  var
    Method: TCPMethod;
    i: Int32;
  begin
    with Memo1.Lines do
    begin
      Clear;

      Method := Parser.MethodAtCaret;
      if (Method <> nil) then
      begin
        Add(Method.Header);
        Add('Static: %s', [BoolToStr(mdStatic in Method.Directives, True)]);
        Add('Overload: %s', [BoolToStr(mdOverload in Method.Directives, True)]);
        Add('Override %s', [BoolToStr(mdOverride in Method.Directives, True)]);

        for i := 0 to High(Method.Variables) do
          Add('Variable "%s" (%s)', [TCPVariable(Method.Variables[i]).Name, TCPVariable(Method.Variables[i]).Typ]);
        for i := 0 to High(Method.Constants) do
          Add('Constant "%s" (%s)', [TCPConstant(Method.Constants[i]).Name, TCPConstant(Method.Constants[i]).Default]);
        for i := 0 to High(Method.Types) do
          Add('Type "%s" (%s)', [TCPType(Method.Types[i]).Name, GetEnumName(TypeInfo(ETypeKind), Ord(TCPType(Method.Types[i]).Kind.Typ))]);
      end;
    end;
  end;

  procedure DebugItems(Parser: TCodeParser);
  var
    Indent: Int32 = 3;

    procedure Write(Item: TDeclaration);
    var
      i: Int32;
    begin
      Memo3.Lines.Add('%s %s', [StringOfChar('-', Indent), Item.ClassName]);

      if (Item.Items.Count > 0) Then
      begin
        Inc(Indent, 3);

        for i := 0 to Item.Items.Count - 1 do
          Write(Item.Items[i]);

        Dec(Indent, 3);
        Memo3.Lines.Add('%s %s', [StringOfChar('-', Indent), Item.ClassName]);
      end;
    end;

  var
    i: Int32;
  begin
    Memo3.Clear;

    for i := 0 to Parser.Items.Count - 1 do
      Write(Parser.Items[i]);
  end;

  procedure DebugTypeOf(Parser: TScriptParser);

    procedure Test(Expr: String);
    var
      Typ: TDeclaration;
    begin
      try
        Typ := Parser.TypeOfExpression(Expr);

        if (Typ.ClassType = TCPType) then
          Memo2.Lines.Add('%s : %s : %s', [Expr, Typ.ClassName, TCPType(Typ).Name, Typ.CleanText])
        else
          Memo2.Lines.Add('%s : %s : %s', [Expr, Typ.ClassName, Typ.CleanText]);
      except
        on E: Exception do
          Memo2.Lines.Add('%s : %s', [Expr, e.Message]);
      end;
    end;

  begin
    with Memo2.Lines do
    begin
      Clear;

      Test('Boxes[0].');
      Test('Points.');
      Test('LotsOfPoints[0][0].');
      Test('Rec.R.');
      Test('Bools[0].Bools[0]');
      Test('Crazyness.Arr.Ints[0]');
      Test('Self[0].');
      Test('Result[0].Str[0].');
      Test('Floats[0].');
      Test('ChatBox.FLines.');
      Test('LocalRec[0].Field.');
    end;
  end;

var
  Parser: TScriptParser;
begin
  //SetHeapTraceOutput('leaks.txt');

  Parser := TScriptParser.Create;
  Parser.addSearchPath('/home/olly/Desktop/Nala/includes/');
  Parser.Run(SynEdit.Text, -1, SynEdit.SelStart);



  DebugMethodAtCaret(Parser);
  DebugDictonaries(Parser);
  DebugItems(Parser.Script);
  DebugTypeOf(Parser);

  Parser.Free;
end;

end.

