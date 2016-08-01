unit nala.AutoComplete;

{$mode objfpc}{$H+}
{$Inline on}

interface

uses
  Classes, SysUtils, nala.SynCompletion, nala.CodeParser, Graphics, nala.ScriptParser;

type

  { TTypeTree }

  TTypeTree = class(TObject)
  private
  type
    TTreeItem = record
      Text: String;
      Dimensions: UInt32;
      Typ: TType;
      Rec: TCPRecord;
    end;

    TTree = array of TTreeItem;
  private
    FTree: TTree;
    FParser: TScriptParser;

    function ParseStartItem: Boolean;
    function ParseItem(LastItem: TTreeItem; var Item: TTreeItem): Boolean;
  public
    function Parse(out ADecl: TType; out ARecord: TCPRecord): Boolean;

    constructor Create(ATree: String; const AParser: TScriptParser);
  end;

  { TItemData }

  TItemData = class(TObject)
  public
    FPaintString: String;
    FColumnString: String;

    constructor Create(Column, Delimiter, Name, Typ: String);
    constructor Create(Decl: TDeclMethod);
  end;

  { TNalaAutoComplete }

  TNalaAutoComplete = class(TSynCompletion)
  private
    FItems: TStringList;
    FColumnWidth: Integer;

    procedure GetExpression(out Expression: String; out StartPos: UInt32);
    procedure FillItems();

    procedure AddVariable(Decl: TDeclVariable);
    procedure AddMethod(Decl: TDeclMethod);
    procedure AddConstant(Decl: TDeclConstant);
    procedure AddType(Decl: TCPTypeDeclaration);
    procedure AddType(AType: TType); overload;
    procedure AddFields(ARecord: TCPRecord);
    procedure AddParams(Decl: TDeclMethod);
    procedure AddSelf(Typ: String);
    procedure AddResult(Typ: String);

    function DoPaint(const AKey: String; ACanvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
    function DoMeasure(const AKey: String; ACanvas: TCanvas; Selected: Boolean; Index: Integer): Classes.TPoint;
  public
    procedure DoSearch(var APosition: Integer);
    procedure DoExecute(Sender: TObject);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  strutils, nala.Helpers, Forms, nala.Strings, nala.Types;

{ TTypeTree }

function TTypeTree.ParseStartItem: Boolean;
var
  Decl: TDeclaration;
  Kind: TCPTypeKind;
begin
  Result := False;
  Kind := nil;
  if (Length(FTree) = 0) then
    Exit;

  if (FParser.FindDeclaration(FTree[0].Text, Decl)) then
  begin
    // Point(x, y).
    if (Decl.ClassType = TDeclMethod) then
    begin
      if (TDeclMethod(Decl).ReturnType = '') then
        Exit;

      Kind := TDeclMethod(Decl).ReturnKind;
    end else
    // var x: TPoint
    if (Decl.ClassType = TDeclVariable) then
    begin
      if (TDeclVariable(Decl).Typ = '') then
        Exit;

      Kind := TDeclVariable(Decl).Kind;
    end else
    // const x: TPoint
    if (Decl.ClassType = TDeclConstant) then
    begin
      if (TDeclConstant(Decl).Typ = '') then
        Exit;

      Kind := TDeclConstant(Decl).Kind;
    end else
    // TPoint.
    if (Decl.ClassType = TCPTypeDeclaration) then
    begin
      if (FParser.FindType(LowerCase(TCPTypeDeclaration(Decl).Name), FTree[0].Typ)) then
        Exit(True);
    end;

    if (Kind = nil) then
    begin
      Writeln('AutoComplete: Start kind = nil');
      Exit;
    end;

    case Kind.Typ of
      tkRecord:
        begin
          FTree[0].Rec := Kind.GetRecord;
          Result := FTree[0].Rec <> nil;
        end;

      tkArray:
        begin
          Result := (Kind.GetArray.Dimensions = FTree[0].Dimensions) and
                    (FParser.FindType(LowerCase(Kind.GetArray.Typ), FTree[0].Typ));
        end;

      tkType:
        begin
          if (FTree[0].Dimensions > 0) then
            Result := FParser.DownArray(LowerCase(Kind.CleanText), FTree[0].Dimensions, FTree[0].Typ)
          else
            Result := FParser.FindType(LowerCase(Kind.CleanText), FTree[0].Typ);
        end;

      else
        Writeln('AutoComplete: Bad kind: ', Kind.Typ);
    end;
  end else
    Writeln('AutoComplete: Start declaration not found');
end;

function TTypeTree.ParseItem(LastItem: TTreeItem; var Item: TTreeItem): Boolean;
var
  Kind: TCPTypeKind;
  Method: TDeclMethod;
  i: Integer;
begin
  Result := False;
  Kind := nil;

  if (LastItem.Typ <> nil) then
  begin
    if (LastItem.Typ.FindFunction(Item.Text, Method)) then
    begin
      if (Method.ReturnType = '') then
        Exit;

      Kind := Method.ReturnKind;
    end else
    if (LastItem.Typ.FindField(Item.Text, Kind)) then
    begin
      if (Kind.CleanText = '') then
        Exit;
    end;
  end else
  if (LastItem.Rec <> nil) then
  begin
    if (LastItem.Rec.FieldCount = 0) then
      Exit;

    for i := 0 to LastItem.Rec.FieldCount - 1 do
      if (LastItem.Rec.Fields[i].Name = Item.Text) then
      begin
        Kind := LastItem.Rec.Fields[i].Kind;
        Break;
      end;
  end;

  if (Kind = nil) then
  begin
    Writeln('AutoComplete: Kind = nil');
    Exit;
  end;

  case Kind.Typ of
    tkRecord:
      begin
        Item.Rec := Kind.GetRecord;
        Result := Item.Rec <> nil;
      end;

    tkArray:
      begin
        Result := (Kind.GetArray.Dimensions = Item.Dimensions) and
                  (FParser.FindType(LowerCase(Kind.GetArray.Typ), Item.Typ));
      end;

    tkType:
      begin
        if (Item.Dimensions > 0) then
          Result := FParser.DownArray(LowerCase(Kind.CleanText), Item.Dimensions, Item.Typ)
        else
          Result := FParser.FindType(LowerCase(Kind.CleanText), Item.Typ);
      end;

    else
      Writeln('AutoComplete: Bad kind: ', Kind.Typ);
  end;
end;

function TTypeTree.Parse(out ADecl: TType; out ARecord: TCPRecord): Boolean;
var
  i: Integer;
begin
  if (not Self.ParseStartItem()) then
    Exit(False);

  for i := 1 to High(FTree) do
    if (not Self.ParseItem(FTree[i - 1], FTree[i])) then
    begin
      Writeln('AutoComplete: Didn''t find ', FTree[i].Text);
      Exit(False);
    end;

  ADecl := FTree[High(FTree)].Typ;
  ARecord := FTree[High(FTree)].Rec;

  Result := True;
end;

constructor TTypeTree.Create(ATree: String; const AParser: TScriptParser);
var
  Arr: TStringArray;
  i: Integer;
begin
  inherited Create;

  FParser := AParser;

  Arr := LowerCase(ATree).Explode('.');  // TODO: only handles [0][0] (not [0, 0]);
  SetLength(FTree, Length(Arr));

  for i := 0 to High(FTree) do
  begin
    FTree[i] := Default(TTreeItem);
    FTree[i].Dimensions := Arr[i].Count('[');
    if (FTree[i].Dimensions > 0) then
      FTree[i].Text := Arr[i].Before('[')
    else
      FTree[i].Text := Arr[i];
  end;
end;

{ TItemData }

constructor TItemData.Create(Decl: TDeclMethod);
begin
  FPaintString := '{B+}' + Decl.Name + '{B-}' + '{I+}' + Decl.ParameterText + '{I-}';
  if (Decl.ReturnType = '') then
    FColumnString := '{C=clNavy}procedure{C=0}'
  else
  begin
    FColumnString := '{C=clNavy}function{C=0}';
    FPaintString += ': ' + '{C=clBlue}' + Decl.ReturnType + '{C=0}';
  end;

  if (Decl.Directive = mdOverload) then
    FPaintString += '; ' + '{C=clMaroon}overload{C=0}';
end;

constructor TItemData.Create(Column, Delimiter, Name, Typ: String);
begin
  FPaintString := '{B+}' + Name + '{B-}' + Delimiter + '{C=clBlue}' + Typ + '{C=0}';
  FColumnString := '{C=clNavy}' + Column + '{C=0}';
end;

{ TNalaAutoComplete }

procedure TNalaAutoComplete.GetExpression(out Expression: String; out StartPos: UInt32);
var
  i: Int32;
  Inside: Boolean;
  Text: String;
begin
  Expression := '';
  StartPos := 0;

  Text := LowerCase(Copy(Editor.LineText, 1, Editor.CaretX - 1));
  Inside := False;

  for i := Length(Text) downto 1 do
    case Text[i] of
      ')': Inside := True;
      '(': if (not Inside) then Break else Inside := False;
      ' ': if (not Inside) then Break;
      else
        if (not Inside) then
          Expression := Text[i] + Expression;
    end;

  StartPos := Editor.SelStart - i - 1;
  if (LastDelimiter('.', Expression) < Length(Expression)) then
    Expression := Copy(Expression, 1, LastDelimiter('.', Expression));
end;

procedure TNalaAutoComplete.FillItems;
var
  Parser: TScriptParser;
  i: Integer;
  Items: TList;
  Decl: TDeclaration;
  Typ: TType;
  Rec: TCPRecord;
  Tree: TTypeTree;
  Decls: TDeclarationArray;
  Expression: String;
  ExpressionPos: UInt32;
begin
  GetExpression(Expression, ExpressionPos);

  Parser := TScriptParser.Create(NalaScript);
  Parser.Run(Editor.Lines.Text, Editor.SelStart, ExpressionPos);

  Writeln('AutoComplete: Expression = ', Expression);
  Writeln('AutoComplete: Current String = ', CurrentString);

  if (Pos('.', Expression) > 0) then
  begin
    Tree := TTypeTree.Create(Expression, Parser);

    try
      if (Tree.Parse(Typ, Rec)) then
      begin
        if (Typ <> nil) then
          AddType(Typ);
        if (Rec <> nil) then
          AddFields(Rec);
      end;
    finally
      Tree.Free;
    end;
  end else
  begin
    Items := Parser.Items;

    // Locals
    if (Parser.LocalMethod <> nil) then
    begin
      if (Parser.LocalMethod.ObjectName <> '') then
      begin
        AddSelf(Parser.LocalMethod.ObjectName);

        if (Parser.FindType(Lowercase(Parser.LocalMethod.ObjectName), Typ)) then
          AddType(Typ);
      end;

      if (Parser.LocalMethod.ReturnType <> '') then
        AddResult(Parser.LocalMethod.ReturnType);

      AddParams(Parser.LocalMethod);

      Decls := Parser.LocalMethod.GetLocal(mlVariables);
      for i := 0 to High(Decls) do
        AddVariable(TDeclVariable(Decls[i]));

      Decls := Parser.LocalMethod.GetLocal(mlConstants);
      for i := 0 to High(Decls) do
        AddConstant(TDeclConstant(Decls[i]));

      Decls := Parser.LocalMethod.GetLocal(mlTypes);
      for i := 0 to High(Decls) do
        AddType(TCPTypeDeclaration(Decls[i]));
    end;

    // Globals
    for i := 0 to Items.Count - 1 do
    begin
      Decl := TDeclaration(Items[i]);

      if (Decl.ClassType = TDeclVariable) then
        AddVariable(TDeclVariable(Decl))
      else
      if (Decl.ClassType = TDeclMethod) and (TDeclMethod(Decl).ObjectName = '') then
        AddMethod(TDeclMethod(Decl))
      else
      if (Decl.ClassType = TDeclConstant) then
        AddConstant(TDeclConstant(Decl))
      else
      if (Decl.ClassType = TCPTypeDeclaration) then
        AddType(TCPTypeDeclaration(Decl))
    end;
  end;

  Parser.Free;
end;

procedure TNalaAutoComplete.AddVariable(Decl: TDeclVariable);
begin
  FItems.AddObject(Decl.Name, TItemData.Create('variable', ': ', Decl.Name, Decl.Typ));
end;

procedure TNalaAutoComplete.AddMethod(Decl: TDeclMethod);
begin
  FItems.AddObject(Decl.Name, TItemData.Create(Decl));
end;

procedure TNalaAutoComplete.AddConstant(Decl: TDeclConstant);
begin
  FItems.AddObject(Decl.Name, TItemData.Create('constant', ' = ', Decl.Name, Decl.Typ));
end;

procedure TNalaAutoComplete.AddType(Decl: TCPTypeDeclaration);
var
  i: Integer;
begin
  FItems.AddObject(Decl.Name, TItemData.Create('type', ' = ', Decl.Name, Decl.Format(False)));

  // Add each enum
  if (Decl.Kind.Typ = tkEnum) then
    with Decl.Kind.GetEnum do
      for i := 0 to EnumCount - 1 do
        FItems.AddObject(Enums[i].Name, TItemData.Create('enum', '', Enums[i].Name, ''));
end;

procedure TNalaAutoComplete.AddType(AType: TType);
var
  i: Integer;
begin
  for i := 0 to High(AType.Methods) do
    AddMethod(AType.Methods[i]);
  if (AType.Decl.Kind.Typ = tkRecord) then
    AddFields(AType.Decl.Kind.GetRecord);
end;

procedure TNalaAutoComplete.AddFields(ARecord: TCPRecord);
var
  i: Integer;
begin
  for i := 0 to ARecord.FieldCount - 1 do
    FItems.AddObject(ARecord.Fields[i].Name, TItemData.Create('field', ': ', ARecord.Fields[i].Name, ARecord.Fields[i].Typ));
end;

procedure TNalaAutoComplete.AddParams(Decl: TDeclMethod);
var
  i: Integer;
begin
  for i := 0 to Decl.ParameterCount - 1 do
    FItems.AddObject(Decl.Parameter[i].Name, TItemData.Create('param', ': ', Decl.Parameter[i].Name, Decl.Parameter[i].Typ));
end;

procedure TNalaAutoComplete.AddSelf(Typ: String);
begin
  FItems.AddObject('Self', TItemData.Create('var', ': ', 'Self', Typ));
end;

procedure TNalaAutoComplete.AddResult(Typ: String);
begin
  FItems.AddObject('Result', TItemData.Create('var', ': ', 'Result', Typ));
end;

function TNalaAutoComplete.DoPaint(const AKey: String; ACanvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
var
  Data: TItemData;
begin
  Result := True;

  Data := TItemData(ItemList.Objects[Index]);

  ACanvas.PrettyTextOut(X, Y, Data.FColumnString);
  ACanvas.PrettyTextOut(X + FColumnWidth, Y, Data.FPaintString);
end;

function TNalaAutoComplete.DoMeasure(const AKey: String; ACanvas: TCanvas; Selected: Boolean; Index: Integer): Classes.TPoint;
begin
  Result.Y := FontHeight;
  Result.X := FColumnWidth + ACanvas.PrettyTextExtent(TItemData(ItemList.Objects[Index]).FPaintString).cx;
end;

procedure TNalaAutoComplete.DoSearch(var APosition: Integer);
var
  i: Integer;
  Curr: String;
  CurrLen: Integer;
begin
  ItemList.BeginUpdate;
  ItemList.Clear;

  if (CurrentString <> '') then
  begin
    Curr := Lowercase(CurrentString);
    CurrLen := Length(CurrentString);

    for i := 0 to FItems.Count - 1 do
      if (LeftStr(Lowercase(FItems[i]), CurrLen) = Curr) then
        ItemList.AddObject(FItems[i], FItems.Objects[i]);
  end else
    ItemList.AddStrings(FItems);

  ItemList.EndUpdate;

  if (ItemList.Count = 0) then
    APosition := -1
  else
    APosition := 0;
end;

procedure TNalaAutoComplete.DoExecute(Sender: TObject);
var
  Pos: Integer = 0;
  t: UInt64;
begin
  t := GetTickCount64();
  try
    ItemList.BeginUpdate;
    ItemList.Clear;

    FItems.BeginUpdate;
    FItems.Clear;

    FillItems();

    DoSearch(Pos);
    Position := Pos;
  finally
    ItemList.EndUpdate;
    FItems.EndUpdate;
  end;

  Writeln('TNalaAutoComplete.DoExecute: ', GetTickCount64() - t, 'ms');
end;

constructor TNalaAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 400;
  ShowSizeDrag := True;

  OnExecute := @DoExecute;
  OnSearchPosition := @DoSearch;
  OnPaintItem := @DoPaint;
  OnMeasureItem := @DoMeasure;

  FItems := TStringList.Create;
  FItems.Sorted := True;
  FItems.Duplicates := dupAccept;
  FItems.OwnsObjects := True;

  TStringList(ItemList).OwnsObjects := False;

  FColumnWidth := TheForm.Canvas.TextWidth('procedure') + 6;
end;

destructor TNalaAutoComplete.Destroy;
begin
  FItems.Free;

  inherited Destroy;
end;

end.

