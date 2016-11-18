unit nala.AutoComplete;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.SynCompletion, nala.Parser.Script,
  Controls, Graphics, Forms, nala.Types, ImgList, nala.CompletionItem, nala.Parser.Code;

type
  TNalaSynCompletionForm = class;

  TNalaSynCompletionFormHint = class(THintWindow)
  private
    FCompletionForm: TNalaSynCompletionForm;
    FFontHeight: Int32;
    FItem: TCompletionItem_Base;
  public
    property FontHeight: Int32 read FFontHeight;

    procedure Paint; override;
    procedure Show(Index: Int32);

    constructor Create(AOwner: TComponent); override;
  end;

  TNalaSynCompletionForm = class(TSynCompletionForm)
  private
    FHintForm: TNalaSynCompletionFormHint;
  public
    property HintForm: TNalaSynCompletionFormHint read FHintForm;

    procedure ShowItemHint(AIndex: Integer); override;
    procedure HideItemHint; override;

    constructor Create(AOwner: TComponent); override;
  end;

  TNalaAutoComplete = class(TSynCompletion)
  private
  type
    TExpression = record
      Pos: Int32;
      Text: String;
      hasDot: Boolean;
    end;
  private
    FItems: TStringList;
    FImageWidth: Int32;
    FImages: TImageList;
    FHintForm: TNalaSynCompletionFormHint;

    function GetExpr: TExpression;

    procedure addMethod(Decl: TDictionaryItem_Method);
    procedure addVariable(AName, ATyp: String; ADefault: String = '');
    procedure addVariable(AVariable: TCPVariable); overload;
    procedure addRecordFields(ARecord: TCPRecord);
    procedure addType(AType: TCPType);
    procedure addConstant(AConstant: TCPConstant);

    procedure addItems;

    function DoPaint(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
  public
    function GetCompletionFormClass: TSynBaseCompletionFormClass; override;

    procedure DoSearch(var APosition: Integer);
    procedure DoExecute(Sender: TObject);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  strutils, Types, LCLIntf, LCLType, math, nala.MainForm,
  nala.Parser.Include, nala.Environment, dateutils, nala.SynEdit
  {$IFDEF WINDOWS},
    Windows
  {$ENDIF};

procedure TNalaSynCompletionForm.ShowItemHint(AIndex: Integer);
begin
  if (AIndex = Self.Position) then
    FHintForm.Show(AIndex);
end;

procedure TNalaSynCompletionForm.HideItemHint;
begin
  FHintForm.Visible := False;
end;

constructor TNalaSynCompletionForm.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);

  FHintForm := TNalaSynCompletionFormHint.Create(Self);
  FHintForm.Color := clWhite;
  FHintForm.Visible := False;
  FHintForm.DoubleBuffered := True;

  {$IFDEF WINDOWS}
    SetClassLong(FHintForm.Handle, GCL_STYLE, GetClassLong(FHintForm.Handle, GCL_STYLE) and not CS_DROPSHADOW);
  {$ENDIF}
end;

procedure TNalaSynCompletionFormHint.Paint;
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clBlack;
  Canvas.FrameRect(ClientRect);

  FItem.Paint(2, 1, Odd(FCompletionForm.Position));
end;

procedure TNalaSynCompletionFormHint.Show(Index: Int32);
var
  P: TPoint;
  Size: TPoint;
begin
  if (Index < 0) or (Index >= FCompletionForm.ItemList.Count) then
    Exit;

  if (FCompletionForm.ItemList.Objects[Index] = nil) then
    Exit;

  P.X := FCompletionForm.Left + FCompletionForm.Width + 1;
  P.Y := Max(FCompletionForm.Top, FCompletionForm.ClientToScreen(Types.Point(0, (Index - FCompletionForm.ScrollBar.Position) * FCompletionForm.FontHeight)).Y);

  FItem := FCompletionForm.ItemList.Objects[Index] as TCompletionItem_Base;
  Size := Types.Point(FItem.Size.X + 4, FItem.Size.Y + 4);

  if (Self.Visible) then
  begin
    Self.BoundsRect := Types.Rect(P.X, P.Y, P.X + Size.X, P.Y + Size.Y);
    Self.Invalidate;
  end else
    Self.ActivateWithBounds(Types.Rect(P.X, P.Y, P.X + Size.X, P.Y + Size.Y), '');
end;

constructor TNalaSynCompletionFormHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FCompletionForm := TNalaSynCompletionForm(AOwner);
  FFontHeight := Graphics.GetFontData(Canvas.Font.Reference.Handle).Height;
end;

function TNalaAutoComplete.GetExpr: TExpression;
var
  i: Int32;
  Text: String;
  InParams: Boolean = False;
begin
  Result := Default(TExpression);
  Text := LowerCase(Copy(Editor.LineText, 1, Editor.CaretX - 1));

  for i := Length(Text) downto 1 do
    case Text[i] of
      ')':
        InParams := True;
      '(':
        if (not InParams) then
          Break
        else
          InParams := False;
      ' ':
        if (not InParams) then
         Break;
      else
        if (not InParams) then
          Result.Text := Text[i] + Result.Text;
    end;

  Result.hasDot := Pos('.', Result.Text) > 0;
  if (Result.hasDot) then
    Result.Text := Copy(Result.Text, 1, LastDelimiter('.', Result.Text));

  Result.Pos := Editor.SelStart - i - 1;
end;

{ TODO : Fix TPointArray.Combine. }

procedure TNalaAutoComplete.addItems;
var
  Parser: TScriptParser;

  procedure addDictionaryType(Name: String);
  var
    Method: TCPMethod;
  begin
    if (not Parser.Dictionary.HasType(Name)) then
      Exit;

    with Parser.Dictionary.GetType(Name) do
    begin
      for Method in Methods do
        if (FItems.IndexOf(Method.Name) = -1) and (Parser.Dictionary.HasMethod(Name + '.' + Method.Name)) then
          Self.addMethod(Parser.Dictionary.GetMethod(Name + '.' + Method.Name));

      if (Decl.Kind.Typ = tkRecord) then
      begin
        addRecordFields(Decl.Kind.GetRecord());

        if (Decl.Kind.GetRecord().Ancestor <> '') then
          addDictionaryType(Decl.Kind.GetRecord().Ancestor);
      end;
    end;
  end;

  procedure addGlobals(List: TDeclarationList);
  var
    i: Int32;
  begin
    for i := 0 to List.Count - 1 do
      if (List[i].ClassType = TCPInclude) and (TCPInclude(List[i]).Declaration <> nil) then
        AddGlobals(TCodeParser(TCPInclude(List[i]).Declaration).Items)
      else
      if (List[i].ClassType = TCPVariable) then
        AddVariable(List[i] as TCPVariable)
      else
      if (List[i].ClassType = TCPConstant) then
        addConstant(List[i] as TCPConstant)
      else
      if (List[i].ClassType = TCPType) then
        addType(List[i] as TCPType);
  end;

var
  Method: TDictionaryItem_Method;
  Decl: TDeclaration;
  Expr: TExpression;
begin
  Expr := GetExpr();
  if (Expr.Pos = 0) then
    Exit;

  Parser := TNalaSynEdit(Editor).Parse(True);

  if (Expr.hasDot) then
  try
    Decl := Parser.TypeOfExpression(Expr.Text);

    if (Decl.ClassType = TCPType) then
      addDictionaryType(TCPType(Decl).Name)
    else
    if (Decl.ClassType = TCPRecord) then
      addRecordFields(Decl as TCPRecord)
    else
      Writeln('Bad type: ', Decl.ClassName);
  except
    on e: Exception do
      Writeln('Error handling expression: ', e.Message);
  end else
  begin
    if (Parser.MethodAtCaret <> nil) then
      with Parser.MethodAtCaret do
      begin
        for Decl in Variables do
          AddVariable(Decl as TCPVariable);
        for Decl in Constants do
          addConstant(Decl as TCPConstant);
        for Decl in Types do
          addType(Decl as TCPType);

        if (ReturnType <> '') then
          addVariable('Result', ReturnType);
        if (isMethodOfObject) then
          addVariable('Self', ObjectType);
      end;

    AddGlobals(Parser.Script.Items);
    AddGlobals(NalaInclude.Items);

    for Method in Parser.Dictionary.Methods do
      if (not Method.Decl.isMethodOfObject) then
        addMethod(Method);
  end;
end;

procedure TNalaAutoComplete.addMethod(Decl: TDictionaryItem_Method);
var
  Item: TCompletionItem_Method;
begin
  Item := TCompletionItem_Method.Create(Decl, FHintForm);
  FItems.AddObject(Item.Name, Item);
end;

procedure TNalaAutoComplete.addVariable(AName, ATyp: String; ADefault: String);
var
  Item: TCompletionItem_Variable;
begin
  Item := TCompletionItem_Variable.Create(AName, ATyp, ADefault, FHintForm);
  FItems.AddObject(Item.Name, Item);
end;

procedure TNalaAutoComplete.addVariable(AVariable: TCPVariable);
var
  Item: TCompletionItem_Variable;
begin
  Item := TCompletionItem_Variable.Create(AVariable, FHintForm);
  FItems.AddObject(Item.Name, Item);
end;

procedure TNalaAutoComplete.addRecordFields(ARecord: TCPRecord);
var
  i: Int32;
begin
  for i := 0 to ARecord.FieldCount - 1 do
    addVariable(ARecord.Fields[i].Name, ARecord.Fields[i].Typ, '');
end;

procedure TNalaAutoComplete.addType(AType: TCPType);
var
  Item: TCompletionItem_Base;
begin
  case AType.Kind.Typ of
    tkRecord:
      Item := TCompletionItem_Record.Create(AType, FHintForm);
    tkEnum:
      Item := TCompletionItem_Enum.Create(AType, FHintForm);
    else
      Item := TCompletionItem_Type.Create(AType, FHintForm);
  end;

  FItems.AddObject(Item.Name, Item);
end;

procedure TNalaAutoComplete.addConstant(AConstant: TCPConstant);
var
  Item: TCompletionItem_Constant;
begin
  Item := TCompletionItem_Constant.Create(AConstant, FHintForm);
  FItems.AddObject(Item.Name, Item);
end;

function TNalaAutoComplete.DoPaint(const Key: String; Canvas: TCanvas; X, Y: Integer; Selected: Boolean; Index: Integer): Boolean;
var
  Item: TCompletionItem_Base;
begin
  Item := ItemList.Objects[Index] as TCompletionItem_Base;
  if (Item <> nil) then
  begin
    FImages.Draw(Canvas, X + 2, Y, Item.Image);

    Canvas.Font.Bold := True;
    Canvas.TextOut(X + FImageWidth, Y, Item.Name);
  end;

  Result := True;
end;

function TNalaAutoComplete.GetCompletionFormClass: TSynBaseCompletionFormClass;
begin
  Result := TNalaSynCompletionForm;
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
begin
  try
    ItemList.BeginUpdate;
    ItemList.Clear;

    FItems.BeginUpdate;
    FItems.Clear;

    addItems();

    DoSearch(Pos);
    Position := Pos;
  finally
    ItemList.EndUpdate;
    FItems.EndUpdate;
  end;
end;

constructor TNalaAutoComplete.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 225;
  ShowSizeDrag := True;
  SelectedColor := clListSelected;


  TheForm.OddBackgroundColor := clListOdd;
  TheForm.BackgroundColor := clListEven;
  TheForm.TextSelectedColor := clBlack;

  FHintForm := TNalaSynCompletionForm(TheForm).HintForm;

  OnExecute := @DoExecute;
  OnSearchPosition := @DoSearch;
  OnPaintItem := @DoPaint;

  FItems := TStringList.Create;
  FItems.Sorted := True;
  FItems.Duplicates := dupAccept;
  FItems.OwnsObjects := True;

  TStringList(ItemList).OwnsObjects := False;

  FImages := NalaForm.Images16x16;
  FImageWidth := FImages.Width + 6;
end;

destructor TNalaAutoComplete.Destroy;
begin
  FItems.Free;

  inherited Destroy;
end;

end.

