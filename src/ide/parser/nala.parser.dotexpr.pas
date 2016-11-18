unit nala.Parser.DotExpr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  nala.Parser.Code, nala.Parser.Script;

type
  TDotExpression = class
  private
  type
    TItemArray = array of record Text: String; Dimensions: Int32; end;
  private
    FParser: TScriptParser;
    FExpr: String;
    FItems: TItemArray;

    procedure SetExpr(AExpression: String);

    function Eval(var Item: TDeclaration; Dimensions: Int32): Boolean;
    function Eval(var Item: TDeclaration; Text: String): Boolean; overload;
  public
    function Evalulate: TDeclaration;
    property Expr: String read FExpr write SetExpr;

    constructor Create(Parser: TScriptParser);
  end;

implementation

uses
  nala.Types, nala.Strings;

procedure TDotExpression.SetExpr(AExpression: String);
var
  Arr: TStringArray;
  i: Int32;
begin
  Arr := AExpression.LowerCase.Explode('.');
  SetLength(FItems, Length(Arr));

  for i := 0 to High(Arr) do
  begin
    FItems[i].Text := Arr[i];
    FItems[i].Dimensions := 0;

    if (FItems[i].Text.Contains('[')) then
    begin
      if (FItems[i].Text.Contains(',')) then
        FItems[i].Dimensions := Length(FItems[i].Text.PosEx(',')) + 1
      else
        FItems[i].Dimensions := Length(FItems[i].Text.PosEx('['));

      FItems[i].Text := FItems[i].Text.Before('[');
    end;
  end;
end;

function TDotExpression.Eval(var Item: TDeclaration; Dimensions: Int32): Boolean;
var
  Kind: TCPTypeKind;
  d: Int32 = 0;
begin
  if (Item.ClassType <> TCPTypeKind) then
    Exit(False);

  Kind := TCPTypeKind(Item);

  case Kind.Typ of
    tkType:
      begin
        Result := FParser.Dictionary.HasType(Item.CleanText);
        if (Result) then
          Item := FParser.Dictionary.GetType(Kind.CleanText).Decl.Kind;
      end;

    tkRecord:
      begin
        if (Kind.Owner <> nil) and (Kind.Owner.ClassType = TCPType) and (FParser.Dictionary.HasType(TCPType(Kind.Owner).Name)) then
          Item := FParser.Dictionary.GetType(TCPType(Kind.Owner).Name).Decl.Kind
        else
          Item := Kind.GetRecord;

        Result := Item <> nil;
      end;

    tkArray:
      begin
        Item := Kind.GetArray;
        Result := Item <> nil;
      end;

    else
      Result := False;
  end;

  if (Result) and (Dimensions > 0) then
  begin
    while (d < Dimensions) do
    begin
      if (Item = nil) or ((Item.ClassType <> TCPTypeKind) and (Item.ClassType <> TCPArray)) then
        Break;

      if (Item.ClassType = TCPArray) then
        with TCPArray(Item) do
        begin
          Inc(d, Dimensions);
          Item := Kind;
        end
      else
        with TCPTypeKind(Item) do
          case Typ of
            tkArray:
              begin
                Inc(d, GetArray.Dimensions);
                Item := GetArray.Kind;
              end;

            tkType:
              if (FParser.Dictionary.HasType(Item.CleanText)) then
                Item := FParser.Dictionary.GetType(Item.CleanText).Decl.Kind
              else
                Item := nil;

            else
              Break;
          end;
    end;

    Result := (d = Dimensions) and (Item <> nil);
  end;
end;

function TDotExpression.Eval(var Item: TDeclaration; Text: String): Boolean;

  function SearchFields(Rec: TCPRecord): Boolean;
  var
    i: Int32;
  begin
    Result := False;

    for i := 0 to Rec.FieldCount - 1 do
      if (LowerCase(Rec.Fields[i].Name) = Text) then
      begin
        Item := Rec.Fields[i].Kind;
        Exit(True);
      end;
  end;

var
  Name: String;
begin
  if (Item.ClassType = TCPRecord) then
    Exit(SearchFields(TCPRecord(Item)));

  if (Item.ClassType = TCPTypeKind) then
  begin
    if (TCPTypeKind(Item).Typ = tkRecord) and (SearchFields(TCPTypeKind(Item).GetRecord)) then
      Exit(True);

   if (Item.Owner <> nil) and (Item.Owner.ClassType = TCPType) then
      Name := TCPType(Item.Owner).Name
    else
      Name := Item.CleanText;

    if (FParser.Dictionary.HasType(Name)) then
    begin
      Item := FParser.Dictionary.GetType(Name).Find(Text);
      if (Item <> nil) then
        Exit(True);
    end;
  end;

  Exit(False);
end;

function TDotExpression.Evalulate: TDeclaration;
var
  Decl: TDeclaration;
  i: Int32;
begin
  if (Length(FItems) = 0) then
    raise Exception.Create('No items to evalulate');

  Decl := FParser.TypeOfName(FItems[0].Text);
  if (Decl = nil) then
    raise Exception.Create('Failed to find start declaration');

  if (not Eval(Decl, FItems[0].Dimensions)) then
    raise Exception.Create('Failed to eval start item');

  for i := 1 to High(FItems) do
  begin
    if (not Eval(Decl, FItems[i].Text)) then
      raise Exception.CreateFmt('Failed to eval-text "%s"', [FItems[i].Text]);
    if (not Eval(Decl, FItems[i].Dimensions)) then
      raise Exception.CreateFmt('Failed to eval-dimensions "%s", "%d"', [FItems[i].Text, FItems[i].Dimensions]);
  end;

  // hacks...
  // We have "record X, Y: Int32; end" checking the owner of that will give us the
  // name "TPoint" then we search for that in the type dictionary
  if (Decl.ClassType = TCPTypeKind) and (TCPTypeKind(Decl).Typ = tkType) and
     (FParser.Dictionary.HasType(TCPTypeKind(Decl).CleanText)) then
    Decl := FParser.Dictionary.GetType(TCPTypeKind(Decl).CleanText).Decl
  else
  if (Decl.ClassType = TCPTypeKind) and (Decl.Owner <> nil) and
     (Decl.Owner.ClassType = TCPType) and (FParser.Dictionary.HasType(TCPType(Decl.Owner).Name)) then
    Decl := FParser.Dictionary.GetType(TCPType(Decl.Owner).Name).Decl
  else
  if (Decl.ClassType = TCPArray) and (Decl.Owner <> nil) and (Decl.Owner.Owner.ClassType = TCPType) and
     (FParser.Dictionary.HasType(TCPType(Decl.Owner.Owner).Name)) then
    Decl := FParser.Dictionary.GetType(TCPType(Decl.Owner.Owner).Name).Decl;

  if (Decl = nil) then
    raise Exception.Create('Nil declaration');

  Result := Decl;
end;

constructor TDotExpression.Create(Parser: TScriptParser);
begin
  FParser := Parser;
  SetLength(FItems, 0);
  FExpr := '';
end;

end.

