unit nala.ScriptParser;

{$mode objfpc}{$H+}
{$inline on}
{$macro on}

interface

uses
  Classes, SysUtils, nala.CodeParser, nala.Dictionary;

type

  { TType }

  TType = class(TObject)
  private
  type
    TMethodArray = array of TDeclMethod;
  private
    FDecl: TCPTypeDeclaration;
    FMethods: TMethodArray;
  public
    property Decl: TCPTypeDeclaration read FDecl;
    property Methods: TMethodArray read FMethods write FMethods;

    procedure AddMethod(Method: TDeclMethod);

    function FindFunction(Name: String; out Found: TDeclMethod): Boolean;
    function FindField(Name: String; out Found: TCPTypeKind): Boolean;

    function Copy: TType;

    constructor Create(ADecl: TCPTypeDeclaration);
  end;

  { TScriptParser }

  TScriptParser = class(TObject)
  private
  type
    TTypeDictionary = specialize TDictionary<String, TType>;
  private
    FBaseParser: TScriptParser;
    FParser: TCodeParser;

    FLocalMethod: TDeclMethod;
    FItems: TList;
    FTypes: TTypeDictionary;

    procedure OnMethodOfObject(Decl: TDeclMethod);
    procedure OnType(Decl: TCPTypeDeclaration);
  public
    procedure Run(constref Script: String; CaretPos: Int32);


    function FindDeclaration(AName: String; var Decl: TDeclaration): Boolean;
    function FindType(AName: String; out Found: TType): Boolean;
    function DownArray(AName: String; Dimensions: Int32; out Typ: TType): Boolean;

    property LocalMethod: TDeclMethod read FLocalMethod;
    property Items: TList read FItems;

    constructor Create(BaseParser: TScriptParser = nil);
    destructor Destroy; override;
  end;

var
  NalaScript: TScriptParser;

implementation

uses
  nala.LapeCompiler;

{ TType }

procedure TType.AddMethod(Method: TDeclMethod);
var
  L: Integer;
begin
  L := Length(FMethods);
  SetLength(FMethods, L + 1);
  FMethods[L] := Method;
end;

function TType.FindFunction(Name: String; out Found: TDeclMethod): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to High(FMethods) do
  begin
    if (Name = LowerCase(FMethods[i].Name)) then
    begin
      Found := FMethods[i];
      Exit(True);
    end;
  end;
end;

function TType.FindField(Name: String; out Found: TCPTypeKind): Boolean;
var
  i: Integer;
begin
  Result := False;

  if (FDecl.Kind.Typ = tkRecord) then
    with FDecl.Kind.GetRecord do
      for i := 0 to FieldCount - 1 do
        if (Name = LowerCase(Fields[i].Name)) then
        begin
          Found := Fields[i].Kind;
          Exit(True);
        end;
end;

function TType.Copy: TType;
begin
  Result := TType.Create(FDecl);
  Result.Methods := System.Copy(Self.Methods);
end;

constructor TType.Create(ADecl: TCPTypeDeclaration);
begin
  inherited Create;

  FDecl := ADecl;
  SetLength(FMethods, 0);
end;

{ TScriptParser }

procedure TScriptParser.OnType(Decl: TCPTypeDeclaration);
var
  Typ: TType;
begin
  if (not FTypes.Get(LowerCase(Decl.Name), Typ)) then
    FTypes.AddFast(LowerCase(Decl.Name), TType.Create(Decl));
end;

procedure TScriptParser.OnMethodOfObject(Decl: TDeclMethod);
var
  Typ: TType;
begin
  if (FTypes.Get(LowerCase(Decl.ObjectName), Typ)) then
    Typ.AddMethod(Decl)
  else
    Writeln('CP: Failed to find type declaration for: ', Decl.ObjectName);
end;

procedure TScriptParser.Run(constref Script: String; CaretPos: Int32);
begin
  FItems.Clear;

  if (FParser.Run(Script, -1, CaretPos)) then
  begin
    FItems.AddList(FParser.Items.List);
    if (FBaseParser <> nil) then
      FItems.AddList(FBaseParser.FParser.Items.List);

    if (FParser.LocalMethod <> nil) then
      FLocalMethod := FParser.LocalMethod;
  end;
end;

function TScriptParser.FindDeclaration(AName: String; var Decl: TDeclaration): Boolean;

  function Find(AName: String; AItems: TDeclarationArray; var Decl: TDeclaration): Boolean;
  var
    i, j: Integer;
  begin
    Result := False;

    for i := 0 to High(AItems) do
    begin
      Decl := AItems[i];

      if (Decl.ClassType = TCPTypeDeclaration) and (AName = LowerCase(TCPTypeDeclaration(Decl).Name)) then
        Exit(True)
      else
      if (Decl.ClassType = TDeclMethod) and (AName = LowerCase(TDeclMethod(Decl).Name)) then
        Exit(True)
      else
      if (Decl.ClassType = TDeclVariable) and (AName = LowerCase(TDeclVariable(Decl).Name)) then
        Exit(True)
      else
      if (Decl.ClassType = TDeclConstant) and (AName = LowerCase(TDeclConstant(Decl).Name)) then
        Exit(True);
    end;
  end;

var
  i: Integer;
  Typ: TType;
  Str: String;
begin
  Result := False;

  AName := Lowercase(AName);
  if (AName = '') then
    Exit;

  // Locals
  if (FParser.LocalMethod <> nil) then
  begin
    if (AName = 'self') then
    begin
      Result := FindType(LowerCase(FParser.LocalMethod.ObjectName), Typ);
      if (Result) then
        Decl := Typ.Decl;

      Exit;
    end else
    if (AName = 'result') then
    begin
      Result := FindType(LowerCase(FParser.LocalMethod.ReturnType), Typ);
      if (Result) then
        Decl := Typ.Decl;

      Exit;
    end;

    if (Find(AName, FParser.LocalMethod.GetLocal(mlVariables), Decl)) or
       (Find(AName, FParser.LocalMethod.GetLocal(mlConstants), Decl)) or
       (Find(AName, FParser.LocalMethod.GetLocal(mlTypes), Decl)) then
      Exit(True);
  end;

  // Search everything
  for i := 0 to FItems.Count - 1 do
  begin
    Decl := TDeclaration(FItems[i]);

    if (Decl.ClassType = TCPTypeDeclaration) and (AName = LowerCase(TCPTypeDeclaration(Decl).Name)) then
      Exit(True)
    else
    if (Decl.ClassType = TDeclMethod) and (AName = LowerCase(TDeclMethod(Decl).Name)) then
      Exit(True)
    else
    if (Decl.ClassType = TDeclVariable) and (AName = LowerCase(TDeclVariable(Decl).Name)) then
      Exit(True)
    else
    if (Decl.ClassType = TDeclConstant) and (AName = LowerCase(TDeclConstant(Decl).Name)) then
      Exit(True);
  end;
end;

function TScriptParser.FindType(AName: String; out Found: TType): Boolean;
begin
  Found := nil;
  Result := FTypes.Get(AName, Found);
end;

// DownArray('T2DPointArray', 1) >> TPointArray
// DownArray('T2DPointArray', 2) >> TPoint
function TScriptParser.DownArray(AName: String; Dimensions: Int32; out
  Typ: TType): Boolean;
var
  Arr: TCPArray;
  i: Integer;
begin
  i := 0;

  while (FindType(LowerCase(AName), Typ)) and (Typ.Decl.Kind.Typ = tkArray) and (i < Dimensions) do
  begin
    Arr := Typ.Decl.Kind.GetArray;
    AName := Arr.Typ;
    Inc(i, Arr.Dimensions);
  end;

  Result := (i = Dimensions);
end;

constructor TScriptParser.Create(BaseParser: TScriptParser);
var
  i, j: Integer;
begin
  inherited Create;

  FBaseParser := BaseParser;
  FItems := TList.Create;
  FLocalMethod := nil;

  FParser := TCodeParser.Create;
  FParser.OnMethodOfObject := @OnMethodOfObject;
  FParser.OnType := @OnType;

  if (FBaseParser <> nil) then
  begin
    FTypes := FBaseParser.FTypes.Copy;
    // Deep copy
    for i := 0 to High(FTypes.Items) do
      for j := 0 to High(FTypes.Items[i]) do
        FTypes.Items[i][j].Val := FTypes.Items[i][j].Val.Copy;
  end else
    FTypes := TTypeDictionary.Create(@HashStr);
end;

destructor TScriptParser.Destroy;
var
  i, j: Integer;
begin
  for i := 0 to High(FTypes.Items) do
    for j := 0 to High(FTypes.Items[i]) do
      FTypes.Items[i][j].Val.Free;

  FTypes.Free;
  FParser.Free;
  FItems.Free;

  inherited Destroy;
end;

var
  Compiler: TLPCompiler;
  Typ: TType;

initialization
  Compiler := TLPCompiler.Create('', True);
  Compiler.Import();

  NalaScript := TScriptParser.Create;
  NalaScript.Run(Compiler.Dump.All, -1);

  Compiler.Free;

finalization;
  NalaScript.Free;

end.

