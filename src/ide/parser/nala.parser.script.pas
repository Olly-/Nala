unit nala.Parser.Script;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.Parser.Code, nala.Dictionary, CastaliaPasLex, nala.Parser.Include;

type
  TDictionaryItem_Method = class
  public
    Decl: TCPMethod;
    Overloads: TCPMethodArray;

    procedure addOverload(AMethod: TCPMethod);
    constructor Create(ADecl: TDeclaration);
  end;
  TDictionaryItem_MethodArray = array of TDictionaryItem_Method;

  TDictionaryItem_Type = class
  public
    Decl: TCPType;
    Methods: TCPMethodArray;

    function Find(Name: String): TCPTypeKind;
    procedure addMethod(AMethod: TCPMethod);
    constructor Create(ADecl: TDeclaration);
  end;
  TDictionaryItem_TypeArray = array of TDictionaryItem_Type;

  TItemDictionary = class
  private
  type
    TMethodDict = specialize TDictionary<String, TDictionaryItem_Method>;
    TTypeDict = specialize TDictionary<String, TDictionaryItem_Type>;
  private
    FMethodDict: TMethodDict;
    FTypeDict: TTypeDict;
  public
    function HasType(Name: String): Boolean;
    function HasMethod(Name: String): Boolean;

    function GetType(Name: String): TDictionaryItem_Type;
    function GetMethod(Name: String): TDictionaryItem_Method;

    function AddType(Decl: TDeclaration): Boolean;
    procedure AddMethod(Decl: TDeclaration);

    procedure SetSize(Size: Int32);
    procedure Clear;

    function Types: TDictionaryItem_TypeArray;
    function Methods: TDictionaryItem_MethodArray;

    constructor Create;
    destructor Destroy; override;
  end;

  TScriptParser = class
  private
    FParser: TCodeParser;
    FDictionary: TItemDictionary;
    FIncludes: TIncludeList;
    FSearchPaths: TStringList;

    procedure FillDictionary(Parser: TCodeParser);

    function GetMethodAtCaret: TCPMethod;
    procedure OnInclude(Sender: TObject; Declaration: TCPInclude);
  public
    procedure addSearchPath(Path: String);
    procedure deleteSearchPath(Path: String);

    procedure Run(constref Script: String; MaxPos: Int32 = -1; CaretPos: Int32 = -1);

    function TypeOfName(Name: String): TCPTypeKind;
    function TypeOfExpression(Expr: String): TDeclaration;

    property MethodAtCaret: TCPMethod read GetMethodAtCaret;
    property Includes: TIncludeList read FIncludes;
    property Dictionary: TItemDictionary read FDictionary;
    property Script: TCodeParser read FParser;

    constructor Create;
    destructor Destroy; override;
  end;


var
  NalaInclude: TCodeParser;

implementation

uses
  LazFileUtils, dateutils,
  nala.LapeCompiler, nala.Parser.DotExpr;

function TItemDictionary.HasType(Name: String): Boolean;
begin
  Result := FTypeDict.Contains(LowerCase(Name));
end;

function TItemDictionary.HasMethod(Name: String): Boolean;
begin
  Result := FMethodDict.Contains(LowerCase(Name));
end;

function TItemDictionary.GetType(Name: String): TDictionaryItem_Type;
begin
  Result := TDictionaryItem_Type(FTypeDict.GetFast(LowerCase(Name)));
end;

function TItemDictionary.GetMethod(Name: String): TDictionaryItem_Method;
begin
  Result := TDictionaryItem_Method(FMethodDict.GetFast(LowerCase(Name)));
end;

function TItemDictionary.AddType(Decl: TDeclaration): Boolean;
var
  Name: String;
begin
  Name := LowerCase(TCPType(Decl).Name);
  Result := (not FTypeDict.Contains(Name));
  if (Result) then
    FTypeDict.AddFast(Name, TDictionaryItem_Type.Create(Decl));
end;

procedure TItemDictionary.AddMethod(Decl: TDeclaration);
var
  Name: String;
begin
  if (TCPMethod(Decl).isMethodOfObject) then
    Name := LowerCase(TCPMethod(Decl).ObjectType + '.' + TCPMethod(Decl).Name)
  else
    Name := LowerCase(TCPMethod(Decl).Name);

  if (not FMethodDict.Contains(Name)) then
    FMethodDict.AddFast(Name, TDictionaryItem_Method.Create(Decl));
end;

procedure TItemDictionary.SetSize(Size: Int32);
begin
  FTypeDict.SetSize(Size);
  FMethodDict.SetSize(Size);
end;

procedure TItemDictionary.Clear;
begin
  FTypeDict.Clear;
  FMethodDict.Clear;
end;

function TItemDictionary.Types: TDictionaryItem_TypeArray;
begin
  Result := FTypeDict.GetValues;
end;

function TItemDictionary.Methods: TDictionaryItem_MethodArray;
begin
  Result := FMethodDict.GetValues;
end;

constructor TItemDictionary.Create;
begin
  FMethodDict := TMethodDict.Create(@HashStr);
  FMethodDict.FreeObjects := True;

  FTypeDict := TTypeDict.Create(@HashStr);
  FTypeDict.FreeObjects := True;
end;

destructor TItemDictionary.Destroy;
begin
  FMethodDict.Free;
  FTypeDict.Free;
end;

procedure TDictionaryItem_Method.addOverload(AMethod: TCPMethod);
begin
  SetLength(Overloads, Length(Overloads) + 1);
  Overloads[High(Overloads)] := AMethod;
end;

constructor TDictionaryItem_Method.Create(ADecl: TDeclaration);
begin
  Decl := TCPMethod(ADecl);
  SetLength(Overloads, 0);
end;

function TDictionaryItem_Type.Find(Name: String): TCPTypeKind;
var
  i: Int32;
begin
  Result := nil;

  for i := 0 to High(Methods) do
    if (Methods[i].ReturnType <> '') and (LowerCase(Methods[i].Name) = Name) then
      Exit(Methods[i].ReturnKind);

  if (Decl.Kind.Typ = tkRecord) then
    with Decl.Kind.GetRecord do
      for i := 0 to FieldCount - 1 do
        if (LowerCase(Fields[i].Name) = Name) then
          Exit(Fields[i].Kind);
end;

procedure TDictionaryItem_Type.addMethod(AMethod: TCPMethod);
begin
  SetLength(Methods, Length(Methods) + 1);
  Methods[High(Methods)] := AMethod;
end;

constructor TDictionaryItem_Type.Create(ADecl: TDeclaration);
begin
  Decl := TCPType(ADecl);
  SetLength(Methods, 0);
end;

procedure TScriptParser.OnInclude(Sender: TObject; Declaration: TCPInclude);
var
  Path, Dir: String;
  i: Int32;
  Incl: TInclude;
begin
  for i := 0 to FSearchPaths.Count - 1 do
  begin
    Path := SetDirSeparators(FSearchPaths[i] + Declaration.FilePath);

    if (FileExists(Path)) then
    begin
      if (FIncludes.Include[Path] = nil) then
      begin
        Dir := IncludeTrailingPathDelimiter(ExtractFileDir(Path));

        addSearchPath(Dir);

        Incl := IncludeBuffer.GetInclude(Path, FParser);
        Declaration.Declaration := Incl;
        FIncludes.Add(Incl);

        deleteSearchPath(Dir);
      end else
      Declaration.Declaration := FIncludes.Include[Path];

      Exit;
    end;
  end;

  //FParser.Error('Couldn''t find include "' + Declaration.FilePath  + '"');
end;

procedure TScriptParser.FillDictionary(Parser: TCodeParser);
var
  i: Int32;
  Name: String;
  Method: TCPMethod;
  Item: TDeclaration;
begin
  if (Parser = nil) then
    Exit;

  for i := 0 to Parser.Items.Count - 1 do
  begin
    Item := Parser.Items[i];

    if (Item.ClassType = TCPInclude) then
    begin
      if (TCPInclude(Item).Declaration <> nil) then
        FillDictionary(TInclude(TCPInclude(Item).Declaration))
    end else
    if (Item.ClassType = TCPType) then
    begin
      if (not FDictionary.AddType(Item)) then
        {Parser.Error(Format('Duplicate type declaration "%s"', [TCPType(Item).Name]))};
    end else
    if (Item.ClassType = TCPMethod) then
    begin
      Method := TCPMethod(Item);

      if (Method.isMethodOfObject) then
      begin
        Name := Method.ObjectType + '.' + Method.Name;

        // Add method to type declaration
        if (FDictionary.HasType(Method.ObjectType)) then
          FDictionary.GetType(Method.ObjectType).addMethod(Method)
        else
          {Parser.Error(Format('No type declaration for "%s"', [Method.ObjectType]))};
      end else
        Name := Method.Name;

      // Add method to method dict (for easy overload access)
      if (FDictionary.HasMethod(Name)) then
        FDictionary.GetMethod(Name).addOverload(Method)
      else
        FDictionary.AddMethod(Item);
    end;
  end;
end;

function TScriptParser.GetMethodAtCaret: TCPMethod;
begin
  Result := FParser.MethodAtCaret;
end;

procedure TScriptParser.Run(constref Script: String; MaxPos: Int32; CaretPos: Int32);
var
  d: TDeclaration;
begin
  FIncludes.Clear;
  FDictionary.Clear;
  FDictionary.SetSize(4096);

  FParser.Run(Script, MaxPos, CaretPos);

  FillDictionary(NalaInclude);
  FillDictionary(FParser);

  if (FParser.MethodAtCaret <> nil) then
    for d in FParser.MethodAtCaret.Types do
      FDictionary.AddType(d);
end;

procedure TScriptParser.addSearchPath(Path: String);
begin
  FSearchPaths.Add(Path);
end;

procedure TScriptParser.deleteSearchPath(Path: String);
var
  Index: Int32;
begin
  Index := FSearchPaths.IndexOf(Path);
  if (Index > -1) then
    FSearchPaths.Delete(Index);
end;

function TScriptParser.TypeOfExpression(Expr: String): TDeclaration;
var
  DotExpression: TDotExpression;
begin
  try
    DotExpression := TDotExpression.Create(Self);
    DotExpression.Expr := Expr;

    Result := DotExpression.Evalulate;
  finally
    DotExpression.Free;
  end;
end;

type
  TDeclarationSearcher = class
  private
    FScriptParser: TScriptParser;

    function MatchingDeclaration(Decl: TDeclaration; var Name: String): Boolean; inline;
  public
    function Find(Name: String; List: TDeclarationList): TDeclaration;
    function Find(Name: String; constref Arr: TDeclarationArray): TDeclaration;
    function Find(Name: String; Parser: TCodeParser): TDeclaration;
    function Find(Name: String; Method: TCPMethod): TDeclaration;

    constructor Create(ScriptParser: TScriptParser);
  end;

function TDeclarationSearcher.MatchingDeclaration(Decl: TDeclaration; var Name: String): Boolean;
begin
  if (Decl.ClassType = TCPVariable) then
    if (LowerCase(TCPVariable(Decl).Name) = Name) then
      Exit(True)
  else
  if (Decl.ClassType = TCPConstant) then
    if (LowerCase(TCPConstant(Decl).Name) = Name) then
      Exit(True)
  else
  if (Decl.ClassType = TCPType) then
    if (LowerCase(TCPType(Decl).Name) = Name) then
      Exit(True)
  else
  if (Decl.ClassType = TCPMethod) then
    if (LowerCase(TCPMethod(Decl).Name) = Name) and (TCPMethod(Decl).ReturnType <> '') then
      Exit(True);

  Exit(False);
end;

function TDeclarationSearcher.Find(Name: String; List: TDeclarationList): TDeclaration;
var
  i: Int32;
begin
  Result := nil;

  for i := 0 to List.Count - 1 do
    if (List[i].ClassType = TCPInclude) and (TCPInclude(List[i]).Declaration <> nil) then
    begin
      Result := Find(Name, TInclude(TCPInclude(List[i]).Declaration).Items);
      if (Result <> nil) then
        Exit;
    end else
      if (MatchingDeclaration(List[i], Name)) then
        Exit(List[i]);
end;

function TDeclarationSearcher.Find(Name: String; Parser: TCodeParser): TDeclaration;
begin
  Result := Find(Name, Parser.Items);
end;

function TDeclarationSearcher.Find(Name: String; constref Arr: TDeclarationArray): TDeclaration;
var
  i: Int32;
begin
  Result := nil;

  for i := 0 to High(Arr) do
    if (MatchingDeclaration(Arr[i], Name)) then
      Exit(Arr[i]);
end;

function TDeclarationSearcher.Find(Name: String; Method: TCPMethod): TDeclaration;
var
  i: Int32;
begin
  Result := nil;

  case Name of
    'self':
      if (Method.isMethodOfObject) and (FScriptParser.FDictionary.HasType(Method.ObjectType)) then
        Result := FScriptParser.Dictionary.GetType(Method.ObjectType).Decl;
    'result':
       if (Method.ReturnKind <> nil) then
         Result := Method.ReturnKind;
    else
      begin
        // Local vars
        Result := Find(Name, Method.Variables);
        if (Result <> nil) then
          Exit;

        // Local constants
        Result := Find(Name, Method.Constants);
        if (Result <> nil) then
          Exit;

        // Local types
        Result := Find(Name, Method.Types);
        if (Result <> nil) then
          Exit;


        // Parameters
        for i := 0 to Method.ParameterCount - 1 do
          if (LowerCase(Method.Parameter[i].Name) = Name) then
          begin
            Result := Method.Parameter[i].Kind;
            Exit;
          end;
      end;
  end;
end;

constructor TDeclarationSearcher.Create(ScriptParser: TScriptParser);
begin
  FScriptParser := ScriptParser;
end;

function TScriptParser.TypeOfName(Name: String): TCPTypeKind;
var
  Decl: TDeclaration;
begin
  Result := nil;
  Decl := nil;
  Name := LowerCase(Name);

  // Cast
  if (Dictionary.HasType(Name)) then
    Decl := Dictionary.GetType(Name).Decl
  else
    with TDeclarationSearcher.Create(Self) do
    try
      // Local
      if (FParser.MethodAtCaret <> nil) then
        Decl := Find(Name, FParser.MethodAtCaret);
      // Everything
      if (Decl = nil) then
        Decl := Find(Name, FParser);
    finally
      Free;
    end;

  if (Decl <> nil) then
  begin
    if (Decl.ClassType = TCPVariable) then
      Result := TCPVariable(Decl).Kind
    else
    if (Decl.ClassType = TCPConstant) then
      Result := TCPConstant(Decl).Kind
    else
    if (Decl.ClassType = TCPType) then
      Result := TCPType(Decl).Kind
    else
    if (Decl.ClassType = TCPMethod) then
      Result := TCPMethod(Decl).ReturnKind
    else
    if (Decl.ClassType = TCPTypeKind) then
      Result := TCPTypeKind(Decl);
  end;
end;

constructor TScriptParser.Create;
begin
  FParser := TCodeParser.Create;
  FParser.OnInclude := @OnInclude;

  FDictionary := TItemDictionary.Create;

  FIncludes := TIncludeList.Create;

  FSearchPaths := TStringList.Create;
  FSearchPaths.Add(GetCurrentDirUTF8() + DirectorySeparator);
  FSearchPaths.Duplicates := dupIgnore;
end;

destructor TScriptParser.Destroy;
begin
  FParser.Free;
  FDictionary.Free;
  FIncludes.Free;
  FSearchPaths.Free;

  inherited Destroy;
end;

initialization
  with TLPCompiler.Create('', AllImports, nil, True) do
  try
    NalaInclude := TCodeParser.Create;
    NalaInclude.FilePath := 'Nala';
    NalaInclude.Run(Dump.Merged, -1);
  finally
    Free;
  end;

finalization;
  NalaInclude.Free;

end.

