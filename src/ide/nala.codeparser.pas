unit nala.CodeParser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Types,
  CastaliaPasLex, CastaliaSimplePasPar, CastaliaPasLexTypes;

type
  TDeclaration = class;
  TDeclarationArray = array of TDeclaration;
  TDeclarationClass = class of TDeclaration;

  TDeclarationStack = class(TObject)
  private
    FItems: TList;
    FTop: TDeclaration;
  public
    procedure Push(Item: TDeclaration);
    function Pop: TDeclaration;

    property Top: TDeclaration read FTop write FTop;

    constructor Create;
    destructor Destroy; override;
  end;

  { TDeclarationList }

  TDeclarationList = class(TObject)
  private
    FItems: TList;

    function GetItem(Index: Integer): TDeclaration;
    function GetCount: Integer;
  public
    property List: TList read FItems;

    procedure AddItem(AItem: TDeclaration);
    procedure DeleteItem(AItem: TDeclaration);

    function FindItemsByClass(AClass: TDeclarationClass; DeepSearch: Boolean = False): TDeclarationArray;
    function FindItemByClass(AClass: TDeclarationClass; DeepSearch: Boolean = False): TDeclaration;

    function ToStringArray(AClass: TDeclarationClass): TStringDynArray;

    procedure Clear;

    constructor Create;
    destructor Destroy; override;

    property Items[Index: Integer]: TDeclaration read GetItem; default;
    property Count: Integer read GetCount;
  end;

  TDeclaration = class(TObject)
  private
    FParser: TmwSimplePasPar;
    FOwner: TDeclaration;
    FOrigin: PAnsiChar;

    FStartPos: Integer;
    FEndPos: Integer;

    FItems: TDeclarationList;

    FRawText: String;
    FCleanText: String;

    function GetRawText: string;
    function GetCleanText: string;
  public
    property Parser: TmwSimplePasPar read FParser;
    property Owner: TDeclaration read FOwner;
    property Origin: PAnsiChar read FOrigin;

    property RawText: string read GetRawText;
    property CleanText: string read GetCleanText;
    property StartPos: Integer read FStartPos write FStartPos;
    property EndPos: Integer read FEndPos write FEndPos;
    property Items: TDeclarationList read FItems;

    constructor Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Integer; AEnd: Integer = -1); virtual;
    destructor Destroy; override;
  end;

  TCPTypeKind = class;

  { TDeclMethod }

  TMethodParameter = record
    Name: String;
    Typ: String;
    Def: String;
  end;

  TMethodDirective = (mdNone, mdOverload, mdOverride);
  TMethodLocal = (mlTypes, mlConstants, mlVariables);

  TDeclMethod = class(TDeclaration)
  public type
  type
    TParameter = record
      Name, Typ, Def: String;
    end;

    TParameterArray = array of TParameter;
  private
    FObjectName: Variant;
    FReturnType: Variant;
    FDirective: Variant;
    FDepth: Variant;
    FName: Variant;

    FParameterCount: Variant;
    FParameters: TParameterArray;
    FParameterText: Variant;

    _FEndPos: Int32;

    function GetDirective: TMethodDirective;
    function GetDepth: UInt32;

    function GetName: String;
    function GetObjectName: String;
    function GetReturnKind: TCPTypeKind;
    function GetReturnType: String;

    function GetParameter(Index: UInt32): TParameter;
    function GetParameterCount: UInt32;
    function GetParameterText: String;
  public
    property _EndPos: Int32 read _FEndPos write _FEndPos;

    property Name: String read GetName;
    property ObjectName: String read GetObjectName;
    property ReturnType: String read GetReturnType;
    property ReturnKind: TCPTypeKind read GetReturnKind;

    property Directive: TMethodDirective read GetDirective;
    property Depth: UInt32 read GetDepth;

    property ParameterText: String read GetParameterText;
    property ParameterCount: UInt32 read GetParameterCount;
    property Parameter[Index: UInt32]: TParameter read GetParameter;

    function GetLocal(Typ: TMethodLocal): TDeclarationArray;
  end;
  { TDeclVariable }

  {*
    Some special handling here:

    "var a, b, c: Integer" = VariableDeclaration

    We split that up into three different declarations
  *}

  TDeclVariable = class(TDeclaration)
  public
  type
    TOwnerData = record
      Names: TStringDynArray;
      Count: Integer;
      Typ: Variant;
      Def: Variant;
    end;
  private
    FOwnerVariable: TDeclVariable;
    FOwnerData: TOwnerData;
    FIndex: Integer;

    function GetName: String;
    function GetDef: String;
    function GetKind: TCPTypeKind;
    function GetTyp: String;
    function _GetName(Index: UInt32): String;
  public
    property OwnerVariable: TDeclVariable write FOwnerVariable;
    property OwnerData: TOwnerData read FOwnerData write FOwnerData;
    property Def: String read GetDef;
    property Typ: String read GetTyp;
    property Kind: TCPTypeKind read GetKind;
    property Name: String read GetName;

    constructor Create(AOwnerVariable: TDeclVariable; AIndex: Integer); overload;
  end;

  { TDeclConstant }

  TDeclConstant = class(TDeclaration)
  private
    FTyp: Variant;
    FDef: Variant;
    FName: Variant;

    function GetName: String;
    function GetDef: String;
    function GetKind: TCPTypeKind;
    function GetTyp: String;
  public
    property Typ: String read GetTyp;
    property Def: String read GetDef;
    property Kind: TCPTypeKind read GetKind;
    property Name: String read GetName;
  end;

  { TCPRecord }

  TCPRecord = class(TDeclaration)
  public
  type
    TField = record Name, Typ: String; Kind: TCPTypeKind; end;
    TFieldArray = array of TField;
  private
    FAncestor: Variant;

    FFieldCount: Variant;
    FFields: TFieldArray;

    function GetAncestor: String;
    function GetField(Index: UInt32): TField;
    function GetFieldCount: UInt32;
  public
    property Ancestor: String read GetAncestor;

    property FieldCount: UInt32 read GetFieldCount;
    property Fields[Index: UInt32]: TField read GetField;
  end;

  { TCPEnum }

  TCPEnum = class(TDeclaration)
  public
  type
    TEnum = record Name, Value: String; end;
    TEnumArray = array of TEnum;
  private
    FEnumCount: Variant;
    FEnums: TEnumArray;

    function GetEnum(Index: UInt32): TEnum;
    function GetEnumCount: UInt32;
  public
    property EnumCount: UInt32 read GetEnumCount;
    property Enums[Index: UInt32]: TEnum read GetEnum;
  end;

  { TCPArray }

  TCPArray = class(TDeclaration)
  private
    FDimensions: Variant;
    FTyp: Variant;

    function GetDimensions: UInt32;
    function GetKind: TCPTypeKind;
    function GetType: String;
  public
    property Typ: String read GetType;
    property Dimensions: UInt32 read GetDimensions;
    property Kind: TCPTypeKind read GetKind;
  end;

  { TCPSet }

  TCPSet = class(TDeclaration)
  private
    FSetType: Variant;

    function GetSetType: String;
  public
    property SetType: String read GetSetType;
  end;

  { TCPTypeCopy }

  TCPTypeCopy = class(TDeclaration)
  private
    function GetCopyType: String;
  public
    property CopyType: String read GetCopyType;
  end;

  { TCPTypeAlias }

  TCPTypeAlias = class(TDeclaration)
  private
    function GetAliasType: String;
  public
    property AliasType: String read GetAliasType;
  end;

  { TCPTypeKind }

  ETypeKind = (tkType, tkRecord, tkEnum, tkSet, tkArray, tkCopy, tkAlias);

  TCPTypeKind = class(TDeclaration)
  private
    FTyp: ETypeKind;
    FTypeItem: TDeclaration;
  public
    property Typ: ETypeKind read FTyp write FTyp;
    property TypeItem: TDeclaration read FTypeItem write FTypeItem;

    function GetRecord: TCPRecord;
    function GetEnum: TCPEnum;
    function GetArray: TCPArray;
    function GetAlias: TCPTypeAlias;
    function GetCopy: TCPTypeCopy;
    function GetSet: TCPSet;

    constructor Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Integer; AEnd: Integer = -1); override;
  end;

  { TCPTypeDeclaration }

  TCPTypeDeclaration = class(TDeclaration)
  private
    FKind: TCPTypeKind;
    FName: Variant;

    function GetName: String;
  public
    property Kind: TCPTypeKind read FKind write FKind;
    property Name: String read GetName;

    function Format(NamePrefix: Boolean = True): String;
  end;

  { TCodeParser }

  TOnType = procedure(Decl: TCPTypeDeclaration) of object; // When type declaration is found
  TOnMethodOfObject = procedure(Decl: TDeclMethod) of object;  // When type method is found

  TCodeParser = class(TmwSimplePasPar)
  protected
    FCaret: Int32;
    FLocalMethod: TDeclMethod;

    FStream: TMemoryStream;

    FStack: TDeclarationStack;
    FItems: TDeclarationList;

    FOnType: TOnType;
    FOnMethodOfObject: TOnMethodOfObject;

    procedure DefaultMessage(Sender: TObject; const Typ : TMessageEventType; const Msg: string; X, Y: Integer);

    function InDeclaration(AClass: TDeclarationClass): Boolean;
    function InDeclaration(AClass: TDeclarationClass; out Decl: TDeclaration; DeepSearch: Boolean = False): Boolean; overload;

    procedure PushStack(AClass: TDeclarationClass; AStart: Integer = -1);
    procedure PopStack(AEnd: Integer = -1);

    procedure NextToken; override;
    procedure ParseFile; override;

    procedure ProcedureDeclarationSection; override;
    procedure VarDeclaration; override;
    procedure VarName; override;
    procedure VarNameList; override;
    procedure TypeKind; override;
    procedure FunctionProcedureName; override;
    procedure ObjectNameOfMethod; override;
    procedure ReturnType; override;
    procedure ConstantValueTyped; override;
    procedure ProceduralDirective; override;
    procedure ConstantType; override;
    procedure ConstantName; override;
    procedure ConstantDeclaration; override;
    procedure ConstantValue; override;
    procedure ConstantExpression; override;
    procedure TypeDeclaration; override;
    procedure TypeName; override;
    procedure RecordType; override;
    procedure AncestorId; override;
    procedure FieldName; override;
    procedure ExplicitType; override;
    procedure SetType; override;
    procedure TypeId; override;
    procedure EnumeratedType; override;
    procedure EnumeratedTypeItem; override;
    procedure FormalParameterList; override;
    procedure FormalParameterSection; override;
    procedure Block; override;
    procedure ArrayType; override;
    procedure ParameterName; override;
    procedure ParameterNameList; override;
    procedure OldFormalParameterType; override;
    procedure NewFormalParameterType; override;
    procedure FieldNameList; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Run(constref Text: String; MaxPos: Int32 = -1; CaretPos: Int32 = -1): Boolean; overload;

    property OnType: TOnType read FOnType write FOnType;
    property OnMethodOfObject: TOnMethodOfObject read FOnMethodOfObject write FOnMethodOfObject;

    property Items: TDeclarationList read FItems;
    property LocalMethod: TDeclMethod read FLocalMethod;
  end;

  TCodeParserArray = array of TCodeParser;

implementation

uses
  Variants, nala.Helpers;

type
  // Types
  TDeclTypeName = class(TDeclaration);
  TDeclAncestor = class(TDeclaration);
  TDeclFieldName = class(TDeclaration);
  TDeclFieldNameList = class(TDeclaration);
  TDeclTypeID = class(TDeclaration);
  TDeclEnumItem = class(TDeclaration);
  TDeclConstantExpression = class(TDeclaration);
  TCPExplicitType = class(TDeclaration);

  // Method
  TDeclMethodName = class(TDeclaration);
  TDeclMethodObjectName = class(TDeclaration);
  TDeclMethodReturnType = class(TDeclaration);
  TDeclMethodDirective = class(TDeclaration);

  // Parameter
  TDeclParameterList = class(TDeclaration);
  TDeclParameterSection = class(TDeclaration);
  TDeclParameterName = class(TDeclaration);
  TDeclParameterNameList = class(TDeclaration);

  // Variable
  TDeclVarNameList = class(TDeclaration);
  TDeclVarName = class(TDeclaration);

  // Constant
  TDeclConstantName = class(TDeclaration);
  TDeclConstantType = class(TDeclaration);
  TDeclConstantValue = class(TDeclaration);
  TDeclConstantValueTyped = class(TDeclaration);

  // Misc
  TDeclJunk = class(TDeclaration);

{ TCPTypeAlias }

function TCPTypeAlias.GetAliasType: String;
begin
  Result := FOwner.CleanText;
end;

{ TCPTypeCopy }

function TCPTypeCopy.GetCopyType: String;
begin
  Result := FOwner.CleanText;
end;

{ TCPSet }

function TCPSet.GetSetType: String;
var
  Decl: TDeclaration;
begin
  if (not FSetType.Assigned) then
  begin
    FSetType := '';

    Decl := FItems.FindItemByClass(TDeclTypeID);
    if (Decl <> nil) then
      FSetType := Decl.CleanText;
  end;

  Result := FSetType;
end;

{ TCPArray }

function TCPArray.GetDimensions: UInt32;
begin
  if (not FDimensions.Assigned) then
    FDimensions := Length(FItems.FindItemsByClass(TCPArray, True)) + 1;

  Result := FDimensions;
end;

function TCPArray.GetKind: TCPTypeKind;
var
  Decls: TDeclarationArray;
begin
  Result := nil;

  Decls := FItems.FindItemsByClass(TCPTypeKind, True);
  if ((Dimensions - 1) < Length(Decls)) then
    Exit(TCPTypeKind(Decls[Dimensions - 1]));
end;

function TCPArray.GetType: String;
var
  Decl: TDeclaration;
begin
  if (not FTyp.Assigned) then
  begin
    FTyp := '';

    Decl := Self.GetKind;
    if (Decl <> nil) then
      FTyp := Decl.CleanText;
  end;

  Result := FTyp;
end;

{ TCPEnum }

function TCPEnum.GetEnum(Index: UInt32): TEnum;
begin
  Result := FEnums[Index];
end;

function TCPEnum.GetEnumCount: UInt32;
var
  Decls: TDeclarationArray;
  Decl: TDeclaration;
  i: Int32;
begin
  if (not FEnumCount.Assigned) then
  begin
    FEnumCount := 0;

    Decls := FItems.FindItemsByClass(TDeclEnumItem, True);
    if (Length(Decls) > 0) then
    begin
      FEnumCount := Length(Decls);
      SetLength(FEnums, FEnumCount);

      for i := 0 to FEnumCount - 1 do
      begin
        FEnums[i].Name := Decls[i].CleanText;

        Decl := Decls[i].Items.FindItemByClass(TDeclConstantExpression);
        if (Decl <> nil) then
        begin
          FEnums[i].Name := Trim(LeftStr(FEnums[i].Name, Pos('=', FEnums[i].Name) - 1));
          FEnums[i].Value := Decl.CleanText;
        end else
          FEnums[i].Value := '';
      end;
    end;
  end;

  Result := FEnumCount;
end;

{ TCPRecord }

function TCPRecord.GetAncestor: String;
var
  Decl: TDeclaration;
begin
  if (not FAncestor.Assigned) then
  begin
    FAncestor := '';

    Decl := FItems.FindItemByClass(TDeclAncestor);
    if (Decl <> nil) then
      FAncestor := Decl.CleanText;
  end;

  Result := FAncestor;
end;

function TCPRecord.GetField(Index: UInt32): TField;
begin
  Result := FFields[Index];
end;

function TCPRecord.GetFieldCount: UInt32;

  procedure AddField(constref Name, Typ: String; Kind: TCPTypeKind);
  begin
    SetLength(FFields, FFieldCount + 1);
    FFields[FFieldCount].Name := Name;
    FFields[FFieldCount].Typ := Typ;
    FFields[FFieldCount].Kind := Kind;
    FFieldCount := FFieldCount + 1;
  end;

var
  i, j: Int32;
  Names: TStringDynArray;
begin
  if (not FFieldCount.Assigned) then
  begin
    FFieldCount := 0;
    SetLength(FFields, 0);

    for i := 0 to FItems.Count - 1 do
      if (FItems[i].ClassType = TDeclFieldNameList) then
        Names := FItems[i].Items.ToStringArray(TDeclFieldName)
      else
      if (FItems[i].ClassType = TCPTypeKind) then
        for j := 0 to High(Names) do
          AddField(Names[j], FItems[i].CleanText, TCPTypeKind(FItems[i]));
  end;

  Result := FFieldCount;
end;

function TCPTypeKind.GetRecord: TCPRecord;
begin
  Result := FTypeItem as TCPRecord;
end;

function TCPTypeKind.GetEnum: TCPEnum;
begin
  Result := FTypeItem as TCPEnum;
end;

function TCPTypeKind.GetArray: TCPArray;
begin
  Result := FTypeItem as TCPArray;
end;

function TCPTypeKind.GetAlias: TCPTypeAlias;
begin
  Result := FTypeItem as TCPTypeAlias;
end;

function TCPTypeKind.GetCopy: TCPTypeCopy;
begin
  Result := FTypeItem as TCPTypeCopy;
end;

function TCPTypeKind.GetSet: TCPSet;
begin
  Result := FTypeItem as TCPSet;
end;

constructor TCPTypeKind.Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Integer; AEnd: Integer);
begin
  inherited Create(AParser, AOwner, AOrigin, AStart, AEnd);

  FTyp := tkType;
  FTypeItem := nil;
end;

{ TCPTypeDeclaration }

function TCPTypeDeclaration.GetName: String;
var
  Decl: TDeclaration;
begin
  if (not FName.Assigned) then
  begin
    Decl := FItems.FindItemByClass(TDeclTypeName);
    if (Decl <> nil) then
      FName := Decl.CleanText;
  end;

  Result := FName;
end;


function TCPTypeDeclaration.Format(NamePrefix: Boolean): String;
var
  i: Integer;
  Decl: TDeclaration;
begin
  Result := '';
  if (NamePrefix) then
    Result += Name + ' = ';

  case Kind.Typ of
    tkSet:
      if (Kind.GetSet() <> nil) then
        Result += 'set of ' + Kind.GetSet.SetType;
    tkAlias:
      if (Kind.GetAlias() <> nil) then
        Result += Kind.GetAlias.AliasType;
    tkCopy:
      if (Kind.GetCopy() <> nil) then
        Result += 'type ' + Kind.GetCopy.CopyType;
    tkArray:
      if (Kind.GetArray() <> nil) then
        Result += Kind.GetArray.CleanText;
    tkRecord:
      if (Kind.GetRecord() <> nil) then
      begin
        Result += 'record';
        if (Kind.GetRecord.Ancestor <> '') then
          Result += '(' + Kind.GetRecord.Ancestor + ')';
      end;
    tkEnum:
      if (Kind.GetEnum() <> nil) then
      begin
        Result += '(';
        with Kind.GetEnum do
          for i := 0 to EnumCount - 1 do
          begin
            Result += Enums[i].Name;
            if (Enums[i].Value <> '') then
              Result += ' = ' + Enums[i].Value;
            Result += BoolToStr(i < EnumCount, ', ', '');
          end;
       Result += ')';
      end;
  end;
end;

{ TDeclVariable }

function TDeclVariable.GetName: String;
begin
  if (FOwnerVariable <> nil) then
    Result := FOwnerVariable._GetName(FIndex)
  else
    Result := _GetName(0);
end;

function TDeclVariable.GetDef: String;
var
  Decl: TDeclaration;
begin
  if (FOwnerVariable <> nil) then
    Exit(FOwnerVariable.Def);

  if (not FOwnerData.Def.Assigned) then
  begin
    FOwnerData.Def := '';

    Decl := FItems.FindItemByClass(TDeclConstantValueTyped);
    if (Decl <> nil) then
      FOwnerData.Def := Decl.CleanText
    else
    begin
      Decl := FItems.FindItemByClass(TDeclConstantValue);
      if (Decl <> nil) then
        FOwnerData.Def := Decl.CleanText;
    end;
  end;

  Result := FOwnerData.Def;
end;

function TDeclVariable.GetKind: TCPTypeKind;
begin
  Result := FItems.FindItemByClass(TCPTypeKind) as TCPTypeKind;
end;

function TDeclVariable._GetName(Index: UInt32): String;
var
  Decl: TDeclaration;
begin
  Result := '';

  if (Length(FOwnerData.Names) <> FOwnerData.Count) then
  begin
    Decl := FItems.FindItemByClass(TDeclVarNameList);
    if (Decl <> nil) then
      FOwnerData.Names := Decl.Items.ToStringArray(TDeclVarName);
    FOwnerData.Count := Length(FOwnerData.Names);
  end;

  if (Index < FOwnerData.Count) then
    Result := FOwnerData.Names[Index];
end;

function TDeclVariable.GetTyp: String;
var
  Decl: TDeclaration;
begin
  if (FOwnerVariable <> nil) then
    Exit(FOwnerVariable.Typ);

  if (not FOwnerData.Typ.Assigned) then
  begin
    FOwnerData.Typ := '';

    Decl := FItems.FindItemByClass(TCPTypeKind);
    if (Decl <> nil) then
      FOwnerData.Typ := Decl.CleanText;
  end;

  Result := FOwnerData.Typ;
end;

constructor TDeclVariable.Create(AOwnerVariable: TDeclVariable; AIndex: Integer);
begin
  inherited Create(AOwnerVariable.Parser, AOwnerVariable.Owner, AOwnerVariable.Origin,
                   AOwnerVariable.StartPos, AOwnerVariable.EndPos);

  FOwnerVariable := AOwnerVariable;
  FIndex := AIndex;
end;

{ TDeclConstant }

function TDeclConstant.GetName: String;
var
  Decl: TDeclaration;
begin
  if (not FName.Assigned) then
  begin
    FName := '';

    Decl := FItems.FindItemByClass(TDeclConstantName);
    if (Decl <> nil) then
      FName := Decl.CleanText;
  end;

  Result := FName;
end;

function TDeclConstant.GetDef: String;
var
  Decl: TDeclaration;
begin
  if (not FDef.Assigned) then
  begin
    FDef := '';

    Decl := FItems.FindItemByClass(TDeclConstantValueTyped);
    if (Decl <> nil) then
      FDef := Decl.CleanText
    else
    begin
      Decl := FItems.FindItemByClass(TDeclConstantValue);
      if (Decl <> nil) then
        FDef := Decl.CleanText;
    end;
  end;

  Result := FDef;
end;

function TDeclConstant.GetKind: TCPTypeKind;
begin
  Result := FItems.FindItemByClass(TDeclConstantType) as TCPTypeKind;
end;

function TDeclConstant.GetTyp: String;
var
  Decl: TDeclaration;
begin
  if (not FDef.Assigned) then
  begin
    FTyp := '';

    Decl := FItems.FindItemByClass(TDeclConstantType);
    if (Decl <> nil) then
      FTyp := Decl.CleanText;
  end;

  Result := FTyp;
end;

{ TDeclMethod }

function TDeclMethod.GetName: String;
var
  Decl: TDeclaration;
begin
  if (not FName.Assigned) then
  begin
    Decl := FItems.FindItemByClass(TDeclMethodName);
    if (Decl <> nil) then
      FName := Decl.CleanText;
  end;

  Result := FName;
end;

function TDeclMethod.GetDirective: TMethodDirective;
var
  Decls: TDeclarationArray;
  i: Integer;
begin
  if (not FDirective.Assigned) then
  begin
    FDirective := Ord(mdNone);

    Decls := FItems.FindItemsByClass(TDeclMethodDirective);
    for i := 0 to High(Decls) do
      case Lowercase(Decls[i].RawText) of
        'overload': FDirective := Ord(mdOverload);
        'override': FDirective := Ord(mdOverride);
      end;
  end;

  Result := TMethodDirective(FDirective);
end;

function TDeclMethod.GetDepth: UInt32;
var
  d: TDeclaration;
begin
  if (not FDepth.Assigned) then
  begin
    FDepth := 0;

    d := Self;
    while (d <> nil) and (d.Owner <> nil) and (d.Owner.ClassType = TDeclMethod) do
    begin
      FDepth := FDepth + 1;
      d := d.Owner;
    end;
  end;

  Result := FDepth;
end;

function TDeclMethod.GetObjectName: String;
var
  Decl: TDeclaration;
begin
  if (not FObjectName.Assigned) then
  begin
    FObjectName := '';

    Decl := FItems.FindItemByClass(TDeclMethodObjectName);
    if (Decl <> nil) then
      FObjectName := Decl.CleanText;
  end;

  Result := FObjectName;
end;

function TDeclMethod.GetReturnKind: TCPTypeKind;
var
  Decl: TDeclaration;
begin
  Result := nil;
  Decl := FItems.FindItemByClass(TDeclMethodReturnType);
  if (Decl <> nil) then
    Result := Decl.Items.FindItemByClass(TCPTypeKind) as TCPTypeKind;
end;

function TDeclMethod.GetParameter(Index: UInt32): TParameter;
begin
  Result := FParameters[Index];
end;

function TDeclMethod.GetParameterCount: UInt32;

  procedure AddParameters(Item: TDeclParameterSection);
  var
    Decl, Kind, Def: TDeclaration;
    Names: TStringDynArray;
    i: Integer;
  begin
    Decl := Item.Items.FindItemByClass(TDeclParameterNameList);
    if (Decl <> nil) then
    begin
      Names := Decl.Items.ToStringArray(TDeclParameterName);
      Kind := Item.Items.FindItemByClass(TCPTypeKind);
      Def := Item.Items.FindItemByClass(TDeclConstantExpression);

      for i := 0 to High(Names) do
      begin
        SetLength(FParameters, FParameterCount + 1);

        FParameters[FParameterCount] := Default(TParameter);
        FParameters[FParameterCount].Name := Names[i];
        if (Kind <> nil) then
          FParameters[FParameterCount].Typ := Kind.CleanText;
        if (Def <> nil) then
          FParameters[FParameterCount].Def := Def.CleanText;

        FParameterCount := FParameterCount + 1;
      end;
    end;
  end;

var
  Decls: TDeclarationArray;
  i: Integer;
begin
  if (not FParameterCount.Assigned) then
  begin
    SetLength(FParameters, 0);
    FParameterCount := 0;

    Decls := FItems.FindItemsByClass(TDeclParameterSection, True);
    for i := 0 to High(Decls) do
      AddParameters(TDeclParameterSection(Decls[i]));
  end;

  Result := FParameterCount;
end;

function TDeclMethod.GetParameterText: String;
var
  Decl: TDeclaration;
begin
  if (not FParameterText.Assigned) then
  begin
    FParameterText := '';

    Decl := FItems.FindItemByClass(TDeclParameterList);
    if (Decl <> nil) then
      FParameterText := Decl.CleanText;
  end;

  Result := FParameterText;
end;

function TDeclMethod.GetReturnType: String;
var
  Decl: TDeclaration;
begin
  if (not FReturnType.Assigned) then
  begin
    FReturnType := '';

    Decl := FItems.FindItemByClass(TDeclMethodReturnType);
    if (Decl <> nil) then
      FReturnType := Decl.CleanText;
  end;

  Result := FReturnType;
end;

function TDeclMethod.GetLocal(Typ: TMethodLocal): TDeclarationArray;
begin
  case Typ of
    mlConstants: Result := FItems.FindItemsByClass(TDeclConstant);
    mlVariables: Result := FItems.FindItemsByClass(TDeclVariable);
    mlTypes: Result := FItems.FindItemsByClass(TCPTypeDeclaration);
  end;
end;

procedure TDeclarationStack.Push(Item: TDeclaration);
begin
  FItems.Add(Item);
  FTop := Item;
end;

function TDeclarationStack.Pop: TDeclaration;
begin
  with FItems do
  begin
    if (Count > 0) then
      Delete(Count - 1);
    if (Count > 0) then
      FTop := TDeclaration(Items[Count - 1])
    else
      FTop := nil;
  end;
  Result := FTop;
end;

constructor TDeclarationStack.Create;
begin
  FItems := TList.Create;
  FTop := nil;
end;

destructor TDeclarationStack.Destroy;
begin
  FItems.Free;

  inherited;
end;

function TDeclarationList.GetItem(Index: Integer): TDeclaration;
begin
  Result := TDeclaration(FItems[Index]);
end;

function TDeclarationList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

procedure TDeclarationList.AddItem(AItem: TDeclaration);
begin
  FItems.Add(AItem);
end;

procedure TDeclarationList.DeleteItem(AItem: TDeclaration);
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
  begin
    if (FItems[i] <> nil) and (TDeclaration(fItems[i]) = AItem) then
    begin
      FItems.Delete(i);
      Exit;
    end;
  end;
end;

function TDeclarationList.FindItemsByClass(AClass: TDeclarationClass; DeepSearch: Boolean): TDeclarationArray;

  procedure Search(AClass: TDeclarationClass; DeepSearch: Boolean; Item: TDeclaration;
                   var AResult: TDeclarationArray; var AResultLength: Integer);
  var
    i: Integer;
  begin
    if (Item <> nil) and (Item.ClassType = AClass) then
    begin
      SetLength(AResult, AResultLength + 1);
      AResult[AResultLength] := Item;
      Inc(AResultLength);
    end;
    if (DeepSearch) and (Item.Items <> nil) then
      for i := 0 to Item.Items.Count - 1 do
        Search(AClass, DeepSearch, Item.Items[i], AResult, AResultLength);
  end;

var
  ResultLength, i: Integer;
begin
  SetLength(Result, 0);
  ResultLength := 0;

  for i := 0 to FItems.Count - 1 do
    Search(AClass, DeepSearch, TDeclaration(FItems[i]), Result, ResultLength);
end;

function TDeclarationList.FindItemByClass(AClass: TDeclarationClass; DeepSearch: Boolean): TDeclaration;

  function Search(AClass: TDeclarationClass; DeepSearch: Boolean; Item: TDeclaration; out AResult: TDeclaration): Boolean;
  var
    i: Integer;
  begin
    if (Item <> nil) and (Item.ClassType = AClass) then
    begin
      AResult := Item;
      Exit(True);
    end;
    if (DeepSearch) and (Item.Items <> nil) then
      for i := 0 to Item.Items.Count - 1 do
        if (Search(AClass, DeepSearch, Item.Items[i], AResult)) then
          Exit(True);

    Result := False;
  end;

var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FItems.Count - 1 do
    if (Search(AClass, DeepSearch, TDeclaration(FItems[i]), Result)) then
      Exit;
end;

procedure TDeclarationList.Clear;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    if (FItems[i] <> nil) then
      TDeclaration(FItems[i]).Free;
  FItems.Clear;
end;

constructor TDeclarationList.Create;
begin
  FItems := TList.Create;
end;

destructor TDeclarationList.Destroy;
begin
  Clear;
  FItems.Free;

  inherited;
end;

function TDeclaration.GetRawText: string;
begin
  if (FRawText <> '') then
    Exit(FRawText);
  if (FStartPos <> FEndPos) and (FOrigin <> nil) then
    SetString(FRawText, FOrigin + FStartPos, FEndPos - FStartPos);

  Result := FRawText;
end;

function TDeclaration.GetCleanText: string;
var
  i: Integer;
  Decls: TDeclarationArray;
begin
  if (FCleanText = '') then
  begin
    FCleanText := RawText;

    Decls := Items.FindItemsByClass(TDeclJunk, True);
    if (Length(Decls) > 0) then
      for i := High(Decls) downto 0 do
        Delete(FCleanText, Decls[i].StartPos - FStartPos + 1, Decls[i].EndPos - Decls[i].StartPos);

    FCleanText := TrimRight(StringReplace(StringReplace(FCleanText, '  ', '', [rfReplaceAll]), LineEnding, '', [rfReplaceAll])); // Magical
  end;

  Result := FCleanText;
end;

function TDeclarationList.ToStringArray(AClass: TDeclarationClass): TStringDynArray;
var
  i, c: Integer;
begin
  SetLength(Result, Count);
  c := 0;

  for i := 0 to Count - 1 do
    if (TDeclaration(FItems[i]).ClassType = AClass) then
    begin
      Result[c] := TDeclaration(FItems[i]).CleanText;
      Inc(c);
    end;

  SetLength(Result, c);
end;

constructor TDeclaration.Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Integer; AEnd: Integer);
begin
  inherited Create;

  FParser := AParser;
  FOwner := AOwner;
  FOrigin := AOrigin;
  FStartPos := AStart;
  if (AEnd > -1) then
    FEndPos := AEnd
  else
    FEndPos := AStart;

  FRawText := '';
  FCleanText := '';

  FItems := TDeclarationList.Create;
end;

destructor TDeclaration.Destroy;
begin
  FItems.Free;

  inherited;
end;

procedure TCodeParser.DefaultMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
begin
  Writeln('CP: ', Msg, ' at line ', Y + 1);
end;

function TCodeParser.InDeclaration(AClass: TDeclarationClass): Boolean;
begin
  if (FStack.Top = nil) then
    Result := False
  else
    Result := FStack.Top.ClassType = AClass;
end;

function TCodeParser.InDeclaration(AClass: TDeclarationClass; out Decl: TDeclaration; DeepSearch: Boolean): Boolean;

  function Search(Item: TDeclaration; AClass: TDeclarationClass; var Decl: TDeclaration; DeepSearch: Boolean): Boolean;
  begin
    Result := False;

    if (Item.ClassType = AClass) then
    begin
      Decl := Item;
      Exit(True);
    end;

    if (DeepSearch) and (Item.Owner <> nil) then
      Result := Search(Item.Owner, AClass, Decl, DeepSearch);
  end;

begin
  Result := False;
  Decl := nil;

  if (FStack.Top <> nil) then
    Result := Search(FStack.Top, AClass, Decl, DeepSearch)
end;

procedure TCodeParser.PushStack(AClass: TDeclarationClass; AStart: Integer = -1);
var
  Decl: TDeclaration;
begin
  if (AStart = -1) then
    AStart := Lexer.TokenPos;
  Decl := AClass.Create(Self, FStack.Top, Lexer.Origin, AStart);
  if (FStack.Top <> nil) then
    FStack.Top.Items.AddItem(Decl)
  else
    FItems.AddItem(Decl);
  FStack.Push(Decl);
end;

procedure TCodeParser.PopStack(AEnd: Integer = -1);
begin
  if (AEnd = -1) then
    AEnd := Lexer.TokenPos;
  if (FStack.Top <> nil) then
    FStack.Top.EndPos := AEnd;
  FStack.Pop;
end;

procedure TCodeParser.NextToken;
begin
  Lexer.Next;
  if (Lexer.IsJunk) and (not InDeclaration(TDeclJunk)) then
  begin
    repeat
      if (Lexer.TokenID in [tokAnsiComment, tokBorComment, tokSlashesComment]) then
      begin
        if (not InDeclaration(TDeclJunk)) then
          PushStack(TDeclJunk);
      end else
        if (InDeclaration(TDeclJunk)) then
          PopStack;
      Lexer.Next;
    until (not Lexer.IsJunk);

    if (InDeclaration(TDeclJunk)) then
      PopStack;
  end;
end;

procedure TCodeParser.ParseFile;
begin
  SkipJunk;

  if (Lexer.GenID = tokProgram) then
  begin
    Expected(tokProgram);
    QualifiedIdentifier;
    if (TokenID = tokRoundOpen) then
    begin
      NextToken;
      IdentifierList;
      Expected(tokRoundClose);
    end;
    SemiColon;
  end;

  while (TokenID in [tokConst, tokFunction, tokLabel, tokProcedure, tokResourceString, tokType, tokVar, tokSquareOpen]) do
    DeclarationSection;

  if (TokenID = tokBegin) then
  begin
    CompoundStatement;

    if (TokenID = tokSemiColon) then
      Expected(tokSemiColon)
    else
      Expected(tokPoint);
  end;
end;

constructor TCodeParser.Create;
begin
  inherited;

  FStream := TMemoryStream.Create;

  FStack := TDeclarationStack.Create;
  FItems := TDeclarationList.Create;

  FOnType := nil;
  FOnMethodOfObject := nil;
end;

destructor TCodeParser.Destroy;
begin
  FStream.Free;

  FStack.Free;
  FItems.Free;

  inherited;
end;

function TCodeParser.Run(constref Text: String; MaxPos: Int32; CaretPos: Int32): Boolean;
begin
  Result := True;

  if (not Assigned(OnMessage)) then
    OnMessage := @DefaultMessage;

  FStream.Clear;
  FStream.Write(Text[1], Length(Text));

  FCaret := CaretPos;
  FLocalMethod := nil;

  try
    Run(FStream, MaxPos);
  except
    on e: ESyntaxError do
     if (OnMessage <> nil) then
       OnMessage(Self, meError, e.ClassName + ': ' + e.Message, -1, -1); // Still result true.

    on e: Exception do
    begin
      if (OnMessage <> nil) then
        OnMessage(Self, meError, e.ClassName + ': ' + e.Message, -1, -1);

      Result := False;
    end;
  end;
end;

procedure TCodeParser.ProcedureDeclarationSection;
var
  Method: TDeclMethod;
begin
  PushStack(TDeclMethod);

  inherited ProcedureDeclarationSection;

  Method := FStack.Top as TDeclMethod;

  if (FOnMethodOfObject <> nil) and (Method.Name <> '') and (Method.ObjectName <> '') then
    FOnMethodOfObject(Method);

  PopStack();

  if (FCaret > -1) and (FLocalMethod = nil) and (FCaret >= Method.StartPos) and (FCaret < Method.EndPos) then
    FLocalMethod := Method;

  // Fix end pos (currently beginning of next block)
  if (not Method.Depth = 0) then
    Method.EndPos := Method._FEndPos;
end;

procedure TCodeParser.VarDeclaration;
var
  Decl: TDeclVariable;
  i: Integer;
begin
  PushStack(TDeclVariable);

  Decl := FStack.Top as TDeclVariable;
  Decl.OwnerVariable := nil;
  Decl.OwnerData := Default(TDeclVariable.TOwnerData);

  inherited VarDeclaration;
  PopStack();

  for i := 1 to Decl.OwnerData.Count - 1 do
    if (Decl.Owner <> nil) then
      Decl.Owner.Items.AddItem(TDeclVariable.Create(Decl, i))
    else
      FItems.AddItem(TDeclVariable.Create(Decl, i));
end;

procedure TCodeParser.VarName;
var
  Decl: TDeclaration;
begin
  if (InDeclaration(TDeclVariable, Decl, True)) then
    with TDeclVariable(Decl).OwnerData do
      Count := Count + 1;

  PushStack(TDeclVarName);
  inherited VarName;
  PopStack();
end;

procedure TCodeParser.TypeKind;
begin
  PushStack(TCPTypeKind);
  inherited TypeKind;
  PopStack();
end;

procedure TCodeParser.FunctionProcedureName;
begin
  PushStack(TDeclMethodName);
  inherited FunctionProcedureName;
  PopStack();
end;

procedure TCodeParser.ObjectNameOfMethod;
begin
  PushStack(TDeclMethodObjectName);
  inherited ObjectNameOfMethod;
  PopStack();
end;

procedure TCodeParser.ReturnType;
begin
  PushStack(TDeclMethodReturnType);
  inherited ReturnType;
  PopStack();
end;

procedure TCodeParser.VarNameList;
begin
  PushStack(TDeclVarNameList);
  inherited VarNameList;
  PopStack();
end;

procedure TCodeParser.ConstantValueTyped;
begin
  PushStack(TDeclConstantValueTyped);
  inherited ConstantValueTyped;
  PopStack();
end;

procedure TCodeParser.ProceduralDirective;
begin
  PushStack(TDeclMethodDirective);
  inherited ProceduralDirective;
  PopStack();
end;

procedure TCodeParser.ConstantType;
begin
  PushStack(TDeclConstantType);
  inherited ConstantType;
  PopStack();
end;

procedure TCodeParser.ConstantName;
begin
  PushStack(TDeclConstantName);
  inherited ConstantName;
  PopStack();
end;

procedure TCodeParser.ConstantDeclaration;
begin
  PushStack(TDeclConstant);
  inherited ConstantDeclaration;
  PopStack();
end;

procedure TCodeParser.ConstantValue;
begin
  PushStack(TDeclConstantValue);
  inherited ConstantValue;
  PopStack();
end;

procedure TCodeParser.ConstantExpression;
begin
  PushStack(TDeclConstantExpression);
  inherited ConstantExpression;
  PopStack();
end;

procedure TCodeParser.TypeDeclaration;
var
  Decl: TCPTypeDeclaration;
begin
  PushStack(TCPTypeDeclaration);
  Decl := FStack.Top as TCPTypeDeclaration;

  inherited TypeDeclaration;

  Decl.Kind := Decl.Items.FindItemByClass(TCPTypeKind) as TCPTypeKind;
  if (Decl.Kind = nil) then
    raise Exception.Create('TCodeParser.TypeDeclaration: Decl.Kind = nil');

  if (Decl.Kind.Typ = tkType) then
  begin
    if (Decl.Items.FindItemByClass(TCPExplicitType) <> nil) then
    begin
      Decl.Kind.Typ := tkCopy;
      Decl.Kind.TypeItem := TCPTypeCopy.Create(Decl.Kind.Parser, Decl.Kind, Decl.Kind.Origin, Decl.Kind.StartPos, Decl.Kind.EndPos);
      Decl.Kind.Items.AddItem(Decl.Kind.TypeItem);
    end else
    begin
      Decl.Kind.Typ := tkAlias;
      Decl.Kind.TypeItem := TCPTypeAlias.Create(Decl.Kind.Parser, Decl.Kind, Decl.Kind.Origin, Decl.Kind.StartPos, Decl.Kind.EndPos);
      Decl.Kind.Items.AddItem(Decl.Kind.TypeItem);
    end;
  end;

  if (FOnType <> nil) then
    FOnType(Decl as TCPTypeDeclaration);

  PopStack();
end;

procedure TCodeParser.TypeName;
begin
  PushStack(TDeclTypeName);
  inherited TypeName;
  PopStack();
end;

procedure TCodeParser.RecordType;
var
  Decl: TDeclaration;
begin
  if (InDeclaration(TCPTypeKind, Decl)) then
    TCPTypeKind(Decl).Typ := tkRecord;

  PushStack(TCPRecord);
  TCPTypeKind(Decl).TypeItem := FStack.Top;
  inherited RecordType;
  PopStack();
end;

procedure TCodeParser.AncestorId;
begin
  PushStack(TDeclAncestor);
  inherited AncestorId;
  PopStack();
end;

procedure TCodeParser.FieldName;
begin
  PushStack(TDeclFieldName);
  inherited FieldName;
  PopStack();
end;

procedure TCodeParser.ExplicitType;
begin
  PushStack(TCPExplicitType);
  inherited ExplicitType;
  PopStack();
end;

procedure TCodeParser.SetType;
var
  Decl: TDeclaration;
begin
  if (InDeclaration(TCPTypeKind, Decl)) then
    TCPTypeKind(Decl).Typ := tkSet;

  PushStack(TCPSet);
  TCPTypeKind(Decl).TypeItem := FStack.Top;
  inherited SetType;
  PopStack();
end;

procedure TCodeParser.TypeId;
begin
  PushStack(TDeclTypeID);
  inherited TypeId;
  PopStack();
end;

procedure TCodeParser.EnumeratedType;
var
  Decl: TDeclaration;
begin
  if (InDeclaration(TCPTypeKind, Decl, True)) then
    TCPTypeKind(Decl).Typ := tkEnum;

  PushStack(TCPEnum);
  TCPTypeKind(Decl).TypeItem := FStack.Top;
  inherited EnumeratedType;
  PopStack();
end;

procedure TCodeParser.EnumeratedTypeItem;
begin
  PushStack(TDeclEnumItem);
  inherited EnumeratedTypeItem;
  PopStack();
end;

procedure TCodeParser.FormalParameterList;
begin
  PushStack(TDeclParameterList);
  inherited FormalParameterList;
  PopStack();
end;

procedure TCodeParser.FormalParameterSection;
begin
  PushStack(TDeclParameterSection);
  inherited FormalParameterSection;
  PopStack();
end;

procedure TCodeParser.Block;
begin
  inherited Block;

  if (InDeclaration(TDeclMethod)) then
    TDeclMethod(FStack.Top)._EndPos := Lexer.TokenPos;
end;

procedure TCodeParser.ArrayType;
var
  Decl: TDeclaration;
begin
  if (InDeclaration(TCPTypeKind, Decl)) then
    TCPTypeKind(Decl).Typ := tkArray;

  PushStack(TCPArray);
  TCPTypeKind(Decl).TypeItem := FStack.Top;
  inherited ArrayType;
  PopStack();
end;

procedure TCodeParser.ParameterName;
begin
  PushStack(TDeclParameterName);
  inherited ParameterName;
  PopStack();
end;

procedure TCodeParser.ParameterNameList;
begin
  PushStack(TDeclParameterNameList);
  inherited ParameterNameList;
  PopStack();
end;

procedure TCodeParser.OldFormalParameterType;
begin
  TypeKind;
end;

procedure TCodeParser.NewFormalParameterType;
begin
  TypeKind;
end;

procedure TCodeParser.FieldNameList;
begin
  PushStack(TDeclFieldNameList);
  inherited FieldNameList;
  PopStack();
end;

end.
