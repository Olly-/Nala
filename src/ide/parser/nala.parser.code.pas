unit nala.Parser.Code;

{$mode objfpc}{$H+}
{$modeswitch typehelpers}

interface

uses
  SysUtils, Classes, nala.Types, fgl,
  CastaliaPasLex, CastaliaSimplePasPar, CastaliaPasLexTypes;

type
  TDeclaration = class;
  TDeclarationClass = class of TDeclaration;
  TDeclarationArray = array of TDeclaration;

  TDeclarationArray_Helper = type Helper for TDeclarationArray
  public
    procedure Append(Declaration: TDeclaration); inline;
  end;

  TDeclarationStack = class
  private
    FItems: TList;
    FTop: TDeclaration;
  public
    procedure Reset;
    procedure Push(Item: TDeclaration);
    function Pop: TDeclaration;

    property Top: TDeclaration read FTop write FTop;

    constructor Create;
    destructor Destroy; override;
  end;

  TDeclarationList = class(specialize TFPGObjectList<TDeclaration>)
  public
    function GetAll(AClass: TDeclarationClass; DeepSearch: Boolean = False): TDeclarationArray;
    function Get(AClass: TDeclarationClass; DeepSearch: Boolean = False): TDeclaration;

    function GetAsString(AClass: TDeclarationClass): TStringArray;
  end;

  TDeclaration = class
  private
    FParser: TmwSimplePasPar;
    FOwner: TDeclaration;
    FOrigin: PAnsiChar;

    FStartPos: Int32;
    FEndPos: Int32;

    FItems: TDeclarationList;

    FRawText: String;
    FCleanText: String;

    function GetRawText: String; inline;
    function GetCleanText: String; inline;
  public
    property Parser: TmwSimplePasPar read FParser;
    property Owner: TDeclaration read FOwner;
    property Origin: PAnsiChar read FOrigin;

    property RawText: string read GetRawText write FRawText;
    property CleanText: string read GetCleanText write FCleanText;
    property StartPos: Int32 read FStartPos write FStartPos;
    property EndPos: Int32 read FEndPos write FEndPos;
    property Items: TDeclarationList read FItems;

    constructor Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Int32; AEnd: Int32 = -1); virtual;
    destructor Destroy; override;
  end;

  TCPTypeKind = class; // Forward

  TCPDocumentation = class(TDeclaration)
  private
    FDescription: Variant;

    function GetDescription: String;
  public
    property Description: String read GetDescription;
  end;

  TMethodDirective = (mdOverload, mdOverride, mdStatic);
  TMethodDirectives = set of TMethodDirective;

  TParameter = record
    Name, Typ, Default: String;
    Kind: TCPTypeKind;

    First: Boolean; // First item in section?
    Last: Boolean;  // Last item in section?
  end;
  TParameterArray = array of TParameter;

  TCPMethod = class(TDeclaration)
  private
    FObjectType: Variant;
    FReturnType: Variant;
    FDirectives: Variant;
    FDepth: Variant;
    FName: Variant;

    FParameterCount: Variant;
    FParameters: TParameterArray;

    FParams: Variant;
    FHeader: Variant;

    FDocumentation: TCPDocumentation;

    function GetDepth: UInt32;
    function GetDirectives: TMethodDirectives;
    function GetHeader: String;

    function GetName: String;
    function GetObjectType: String;
    function GetParams: String;
    function GetReturnKind: TCPTypeKind;
    function GetReturnType: String;

    function GetParameter(Index: UInt32): TParameter;
    function GetParameterCount: UInt32;
  public
    property Name: String read GetName;
    property ObjectType: String read GetObjectType;
    property ReturnType: String read GetReturnType;
    property ReturnKind: TCPTypeKind read GetReturnKind;

    property Directives: TMethodDirectives read GetDirectives;
    property Depth: UInt32 read GetDepth;

    property Header: String read GetHeader;
    property Params: String read GetParams;

    property ParameterCount: UInt32 read GetParameterCount;
    property Parameter[Index: UInt32]: TParameter read GetParameter;

    property Documentation: TCPDocumentation read FDocumentation write FDocumentation;

    function isMethodOfObject: Boolean;

    function Variables: TDeclarationArray;
    function Constants: TDeclarationArray;
    function Types: TDeclarationArray;
  end;

  TCPMethodArray = array of TCPMethod;

  (*
    Some special handling here:

    "var a, b, c: Integer" = VariableDeclaration

    We split that up into three different declarations
  *)

  TCPVariable = class(TDeclaration)
  public
  type
    TOwnerData = record
      Names: TStringArray;
      Count: Integer;
      Typ: Variant;
      Default: Variant;
    end;
  private
    FOwnerVariable: TCPVariable;
    FOwnerData: TOwnerData;
    FIndex: Integer;

    function GetName: String;
    function GetDefault: String;
    function GetKind: TCPTypeKind;
    function GetTyp: String;
    function _GetName(Index: UInt32): String;
  public
    property OwnerVariable: TCPVariable write FOwnerVariable;
    property OwnerData: TOwnerData read FOwnerData write FOwnerData;
    property Default: String read GetDefault;
    property Typ: String read GetTyp;
    property Kind: TCPTypeKind read GetKind;
    property Name: String read GetName;

    constructor Create(AOwnerVariable: TCPVariable; AIndex: Integer); overload;
  end;

  TCPConstant = class(TDeclaration)
  private
    FTyp: Variant;
    FDefault: Variant;
    FName: Variant;

    function GetName: String;
    function GetDefault: String;
    function GetKind: TCPTypeKind;
    function GetTyp: String;
  public
    property Typ: String read GetTyp;
    property Default: String read GetDefault;
    property Kind: TCPTypeKind read GetKind;
    property Name: String read GetName;
  end;

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

  TCPSet = class(TDeclaration)
  private
    FSetType: Variant;

    function GetSetType: String;
  public
    property SetType: String read GetSetType;
  end;

  TCPTypeCopy = class(TDeclaration)
  private
    function GetCopyType: String;
  public
    property CopyType: String read GetCopyType;
  end;

  TCPTypeAlias = class(TDeclaration)
  private
    function GetAliasType: String;
  public
    property AliasType: String read GetAliasType;
  end;

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

  TCPType = class(TDeclaration)
  private
    FKind: TCPTypeKind;
    FName: Variant;

    function GetName: String;
  public
    property Kind: TCPTypeKind read FKind write FKind;
    property Name: String read GetName;

    function Format(PrefixName: Boolean = True): String;
  end;

  TCPInclude = class(TDeclaration)
  public
    Declaration: TObject;

    property FilePath: String read GetRawText write FRawText;
  end;

  TCPOnInclude = procedure(Sender: TObject; Declaration: TCPInclude) of object;

  TCodeParser = class(TmwSimplePasPar)
  private
    FFilePath: String;

    FCaretPos: Int32;
    FMethodAtCaret: TCPMethod;

    FStream: TMemoryStream;

    FStack: TDeclarationStack;
    FItems: TDeclarationList;

    FOnInclude: TCPOnInclude;

    procedure WriteMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);

    procedure PushStack(AClass: TDeclarationClass; AStart: Integer = -1); inline;
    procedure PopStack(AEnd: Integer = -1); inline;

    function InDeclaration(AClass: TDeclarationClass): Boolean;
    function InDeclaration(AClass: TDeclarationClass; out Decl: TDeclaration; DeepSearch: Boolean = False): Boolean; overload;
  protected
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

    procedure HandleIncludeDirect(Sender: TmwBasePasLex);
    procedure HandlePtCompDirect(Sender: TmwBasePasLex); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(From: TObject); override;
    procedure Error(AMessage: String);

    function Run(constref Code: String; MaxPos: Int32 = -1; CaretPos: Int32 = -1): Boolean; overload;
    function Run(MaxPos: Int32 = -1; CaretPos: Int32 = -1): Boolean; overload;

    property FilePath: String read FFilePath write FFilePath;
    property Items: TDeclarationList read FItems;
    property MethodAtCaret: TCPMethod read FMethodAtCaret;
    property OnInclude: TCPOnInclude read FOnInclude write FOnInclude;
  end;

implementation

uses
  Variants, strutils;

type
  // Types
  TCPTypeName = class(TDeclaration);
  TCPAncestor = class(TDeclaration);
  TCPFieldName = class(TDeclaration);
  TCPFieldNameList = class(TDeclaration);
  TCPTypeID = class(TDeclaration);
  TCPEnumItem = class(TDeclaration);
  TCPConstantExpression = class(TDeclaration);
  TCPExplicitType = class(TDeclaration);

  // Method
  TCPMethodName = class(TDeclaration);
  TCPMethodObjectName = class(TDeclaration);
  TCPMethodReturnType = class(TDeclaration);
  TCPMethodDirective = class(TDeclaration);

  // Parameter
  TCPParameterList = class(TDeclaration);
  TCPParameterSection = class(TDeclaration);
  TCPParameterName = class(TDeclaration);
  TCPParameterNameList = class(TDeclaration);

  // Variable
  TCPVarNameList = class(TDeclaration);
  TCPVarName = class(TDeclaration);

  // Constant
  TCPConstantName = class(TDeclaration);
  TCPConstantType = class(TDeclaration);
  TCPConstantValue = class(TDeclaration);
  TCPConstantValueTyped = class(TDeclaration);

  // Misc
  TCPJunk = class(TDeclaration);

  TVariantHelper = type Helper for Variant
    function Assigned: Boolean;
  end;

procedure TDeclarationArray_Helper.Append(Declaration: TDeclaration);
var
  Len: Int32;
begin
  Len := Length(Self);
  SetLength(Self, Len + 1);
  Self[Len] := Declaration;
end;

function TCPDocumentation.GetDescription: String;
var
  P, Offset, Trimmed: Int32;
  Trimming: Boolean;
  Text: String;
begin
  if (not FDescription.Assigned) then
  begin
    FDescription := '';

    Text := Copy(RawText, 1, Length(RawText) - 1);
    P := Pos('@desc:', Text);
    if (P > 0) then
    begin
      Inc(P, 6);
      Offset := 0;
      Trimming := False;

      // Calulate first line offset
      for P := P to Length(Text) do
        case Text[P] of
          #32: Inc(Offset);
          #10, #13: Offset := 0;
        else
          Break;
        end;

      for P := P to Length(Text) do
        if (Text[P] = #10) or (Text[P] = #13) then
        begin
          FDescription += Text[P];

          Trimming := True;
          Trimmed := 0;
        end else
        if (Trimming) then
        begin
          Inc(Trimmed);
          if (Trimmed = Offset) then
            Trimming := False;
        end else
          FDescription += Text[P];
    end;

    FDescription := TrimRight(FDescription);
  end;

  Result := FDescription;
end;

function TVariantHelper.Assigned: Boolean;
begin
  Result := TVarData(Self).VType <> varEmpty;
end;

function TCPTypeAlias.GetAliasType: String;
begin
  Result := FOwner.CleanText;
end;

function TCPTypeCopy.GetCopyType: String;
begin
  Result := FOwner.CleanText;
end;

function TCPSet.GetSetType: String;
var
  Decl: TDeclaration;
begin
  if (not FSetType.Assigned) then
  begin
    FSetType := '';

    Decl := FItems.Get(TCPTypeID);
    if (Decl <> nil) then
      FSetType := Decl.CleanText;
  end;

  Result := FSetType;
end;

function TCPArray.GetDimensions: UInt32;
begin
  if (not FDimensions.Assigned) then
    FDimensions := Length(FItems.GetAll(TCPArray, True)) + 1;

  Result := FDimensions;
end;

function TCPArray.GetKind: TCPTypeKind;
var
  Decls: TDeclarationArray;
begin
  Result := nil;

  Decls := FItems.GetAll(TCPTypeKind, True);
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

    Decls := FItems.GetAll(TCPEnumItem, True);
    if (Length(Decls) > 0) then
    begin
      FEnumCount := Length(Decls);
      SetLength(FEnums, FEnumCount);

      for i := 0 to FEnumCount - 1 do
      begin
        FEnums[i].Name := Decls[i].CleanText;

        Decl := Decls[i].Items.Get(TCPConstantExpression);
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

function TCPRecord.GetAncestor: String;
var
  Decl: TDeclaration;
begin
  if (not FAncestor.Assigned) then
  begin
    FAncestor := '';

    Decl := FItems.Get(TCPAncestor);
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
  Names: TStringArray;
begin
  if (not FFieldCount.Assigned) then
  begin
    FFieldCount := 0;
    SetLength(FFields, 0);

    for i := 0 to FItems.Count - 1 do
      if (FItems[i].ClassType = TCPFieldNameList) then
        Names := FItems[i].Items.GetAsString(TCPFieldName)
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

function TCPType.GetName: String;
var
  Decl: TDeclaration;
begin
  if (not FName.Assigned) then
  begin
    Decl := FItems.Get(TCPTypeName);
    if (Decl <> nil) then
      FName := Decl.CleanText;
  end;

  Result := FName;
end;

function TCPType.Format(PrefixName: Boolean): String;
var
  i: Integer;
begin
  Result := '';
  if (PrefixName) then
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
            Result += BoolToStr(i < (EnumCount - 1), ', ', '');
          end;
       Result += ')';
      end;
  end;
end;

function TCPVariable.GetName: String;
begin
  if (FOwnerVariable <> nil) then
    Result := FOwnerVariable._GetName(FIndex)
  else
    Result := _GetName(0);
end;

function TCPVariable.GetDefault: String;
var
  Decl: TDeclaration;
begin
  if (FOwnerVariable <> nil) then
    Exit(FOwnerVariable.Default);

  if (not FOwnerData.Default.Assigned) then
  begin
    FOwnerData.Default := '';

    Decl := FItems.Get(TCPConstantValueTyped);
    if (Decl <> nil) then
      FOwnerData.Default := Decl.CleanText
    else
    begin
      Decl := FItems.Get(TCPConstantValue);
      if (Decl <> nil) then
        FOwnerData.Default := Decl.CleanText;
    end;
  end;

  Result := FOwnerData.Default;
end;

function TCPVariable.GetKind: TCPTypeKind;
begin
  Result := FItems.Get(TCPTypeKind) as TCPTypeKind;
end;

function TCPVariable._GetName(Index: UInt32): String;
var
  Decl: TDeclaration;
begin
  Result := '';

  if (Length(FOwnerData.Names) <> FOwnerData.Count) then
  begin
    Decl := FItems.Get(TCPVarNameList);
    if (Decl <> nil) then
      FOwnerData.Names := Decl.Items.GetAsString(TCPVarName);
    FOwnerData.Count := Length(FOwnerData.Names);
  end;

  if (Index < FOwnerData.Count) then
    Result := FOwnerData.Names[Index];
end;

function TCPVariable.GetTyp: String;
var
  Decl: TDeclaration;
begin
  if (FOwnerVariable <> nil) then
    Exit(FOwnerVariable.Typ);

  if (not FOwnerData.Typ.Assigned) then
  begin
    FOwnerData.Typ := '';

    Decl := FItems.Get(TCPTypeKind);
    if (Decl <> nil) then
      FOwnerData.Typ := Decl.CleanText;
  end;

  Result := FOwnerData.Typ;
end;

constructor TCPVariable.Create(AOwnerVariable: TCPVariable; AIndex: Integer);
begin
  inherited Create(AOwnerVariable.Parser, AOwnerVariable.Owner, AOwnerVariable.Origin,
                   AOwnerVariable.StartPos, AOwnerVariable.EndPos);

  FOwnerVariable := AOwnerVariable;
  FIndex := AIndex;
end;

function TCPConstant.GetName: String;
var
  Decl: TDeclaration;
begin
  if (not FName.Assigned) then
  begin
    FName := '';

    Decl := FItems.Get(TCPConstantName);
    if (Decl <> nil) then
      FName := Decl.CleanText;
  end;

  Result := FName;
end;

function TCPConstant.GetDefault: String;
var
  Decl: TDeclaration;
begin
  if (not FDefault.Assigned) then
  begin
    FDefault := '';

    Decl := FItems.Get(TCPConstantValueTyped);
    if (Decl <> nil) then
      FDefault := Decl.CleanText
    else
    begin
      Decl := FItems.Get(TCPConstantValue);
      if (Decl <> nil) then
        FDefault := Decl.CleanText;
    end;
  end;

  Result := FDefault;
end;

function TCPConstant.GetKind: TCPTypeKind;
begin
  Result := FItems.Get(TCPConstantType) as TCPTypeKind;
end;

function TCPConstant.GetTyp: String;
var
  Decl: TDeclaration;
begin
  if (not FTyp.Assigned) then
  begin
    FTyp := '';

    Decl := FItems.Get(TCPConstantType);
    if (Decl <> nil) then
      FTyp := Decl.CleanText;
  end;

  Result := FTyp;
end;

function TCPMethod.GetName: String;
var
  Decl: TDeclaration;
begin
  if (not FName.Assigned) then
  begin
    FName := '';

    Decl := FItems.Get(TCPMethodName);
    if (Decl <> nil) then
      FName := Decl.CleanText;
  end;

  Result := FName;
end;

function TCPMethod.GetDepth: UInt32;
var
  d: TDeclaration;
begin
  if (not FDepth.Assigned) then
  begin
    FDepth := 0;

    d := Self;
    while (d <> nil) and (d.Owner <> nil) and (d.Owner.ClassType = TCPMethod) do
    begin
      FDepth := FDepth + 1;
      d := d.Owner;
    end;
  end;

  Result := FDepth;
end;

function TCPMethod.GetDirectives: TMethodDirectives;
var
  Decls: TDeclarationArray;
  i: Integer;
  _Directives: TMethodDirectives;
begin
  if (not FDirectives.Assigned) then
  begin
    _Directives := [];

    Decls := FItems.GetAll(TCPMethodDirective);
    for i := 0 to High(Decls) do
      case Lowercase(Decls[i].RawText) of
        'overload': Include(_Directives, mdOverload);
        'override': Include(_Directives, mdOverride);
        'static': Include(_Directives, mdStatic);
      end;

    FDirectives := Integer(_Directives);
  end;

  Result := TMethodDirectives(Integer(FDirectives));
end;

function TCPMethod.isMethodOfObject: Boolean;
begin
  Result := GetObjectType <> '';
end;

function TCPMethod.Variables: TDeclarationArray;
begin
  Result := FItems.GetAll(TCPVariable);
end;

function TCPMethod.Constants: TDeclarationArray;
begin
  Result := FItems.GetAll(TCPConstant);
end;

function TCPMethod.Types: TDeclarationArray;
begin
  Result := FItems.GetAll(TCPType);
end;

function TCPMethod.GetHeader: String;
begin
  if (not FHeader.Assigned) then
  begin
    if (GetReturnType <> '') then
      FHeader := 'function '
    else
      FHeader := 'procedure ';

    if (GetObjectType <> '') then
      FHeader += GetObjectType + '.';
    FHeader += GetName;
    FHeader += GetParams;
    if (GetReturnType <> '') then
      FHeader += ': ' + GetReturnType;
  end;

  Result := FHeader;
end;

function TCPMethod.GetObjectType: String;
var
  Decl: TDeclaration;
begin
  if (not FObjectType.Assigned) then
  begin
    FObjectType := '';

    Decl := FItems.Get(TCPMethodObjectName);
    if (Decl <> nil) then
      FObjectType := Decl.CleanText;
  end;

  Result := FObjectType;
end;

function TCPMethod.GetParams: String;
var
  Decl: TDeclaration;
begin
  if (not FParams.Assigned) then
  begin
    FParams := '';

    if (GetParameterCount > 0) then
    begin
      Decl := FItems.Get(TCPParameterList);
      if (Decl <> nil) then
        FParams := Decl.CleanText;
    end;
  end;

  Result := FParams;
end;

function TCPMethod.GetReturnKind: TCPTypeKind;
var
  Decl: TDeclaration;
begin
  Result := nil;
  Decl := FItems.Get(TCPMethodReturnType);
  if (Decl <> nil) then
    Result := Decl.Items.Get(TCPTypeKind) as TCPTypeKind;
end;

function TCPMethod.GetParameter(Index: UInt32): TParameter;
begin
  Result := FParameters[Index];
end;

function TCPMethod.GetParameterCount: UInt32;

  procedure AddParameters(Item: TCPParameterSection);
  var
    Decl, Kind, Def: TDeclaration;
    Names: TStringArray;
    i: Integer;
  begin
    Decl := Item.Items.Get(TCPParameterNameList);
    if (Decl <> nil) then
    begin
      Names := Decl.Items.GetAsString(TCPParameterName);
      Kind := Item.Items.Get(TCPTypeKind);
      Def := Item.Items.Get(TCPConstantExpression);

      for i := 0 to High(Names) do
      begin
        SetLength(FParameters, FParameterCount + 1);

        FParameters[FParameterCount] := Default(TParameter);
        FParameters[FParameterCount].Name := Names[i];
        FParameters[FParameterCount].Kind := TCPTypeKind(Kind);
        FParameters[FParameterCount].First := (i = 0);
        FParameters[FParameterCount].Last := (i = High(Names));
        if (Kind <> nil) then
          FParameters[FParameterCount].Typ := Kind.CleanText;
        if (Def <> nil) then
          FParameters[FParameterCount].Default := Def.CleanText;

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

    Decls := FItems.GetAll(TCPParameterSection, True);
    for i := 0 to High(Decls) do
      AddParameters(TCPParameterSection(Decls[i]));
  end;

  Result := FParameterCount;
end;


function TCPMethod.GetReturnType: String;
var
  Decl: TDeclaration;
begin
  if (not FReturnType.Assigned) then
  begin
    FReturnType := '';

    Decl := FItems.Get(TCPMethodReturnType);
    if (Decl <> nil) then
      FReturnType := Decl.CleanText;
  end;

  Result := FReturnType;
end;

procedure TDeclarationStack.Reset;
begin
  FItems.Clear;
  FTop := nil;
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

  inherited Destroy;
end;

function TDeclarationList.GetAll(AClass: TDeclarationClass; DeepSearch: Boolean): TDeclarationArray;

  procedure Search(Decl: TDeclaration; var Decls: TDeclarationArray);
  var
    i: Int32;
  begin
    if (Decl.ClassType = AClass) then
      Decls.Append(Decl);

    if (DeepSearch) then
      for i := 0 to Decl.Items.Count - 1 do
        Search(Decl.Items[i], Decls);
  end;

var
  i: Int32;
begin
  SetLength(Result, 0);

  for i := 0 to Count - 1 do
    Search(Items[i], Result);
end;

function TDeclarationList.Get(AClass: TDeclarationClass; DeepSearch: Boolean): TDeclaration;

  function Search(Decl: TDeclaration; var FoundDeclaration: TDeclaration): Boolean;
  var
    i: Int32;
  begin
    if (Decl.ClassType = AClass) then
    begin
      FoundDeclaration := Decl;
      Exit(True);
    end;

    if (DeepSearch) then
      for i := 0 to Decl.Items.Count - 1 do
        if (Search(Decl.Items[i], FoundDeclaration)) then
          Exit(True);

    Result := False;
  end;

var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
    if (Search(Items[i], Result)) then
      Exit;
end;

function TDeclaration.GetRawText: String;
begin
  if (FRawText <> '') then
    Exit(FRawText);
  if (FStartPos <> FEndPos) and (FOrigin <> nil) then
    SetString(FRawText, FOrigin + FStartPos, FEndPos - FStartPos);

  Result := FRawText;
end;

function TDeclaration.GetCleanText: String;
var
  i: Integer;
  Decls: TDeclarationArray;
begin
  if (FCleanText = '') then
  begin
    FCleanText := RawText;

    Decls := Items.GetAll(TCPJunk, True);
    if (Length(Decls) > 0) then
      for i := High(Decls) downto 0 do
        Delete(FCleanText, Decls[i].StartPos - FStartPos + 1, Decls[i].EndPos - Decls[i].StartPos);

    FCleanText := TrimRight(StringReplace(StringReplace(FCleanText, '  ', '', [rfReplaceAll]), LineEnding, '', [rfReplaceAll])); // Magical
  end;

  Result := FCleanText;
end;

function TDeclarationList.GetAsString(AClass: TDeclarationClass): TStringArray;
var
  Decls: TDeclarationArray;
  i: Int32;
begin
  Decls := GetAll(AClass);
  SetLength(Result, Length(Decls));
  for i := 0 to High(Decls) do
    Result[i] := Decls[i].CleanText;
end;

constructor TDeclaration.Create(AParser: TmwSimplePasPar; AOwner: TDeclaration; AOrigin: PAnsiChar; AStart: Int32; AEnd: Int32);
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

procedure TCodeParser.WriteMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: string; X, Y: Integer);
var
  Message: String;
begin
  Message := Format('Warning: %s at line %d', [Msg, Y + 1]);
  if (TCodeParser(Sender).FilePath <> '') then
    Message += Format(' in file "%s"', [ExtractFileName(TCodeParser(Sender).FilePath)]);

  WriteLn(Message);
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
    FStack.Top.Items.Add(Decl)
  else
    FItems.Add(Decl);
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

{!DOCREF} {
  @method: procedure TCodeParser.NextToken;
  @desc:
    To the next token we go!
}
procedure TCodeParser.NextToken;
begin
  Lexer.Next;

  if (Lexer.IsJunk) then
  begin
    repeat
      if (Lexer.TokenID in [tokAnsiComment, tokBorComment, tokSlashesComment]) then
      begin
        // Documentation
        if (Lexer.TokenID = tokBorComment) and (Lexer.TokenLen = 9) and (Lexer.Token = '{!DOCREF}') and (not InDeclaration(TCPDocumentation)) then
        begin
          if (InDeclaration(TCPJunk)) then
            PopStack;

          Lexer.NextID(tokBorComment);
          if (Lexer.TokenID = tokBorComment) then
          begin
            PushStack(TCPDocumentation);
            PopStack(Lexer.TokenPos + Lexer.TokenLen);
          end;
        end else
        // Junk
        begin
          if (FItems.Count > 0) and (FItems.Last.ClassType = TCPJunk) then
            FItems.Last.EndPos := Lexer.TokenPos + Lexer.TokenLen
          else
          if (not InDeclaration(TCPJunk)) then
            PushStack(TCPJunk);
        end;
      end else
      if (InDeclaration(TCPJunk)) then
        PopStack;

      Lexer.Next;
    until (not Lexer.IsJunk);

    if (InDeclaration(TCPJunk)) then
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
  inherited Create;

  FOnInclude := nil;

  FStream := TMemoryStream.Create;
  FStack := TDeclarationStack.Create;
  FItems := TDeclarationList.Create;
  FFilePath := '';

  Lexer.OnIncludeDirect := @HandleIncludeDirect;

  OnMessage := @WriteMessage;
end;

destructor TCodeParser.Destroy;
begin
  FStream.Free;
  FStack.Free;
  FItems.Free;

  inherited Destroy;
end;

procedure TCodeParser.Assign(From: TObject);
begin
  inherited Assign(From);

  FOnInclude := TCodeParser(From).OnInclude;
end;

procedure TCodeParser.Error(AMessage: String);
begin
  OnMessage(Self, meError, AMessage, Lexer.PosXY.X, Lexer.PosXY.Y);
end;

function TCodeParser.Run(constref Code: String; MaxPos: Int32; CaretPos: Int32): Boolean;
begin
  Result := True;

  if (Code <> '') then
  begin
    FCaretPos := CaretPos;
    FMethodAtCaret := nil;

    FStream.Clear;
    FStream.Write(Code[1], Length(Code));

    FItems.Clear;
    FStack.Reset;

    if (MaxPos = 0) then
      Exit;

    try
      Run(FStream, MaxPos);
    except
      on e: ESyntaxError do
        { nothing };

      on e: Exception do
      begin
        if (OnMessage <> nil) then
          OnMessage(Self, meError, e.ClassName + ': ' + e.Message, -1, -1);

        Result := False;
      end;
    end;
  end;
end;

function TCodeParser.Run(MaxPos: Int32; CaretPos: Int32): Boolean;
var
  Text: String;
begin
  if (not FileExists(FilePath)) then
    Exit(False);

  with TFileStream.Create(FilePath, fmOpenRead) do
  try
    SetLength(Text, Size);
    Read(Text[1], Size);
  finally
    Free;
  end;

  Result := Run(Text, MaxPos, CaretPos);
end;

procedure TCodeParser.ProcedureDeclarationSection;
begin
  PushStack(TCPMethod);
  inherited ProcedureDeclarationSection;
  // PopStack in TCodeParser.Block
end;

procedure TCodeParser.VarDeclaration;
var
  Decl: TCPVariable;
  i: Integer;
begin
  PushStack(TCPVariable);

  Decl := FStack.Top as TCPVariable;
  Decl.OwnerVariable := nil;
  Decl.OwnerData := Default(TCPVariable.TOwnerData);

  inherited VarDeclaration;
  PopStack();

  for i := 1 to Decl.OwnerData.Count - 1 do
    if (Decl.Owner <> nil) then
      Decl.Owner.Items.Add(TCPVariable.Create(Decl, i))
    else
      FItems.Add(TCPVariable.Create(Decl, i));
end;

procedure TCodeParser.VarName;
var
  Decl: TDeclaration;
begin
  if (InDeclaration(TCPVariable, Decl, True)) then
    with TCPVariable(Decl).OwnerData do
      Count := Count + 1;

  PushStack(TCPVarName);
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
  PushStack(TCPMethodName);
  inherited FunctionProcedureName;
  PopStack();
end;

procedure TCodeParser.ObjectNameOfMethod;
begin
  PushStack(TCPMethodObjectName);
  inherited ObjectNameOfMethod;
  PopStack();
end;

procedure TCodeParser.ReturnType;
begin
  PushStack(TCPMethodReturnType);
  inherited ReturnType;
  PopStack();
end;

procedure TCodeParser.VarNameList;
begin
  PushStack(TCPVarNameList);
  inherited VarNameList;
  PopStack();
end;

procedure TCodeParser.ConstantValueTyped;
begin
  PushStack(TCPConstantValueTyped);
  inherited ConstantValueTyped;
  PopStack();
end;

procedure TCodeParser.ProceduralDirective;
begin
  PushStack(TCPMethodDirective);
  inherited ProceduralDirective;
  PopStack();
end;

procedure TCodeParser.ConstantType;
begin
  PushStack(TCPConstantType);
  inherited ConstantType;
  PopStack();
end;

procedure TCodeParser.ConstantName;
begin
  PushStack(TCPConstantName);
  inherited ConstantName;
  PopStack();
end;

procedure TCodeParser.ConstantDeclaration;
begin
  PushStack(TCPConstant);
  inherited ConstantDeclaration;
  PopStack();
end;

procedure TCodeParser.ConstantValue;
begin
  PushStack(TCPConstantValue);
  inherited ConstantValue;
  PopStack();
end;

procedure TCodeParser.ConstantExpression;
begin
  PushStack(TCPConstantExpression);
  inherited ConstantExpression;
  PopStack();
end;

procedure TCodeParser.TypeDeclaration;
var
  Decl: TCPType;
begin
  PushStack(TCPType);
  Decl := FStack.Top as TCPType;

  inherited TypeDeclaration;

  Decl.Kind := Decl.Items.Get(TCPTypeKind) as TCPTypeKind;
  if (Decl.Kind = nil) then
    raise Exception.Create('TCodeParser.TypeDeclaration: Decl.Kind = nil');

  if (Decl.Kind.Typ = tkType) then
  begin
    if (Decl.Items.Get(TCPExplicitType) <> nil) then
    begin
      Decl.Kind.Typ := tkCopy;
      Decl.Kind.TypeItem := TCPTypeCopy.Create(Decl.Kind.Parser, Decl.Kind, Decl.Kind.Origin, Decl.Kind.StartPos, Decl.Kind.EndPos);
      Decl.Kind.Items.Add(Decl.Kind.TypeItem);
    end else
    begin
      Decl.Kind.Typ := tkAlias;
      Decl.Kind.TypeItem := TCPTypeAlias.Create(Decl.Kind.Parser, Decl.Kind, Decl.Kind.Origin, Decl.Kind.StartPos, Decl.Kind.EndPos);
      Decl.Kind.Items.Add(Decl.Kind.TypeItem);
    end;
  end;

  PopStack();
end;

procedure TCodeParser.TypeName;
begin
  PushStack(TCPTypeName);
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
  PushStack(TCPAncestor);
  inherited AncestorId;
  PopStack();
end;

procedure TCodeParser.FieldName;
begin
  PushStack(TCPFieldName);
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
  PushStack(TCPTypeID);
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
  PushStack(TCPEnumItem);
  inherited EnumeratedTypeItem;
  PopStack();
end;

procedure TCodeParser.FormalParameterList;
begin
  PushStack(TCPParameterList);
  inherited FormalParameterList;
  PopStack();
end;

procedure TCodeParser.FormalParameterSection;
begin
  PushStack(TCPParameterSection);
  inherited FormalParameterSection;
  PopStack();
end;

procedure TCodeParser.Block;
var
  Method: TCPMethod;
begin
  inherited Block;

  Method := FStack.Top as TCPMethod;
  if (Method <> nil) then
  begin
    PopStack();

    if (FItems.Count > 1) and (FItems[FItems.Count - 2].ClassType = TCPDocumentation) then
      Method.Documentation := TCPDocumentation(FItems[FItems.Count - 2]);

    if (FCaretPos > -1) and (FMethodAtCaret = nil) and (FCaretPos >= Method.StartPos) and (FCaretPos < Method.EndPos) then
      FMethodAtCaret := Method;
  end;
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
  PushStack(TCPParameterName);
  inherited ParameterName;
  PopStack();
end;

procedure TCodeParser.ParameterNameList;
begin
  PushStack(TCPParameterNameList);
  inherited ParameterNameList;
  PopStack();
end;

procedure TCodeParser.OldFormalParameterType;
begin
  TypeKind();
end;

procedure TCodeParser.NewFormalParameterType;
begin
  TypeKind();
end;

procedure TCodeParser.FieldNameList;
begin
  PushStack(TCPFieldNameList);
  inherited FieldNameList;
  PopStack();
end;

procedure TCodeParser.HandleIncludeDirect(Sender: TmwBasePasLex);
var
  Include: TCPInclude;
begin
  if (not Sender.IsJunk) then
  begin
    PushStack(TCPInclude, Sender.TokenPos);
    Include := TCPInclude(FStack.Top);
    Include.RawText := SetDirSeparators(Sender.DirectiveParamOriginal);
    PopStack(Sender.TokenPos + Sender.TokenLen);

    if (@OnInclude <> nil) then
      OnInclude(Self, Include);
  end;

  Sender.Next;
end;

procedure TCodeParser.HandlePtCompDirect(Sender: TmwBasePasLex);
begin
  Sender.Next();
end;

end.
