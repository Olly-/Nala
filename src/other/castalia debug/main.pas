unit main;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, CastaliaSimplePasPar, CastaliaPasLexTypes;

type

  { TTestParser }

  TTestParser = class(TmwSimplePasPar)
  public
    procedure AccessSpecifier; override;
    procedure AdditiveOperator; override;
    procedure AncestorIdList; override; // !! Added ancestorIdList back in...
    procedure AncestorId; override; // !! Added ancestorId back in...
    procedure AnonymousMethod; override;
    procedure AnonymousMethodType; override;
    procedure ArrayConstant; override;
    procedure ArrayType; override;
    procedure AsmStatement; override;
    procedure Block; override;
    procedure CaseLabel; override;
    procedure CaseSelector; override;
    procedure CaseStatement; override;
    procedure CharString; override;
    procedure ClassField; override;
    procedure ClassForward; override;
    procedure ClassFunctionHeading; override;
    procedure ClassHeritage; override;
    procedure ClassMemberList; override;
    procedure ClassMethodDirective; override;
    procedure ClassMethodHeading; override;
    procedure ClassMethodOrProperty; override;
    procedure ClassMethodResolution; override;
    procedure ClassProcedureHeading; override;
    procedure ClassClass; override;
    procedure ClassProperty; override;
    procedure ClassReferenceType; override;
    procedure ClassType; override;
    procedure ClassTypeEnd; override; // DR 2001-07-31
    procedure ClassVisibility; override;
    procedure CompoundStatement; override;
    procedure ConstantColon; override;
    procedure ConstantDeclaration; override;
    procedure ConstantEqual; override;
    procedure ConstantExpression; override;
    procedure ConstantName; override;
//JR added constant type
    procedure ConstantType; override;
    procedure ConstantValue; override;
    procedure ConstantValueTyped; override;
    procedure ConstParameter; override;
    procedure ConstructorHeading; override;
    procedure ConstructorName; override;
    procedure ConstSection; override;
    procedure ContainsClause; override;
    procedure ContainsExpression; override;
    procedure ContainsIdentifier; override;
    procedure ContainsStatement; override;
    {$IFDEF D8_NEWER}
    procedure CustomAttribute; override; //JThurman 2004-03-03
    {$ENDIF}
    procedure DeclarationSection; override;
    procedure Designator; override;
    procedure DestructorHeading; override;
    procedure DestructorName; override;
    procedure Directive16Bit; override;
    procedure DirectiveBinding; override;
    procedure DirectiveCalling; override;
    procedure DirectiveDeprecated; override; // DR 2001-10-20
    procedure DirectiveLibrary; override; // DR 2001-10-20
    procedure DirectiveLocal; override; // DR 2001-11-14
    procedure DirectivePlatform; override; // DR 2001-10-20
    procedure DirectiveVarargs; override; // DR 2001-11-14
    procedure DispInterfaceForward; override;
    procedure DispIDSpecifier; override; // DR 2001-07-26
    procedure EmptyStatement; override;
    procedure EnumeratedType; override;
    procedure EnumeratedTypeItem; override; // DR 2001-10-29
    procedure ExceptBlock; override;
    procedure ExceptionBlockElseBranch; override;
    procedure ExceptionClassTypeIdentifier; override;
    procedure ExceptionHandler; override;
    procedure ExceptionHandlerList; override;
    procedure ExceptionIdentifier; override;
    procedure ExceptionVariable; override;
    procedure ExplicitType; override; // !! changed spelling to "Explicit"
    procedure ExportedHeading; override;
    procedure ExportsClause; override;
    procedure ExportsElement; override;
    procedure Expression; override;
    procedure ExpressionList; override;
    procedure ExternalDirective; override;
    procedure ExternalDirectiveThree; override;
    procedure ExternalDirectiveTwo; override;
    procedure Factor; override;
    procedure FieldDeclaration; override;
    procedure FieldList; override;
    procedure FieldNameList; override;
    procedure FieldName; override;
    procedure FileType; override;
    procedure FormalParameterList; override;
    procedure FormalParameterSection; override;
    procedure ForStatement; override;
    procedure ForwardDeclaration; override; {GLC: corrected spelling}
    procedure FunctionHeading; override;
    procedure FunctionMethodDeclaration; override;
    procedure FunctionMethodName; override;
    procedure FunctionProcedureBlock; override;
    procedure FunctionProcedureName; override;
    procedure Identifier; override;
    procedure IdentifierList; override;
    procedure IfStatement; override;
    procedure ImplementationSection; override;
    procedure IncludeFile; override;
    procedure IndexSpecifier; override; // DR 2001-07-26
    procedure InheritedStatement; override;
    procedure InitializationSection; override;
    procedure InlineStatement; override;
    procedure InParameter; override;
    procedure InterfaceDeclaration; override;
    procedure InterfaceForward; override;
    procedure InterfaceGUID; override;
    procedure InterfaceHeritage; override;
    procedure InterfaceMemberList; override;
    procedure InterfaceSection; override;
    procedure InterfaceType; override;
    procedure LabelDeclarationSection; override;
    procedure LabeledStatement; override;
    procedure LabelId; override;
    procedure LibraryFile; override;
    procedure MainUsedUnitExpression; override;
    procedure MainUsedUnitName; override;
    procedure MainUsedUnitStatement; override;
    procedure MainUsesClause; override;
    procedure MultiplicativeOperator; override;
    procedure NewFormalParameterType; override;
    procedure Number; override;
    procedure ObjectConstructorHeading; override;
    procedure ObjectDestructorHeading; override;
    procedure ObjectField; override;
    procedure ObjectForward; override;
    procedure ObjectFunctionHeading; override;
    procedure ObjectHeritage; override;
    procedure ObjectMemberList; override;
    procedure ObjectMethodDirective; override;
    procedure ObjectMethodHeading; override;
    procedure ObjectNameOfMethod; override;
    procedure ObjectProperty; override;
    procedure ObjectPropertySpecifiers; override;
    procedure ObjectProcedureHeading; override;
    procedure ObjectType; override;
    procedure ObjectTypeEnd; override; // DR 2001-08-07
    procedure ObjectVisibility; override;
    procedure OldFormalParameterType; override;
    procedure OrdinalIdentifier; override;
    procedure OrdinalType; override;
    procedure OutParameter; override;
    procedure PackageFile; override;
    procedure ParameterFormal; override;
    procedure ParameterName; override;
    procedure ParameterNameList; override;
    procedure ParseFile; override;
    procedure PointerType; override;
    procedure ProceduralDirective; override;
    procedure ProceduralType; override;
    procedure ProcedureDeclarationSection; override;
    procedure ProcedureHeading; override;
    procedure ProcedureMethodDeclaration; override;
    procedure ProcedureMethodName; override;
    procedure ProgramBlock; override;
    procedure ProgramFile; override;
    procedure PropertyDefault; override;
    procedure PropertyInterface; override;
    procedure PropertyName; override;
    procedure PropertyParameterConst; override;
    procedure PropertyParameterList; override;
    procedure PropertySpecifiers; override;
    procedure QualifiedIdentifier; override;
    procedure QualifiedIdentifierList; override;
    procedure RaiseStatement; override;
    procedure ReadAccessIdentifier; override;
    procedure RealIdentifier; override;
    procedure RealType; override;
    procedure RecordConstant; override;
    procedure RecordFieldConstant; override;
    procedure RecordType; override;
    procedure RecordVariant; override;
    procedure RelativeOperator; override;
    procedure RepeatStatement; override;
    procedure RequiresClause; override;
    procedure RequiresIdentifier; override;
    procedure ResolutionInterfaceName; override;
    procedure ResourceDeclaration; override;
    procedure ReturnType; override;
    procedure SetConstructor; override;
    procedure SetElement; override;
    procedure SetType; override;
    procedure SimpleExpression; override;
    procedure SimpleStatement; override;
    procedure SimpleType; override;
    procedure SkipAnsiComment; override;
    procedure SkipBorComment; override;
    procedure SkipSlashesComment; override;
    procedure SkipSpace; override; //XM Jul-2000
    procedure SkipCRLFco; override; //XM Jul-2000
    procedure SkipCRLF; override; //XM Jul-2000
    procedure Statement; override;
    procedure StatementList; override;
    procedure StorageExpression; override;
    procedure StorageIdentifier; override;
    procedure StorageDefault; override;
    procedure StorageNoDefault; override;
    procedure StorageSpecifier; override;
    procedure StorageStored; override;
    procedure StringIdentifier; override;
    procedure StringStatement; override;
    procedure StringType; override;
    procedure StructuredType; override;
    procedure SubrangeType; override;
    procedure TagField; override;
    procedure TagFieldName; override;
    procedure TagFieldTypeName; override;
    procedure Term; override;
    procedure TryStatement; override;
    procedure TypedConstant; override;
    procedure TypeDeclaration; override;
    procedure TypeId; override;
    procedure TypeKind; override;
    procedure TypeName; override;
    //generics
    procedure TypeArgs; override;
    procedure TypeParams; override;
    procedure TypeParamDecl; override;
    procedure TypeParamDeclList; override;
    procedure TypeParamList; override;
    procedure ConstraintList; override;
    procedure Constraint; override;
    //end generics
    procedure TypeSection; override;
    procedure UnitFile; override;
    procedure UnitId; override;
    procedure UnitName; override;
    procedure UsedUnitName; override;
    procedure UsedUnitsList; override;
    procedure UsesClause; override;
    procedure VarAbsolute; override;
    procedure VarEqual; override;
    procedure VarDeclaration; override;
    procedure Variable; override;
    procedure VariableList; override;
    procedure VariableReference; override;
    procedure VariableTwo; override;
    procedure VariantIdentifier; override;
    procedure VariantSection; override;
    procedure VarParameter; override;
    procedure VarName; override; //!! Added VarName and VarNameList back in...
    procedure VarNameList; override;
    procedure VarSection; override;
    procedure VisibilityAutomated; override;
    procedure VisibilityPrivate; override;
    procedure VisibilityProtected; override;
    procedure VisibilityPublic; override;
    procedure VisibilityPublished; override;
    procedure VisibilityUnknown; override;
    procedure WhileStatement; override;
    procedure WithStatement; override;
    procedure WriteAccessIdentifier; override;
  end;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    SynEdit1: TSynEdit;

    procedure Button1Click(Sender: TObject);

    procedure DoMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: String; X, Y: Integer);
    procedure SynEdit1Change(Sender: TObject);
  public
    FParser: TTestParser;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  Stream: TMemoryStream;
begin
  Memo1.Lines.Clear;

  Stream := TMemoryStream.Create;
  SynEdit1.Lines.SaveToStream(Stream);

  FParser := TTestParser.Create;
  FParser.OnMessage := @DoMessage;
  FParser.Run(Stream, -1);
  FParser.Free;
  Stream.Free;
end;

procedure TForm1.DoMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: String; X, Y: Integer);
begin
  Memo1.Lines.Append(Msg  + 'at line: ' + IntToStr(Y + 1));
end;

procedure TForm1.SynEdit1Change(Sender: TObject);
begin

end;

var
  _Indent: Integer = 0;

procedure addStart(Str: String);
var
  i: Integer;
  s: String;
begin
  Inc(_Indent);
  for i := 0 to _Indent - 1 do
    s += '--';
  Str := s + ' ' + Str + ': Start [Line: ' + IntToStr(Form1.FParser.Lexer.LineNumber + 1) + ']: [Pos: ' + IntToStr(Form1.FParser.Lexer.TokenPos) + '];';
  Form1.Memo1.Lines.Append(Str);
end;

procedure addEnd(Str: String);
var
  i: Integer;
  s: String;
begin
  for i := 0 to _Indent - 1 do
    s += '--';
  Str := s + ' ' + Str + ': End [Line: ' + IntToStr(Form1.FParser.Lexer.LineNumber + 1) + ']: [Pos: ' + IntToStr(Form1.FParser.Lexer.TokenPos) + '];';
  Form1.Memo1.Lines.Append(Str);
  Dec(_Indent);
end;

{ TTestParser }

procedure TTestParser.AccessSpecifier;
begin
  inherited AccessSpecifier;
end;

procedure TTestParser.AdditiveOperator;
begin
  inherited AdditiveOperator;
end;

procedure TTestParser.AncestorIdList;
begin
  inherited AncestorIdList;
end;

procedure TTestParser.AncestorId;
begin
  addStart('AncestorId');
  inherited AncestorId;
  addEnd('AncestorId');
end;

procedure TTestParser.AnonymousMethod;
begin
  inherited AnonymousMethod;
end;

procedure TTestParser.AnonymousMethodType;
begin
  inherited AnonymousMethodType;
end;

procedure TTestParser.ArrayConstant;
begin
  addStart('ArrayConstant');
  inherited ArrayConstant;
  addEnd('ArrayConstant');
end;

procedure TTestParser.ArrayType;
begin
  addStart('ArrayType');
  inherited ArrayType;
  addEnd('ArrayType');
end;

procedure TTestParser.AsmStatement;
begin
  inherited AsmStatement;
end;

procedure TTestParser.Block;
begin
  addStart('Block');
  inherited Block;
  addEnd('Block');
end;

procedure TTestParser.CaseLabel;
begin
  inherited CaseLabel;
end;

procedure TTestParser.CaseSelector;
begin
  inherited CaseSelector;
end;

procedure TTestParser.CaseStatement;
begin
  inherited CaseStatement;
end;

procedure TTestParser.CharString;
begin
  inherited CharString;
end;

procedure TTestParser.ClassField;
begin
  inherited ClassField;
end;

procedure TTestParser.ClassForward;
begin
  inherited ClassForward;
end;

procedure TTestParser.ClassFunctionHeading;
begin
  inherited ClassFunctionHeading;
end;

procedure TTestParser.ClassHeritage;
begin
  inherited ClassHeritage;
end;

procedure TTestParser.ClassMemberList;
begin
  inherited ClassMemberList;
end;

procedure TTestParser.ClassMethodDirective;
begin
  inherited ClassMethodDirective;
end;

procedure TTestParser.ClassMethodHeading;
begin
  addStart('ClassMethodHeading');
  inherited ClassMethodHeading;
  addEnd('ClassMethodHeading');
end;

procedure TTestParser.ClassMethodOrProperty;
begin
  inherited ClassMethodOrProperty;
end;

procedure TTestParser.ClassMethodResolution;
begin
  inherited ClassMethodResolution;
end;

procedure TTestParser.ClassProcedureHeading;
begin
  addStart('ClassProcedureHeading');
  inherited ClassProcedureHeading;
  addEnd('ClassProcedureHeading');
end;

procedure TTestParser.ClassClass;
begin
  inherited ClassClass;
end;

procedure TTestParser.ClassProperty;
begin
  inherited ClassProperty;
end;

procedure TTestParser.ClassReferenceType;
begin
  inherited ClassReferenceType;
end;

procedure TTestParser.ClassType;
begin
  addStart('ClassType');
  inherited ClassType;
  addEnd('ClassType');
end;

procedure TTestParser.ClassTypeEnd;
begin
  inherited ClassTypeEnd;
end;

procedure TTestParser.ClassVisibility;
begin
  inherited ClassVisibility;
end;

procedure TTestParser.CompoundStatement;
begin
  inherited CompoundStatement;
end;

procedure TTestParser.ConstantColon;
begin
  inherited ConstantColon;
end;

procedure TTestParser.ConstantDeclaration;
begin
  addStart('ConstantDeclaration');
  inherited ConstantDeclaration;
  addEnd('ConstantDeclaration');
end;

procedure TTestParser.ConstantEqual;
begin
  inherited ConstantEqual;
end;

procedure TTestParser.ConstantExpression;
begin
  addStart('ConstantExpression');
  inherited ConstantExpression;
  addEnd('ConstantExpression');
end;

procedure TTestParser.ConstantName;
begin
  addStart('ConstantName');
  inherited ConstantName;
  addEnd('ConstantName');
end;

procedure TTestParser.ConstantType;
begin
  addStart('ConstantType');
  inherited ConstantType;
  addEnd('ConstantType');
end;

procedure TTestParser.ConstantValue;
begin
  addStart('ConstantValue');
  inherited ConstantValue;
  addEnd('ConstantValue');
end;

procedure TTestParser.ConstantValueTyped;
begin
  addStart('ConstantValueTyped');
  inherited ConstantValueTyped;
  addEnd('ConstantValueTyped');
end;

procedure TTestParser.ConstParameter;
begin
  addStart('ConstParameter');
  inherited ConstParameter;
  addEnd('ConstParameter');
end;

procedure TTestParser.ConstructorHeading;
begin
  inherited ConstructorHeading;
end;

procedure TTestParser.ConstructorName;
begin
  inherited ConstructorName;
end;

procedure TTestParser.ConstSection;
begin
  inherited ConstSection;
end;

procedure TTestParser.ContainsClause;
begin
  inherited ContainsClause;
end;

procedure TTestParser.ContainsExpression;
begin
  inherited ContainsExpression;
end;

procedure TTestParser.ContainsIdentifier;
begin
  inherited ContainsIdentifier;
end;

procedure TTestParser.ContainsStatement;
begin
  inherited ContainsStatement;
end;

procedure TTestParser.DeclarationSection;
begin
  inherited DeclarationSection;
end;

procedure TTestParser.Designator;
begin
  inherited Designator;
end;

procedure TTestParser.DestructorHeading;
begin
  inherited DestructorHeading;
end;

procedure TTestParser.DestructorName;
begin
  inherited DestructorName;
end;

procedure TTestParser.Directive16Bit;
begin
  inherited Directive16Bit;
end;

procedure TTestParser.DirectiveBinding;
begin
  inherited DirectiveBinding;
end;

procedure TTestParser.DirectiveCalling;
begin
  inherited DirectiveCalling;
end;

procedure TTestParser.DirectiveDeprecated;
begin
  inherited DirectiveDeprecated;
end;

procedure TTestParser.DirectiveLibrary;
begin
  inherited DirectiveLibrary;
end;

procedure TTestParser.DirectiveLocal;
begin
  inherited DirectiveLocal;
end;

procedure TTestParser.DirectivePlatform;
begin
  inherited DirectivePlatform;
end;

procedure TTestParser.DirectiveVarargs;
begin
  inherited DirectiveVarargs;
end;

procedure TTestParser.DispInterfaceForward;
begin
  inherited DispInterfaceForward;
end;

procedure TTestParser.DispIDSpecifier;
begin
  inherited DispIDSpecifier;
end;

procedure TTestParser.EmptyStatement;
begin
  inherited EmptyStatement;
end;

procedure TTestParser.EnumeratedType;
begin
  addStart('EnumeratedType');
  inherited EnumeratedType;
  addEnd('EnumeratedType');
end;

procedure TTestParser.EnumeratedTypeItem;
begin
  addStart('EnumeratedTypeItem');
  inherited EnumeratedTypeItem;
  addEnd('EnumeratedTypeItem');
end;

procedure TTestParser.ExceptBlock;
begin
  inherited ExceptBlock;
end;

procedure TTestParser.ExceptionBlockElseBranch;
begin
  inherited ExceptionBlockElseBranch;
end;

procedure TTestParser.ExceptionClassTypeIdentifier;
begin
  inherited ExceptionClassTypeIdentifier;
end;

procedure TTestParser.ExceptionHandler;
begin
  inherited ExceptionHandler;
end;

procedure TTestParser.ExceptionHandlerList;
begin
  inherited ExceptionHandlerList;
end;

procedure TTestParser.ExceptionIdentifier;
begin
  inherited ExceptionIdentifier;
end;

procedure TTestParser.ExceptionVariable;
begin
  inherited ExceptionVariable;
end;

procedure TTestParser.ExplicitType;
begin
  addStart('ExplicitType');
  inherited ExplicitType;
  addEnd('ExplicitType');
end;

procedure TTestParser.ExportedHeading;
begin
  inherited ExportedHeading;
end;

procedure TTestParser.ExportsClause;
begin
  inherited ExportsClause;
end;

procedure TTestParser.ExportsElement;
begin
  inherited ExportsElement;
end;

procedure TTestParser.Expression;
begin
  inherited Expression;
end;

procedure TTestParser.ExpressionList;
begin
  inherited ExpressionList;
end;

procedure TTestParser.ExternalDirective;
begin
  inherited ExternalDirective;
end;

procedure TTestParser.ExternalDirectiveThree;
begin
  inherited ExternalDirectiveThree;
end;

procedure TTestParser.ExternalDirectiveTwo;
begin
  inherited ExternalDirectiveTwo;
end;

procedure TTestParser.Factor;
begin
  inherited Factor;
end;

procedure TTestParser.FieldDeclaration;
begin
  addStart('FieldDeclaration');
  inherited FieldDeclaration;
  addEnd('FielDeclaration');
end;

procedure TTestParser.FieldList;
begin
  inherited FieldList;
end;

procedure TTestParser.FieldNameList;
begin
  addStart('FieldNameList');
  inherited FieldNameList;
  addEnd('FieldNameList');
end;

procedure TTestParser.FieldName;
begin
  addStart('FieldName');
  inherited FieldName;
  addEnd('FieldName');
end;

procedure TTestParser.FileType;
begin
  inherited FileType;
end;

procedure TTestParser.FormalParameterList;
begin
  addStart('FormalParameterList');
  inherited FormalParameterList;
  addEnd('FormalParameterList');
end;

procedure TTestParser.FormalParameterSection;
begin
  addStart('FormalParameterSection');
  inherited FormalParameterSection;
  addEnd('FormalParameterSection');
end;

procedure TTestParser.ForStatement;
begin
  inherited ForStatement;
end;

procedure TTestParser.ForwardDeclaration;
begin
  inherited ForwardDeclaration;
end;

procedure TTestParser.FunctionHeading;
begin
  inherited FunctionHeading;
end;

procedure TTestParser.FunctionMethodDeclaration;
begin
  inherited FunctionMethodDeclaration;
end;

procedure TTestParser.FunctionMethodName;
begin
  addStart('FunctionMethodName');
  inherited FunctionMethodName;
  addEnd('FunctionMethodName');
end;

procedure TTestParser.FunctionProcedureBlock;
begin
  inherited FunctionProcedureBlock;
end;

procedure TTestParser.FunctionProcedureName;
begin
  addStart('FunctionProcedureName');
  inherited FunctionProcedureName;
  addEnd('FunctionProcedureName');
end;

procedure TTestParser.Identifier;
begin
  inherited Identifier;
end;

procedure TTestParser.IdentifierList;
begin
  inherited IdentifierList;
end;

procedure TTestParser.IfStatement;
begin
  inherited IfStatement;
end;

procedure TTestParser.ImplementationSection;
begin
  inherited ImplementationSection;
end;

procedure TTestParser.IncludeFile;
begin
  inherited IncludeFile;
end;

procedure TTestParser.IndexSpecifier;
begin
  inherited IndexSpecifier;
end;

procedure TTestParser.InheritedStatement;
begin
  inherited InheritedStatement;
end;

procedure TTestParser.InitializationSection;
begin
  inherited InitializationSection;
end;

procedure TTestParser.InlineStatement;
begin
  inherited InlineStatement;
end;

procedure TTestParser.InParameter;
begin
  inherited InParameter;
end;

procedure TTestParser.InterfaceDeclaration;
begin
  inherited InterfaceDeclaration;
end;

procedure TTestParser.InterfaceForward;
begin
  inherited InterfaceForward;
end;

procedure TTestParser.InterfaceGUID;
begin
  inherited InterfaceGUID;
end;

procedure TTestParser.InterfaceHeritage;
begin
  inherited InterfaceHeritage;
end;

procedure TTestParser.InterfaceMemberList;
begin
  inherited InterfaceMemberList;
end;

procedure TTestParser.InterfaceSection;
begin
  inherited InterfaceSection;
end;

procedure TTestParser.InterfaceType;
begin
  inherited InterfaceType;
end;

procedure TTestParser.LabelDeclarationSection;
begin
  inherited LabelDeclarationSection;
end;

procedure TTestParser.LabeledStatement;
begin
  inherited LabeledStatement;
end;

procedure TTestParser.LabelId;
begin
  inherited LabelId;
end;

procedure TTestParser.LibraryFile;
begin
  inherited LibraryFile;
end;

procedure TTestParser.MainUsedUnitExpression;
begin
  inherited MainUsedUnitExpression;
end;

procedure TTestParser.MainUsedUnitName;
begin
  inherited MainUsedUnitName;
end;

procedure TTestParser.MainUsedUnitStatement;
begin
  inherited MainUsedUnitStatement;
end;

procedure TTestParser.MainUsesClause;
begin
  inherited MainUsesClause;
end;

procedure TTestParser.MultiplicativeOperator;
begin
  inherited MultiplicativeOperator;
end;

procedure TTestParser.NewFormalParameterType;
begin
  inherited NewFormalParameterType;
end;

procedure TTestParser.Number;
begin
  inherited Number;
end;

procedure TTestParser.ObjectConstructorHeading;
begin
  inherited ObjectConstructorHeading;
end;

procedure TTestParser.ObjectDestructorHeading;
begin
  inherited ObjectDestructorHeading;
end;

procedure TTestParser.ObjectField;
begin
  inherited ObjectField;
end;

procedure TTestParser.ObjectForward;
begin
  inherited ObjectForward;
end;

procedure TTestParser.ObjectFunctionHeading;
begin
  addStart('ObjectFunctionHeading');
  inherited ObjectFunctionHeading;
  addEnd('ObjectFunctionHeading');
end;

procedure TTestParser.ObjectHeritage;
begin
  inherited ObjectHeritage;
end;

procedure TTestParser.ObjectMemberList;
begin
  inherited ObjectMemberList;
end;

procedure TTestParser.ObjectMethodDirective;
begin
  inherited ObjectMethodDirective;
end;

procedure TTestParser.ObjectMethodHeading;
begin
  addStart('ObjectMethodHeading');
  inherited ObjectMethodHeading;
  addEnd('ObjectMethodHeading');
end;

procedure TTestParser.ObjectNameOfMethod;
begin
  addStart('ObjectNameOfMethod');
  inherited ObjectNameOfMethod;
  addEnd('ObjectNameOfMethod');
end;

procedure TTestParser.ObjectProperty;
begin
  inherited ObjectProperty;
end;

procedure TTestParser.ObjectPropertySpecifiers;
begin
  inherited ObjectPropertySpecifiers;
end;

procedure TTestParser.ObjectProcedureHeading;
begin
  inherited ObjectProcedureHeading;
end;

procedure TTestParser.ObjectType;
begin
  addStart('ObjectType');
  inherited ObjectType;
  addEnd('ObjectType');
end;

procedure TTestParser.ObjectTypeEnd;
begin
  inherited ObjectTypeEnd;
end;

procedure TTestParser.ObjectVisibility;
begin
  inherited ObjectVisibility;
end;

procedure TTestParser.OldFormalParameterType;
begin
  addStart('OldFormalParameterType');
  inherited OldFormalParameterType;
  addEnd('OldFormalParameterType');
end;

procedure TTestParser.OrdinalIdentifier;
begin
  addStart('OrdinalIdentifier');
  inherited OrdinalIdentifier;
  addEnd('OrdinalIdentifier');
end;

procedure TTestParser.OrdinalType;
begin
  inherited OrdinalType;
end;

procedure TTestParser.OutParameter;
begin
  inherited OutParameter;
end;

procedure TTestParser.PackageFile;
begin
  inherited PackageFile;
end;

procedure TTestParser.ParameterFormal;
begin
  inherited ParameterFormal;
end;

procedure TTestParser.ParameterName;
begin
  addStart('ParameterName');
  inherited ParameterName;
  addEnd('ParameterName');
end;

procedure TTestParser.ParameterNameList;
begin
  addStart('ParameterNameList');
  inherited ParameterNameList;
  addEnd('ParameterNameList');
end;

procedure TTestParser.ParseFile;
begin
  inherited ParseFile;
end;

procedure TTestParser.PointerType;
begin
  inherited PointerType;
end;

procedure TTestParser.ProceduralDirective;
begin
  addStart('ProceduralDirective');
  inherited ProceduralDirective;
  addEnd('ProceduralDirective');
end;

procedure TTestParser.ProceduralType;
begin
  addStart('ProceduralType');
  inherited ProceduralType;
  addEnd('ProceduralType');
end;

procedure TTestParser.ProcedureDeclarationSection;
begin
  addStart('ProcedureDeclarationSection');
  inherited ProcedureDeclarationSection;
  addEnd('ProcedureDeclarationSection');
end;

procedure TTestParser.ProcedureHeading;
begin
  addStart('ProcedureHeading');
  inherited ProcedureHeading;
  addEnd('ProcedureHeading');
end;

procedure TTestParser.ProcedureMethodDeclaration;
begin
  addStart('ProcedureMethodDeclaration');
  inherited ProcedureMethodDeclaration;
  addEnd('ProcedureMethodDeclaration');
end;

procedure TTestParser.ProcedureMethodName;
begin
  addStart('ProcedureMethodName');
  inherited ProcedureMethodName;
  addEnd('ProcedureMethodName');
end;

procedure TTestParser.ProgramBlock;
begin
  inherited ProgramBlock;
end;

procedure TTestParser.ProgramFile;
begin
  inherited ProgramFile;
end;

procedure TTestParser.PropertyDefault;
begin
  inherited PropertyDefault;
end;

procedure TTestParser.PropertyInterface;
begin
  inherited PropertyInterface;
end;

procedure TTestParser.PropertyName;
begin
  inherited PropertyName;
end;

procedure TTestParser.PropertyParameterConst;
begin
  inherited PropertyParameterConst;
end;

procedure TTestParser.PropertyParameterList;
begin
  inherited PropertyParameterList;
end;

procedure TTestParser.PropertySpecifiers;
begin
  inherited PropertySpecifiers;
end;

procedure TTestParser.QualifiedIdentifier;
begin
  inherited QualifiedIdentifier;
end;

procedure TTestParser.QualifiedIdentifierList;
begin
  inherited QualifiedIdentifierList;
end;

procedure TTestParser.RaiseStatement;
begin
  inherited RaiseStatement;
end;

procedure TTestParser.ReadAccessIdentifier;
begin
  inherited ReadAccessIdentifier;
end;

procedure TTestParser.RealIdentifier;
begin
  inherited RealIdentifier;
end;

procedure TTestParser.RealType;
begin
  addStart('RealType');
  inherited RealType;
  addEnd('RealType');
end;

procedure TTestParser.RecordConstant;
begin
  addStart('RecordConstant');
  inherited RecordConstant;
  addEnd('RecordConstant');
end;

procedure TTestParser.RecordFieldConstant;
begin
  inherited RecordFieldConstant;
end;

procedure TTestParser.RecordType;
begin
  addStart('RecordType');
  inherited RecordType;
  addEnd('RecordType');
end;

procedure TTestParser.RecordVariant;
begin
  inherited RecordVariant;
end;

procedure TTestParser.RelativeOperator;
begin
  inherited RelativeOperator;
end;

procedure TTestParser.RepeatStatement;
begin
  inherited RepeatStatement;
end;

procedure TTestParser.RequiresClause;
begin
  inherited RequiresClause;
end;

procedure TTestParser.RequiresIdentifier;
begin
  inherited RequiresIdentifier;
end;

procedure TTestParser.ResolutionInterfaceName;
begin
  inherited ResolutionInterfaceName;
end;

procedure TTestParser.ResourceDeclaration;
begin
  inherited ResourceDeclaration;
end;

procedure TTestParser.ReturnType;
begin
  addStart('ReturnType');
  inherited ReturnType;
  addEnd('ReturnType');
end;

procedure TTestParser.SetConstructor;
begin
  addStart('SetConstructor');
  inherited SetConstructor;
  addEnd('SetConstructor');
end;

procedure TTestParser.SetElement;
begin
  addStart('SetElement');
  inherited SetElement;
  addEnd('SetElement');
end;

procedure TTestParser.SetType;
begin
  addStart('SetType');
  inherited SetType;
  addEnd('SetType');
end;

procedure TTestParser.SimpleExpression;
begin
  inherited SimpleExpression;
end;

procedure TTestParser.SimpleStatement;
begin
  inherited SimpleStatement;
end;

procedure TTestParser.SimpleType;
begin
  addStart('SimpleType');
  inherited SimpleType;
  addEnd('SimpleType');
end;

procedure TTestParser.SkipAnsiComment;
begin
  inherited SkipAnsiComment;
end;

procedure TTestParser.SkipBorComment;
begin
  inherited SkipBorComment;
end;

procedure TTestParser.SkipSlashesComment;
begin
  inherited SkipSlashesComment;
end;

procedure TTestParser.SkipSpace;
begin
  inherited SkipSpace;
end;

procedure TTestParser.SkipCRLFco;
begin
  inherited SkipCRLFco;
end;

procedure TTestParser.SkipCRLF;
begin
  inherited SkipCRLF;
end;

procedure TTestParser.Statement;
begin
  inherited Statement;
end;

procedure TTestParser.StatementList;
begin
  inherited StatementList;
end;

procedure TTestParser.StorageExpression;
begin
  inherited StorageExpression;
end;

procedure TTestParser.StorageIdentifier;
begin
  inherited StorageIdentifier;
end;

procedure TTestParser.StorageDefault;
begin
  inherited StorageDefault;
end;

procedure TTestParser.StorageNoDefault;
begin
  inherited StorageNoDefault;
end;

procedure TTestParser.StorageSpecifier;
begin
  inherited StorageSpecifier;
end;

procedure TTestParser.StorageStored;
begin
  inherited StorageStored;
end;

procedure TTestParser.StringIdentifier;
begin
  inherited StringIdentifier;
end;

procedure TTestParser.StringStatement;
begin
  inherited StringStatement;
end;

procedure TTestParser.StringType;
begin
  inherited StringType;
end;

procedure TTestParser.StructuredType;
begin
  inherited StructuredType;
end;

procedure TTestParser.SubrangeType;
begin
  inherited SubrangeType;
end;

procedure TTestParser.TagField;
begin
  inherited TagField;
end;

procedure TTestParser.TagFieldName;
begin
  inherited TagFieldName;
end;

procedure TTestParser.TagFieldTypeName;
begin
  inherited TagFieldTypeName;
end;

procedure TTestParser.Term;
begin
  inherited Term;
end;

procedure TTestParser.TryStatement;
begin
  addStart('TryStatement');
  inherited TryStatement;
  addEnd('TryStatement');
end;

procedure TTestParser.TypedConstant;
begin
  addStart('TypedConstant');
  inherited TypedConstant;
  addEnd('TypedConstant');
end;

procedure TTestParser.TypeDeclaration;
begin
  addStart('TypeDeclaration');
  inherited TypeDeclaration;
  addEnd('TypeDeclaration');
end;

procedure TTestParser.TypeId;
begin
  addStart('TypeID');
  inherited TypeId;
  addEnd('TypeID');
end;

procedure TTestParser.TypeKind;
begin
  addStart('TypeKind');
  inherited TypeKind;
  addEnd('TypeKind');
end;

procedure TTestParser.TypeName;
begin
  addStart('TypeName');
  inherited TypeName;
  addEnd('TypeName');
end;

procedure TTestParser.TypeArgs;
begin
  addStart('TypeArgs');
  inherited TypeArgs;
  addEnd('TypeArgs');
end;

procedure TTestParser.TypeParams;
begin
  addStart('TypeParams');
  inherited TypeParams;
  addEnd('TypeParamDecl');
end;

procedure TTestParser.TypeParamDecl;
begin
  addStart('TypeParamDecl');
  inherited TypeParamDecl;
  addEnd('TypeParamDecl');
end;

procedure TTestParser.TypeParamDeclList;
begin
  addStart('TypeParamDeclList');
  inherited TypeParamDeclList;
  addEnd('TypeParamDeclList');
end;

procedure TTestParser.TypeParamList;
begin
  addStart('TypeParamList');
  inherited TypeParamList;
  addEnd('TypeParamList');
end;

procedure TTestParser.ConstraintList;
begin
  inherited ConstraintList;
end;

procedure TTestParser.Constraint;
begin
  inherited Constraint;
end;

procedure TTestParser.TypeSection;
begin
  inherited TypeSection;
end;

procedure TTestParser.UnitFile;
begin
  inherited UnitFile;
end;

procedure TTestParser.UnitId;
begin
  inherited UnitId;
end;

procedure TTestParser.UnitName;
begin
  inherited UnitName;
end;

procedure TTestParser.UsedUnitName;
begin
  inherited UsedUnitName;
end;

procedure TTestParser.UsedUnitsList;
begin
  inherited UsedUnitsList;
end;

procedure TTestParser.UsesClause;
begin
  inherited UsesClause;
end;

procedure TTestParser.VarAbsolute;
begin
  inherited VarAbsolute;
end;

procedure TTestParser.VarEqual;
begin
  addStart('VarEqual');
  inherited VarEqual;
  addEnd('VarEqual');
end;

procedure TTestParser.VarDeclaration;
begin
  addStart('VarDeclaration');
  inherited VarDeclaration;
  addEnd('VarDeclartion');
end;

procedure TTestParser.Variable;
begin
  addStart('Variable');
  inherited Variable;
  addEnd('Variable');
end;

procedure TTestParser.VariableList;
begin
  addStart('VariableList');
  inherited VariableList;
  addEnd('VariableList');
end;

procedure TTestParser.VariableReference;
begin
  inherited VariableReference;
end;

procedure TTestParser.VariableTwo;
begin
  inherited VariableTwo;
end;

procedure TTestParser.VariantIdentifier;
begin
  inherited VariantIdentifier;
end;

procedure TTestParser.VariantSection;
begin
  inherited VariantSection;
end;

procedure TTestParser.VarParameter;
begin
  addStart('VarParameter');
  inherited VarParameter;
  addEnd('VarParameter');
end;

procedure TTestParser.VarName;
begin
  addStart('VarName');
  inherited VarName;
  addEnd('VarName');
end;

procedure TTestParser.VarNameList;
begin
  addStart('VarNameList');
  inherited VarNameList;
  addEnd('VarNameList');
end;

procedure TTestParser.VarSection;
begin
  addStart('VarSection');
  inherited VarSection;
  addEnd('VarSection');
end;

procedure TTestParser.VisibilityAutomated;
begin
  inherited VisibilityAutomated;
end;

procedure TTestParser.VisibilityPrivate;
begin
  inherited VisibilityPrivate;
end;

procedure TTestParser.VisibilityProtected;
begin
  inherited VisibilityProtected;
end;

procedure TTestParser.VisibilityPublic;
begin
  inherited VisibilityPublic;
end;

procedure TTestParser.VisibilityPublished;
begin
  inherited VisibilityPublished;
end;

procedure TTestParser.VisibilityUnknown;
begin
  inherited VisibilityUnknown;
end;

procedure TTestParser.WhileStatement;
begin
  addStart('WhileStatement');
  inherited WhileStatement;
  addEnd('WhileStatement');
end;

procedure TTestParser.WithStatement;
begin
  addStart('WithStatement');
  inherited WithStatement;
  addEnd('WithStatement');
end;

procedure TTestParser.WriteAccessIdentifier;
begin
  inherited WriteAccessIdentifier;
end;

end.

