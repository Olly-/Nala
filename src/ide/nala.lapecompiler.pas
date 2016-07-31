unit nala.LapeCompiler;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils,
  lpcompiler, lptypes, lpvartypes, lpeval, lputils, lpparser, lptree,
  nala.Types, nala.Dictionary, lpexceptions;

type

  { TLPCompilerDump }

  TLPCompilerDump = class(TObject)
  private
  type
    TDict = specialize TDictionary<String, TStringList>;
  private
    FDictionary: TDict;
    FCurrent: TStringList;

    function GetSectionDump(Name: String): String;
    procedure SetSection(Name: String);
  public
  type
    TCallback = procedure(Name: String; constref SectionDump: String) of object;
  public
    property Section: String write SetSection;
    property SectionDump[Name: String]: String read GetSectionDump;

    procedure Add(constref Text: String);
    function Merged: String;
    procedure TraverseSections(Callback: TCallback);

    constructor Create(BaseSection: String);
    destructor Destroy; override;
  end;

  { TLPCompiler }

  TLPImport = (lpiCore, lpiTime, lpiString, lpiBox);
  TLPImports = set of TLPImport;

const
  AllImports: TLPImports = [lpiCore, lpiTime, lpiString, lpiBox];

type
  TLPCompiler = class(TLapeCompiler)
  private
    FDump: TLPCompilerDump;
    FDumping: Boolean;

    procedure AppendDump(constref Str: String);
  public
    function addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar; override;
    function addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar; override;

    function addGlobalType(Str: lpString; AName: lpString): TLapeType; override;
    function addGlobalType(Typ: TLapeType; AName: lpString=''; ACopy: Boolean = True): TLapeType; override;

    function addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar; override;

    function addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar; overload;

    function addDelayedCode(ACode: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;

    property Dump: TLPCompilerDump read FDump;

    constructor Create(AScript: String; Imports: TLPImports; Thread: TThread; Dumping: Boolean = False); overload;
    destructor Destroy; override;
  end;

implementation

uses
  nala.Imports.Time, nala.Imports.Strings, nala.Imports.Box, nala.Imports.Core;

{ TLPCompilerDump }

procedure TLPCompilerDump.Add(constref Text: String);
var
  Str: String;
begin
  Str := Trim(Text);
  if (Str <> '') then
  begin
    if (Str[Length(Str)] <> ';') then
      Str += ';';
    FCurrent.Add(Str);
  end;
end;

procedure TLPCompilerDump.SetSection(Name: String);
begin
  if (FDictionary.Get(Name, FCurrent)) then
    Exit;

  FCurrent := TStringList.Create;
  FDictionary.Add(Name, FCurrent);
end;

function TLPCompilerDump.GetSectionDump(Name: String): String;
var
  List: TStringList;
begin
  Result := '';

  if (FDictionary.Get(Name, List)) then
    Result := List.Text;
end;

function TLPCompilerDump.Merged: String;
var
  i, j: Integer;
begin
  Result := '';

  for i := 0 to High(FDictionary.Items) do
    for j := 0 to High(FDictionary.Items[i]) do
      Result += FDictionary.Items[i][j].Val.Text + LineEnding;
end;

procedure TLPCompilerDump.TraverseSections(Callback: TCallback);
var
  i, j: Integer;
begin
  for i := 0 to High(FDictionary.Items) do
    for j := 0 to High(FDictionary.Items[i]) do
      Callback(FDictionary.Items[i][j].Key, FDictionary.Items[i][j].Val.Text);
end;

constructor TLPCompilerDump.Create(BaseSection: String);
begin
  inherited Create;

  FDictionary := TDict.Create(@HashStr);
  Section := BaseSection;
end;

destructor TLPCompilerDump.Destroy;
var
  i, j: Integer;
begin
  for i := 0 to High(FDictionary.Items) do
    for j := 0 to High(FDictionary.Items[i]) do
      FDictionary.Items[i][j].val.Free;
  FDictionary.Free;

  inherited Destroy;
end;

{ TLPCompiler }

procedure TLPCompiler.AppendDump(constref Str: String);
begin
  if (FDumping) then
    FDump.Add(Str);
end;

function TLPCompiler.addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar;
begin
  AppendDump(AHeader + ' begin end');
  Result := inherited;
end;

function TLPCompiler.addGlobalType(Str: lpString; AName: lpString): TLapeType;
begin
  AppendDump('type ' + AName + ' = ' + Str);
  Result := inherited;
end;

function TLPCompiler.addGlobalType(Typ: TLapeType; AName: lpString; ACopy: Boolean): TLapeType;
begin
  if (AName[1] <> '!') then
    AppendDump('type ' + AName + ' = ' + AName);

  Result := inherited;
end;

function TLPCompiler.addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar;
begin
  AppendDump(AHeader + ' begin end');
  Result := inherited;
end;

function TLPCompiler.addDelayedCode(ACode: lpString; AFileName: lpString; AfterCompilation: Boolean; IsGlobal: Boolean): TLapeTree_Base;
begin
  if (AFileName <> '') and (AFileName[1] <> '!') then
    AppendDump(ACode);

  Result := inherited;
end;

function TLPCompiler.addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  AppendDump('var ' + AName + ': Extended := ' + FloatToStr(Value));
  Result := inherited;
end;

function TLPCompiler.addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  AppendDump('var ' + AName + ': Int32 := ' + IntToStr(Value));
  Result := inherited;
end;

function TLPCompiler.addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  AppendDump('var ' + AName + ': UInt32 := ' + IntToStr(Value));
  Result := inherited;
end;

function TLPCompiler.addGlobalVar(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  AppendDump('var ' + AName + ': String := ' + Value);
  Result := inherited;
end;

function TLPCompiler.addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  AppendDump('const ' + AName + ': UInt32 = ' + IntToStr(Value));

  Result := addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDumping) then
    FDump.FCurrent.Delete(FDump.FCurrent.Count - 1); // Delete variable dump
end;

function TLPCompiler.addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  AppendDump('const ' + AName + ': Int32 = ' + IntToStr(Value));

  Result := addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDumping) then
    FDump.FCurrent.Delete(FDump.FCurrent.Count - 1); // Delete variable dump
end;

function TLPCompiler.addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  AppendDump('const ' + AName + ': Extended = ' + FloatToStr(Value));

  Result := addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDumping) then
    FDump.FCurrent.Delete(FDump.FCurrent.Count - 1); // Delete variable dump
end;

function TLPCompiler.addGlobalConst(Value: AnsiString; AName: lpString): TLapeGlobalVar;
begin
  AppendDump('const ' + AName + ': String = ' + Value);

  Result := addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FDumping) then
    FDump.FCurrent.Delete(FDump.FCurrent.Count - 1); // Delete variable dump
end;

constructor TLPCompiler.Create(AScript: String; Imports: TLPImports; Thread: TThread; Dumping: Boolean);
var
  Typ: ELapeBaseType;
begin
  FDumping := Dumping;
  FDump := TLPCompilerDump.Create('Lape');

  inherited Create(TLapeTokenizerString.Create(AScript));

  InitializePascalScriptBasics(Self, [psiSettings, psiExceptions, psiTypeAlias]);
  ExposeGlobals(Self);

  try
    if (lpiCore in Imports) then Import_Core(Self, Thread);
    if (lpiString in Imports) then Import_String(Self);
    if (lpiBox in Imports) then Import_Box(Self);
    if (lpiTime in Imports) then Import_Time(Self);
  except
    on e: Exception do
      LapeException('Exception on importing unit: ' + e.Message);
  end;

  if (FDumping) then
  begin
    Dump.Section := 'Lape';

    for Typ := Low(ELapeBaseType) to High(ELapeBaseType) do
      if (FBaseTypes[Typ] <> nil) then
        AppendDump('type ' + FBaseTypes[Typ].Name + ' = ' + FBaseTypes[Typ].Name);
  end;
end;

destructor TLPCompiler.Destroy;
begin
  FDump.Free;

  inherited Destroy;
end;

end.

