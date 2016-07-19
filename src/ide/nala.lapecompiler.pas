unit nala.LapeCompiler;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils,
  lpcompiler, lptypes, lpvartypes, lpeval, lputils, lpparser, lptree,
  nala.Dictionary;

type

  { TLPCompilerDump }

  TLPCompilerDump = class(TObject)
  public
  type
    TStringListArray = array of TStringList;
  private
  type
    TDict = specialize TDictionary<String, TStringList>;
  private
    FDict: TDict;
    FCurrent: TStringList;

    procedure Add(Decl: String);
    function GetDump(ASection: String): TStringList;
    procedure SetSection(AValue: String);
  public
    property Section: String write SetSection;
    property Dump[ASection: String]: TStringList read GetDump; default;
    function Sections: TStringArray;
    function All: String;

    constructor Create(BaseSection: String);
    destructor Destroy; override;
  end;

  { TLPCompiler }

  TLPImport = (lpiCore, lpiTime, lpiString, lpiBox);
  TLPImports = set of TLPImport;


  TLPCompiler = class(TLapeCompiler)
  private
    FDump: TLPCompilerDump;
    FUseDump: Boolean;

    procedure AppendDump(Str: String);
  public
    function addGlobalFunc(AHeader: lpString; Value: Pointer): TLapeGlobalVar; overload; override;
    function addGlobalType(Str: lpString; AName: lpString): TLapeType; overload; override;
    function addGlobalType(Typ: TLapeType; AName: lpString=''; ACopy: Boolean = True): TLapeType; overload; override;
    function addGlobalMethod(AHeader: lpString; AMethod, ASelf: Pointer): TLapeGlobalVar; overload; override;
    function addDelayedCode(ACode: lpString; AFileName: lpString = ''; AfterCompilation: Boolean = True; IsGlobal: Boolean = True): TLapeTree_Base; override;

    function addGlobalVar(Value: Extended; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: Int32; AName: lpString): TLapeGlobalVar; override;
    function addGlobalVar(Value: UInt32; AName: lpString): TLapeGlobalVar; override;

    function addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar; overload;
    function addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar; overload;

    procedure Import(Imports: TLPImports = [lpiCore, lpiTime, lpiString, lpiBox]; Thread: TThread = nil);

    property Dump: TLPCompilerDump read FDump;

    constructor Create(AScript: String; UseDump: Boolean = False);
    destructor Destroy; override;
  end;

implementation

uses
  nala.Imports.Time, nala.Imports.Strings, nala.Imports.Box, nala.Imports.Core;

{ TLPCompilerDump }

procedure TLPCompilerDump.Add(Decl: String);
begin
  Decl := TrimRight(Decl);
  if (Length(Decl) > 0) then
  begin
    if (Decl[Length(Decl)] <> ';') then
      Decl += ';';
    FCurrent.Add(Decl);
  end;
end;

function TLPCompilerDump.GetDump(ASection: String): TStringList;
begin
  Result := FDict.GetDef(ASection, nil);
end;

procedure TLPCompilerDump.SetSection(AValue: String);
begin
  if (FDict.Get(AValue, FCurrent)) then
    Exit;

  FCurrent := TStringList.Create;
  FDict.AddFast(AValue, FCurrent);
end;

function TLPCompilerDump.Sections: TStringArray;
var
  i, j: Integer;
begin
  for i := 0 to High(FDict.Items) do
    for j := 0 to High(FDict.Items[i]) do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := FDict.Items[i][j].Key;
    end;
end;

function TLPCompilerDump.All: String;
var
  i, j: Integer;
begin
  for i := 0 to High(FDict.Items) do
    for j := 0 to High(FDict.Items[i]) do
      Result += FDict.Items[i][j].Val.Text + LineEnding;
end;

constructor TLPCompilerDump.Create(BaseSection: String);
begin
  inherited Create;

  FDict := TDict.Create(@HashStr);
  Section := BaseSection;
end;

destructor TLPCompilerDump.Destroy;
var
  i, j: Integer;
begin
  for i := 0 to High(FDict.Items) do
    for j := 0 to High(FDict.Items[i]) do
      FDict.Items[i][j].val.Free;
  FDict.Free;

  inherited Destroy;
end;

{ TLPCompiler }

procedure TLPCompiler.AppendDump(Str: String);
begin
  if (FUseDump) then
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

function TLPCompiler.addGlobalConst(Value: UInt32; AName: lpString): TLapeGlobalVar;
begin
  if (FUseDump) then
    AppendDump('const ' + AName + ': UInt32 = ' + IntToStr(Value));

  Result := addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FUseDump) then
    FDump.FCurrent.Delete(FDump.FCurrent.Count - 1); // Delete variable dump
end;

function TLPCompiler.addGlobalConst(Value: Int32; AName: lpString): TLapeGlobalVar;
begin
  if (FUseDump) then
    AppendDump('const ' + AName + ': Int32 = ' + IntToStr(Value));

  Result := addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FUseDump) then
    FDump.FCurrent.Delete(FDump.FCurrent.Count - 1); // Delete variable dump
end;

function TLPCompiler.addGlobalConst(Value: Extended; AName: lpString): TLapeGlobalVar;
begin
  if (FUseDump) then
    AppendDump('const ' + AName + ': Extended = ' + FloatToStr(Value));

  Result := addGlobalVar(Value, AName);
  Result.isConstant := True;

  if (FUseDump) then
    FDump.FCurrent.Delete(FDump.FCurrent.Count - 1); // Delete variable dump
end;

procedure TLPCompiler.Import(Imports: TLPImports; Thread: TThread);
begin
  if (lpiCore in Imports) then
    Import_Core(Self, Thread);
  if (lpiString in Imports) then
    Import_String(Self);
  if (lpiBox in Imports) then
    Import_Box(Self);
  if (lpiTime in Imports) then
    Import_Time(Self);
end;

constructor TLPCompiler.Create(AScript: String; UseDump: Boolean);
var
  Typ: ELapeBaseType;
begin
  FUseDump := UseDump;
  FDump := TLPCompilerDump.Create('Lape');

  inherited Create(TLapeTokenizerString.Create(AScript));

  InitializePascalScriptBasics(Self, [psiSettings, psiExceptions, psiTypeAlias]);
  ExposeGlobals(Self);

  if (FUseDump) then
    for Typ := Low(ELapeBaseType) to High(ELapeBaseType) do
      if (FBaseTypes[Typ] <> nil) then
        AppendDump('type ' + FBaseTypes[Typ].Name + ' = ' + FBaseTypes[Typ].Name);
end;

destructor TLPCompiler.Destroy;
begin
  FDump.Free;

  inherited Destroy;
end;

end.

