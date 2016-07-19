unit nala.CodeInfo;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, nala.CodeParser, CastaliaPasLex, CastaliaSimplePasPar, CastaliaPasLexTypes,
  nala.Dictionary;

type

  { TCIFile }

  TCIFile = class;
  TCIFileArray = array of TCIFile;

  TCIFile = class(TCodeParser)
  private
    FPath: String;
    FStream: TMemoryStream;
    FIncludes: TCIFileArray;
    FIncludePaths: TStringList;

    function AddInclude(APath: String): Boolean;
    procedure OnInclude(Sender: TmwBasePasLex);
    procedure OnDirect(Sender: TmwBasePasLex);
  public
    property IncludePaths: TStringList read FIncludePaths write FIncludePaths;

    procedure Assign(AssignFrom: TCIFile);
    procedure Run; overload;

    constructor Create(AString: String = ''; APath: String = ''); overload;
    destructor Destroy; override;
  end;

  { TTypeCache }

  TTypeCache = class(TObject)
    Decl: TDeclType;
    Methods: TDeclMethodArray;
  end;
  TTypeDictionary = specialize TDictionary<String, TTypeCache>;

  { TCIScript }

  TCIMessageEvent = procedure(Str: String) of object;

  TCIScript = class(TObject)
  private
    FFile: TCIFile;
    FTypeDict: TTypeDictionary;
    FOnMessage: TCIMessageEvent;
    FLastCache: TTypeCache;
    FIncludePaths: TStringList;

    //procedure OnFindType(Decl: TCPType); inline;
    //procedure OnFindMethodOfObject(Decl: TCPMethod); inline;
    procedure OnOutputMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: String; X, Y: Integer);
  public
    property OnMessage: TCIMessageEvent read FOnMessage write FOnMessage;
    property TypeDict: TTypeDictionary read FTypeDict;
    property IncludePaths: TStringList read FIncludePaths write FIncludePaths;

    //function GetTypeTree(var TypeName: String): TDeclarationArray; // T2DIntArray > TIntArray > array of Integer
    //function GetTypeDecl(var TypeName: String): TCPType;

    procedure Run;

    constructor Create(AScript: String);
    destructor Destroy; override;
  end;

function TrimDecl(constref Text: String): String;

implementation

uses
  Math, strutils;

{ TCIScript }

{
procedure TCIScript.OnFindType(Decl: TCPType);
var
  Pos: THashIndex;
  Name: String;
  Cache: TTypeCache;
begin
  if (Decl.Typ = nil) or (Decl.Typ.ClassType = TCPSet) or (Decl.Typ.ClassType = TCPEnum) or (Decl.Name = '') then
    Exit;

  Name := Decl.Name;
  if (not FTypeDict.Find(Name, Pos)) then
  begin
    Cache := TTypeCache.Create;
    SetLength(Cache.Methods, 0);
    Cache.Decl := Decl;

    FTypeDict.Add(Name, Cache);
  end;
end;

procedure TCIScript.OnFindMethodOfObject(Decl: TCPMethod);
var
  Cache: TTypeCache = nil;
  l: Integer;
begin
  if (FLastCache <> nil) and (Decl.ObjectName = FLastCache.Decl.Name) then
    Cache := FLastCache
  else
    if (not FTypeDict.Get(Decl.ObjectName, Cache)) then
      Exit;

  l := Length(Cache.Methods);
  SetLength(Cache.Methods, l + 1);
  Cache.Methods[l] := Decl;

  FLastCache := Cache;
end;
}

procedure TCIScript.OnOutputMessage(Sender: TObject; const Typ: TMessageEventType; const Msg: String; X, Y: Integer);
begin
  if (FOnMessage <> nil) then
    FOnMessage('CI Error: ' + Msg + ' on line: ' + IntToStr(Y + 1) + ' in ' + (Sender as TCIFile).FPath);
end;
 {
function TCIScript.GetTypeTree(var TypeName: String): TDeclarationArray;
begin

end;

function TCIScript.GetTypeDecl(var TypeName: String): TCPType;
begin

end;
}

procedure TCIScript.Run;
begin
  FFile.Run();
end;

constructor TCIScript.Create(AScript: String);
begin
  inherited Create;

  FOnMessage := nil;
  FLastCache := nil;

  FIncludePaths := TStringList.Create;
  FTypeDict := TTypeDictionary.Create(@HashStr);
  {
  FFile := TCIFile.Create(AScript, '');
  FFile.OnType := @OnFindType;
  FFile.OnMethodOfObject := @OnFindMethodOfObject;
  FFile.OnMessage := @OnOutputMessage;
  FFile.IncludePaths := FIncludePaths;
  }
end;

destructor TCIScript.Destroy;
var
  i, j: Integer;
begin
  FIncludePaths.Free;

  for i := 0 to High(TypeDict.Items) do
    for j := 0 to High(TypeDict.Items[i]) do
      TypeDict.Items[i][j].Val.Free;

  inherited Destroy;
end;

{ TCIFile }

function TCIFile.AddInclude(APath: String): Boolean;
var
  i: Integer = 0;
  Path: String;
begin
  APath := SetDirSeparators(APath);
  Path := APath;
  Result := FileExists(Path);

  if (not Result) then
  begin
    Path := SetDirSeparators(ExtractFileDir(FPath)) + DirectorySeparator + APath;
    Result := FileExists(Path);
  end;

  while (not Result) and (i < FIncludePaths.Count) do
  begin
    Path := SetDirSeparators(FIncludePaths[i]) + APath;
    Result := FileExists(Path);

    Inc(i);
  end;

  if (Result) then
  begin
    i := Length(FIncludes);
    SetLength(FIncludes, i + 1);
    FIncludes[i] := TCIFile.Create('', Path);
    FIncludes[i].Assign(Self);
    FIncludes[i].Run();
  end;
end;

procedure TCIFile.OnInclude(Sender: TmwBasePasLex);
var
  Str: String = '';
  p, i: Integer;
begin
  p := Max(Pos('$include_once ', Lowercase(Sender.Token)), Pos('$i ', Lowercase(Sender.Token)));
  if (p = 0) then
    Exit;
  p := PosEx(' ', Sender.Token, p);
  if (p = 0) then
    Exit;
  for i := p to Length(Sender.Token) do
    if (Sender.Token[i] = '}') then
      Break
    else
      Str += Sender.Token[i];
  Str := Trim(Str);

  if (not AddInclude(Str)) then
    OnMessage(Self, meError, 'Failed to find file: ' + Str + ' in file: ' + FPath, Lexer.PosXY.X, Lexer.PosXY.Y);

  Sender.Next;
end;

procedure TCIFile.OnDirect(Sender: TmwBasePasLex);
begin
  Sender.Next;
end;

procedure TCIFile.Assign(AssignFrom: TCIFile);
begin
  OnMessage := AssignFrom.OnMessage;
  OnType := AssignFrom.OnType;
  OnMethodOfObject := AssignFrom.OnMethodOfObject;
  FIncludePaths := AssignFrom.FIncludePaths;
end;

destructor TCIFile.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(FIncludes) do
    FIncludes[i].Free;
  FStream.Free;

  inherited Destroy;
end;

procedure TCIFile.Run;
begin
  inherited Run('', FStream);
end;

constructor TCIFile.Create(AString: String = ''; APath: String = '');
begin
  inherited Create;

  SetLength(FIncludes, 0);
  FStream := TMemoryStream.Create;
  FPath := APath;

  Lexer.OnIncludeDirect := @OnInclude;
  Lexer.OnDefineDirect := @OnDirect;
  Lexer.OnElseDirect := @OnDirect;
  Lexer.OnEndIfDirect := @OnDirect;
  Lexer.OnIfDefDirect := @OnDirect;
  Lexer.OnIfNDefDirect := @OnDirect;
  Lexer.OnUnDefDirect := @OnDirect;
  Lexer.OnIfDirect := @OnDirect;
  Lexer.OnIfEndDirect := @OnDirect;
  Lexer.OnElseIfDirect := @OnDirect;

  with TStringList.Create do
  try
    if (AString <> '') then
      Text := AString
    else
    if (APath <> '') and (FileExists(FPath)) then
      LoadFromFile(FPath);

    SaveToStream(FStream);
  finally
    Free;
  end;
end;

function TrimDecl(constref Text: String): String;
var
  p: Integer;
begin
  Result := Trim(Text);
  p := Pos(LineEnding, Result);
  if (p > 0) then
    Result := Copy(Result, 1, p-2) + ' ...';
end;

end.

