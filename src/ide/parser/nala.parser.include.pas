unit nala.Parser.Include;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, syncobjs,
  nala.Parser.Code;

type
  TBufferItem = class(TCodeParser)
  public
    Age: Int32;
    RefCount: Int32;
    Outdated: Boolean;

    constructor Create;
  end;

  TInclude = class(TCodeParser)
  private
    FBuffer: TBufferItem;
    FManagedDeclarations: TDeclarationArray;
  public
    procedure AddRef;
    procedure DecRef;

    constructor Create(ABuffer: TBufferItem; OnIncludeHandler: TCPOnInclude);
    destructor Destroy; override;
  end;

  TIncludeArray = array of TInclude;

  TIncludeList = class
  private
  type
    TList = specialize TFPGObjectList<TInclude>;
  private
    FList: TList;

    function GetCount: Int32;
    function GetInclude(FilePath: String): TInclude;
  public
    function Includes: TIncludeArray;
    procedure Clear;
    procedure Add(AInclude: TInclude);

    property Include[FilePath: String]: TInclude read GetInclude; default;

    constructor Create;
    destructor Destroy; override;
  end;

  TIncludeBuffer = class
  private
  type
    TList = specialize TFPGObjectList<TBufferItem>;
  private
    FList: TList;

    procedure Clean;
    function Find(FilePath: String): TBufferItem;
  public
    function GetInclude(FilePath: String; Parser: TCodeParser): TInclude;

    constructor Create;
    destructor Destroy; override;
  end;

var
  IncludeBuffer: TIncludeBuffer;

implementation

var
  CriticalSection: TCriticalSection;

constructor TBufferItem.Create;
begin
  inherited Create;

  RefCount := 0;
  Age := 0;
  Outdated := False;
end;

procedure TInclude.AddRef;
begin
  Inc(FBuffer.RefCount);
end;

procedure TInclude.DecRef;
begin
  Dec(FBuffer.RefCount);
end;

constructor TInclude.Create(ABuffer: TBufferItem; OnIncludeHandler: TCPOnInclude);

  function GetIncludeDeclaration(From: TCPInclude): TCPInclude;
  begin
    with From do
    begin
      Result := TCPInclude.Create(Parser, Owner, Origin, StartPos, EndPos);
      Result.Declaration := nil;
      Result.FilePath := FilePath;
    end;

    OnIncludeHandler(Self, Result);
    FManagedDeclarations.Append(Result);
  end;

var
  i: Int32;
begin
  inherited Create;

  FBuffer := ABuffer;
  SetLength(FManagedDeclarations, 0);

  FilePath := FBuffer.FilePath;
  Run(0);

  Items.Count := FBuffer.Items.Count;
  Items.FreeObjects := False;

  for i := 0 to FBuffer.Items.Count - 1 do
    if (FBuffer.Items[i].ClassType = TCPInclude) then
      Items[i] := GetIncludeDeclaration(TCPInclude(FBuffer.Items[i]))
    else
      Items[i] := FBuffer.Items[i];

  AddRef();
end;

destructor TInclude.Destroy;
var
  Decl: TDeclaration;
begin
  for Decl in FManagedDeclarations do
    Decl.Free;

  DecRef();

  inherited Destroy;
end;

procedure TIncludeList.Clear;
begin
  FList.Clear;
end;

procedure TIncludeList.Add(AInclude: TInclude);
begin
  FList.Add(AInclude);
end;

function TIncludeList.GetCount: Int32;
begin
  Result := FList.Count;
end;

function TIncludeList.GetInclude(FilePath: String): TInclude;
var
  i: Int32;
begin
  Result := nil;

  for i := 0 to FList.Count - 1 do
    if (FList[i].FilePath = FilePath) then
      Exit(FList[i]);
end;

function TIncludeList.Includes: TIncludeArray;
var
  i: Int32;
begin
  SetLength(Result, FList.Count);
  for i := 0 to High(Result) do
    Result[i] := FList[i];
end;

constructor TIncludeList.Create;
begin
  FList := TList.Create(True);
end;

destructor TIncludeList.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

procedure TIncludeBuffer.Clean;
var
  i: Int32;
begin
  for i := FList.Count - 1 downto 0 do
    if (FList[i].Outdated) and (FList[i].RefCount = 0) then
    begin
      Writeln(Format('Deleting outdated buffer "%s" (%d, %d)', [ExtractFileName(FList[i].FilePath), i, FList.Count - 1]));
      FList.Delete(i);
    end;
end;

function TIncludeBuffer.Find(FilePath: String): TBufferItem;
var
  i: Int32;
begin
  Result := nil;

  for i := 0 to FList.Count - 1 do
    if (FList[i].FilePath = FilePath) then
      Exit(FList[i]);
end;

function TIncludeBuffer.GetInclude(FilePath: String; Parser: TCodeParser): TInclude;

  function ParseFile: TBufferItem;
  begin
    Result := TBufferItem.Create;
    Result.FilePath := FilePath;
    Result.Assign(Parser);
    Result.Run;
    Result.Age := FileAge(FilePath);

    FList.Add(Result);
  end;

var
  Include: TBufferItem;
begin
  CriticalSection.Enter;

  try
    Clean();
    Include := Find(FilePath);

    // No buffer for file, let's create one...
    if (Include = nil) then
      Include := ParseFile()
    else if (Include.Age <> FileAge(FilePath)) then
    begin
      Writeln('File "', ExtractFileName(Include.FilePath), '" has been updated, parsing...');

      Include.Outdated := True;
      Include := ParseFile();
    end;

    // Make a copy type thing...
    Result := TInclude.Create(Include, Parser.OnInclude);
  finally
    CriticalSection.Leave;
  end;
end;

constructor TIncludeBuffer.Create;
begin
  FList := TList.Create(True);
end;

destructor TIncludeBuffer.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

initialization
  IncludeBuffer := TIncludeBuffer.Create;
  CriticalSection := TCriticalSection.Create;

finalization
  IncludeBuffer.Free;
  CriticalSection.Free;

end.

