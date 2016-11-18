unit nala.Environment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TNalaEnvironment }

  TNalaEnvironment = class(TObject)
  private
    FPaths: TStringList;
    FFiles: TStringList;

    function GetFile(Name: String): String;
    function GetPath(Name: String): String;
  public
    property Paths[Name: String]: String read GetPath;
    property Files[Name: String]: String read GetFile;

    constructor Create;
    destructor Destroy; override;
  end;

var
  NalaEnvironment: TNalaEnvironment;

implementation

uses
  LazFileUtils, Forms;

{ TNalaEnvironment }

function TNalaEnvironment.GetFile(Name: String): String;
begin
  Result := FFiles.Values[Name];
end;

function TNalaEnvironment.GetPath(Name: String): String;
begin
  Result := FPaths.Values[Name];
end;

constructor TNalaEnvironment.Create;
var
  i: Int32;
begin
  inherited Create;

  FPaths := TStringList.Create;
  FFiles := TStringList.Create;

  FPaths.Values['Nala'] := Application.Location;
  FPaths.Values['AppData'] := Application.Location + 'appdata' + DirectorySeparator;
  FPaths.Values['Includes'] := Application.Location + 'includes' + DirectorySeparator;
  FPaths.Values['Plugins'] := Application.Location + 'plugins' + DirectorySeparator;
  FPaths.Values['Templates'] := FPaths.Values['AppData'] + 'templates' + DirectorySeparator;

  FFiles.Values['Nala'] := Application.ExeName;
  FFiles.Values['MemoryLeaks'] := FPaths.Values['AppData'] + 'memoryleaks.txt';

  if (not DirectoryExistsUTF8(FPaths.Values['AppData'])) then
    CreateDirUTF8(FPaths.Values['AppData']);
  if (not DirectoryExistsUTF8(FPaths.Values['Includes'])) then
    CreateDirUTF8(FPaths.Values['Includes']);
  if (not DirectoryExistsUTF8(FPaths.Values['Plugins'])) then
    CreateDirUTF8(FPaths.Values['Plugins']);
  if (not DirectoryExistsUTF8(FPaths.Values['Templates'])) then
    CreateDirUTF8(FPaths.Values['Templates']);

  Writeln('Paths:');
  for i := 0 to FPaths.Count - 1 do
    Writeln(' - ', FPaths.Names[i], ' = ', FPaths.ValueFromIndex[i]);

  Writeln('Files:');
  for i := 0 to FFiles.Count - 1 do
    Writeln(' - ', FFiles.Names[i], ' = ', FFiles.ValueFromIndex[i]);
end;

destructor TNalaEnvironment.Destroy;
begin
  FPaths.Free;
  FFiles.Free;

  inherited Destroy;
end;

initialization
  NalaEnvironment := TNalaEnvironment.Create;

finalization
  NalaEnvironment.Free;

end.

