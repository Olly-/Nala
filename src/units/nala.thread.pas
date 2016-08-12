unit nala.Thread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TThreadedMethod = procedure(constref Data: Pointer);
  PThreadedMethod = ^TThreadedMethod;

  TNalaThread = class(TThread)
  private
    FMethod: TThreadedMethod;
    FData: Pointer;
  protected
    procedure Execute; override;
  public
    constructor Create(Method: TThreadedMethod; Data: Pointer);
  end;

  TNalaThreadList = class(TObject)
  private
  type
    TList = specialize TFPGList<TNalaThread>;
  private
    FList: TList;

    procedure OnThreadTerminate(Sender: TObject);
  public
    procedure Add(Thread: TNalaThread);
    function Pop: TNalaThread;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure TNalaThreadList.OnThreadTerminate(Sender: TObject);
var
  i: Int32;
begin
  for i := 0 to FList.Count - 1 do
    if (Sender = FList[i]) then
    begin
      FList.Delete(i);
      Exit;
    end;
end;

procedure TNalaThreadList.Add(Thread: TNalaThread);
begin
  Thread.OnTerminate := @Self.OnThreadTerminate;
  FList.Add(Thread);
end;

function TNalaThreadList.Pop: TNalaThread;
begin
  Result := nil;

  if (FList.Count > 0) then
  begin
    Result := FList.Last;
    FList.Delete(FList.Count - 1);
  end;
end;

constructor TNalaThreadList.Create;
begin
  inherited Create;

  FList := TList.Create;
end;

destructor TNalaThreadList.Destroy;
begin
  FList.Free;

  inherited Destroy;
end;

procedure TNalaThread.Execute;
begin
  FMethod(FData);
end;

constructor TNalaThread.Create(Method: TThreadedMethod; Data: Pointer);
begin
  inherited Create(False);

  FreeOnTerminate := True;

  FMethod := Method;
  FData := Data;
end;

end.

