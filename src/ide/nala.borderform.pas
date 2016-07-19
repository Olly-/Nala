unit nala.BorderForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLType;

type

  { TPickerForm }

  EPickerForm = (pfLeft, pfTop, pfRight, pfBottom, pfMiddle);
  TPickerForm = class(TObject)
  private
  const
    BorderSize = 3;
  private
    FForms: array[EPickerForm] of TForm;
  public
    procedure Show(ALeft, ATop, AWidth, AHeight: Integer);
    function IsHandle(AHandle: HWND): Boolean;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF WINDOWS}
  Windows
{$ENDIF}
{$IFDEF LINUX}
  Gtk2Proc
{$ENDIF};

{ TPickerForm }

procedure TPickerForm.Show(ALeft, ATop, AWidth, AHeight: Integer);
var
  i: Integer;
begin
  FForms[pfLeft].SetBounds(ALeft, ATop, BorderSize, AHeight);
  FForms[pfRight].SetBounds(ALeft + (AWidth - BorderSize), ATop, BorderSize, AHeight);
  FForms[pfTop].SetBounds(ALeft + BorderSize, ATop, AWidth - (BorderSize * 2), BorderSize);
  FForms[pfBottom].SetBounds(ALeft + BorderSize, (ATop + AHeight) - BorderSize, AWidth - (BorderSize * 2), BorderSize);
  FForms[pfMiddle].SetBounds(ALeft, ATop, AWidth, AHeight);

  for i := 0 to Length(FForms) - {$IFDEF WINDOWS}1{$ELSE}2{$ENDIF} do
    FForms[EPickerForm(i)].Show;
end;

function TPickerForm.IsHandle(AHandle: HWND): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(FForms) - 1 do
  {$IFDEF LINUX}
    if (FormToX11Window(FForms[EPickerForm(i)]) = AHandle) then
      Exit(True);
  {$ENDIF}
  {$IFDEF WINDOWS}
    if (FForms[EPickerForm(i)].Handle = AHandle) then
      Exit(True);
  {$ENDIF}
end;

constructor TPickerForm.Create;
var
  i: Integer;
begin
  for i := 0 to Length(FForms) - 1 do
  begin
    FForms[EPickerForm(i)] := TForm.CreateNew(nil);
    FForms[EPickerForm(i)].Color := clGreen;
    FForms[EPickerForm(i)].ShowInTaskBar := stNever;
    FForms[EPickerForm(i)].BorderStyle := bsNone;
  end;

  {$IFDEF WINDOWS}
    with FForms[pfMiddle] do
    begin
      AlphaBlend := True;
      AlphaBlendValue := 30;
      SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_TRANSPARENT);
    end;
  {$ENDIF}
end;

destructor TPickerForm.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(FForms) - 1 do
    FForms[EPickerForm(i)].Free;

  inherited Destroy;
end;

end.


