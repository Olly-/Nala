unit nala.ClientPicker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nala.MouseBase, nala.Mouse, LCLType, Controls;

function PickClient: HWND;

implementation

uses
  Nala.BorderForm, nala.OSUtils,
  Forms, Graphics, nala.Window;

function PickClient: HWND;
var
  Window: TNalaWindow;
  Form: TPickerForm;
  Mouse: TNalaMouse;
  Handle: HWND;
begin
  Result := 0;

  Window := TNalaWindow.Create;
  Mouse := TNalaMouse.Create;
  Form := TPickerForm.Create;

  try
    writeln('test');
    while (not Mouse.Pressed(nala.MouseBase.mbLeft)) do
    begin
      Handle := OSUtils.WindowFromPoint(Mouse.GetPosition(nil));

      if (not Form.IsHandle(Handle)) and (Result <> Handle) then
      try
        Window.Handle := Handle;
        Form.Show(Window.Left, Window.Top, Window.Width, Window.Height);

        Result := Handle;
      except
      end;

      Application.ProcessMessages;
      Sleep(50);
    end;
    writeln('done');
  finally
    Window.Free;
    Mouse.Free;
    Form.Free;
  end;
end;

end.

