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
  X, Y: UInt32;
begin
  Result := 0;

  Window := TNalaWindow.Create;
  Mouse := TNalaMouse.Create;
  Form := TPickerForm.Create;

  try
    while (not Mouse.Pressed(nala.MouseBase.mbLeft)) do
    begin
      Handle := OSUtils.WindowFromPoint(Mouse.GetPosition(nil));

      if (not Form.IsHandle(Handle)) and (Result <> Handle) then
      try
        Window.Handle := Handle;

        if (Window.HandleIsVaild) then
        begin
          {$IFDEF WINDOWS}
          writeln('test');
          X := Window.Left;
            Y := Window.Top;
            if (Window.IsTopWindow) then
              Window.OffsetBorder(X, Y);
            Form.Show(X, Y, Window.Width, Window.Height);
          {$ELSE}
            Form.Show(Window.Left, Window.Top, Window.Width, Window.Height);
          {$ENDIF}

          Result := Handle;
        end;
      except
      end;

      Application.ProcessMessages;
      Sleep(50);
    end;
  finally
    Window.Free;
    Mouse.Free;
    Form.Free;
  end;
end;

end.

