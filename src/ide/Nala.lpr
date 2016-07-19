program Nala;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}

  Classes, SysUtils, Interfaces, Forms, anchordockpkg,

  nala.MainForm, nala.Panel, nala.SynEdit, nala.Messages,
  nala.CodeExplorer, nala.Helpers, nala.ScriptThread, nala.Imports.Core,
  nala.Imports.Time, nala.Time, nala.WindowBase, nala.ClientPanel, nala.Bitmap,
  nala.ColorMath, nala.ColorPanel, nala.MiscComponents, nala.BorderForm,
  nala.ColorPicker, nala.ScriptTab, nala.Imports.Strings, nala.TBox,
  nala.Imports.Box, nala.LapeCompiler, nala.MouseBase,
  nala.Mouse, nala.Window, nala.Strings, nala.CodeTree, nala.ScriptTree,
  nala.AutoComplete, nala.ScriptParser, nala.SynCompletion, nala.OSUtils;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Initialize;
  Application.CreateForm(TNalaForm, NalaForm);
  Application.Run;
end.
