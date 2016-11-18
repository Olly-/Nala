program Nala;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
    cthreads,
  {$ENDIF}

  Classes, SysUtils, Interfaces, Forms, anchordockpkg, lazcontrols,

  nala.MainForm, nala.Panel, nala.SynEdit, nala.Messages, nala.CodeExplorer,
  nala.Helpers, nala.ScriptThread, nala.Imports.Core, nala.Imports.Time,
  nala.Time, nala.WindowBase, nala.ClientPanel, nala.Bitmap, nala.ColorMath,
  nala.ColorPanel, nala.MiscComponents, nala.BorderForm, nala.ColorPicker,
  nala.ScriptTab, nala.Imports.Strings, nala.TBox, nala.Imports.TBox,
  nala.LapeCompiler, nala.MouseBase, nala.Mouse, nala.Window, nala.Strings,
  nala.CodeTree, nala.AutoComplete, nala.Parser.Script, nala.Parser.Code,
  nala.Parser.Include, nala.Parser.DotExpr, nala.SynCompletion, nala.OSUtils,
  nala.Environment, nala.TemplateForm, nala.Types.TPoint, nala.Imports.TPoint,
  nala.Imports.Threading, nala.Thread, nala.DrawString, nala.ParameterHint,
  nala.MessageList;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  Application.Initialize;
  Application.CreateForm(TNalaForm, NalaForm);
  Application.CreateForm(TTemplateForm, TemplateForm);
  Application.Run;
end.
