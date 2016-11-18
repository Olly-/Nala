unit nala.SynEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, SynEdit, SynEditMiscClasses,
  SynGutterLineOverview, SynEditKeyCmds, LCLType, Types, SynGutter, SynHighlighterLape,
  nala.AutoComplete, Dialogs, Messages, nala.ParameterHint, nala.Parser.Script;

type
  TNalaSynEdit = class(TSynEdit)
  private
    FLastModified: TDateTime;
    FOverView: TSynGutterLineOverview;
    FAutoComplete: TNalaAutoComplete;
    FParameterHint: TNalaParameterHint;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
  public
    function Parse(FullScript: Boolean): TScriptParser;
    procedure DoAutoComplete;

    property LastModified: TDateTime read FLastModified write FLastModified;

    procedure CommandProcessor(Command: TSynEditorCommand; AChar: TUTF8Char; Data: Pointer; ASkipHooks: THookedCommandFlags=[]); override;

    procedure Load(Path: String);
    procedure Save(Path: String);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Graphics, Forms, LazFileUtils, nala.MainForm, SynEditHighlighter, SynEditStrConst,
  nala.Environment, nala.Types;

var
  SyntaxHighlighter: TSynFreePascalSyn;

constructor TNalaSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Parent := AOwner as TWinControl;
  Align := alClient;
  BorderStyle := bsNone;
  OnSpecialLineMarkup := @DoSpecialLineMarkup;
  Highlighter := SyntaxHighlighter;
  Font.Quality := fqDefault;
  TabWidth := 2;
  BlockIndent := 2;

  Options := [eoAutoIndent, eoScrollPastEol, eoSmartTabs, eoTabsToSpaces,
              eoTrimTrailingSpaces, eoGroupUndo, eoBracketHighlight, eoShowScrollHint];
  Options2 := [eoFoldedCopyPaste, eoOverwriteBlock];

  Gutter.LineNumberPart.MarkupInfo.Background := Gutter.Color;

  TSynGutterSeparator.Create(RightGutter.Parts);
  FOverview := TSynGutterLineOverview.Create(RightGutter.Parts);
  with FOverview do
  begin
    AutoSize := False;
    Width := 12;

    TSynGutterLOvProviderCurrentPage.Create(Providers).Priority := 1;
    TSynGutterLOvProviderModifiedLines.Create(Providers).Priority := 2;
  end;

  FAutoComplete := TNalaAutoComplete.Create(nil);
  FAutoComplete.Editor := Self;

  FParameterHint := TNalaParameterHint.Create(nil);
  FParameterHint.Editor := Self;
end;

destructor TNalaSynEdit.Destroy;
begin
  FAutoComplete.Free;
  FParameterHint.Free;

  inherited Destroy;
end;

procedure TNalaSynEdit.CommandProcessor(Command: TSynEditorCommand; AChar: TUTF8Char; Data: Pointer; ASkipHooks: THookedCommandFlags);

  function isJunk: Boolean;
  var
    Attri: TSynHighlighterAttributes;
    Token: String;
  begin
    Result := (GetHighlighterAttriAtRowCol(Point(CaretX - 1, CaretY), Token, Attri)) and
              ((Attri.Name = SYNS_AttrComment) or (Attri.Name = SYNS_AttrString) or
               (Attri.Name = SYNS_AttrDirective) or (Attri.Name = SYNS_AttrNumber));
  end;

begin
  inherited CommandProcessor(Command, AChar, Data, ASkipHooks);

  (*
    ecDeleteLastChar  = 501;
    ecDeleteChar      = 502;
    ecDeleteWord      = 503;
    ecDeleteLastWord  = 504;
    ecDeleteLine      = 507;
    ecClearAll        = 508;
    ecLineBreak       = 509;
    ecInsertLine      = 510;
    ecChar            = 511;
    ecUndo            = 601;
    ecRedo            = 602;
    ecCut             = 603;
    ecPaste           = 604;
    ecString          = 630;
    ecTab             = 612;
  *)

  if ((Command >= ecDeleteLastChar) and (Command <= ecChar)) or
     ((Command >= ecUndo) and (Command <= ecPaste)) or
      (Command = ecString) or (Command = ecTab) then
      begin
        if (FParameterHint.Visible) then
          FParameterHint.Invalidate();

        FLastModified := Now();
      end;

  if ((Command = ecLeft) or (Command = ecRight) or (Command = ecUp) or (Command = ecDown)) and (FParameterHint.Visible) then
    FParameterHint.Invalidate()
  else
  if (AChar = '.') and (not isJunk()) then
    FAutoComplete.Execute('', ClientToScreen(Point(CaretXPix, CaretYPix + LineHeight + 1)))
  else
  if (AChar = '(') or (Command = ecParameterHint) and (not isJunk()) then
    FParameterHint.Execute();
end;

procedure TNalaSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  P := CaretXY;

  inherited MouseDown(Button, Shift, X, Y);

  if (FParameterHint.Visible) and ((CaretX <> P.X) or (CaretY <> P.Y)) then
    FParameterHint.Invalidate;
end;

procedure TNalaSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  if (FParameterHint.Visible) then
    FParameterHint.Hide;
end;

procedure TNalaSynEdit.DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
begin
  Special := False;
  {
  with AMarkup do
  begin
    Foreground := clNone;
    if (Odd(Line)) then
      Background := clListOdd
    else
      Background := clListEven;
    BackAlpha := 0;
  end;
  }
end;

function TNalaSynEdit.Parse(FullScript: Boolean): TScriptParser;
begin
  Result := TScriptParser.Create;
  Result.addSearchPath(NalaEnvironment.Paths['Includes']);

  if (FullScript) then
    Result.Run(Text, -1, SelStart)
  else
    Result.Run(Text, SelStart, SelStart);
end;

procedure TNalaSynEdit.DoAutoComplete;
begin
  FAutoComplete.Execute('', Point(CaretXPix, CaretYPix + LineHeight + 1))
end;

procedure TNalaSynEdit.Load(Path: String);
begin
  Lines.LoadFromFile(Path);
  MarkTextAsSaved;
end;

procedure TNalaSynEdit.Save(Path: String);
begin
  Lines.SaveToFile(Path);
  MarkTextAsSaved;
end;

initialization
  SyntaxHighlighter := TSynFreePascalSyn.Create(nil);

  with SyntaxHighlighter do
  begin
    CommentAttri.Foreground := clBlue;
    CommentAttri.Style := [fsBold];
    IdentifierAttri.Foreground := clDefault;
    NumberAttri.Foreground := clNavy;
    StringAttri.Foreground := clBlue;
    SymbolAttri.Foreground := clRed;
    DirectiveAttri.Foreground := clRed;
    DirectiveAttri.Style := [fsBold];
    NestedComments := False;
    StringKeywordMode := spsmNone;
  end;

finalization
  SyntaxHighlighter.Free;

end.

