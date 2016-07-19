unit nala.SynEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, SynEdit, SynEditMarkup, SynEditTypes, SynEditMiscClasses,
  SynGutterLineOverview, SynEditKeyCmds, LCLType, Types, SynGutter, SynHighlighterLape,
  nala.AutoComplete;

type
  { TNalaSynEdit }

  TNalaSynEdit = class(TSynEdit)
  private
    FLastModified: TDateTime;
    FOverView: TSynGutterLineOverview;
    FAutoComplete: TNalaAutoComplete;

    procedure DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
  public
    property LastModified: TDateTime read FLastModified write FLastModified;

    procedure ExecuteCommand(Command: TSynEditorCommand; const AChar: TUTF8Char; Data: pointer); override;

    procedure Load(Path: String);
    procedure Save(Path: String);

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Graphics, Forms;

var
  SynEditHighlighter: TSynFreePascalSyn;

constructor TNalaSynEdit.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited Create(AOwner);

  Parent := AOwner as TWinControl;
  Align := alClient;
  BorderStyle := bsNone;
  RightEdge := 90;
  OnSpecialLineMarkup := @DoSpecialLineMarkup;
  Highlighter := SynEditHighlighter;

  Options := [eoAutoIndent, eoScrollPastEol, eoSmartTabs, eoTabsToSpaces,
              eoTrimTrailingSpaces, eoGroupUndo, eoBracketHighlight, eoShowScrollHint] - [eoScrollPastEol];
  Options2 := [eoFoldedCopyPaste, eoOverwriteBlock];

  TSynGutterSeparator.Create(RightGutter.Parts);
  FOverview := TSynGutterLineOverview.Create(RightGutter.Parts);
  with FOverview do
  begin
    AutoSize := False;
    Width := 12;

    with TSynGutterLOvProviderModifiedLines.Create(Providers) do
      Priority := 2;
    with TSynGutterLOvProviderCurrentPage.Create(Providers) do
      Priority := 1;
  end;

  FAutoComplete := TNalaAutoComplete.Create(nil);
  FAutoComplete.Editor := Self;
end;

destructor TNalaSynEdit.Destroy;
begin
  FAutoComplete.Free;

  inherited Destroy;
end;

procedure TNalaSynEdit.ExecuteCommand(Command: TSynEditorCommand;
  const AChar: TUTF8Char; Data: pointer);
begin
  inherited ExecuteCommand(Command, AChar, Data);

  (*
    ecDeleteLastChar  = 501;
    ecDeleteChar      = 502;
    ecDeleteWord      = 503;
    ecDeleteLastWord  = 504;
    ecDeleteLine      = 507;
    ecClearAll        = 508;
    ecLineBreak       = 509;
    ecInsertLine      = 510
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
       FLastModified := Now();
end;

procedure TNalaSynEdit.DoSpecialLineMarkup(Sender: TObject; Line: Integer; var Special: Boolean; AMarkup: TSynSelectedColor);
begin
  Special := False;
  {
  if (Lines.Objects[Line] <> nil) then
  begin
    if (Lines.Objects[Line] as TIntObject).Int = Ord(ltNone) then
      Exit;

    Special := True;
    case (Lines.Objects[Line] as TIntObject).Int of
      Ord(ltParsingError):
        begin
          AMarkup.Foreground := clNone;
          AMarkup.Background := clRed;
          AMarkup.BackAlpha := 15;
        end;
    end;
  end; }
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
  SynEditHighlighter := TSynFreePascalSyn.Create(nil);
finalization
  SynEditHighlighter.Free;

end.

