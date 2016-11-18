unit nala.TemplateForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, ButtonPanel,
  nala.SynEdit;

type

  { TTemplateTab }

  TTemplateTab = class(TPage)
  private
    FSynEdit: TNalaSynEdit;
    FPath: String;
  public
    property Path: String read FPath write FPath;

    procedure Load;
    procedure Save;

    constructor Create(TheOwner: TComponent); override;
  end;

  { TTemplateForm }

  TTemplateForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonPanel1: TButtonPanel;
    ListBox: TListBox;
    Notebook: TNotebook;
    Panel1: TPanel;
    SynEdit: TNalaSynEdit;

    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure OKButtonClick(Sender: TObject);
  private
    FDirectory: String;

    function AddTab(Path: String = ''): TTemplateTab;

    procedure LoadFromDirectory(Directory: String);
    procedure UpdateFiles;
  end;

var
  TemplateForm: TTemplateForm;

implementation

uses
  LazFileUtils, FileUtil, nala.Environment;

{$R *.lfm}

{ TTemplateTab }

procedure TTemplateTab.Load;
begin
  FSynEdit.Load(FPath);
end;

procedure TTemplateTab.Save;
begin
  FSynEdit.Save(FPath);
end;

constructor TTemplateTab.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  FSynEdit := TNalaSynEdit.Create(Self);
  with FSynEdit do
  begin
    BorderSpacing.Top := 4;
    BorderSpacing.Right := 4;
    BorderSpacing.Bottom := 4;

    ScrollBars := ssAutoBoth;
    BorderStyle := bsSingle;

    Lines.AddStrings(['program new;', 'begin', 'end;']);
  end;
end;

{ TTemplateForm }

procedure TTemplateForm.FormCreate(Sender: TObject);
begin
  ActiveControl := Notebook;
  LoadFromDirectory(NalaEnvironment.Paths['Templates']);
end;

procedure TTemplateForm.ListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  if (ListBox.ItemIndex >= 0) then
    NoteBook.ShowControl(TTemplateTab(ListBox.Items.Objects[ListBox.ItemIndex]));
end;

procedure TTemplateForm.OKButtonClick(Sender: TObject);
begin
  UpdateFiles;
end;

function TTemplateForm.AddTab(Path: String): TTemplateTab;
begin
  Result := TTemplateTab.Create(Notebook);
  Result.Parent := Notebook;
  Result.Path := Path;

  if (LazFileUtils.FileExistsUTF8(Result.Path)) then
    Result.Load;
end;

procedure TTemplateForm.LoadFromDirectory(Directory: String);
var
  Files: TStringList;
  i: Int32;
begin
  FDirectory := Directory;
  Files := FindAllFiles(FDirectory, '*.nala');

  try
    for i := 0 to Files.Count - 1 do
      ListBox.AddItem(ExtractFileNameWithoutExt(ExtractFileName(Files[i])), Self.AddTab(Files[i]));
  finally
    Files.Free;
  end;
end;

procedure TTemplateForm.UpdateFiles;
var
  i: Int32;
begin
  for i := 0 to ListBox.Count - 1 do
    TTemplateTab(ListBox.Items.Objects[i]).Save;
end;

procedure TTemplateForm.Button1Click(Sender: TObject);
var
  Str: String;
begin
  if (InputQuery('New Template', 'Template Name:', Str)) then
  begin
    ListBox.AddItem(Str, Self.AddTab(FDirectory + Str + '.nala'));
    Notebook.PageIndex := Notebook.PageCount - 1;
  end;
end;

procedure TTemplateForm.Button2Click(Sender: TObject);
var
  Index: Int32;
begin
  Index := ListBox.ItemIndex;

  if (Index > -1) then
  begin
    LazFileUtils.DeleteFileUTF8(TTemplateTab(ListBox.Items.Objects[Index]).Path);
    TTemplateTab(ListBox.Items.Objects[Index]).Hide;
    ListBox.Items.Delete(Index);
  end;
end;

end.

