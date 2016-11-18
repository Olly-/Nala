unit nala.CompletionItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  nala.Parser.Code, nala.Parser.Script, nala.DrawString, nala.Types, Forms;

type
  TCompletionItem_Base = class
  private
  const
    clOdd = $E7E7E7;
    clEven = $CFCFCF;
  private
    FSize: TPoint;
    FImage: Int32;
    FName: String;
    FHintForm: THintWindow;

    function GetSize: TPoint; virtual;
  public
    property Size: TPoint read GetSize;
    property Image: Int32 read FImage;
    property Name: String read FName;

    procedure Paint(X, Y: Int32; Odd: Boolean); virtual;
    constructor Create(HintForm: THintWindow);
  end;

  TCompletionItem = class(TCompletionItem_Base) // Generic methods for one liners
  private
    FItem: TDrawString;

    function GetSize: TPoint; override;
  public
    procedure Paint(X, Y: Int32; Odd: Boolean); override;
  end;

  TCompletionItem_Variable = class(TCompletionItem)
  public
    constructor Create(Variable: TCPVariable; HintForm: THintWindow);
    constructor Create(VarName, VarType, VarDefault: String; HintForm: THintWindow);
  end;

  TCompletionItem_Constant = class(TCompletionItem)
  public
    constructor Create(Constant: TCPConstant; HintForm: THintWindow);
  end;

  TCompletionItem_Method = class(TCompletionItem_Base)
  private
  const
    DOC_OFFSET = 8;
  private
  type
    TItem = record Header: TDrawString; Documentation: TDrawString; end;
    TItems = array of TItem;
  private
    FItems: TItems;

    function GetSize: TPoint; override;
  public
    procedure Paint(X, Y: Int32; Odd: Boolean); override;

    constructor Create(Method: TDictionaryItem_Method; HintForm: THintWindow);
  end;

  TCompletionItem_Record = class(TCompletionItem)
  public
    constructor Create(Typ: TCPType; HintForm: THintWindow);
  end;

  TCompletionItem_Enum = class(TCompletionItem)
  public
    constructor Create(Typ: TCPType; HintForm: THintWindow);
  end;

  TCompletionItem_Type = class(TCompletionItem)
  public
    constructor Create(Typ: TCPType; HintForm: THintWindow);
  end;

implementation

uses
  nala.AutoComplete;

{ TCompletionItem_Enum }

constructor TCompletionItem_Enum.Create(Typ: TCPType; HintForm: THintWindow);
var
  i, c: Int32;
  Enum: TCPEnum;
begin
  inherited Create(HintForm);

  Enum := Typ.Kind.GetEnum();
  c := 0;

  FName := Typ.Name;
  FImage := 4;

  FItem := '[C=clBlack][B+]' + 'type ' + Typ.Name + ' = ' + LineEnding + '  ';
  FItem += '(';

  for i := 0 to Enum.EnumCount - 1 do
  begin
    FItem += Enum.Enums[i].Name;
    if (Enum.Enums[i].Value <> '') then
      FItem += ' = ' + Enum.Enums[i].Value;
    if (i < (Enum.EnumCount - 1)) then
      FItem += ', '
    else
      FItem += ')';

    Inc(c);
    if (c = 5) then
    begin
      FItem += LineEnding + '   ';
      c := 0;
    end;
  end;

  FItem += '[B-]';
end;

constructor TCompletionItem_Type.Create(Typ: TCPType; HintForm: THintWindow);
begin
  inherited Create(HintForm);

  FName := Typ.Name;
  FImage := 4;

  FItem := '[C=clBlack][B+]' + 'type ' + Typ.Format(True) + '[B-]';
end;

constructor TCompletionItem_Record.Create(Typ: TCPType; HintForm: THintWindow);
var
  i: Int32;
begin
  inherited Create(HintForm);

  FName := Typ.Name;
  FImage := 4;

  with Typ.Kind.GetRecord do
  begin
    FItem := '[C=clBlack][B+]';
    FItem += 'type' + LineEnding;
    FItem += '  ' + Typ.Name + ' = record';
    if (Ancestor <> '') then
      FItem += '(' + Ancestor + ')';
    FItem += LineEnding;
    for i := 0 to FieldCount - 1 do
      FItem += '    ' + Fields[i].Name + ': ' + Fields[i].Typ + ';' + LineEnding;
    FItem += 'end';
    FItem += '[B-]';
  end;
end;

function TCompletionItem_Base.GetSize: TPoint;
begin
  raise Exception.Create('GetSize not implemented');
end;

procedure TCompletionItem_Base.Paint(X, Y: Int32; Odd: Boolean);
begin
  raise Exception.Create('Paint not implemented');
end;

constructor TCompletionItem_Base.Create(HintForm: THintWindow);
begin
  FSize.X := 0;
  FSize.Y := 0;
  FImage := 0;
  FName := '';
  FHintForm := HintForm;
end;

function TCompletionItem.GetSize: TPoint;
begin
  if (FSize.X = 0) and (FSize.Y = 0) and (FHintForm <> nil) then
    FSize := FItem.Draw(0, 0, FHintForm.Canvas, False);

  Result := FSize;
end;

procedure TCompletionItem.Paint(X, Y: Int32; Odd: Boolean);
begin
  if (FSize.X = 0) or (FSize.Y = 0) or (FHintForm = nil) then
    Exit;

  if (Odd) then
    FHintForm.Canvas.Brush.Color := clOdd
  else
    FHintForm.Canvas.Brush.Color := clEven;

  FHintForm.Canvas.FillRect(1, Y, FHintForm.Width - 1, Y + FSize.Y + 2);
  FHintForm.Canvas.Brush.Style := bsClear;

  FItem.Draw(X, Y, FHintForm.Canvas);
end;

constructor TCompletionItem_Variable.Create(Variable: TCPVariable; HintForm: THintWindow);
begin
  inherited Create(HintForm);

  FName := Variable.Name;
  FImage := 1;

  FItem := '[C=clBlack][B+]var ' + Variable.Name + ': ' + Variable.Typ;
  if (Variable.Default <> '') then
    FItem += ' = ' + Variable.Default;
  FItem += '[B-]';
end;

constructor TCompletionItem_Variable.Create(VarName, VarType, VarDefault: String; HintForm: THintWindow);
begin
  inherited Create(HintForm);

  FName := VarName;
  FImage := 1;

  FItem := '[C=clBlack][B+]var ' + VarName + ': ' + VarType;
  if (VarDefault <> '') then
    FItem += ' = ' + VarDefault;
  FItem += '[B-]';
end;

constructor TCompletionItem_Constant.Create(Constant: TCPConstant; HintForm: THintWindow);
begin
  inherited Create(HintForm);

  FName := Constant.Name;
  FImage := 0;

  FItem := '[C=clBlack][B+]const ' + Constant.Name;
  if (Constant.Typ <> '') then
    FItem += ': ' + Constant.Typ;
  FItem += ' = ' + Constant.Default;
end;

function TCompletionItem_Method.GetSize: TPoint;
var
  P: TPoint;
  i: Int32;
begin
  if (FSize.X = 0) and (FSize.Y = 0) and (FHintForm <> nil) then
    for i := 0 to High(FItems) do
    begin
      // Header
      P := FItems[i].Header.Draw(0, 0, FHintForm.Canvas, False);
      if (P.X > FSize.X) then
        FSize.X := P.X;
      Inc(FSize.Y, P.Y);

      // Documentation
      P := FItems[i].Documentation.Draw(0, 0, FHintForm.Canvas, False);
      if ((P.X + DOC_OFFSET) > FSize.X) then
        FSize.X := (P.X + DOC_OFFSET);
      Inc(FSize.Y, P.Y);

      // Line spacing
      if (i < High(FItems)) then
        Inc(FSize.Y, 2);
    end;

  Result := FSize;
end;

procedure TCompletionItem_Method.Paint(X, Y: Int32; Odd: Boolean);
var
  P: TPoint;
  i, ItemHeight: Int32;
begin
  if (FSize.X = 0) or (FSize.Y = 0) or (FHintForm = nil) then
    Exit;

  for i := 0 to High(FItems) do
  begin
    ItemHeight := FItems[i].Header.Draw(0, 0, FHintForm.Canvas, False).Y +
                  FItems[i].Documentation.Draw(0, 0, FHintForm.Canvas, False).Y + 2;

    if (Odd) then
      FHintForm.Canvas.Brush.Color := clOdd
    else
      FHintForm.Canvas.Brush.Color := clEven;

    FHintForm.Canvas.FillRect(1, Y, FHintForm.Width - 1, Y + ItemHeight);
    FHintForm.Canvas.Brush.Style := bsClear;

    P := FItems[i].Header.Draw(X, Y, FHintForm.Canvas);
    Inc(Y, P.Y);

    P := FItems[i].Documentation.Draw(X + DOC_OFFSET, Y + 1, FHintForm.Canvas);
    Inc(Y, P.Y);
    Inc(Y, 2);

    Odd := not Odd;
  end;
end;

constructor TCompletionItem_Method.Create(Method: TDictionaryItem_Method; HintForm: THintWindow);

  procedure AddDeclaration(Decl: TCPMethod);
  var
    Len: Int32;
    Name: String;
  begin
    if (mdOverride in Decl.Directives) then
      Exit;

    Len := Length(FItems);
    SetLength(FItems, Len + 1);

    with FItems[Len] do
    begin
      if (Decl.isMethodOfObject) then
        Name := Decl.ObjectType + '.' + Decl.Name
      else
        Name := Decl.Name;

      if (Decl.ReturnType = '') then
        Header := '[B+][c=clBlack]procedure [C=clBlack]' + Name  + '[I+]' + Decl.Params + '[I-][B-]'
      else
        Header := '[B+][C=clBlack]function [C=clBlack]' + Name + '[I+]' + Decl.Params + '[I-]' + ': [C=clBlack]' + Decl.ReturnType + '[C=clBlack][B-]';

      if (Decl.Documentation <> nil) then
        Documentation := '[H=' + IntToStr(TNalaSynCompletionFormHint(FHintForm).FontHeight - 2) + '][C=clNavy]' + Decl.Documentation.Description + '[C=clBlack][B-][H=' + IntToStr(TNalaSynCompletionFormHint(FHintForm).FontHeight) + ']'
      else
        Documentation := '';
    end;
  end;

var
  i: Int32;
begin
  inherited Create(HintForm);

  FName := Method.Decl.Name;

  if (Method.Decl.ReturnType = '') then
    FImage := 3
  else
    FImage := 2;

  AddDeclaration(Method.Decl);
  for i := 0 to High(Method.Overloads) do
    AddDeclaration(Method.Overloads[i]);
end;

end.

