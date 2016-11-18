unit nala.ParameterHint;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SynEdit, Forms, Types, nala.Types, Graphics,
  nala.Parser.Code, nala.Parser.Script, nala.DrawString, SynEditKeyCmds;

const
  ecParameterHint = ecUserFirst + 1;

type
  TNalaParameterHint = class(THintWindow)
  private
    FEditor: TSynEdit;
    FParser: TScriptParser;
    FMethod: TDictionaryItem_Method;
    FBracketPos: Int32;
    FMethodPos: Int32;

    function CountCommas: Int32;
    function Finished: Boolean;

    function GetMethodName: String;
    function GetMethodTree: String;

    function Draw(APaint: Boolean): TPoint;
    procedure SetEditor(AEditor: TSynEdit);
  public
    property Editor: TSynEdit read FEditor write SetEditor;


    procedure Paint; override;
    procedure Execute;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  nala.SynEdit, LCLType;

function TNalaParameterHint.Draw(APaint: Boolean): TPoint;
var
  i, CommaCount: Int32;

   procedure PaintMethod(Method: TCPMethod);

     function BoldString(Str: String; Bold: Boolean): String;
     begin
       Result := Str;

       if (Bold) then
         Result := '[B+]' + Str + '[B-]';
     end;

   var
     i, X: Int32;
     Str: TDrawString;
     Bold: Boolean = False;
   begin
     Str := Method.Name;
     Str += '(';

     for i := 0 to Method.ParameterCount - 1 do
       with Method.Parameter[i] do
       begin
         if (i = CommaCount) then
           Bold := True;

         Str += BoldString(Name, (i = CommaCount));

         if (not Last) then
           Str += ', '
         else
         begin
           Str += BoldString(': ' + Typ, Bold);
           if (i < (Method.ParameterCount - 1)) then
             Str += ';';

           if (Bold) then
             Bold := False;
         end;
       end;

     Str += ')';

     if (APaint) then
       Canvas.FillRect(0, Result.Y, Self.Width, Result.Y + FEditor.LineHeight + 1);

     X := Str.Draw(2, Result.Y, Canvas, APaint).X + 1;
     if (X > Result.X) then
       Result.X := X;

     Inc(Result.Y, FEditor.LineHeight + 1);
   end;

begin
  Result := Point(0, 1);

  if (Finished) then
  begin
    Visible := False;
    Exit;
  end;

  CommaCount := CountCommas();

  Canvas.Font.Color := clBlack;
  Canvas.Brush.Color := clListEven;

  with FMethod do
  begin
    PaintMethod(Decl);

    for i := 0 to High(Overloads) do
    begin
      if (Odd(i + 1)) then
        Canvas.Brush.Color := clListOdd
      else
        Canvas.Brush.Color := clListEven;

      PaintMethod(Overloads[i]);
    end;
  end;

  if (APaint) then
  begin
    Canvas.Brush.Color := clBlack;
    Canvas.FrameRect(0, 0, Width, Height);
  end;

  Inc(Result.Y, 1);
end;

procedure TNalaParameterHint.setEditor(AEditor: TSynEdit);
begin
  FEditor := AEditor;

  with AEditor.KeyStrokes.Add do
  begin
    Command := ecParameterHint;
    Key := VK_SPACE;
    Shift := [ssCtrl, ssShift];
  end;
end;

// Counts the commas between bracket open and the caret
function TNalaParameterHint.CountCommas: Int32;
var
  i: Int32;
  InParameters: Boolean = False;
begin
  Result := 0;

  for i := FBracketPos + 1 to FEditor.SelStart - 1 do
    case Editor.Text[i] of
      ',':
        if (not InParameters) then
          Inc(Result);
      '(':
        InParameters := True;
      ')':
        InParameters := False;
    end;
end;

function TNalaParameterHint.Finished: Boolean;
var
  i: Int32;
  InParameters: Boolean = False;
begin
  if (FEditor.SelStart <= FBracketPos) then
    Exit(True);

  for i := FBracketPos + 1 to FEditor.SelStart - 1 do
    case Editor.Text[i] of
      '(':
        InParameters := True;
      ')':
        if (not InParameters) then
          Exit(True)
        else
          InParameters := False;
    end;

  Exit(False);
end;

procedure TNalaParameterHint.Paint;
var
  Size: TPoint;
begin
  Size := Draw(True);

  Width := Size.X;
  Height := Size.Y;
end;

function TNalaParameterHint.GetMethodName: String;
var
  i: Int32;
  LineText: String;
  InParameters: Boolean = False;
begin
  LineText := Copy(FEditor.LineText, 1, FEditor.CaretX - 1);
  FBracketPos := FEditor.SelStart;

  while (Length(LineText) > 0) and ((LineText[Length(LineText)] <> '(') or (InParameters)) do
  begin
    if (LineText[Length(LineText)] = ')') then
      InParameters := True
    else
    if (LineText[Length(LineText)] = '(') then
      InParameters := False;

    Delete(LineText, Length(LineText), 1);
    Dec(FBracketPos);
  end;

  if (FEditor.Text[FBracketPos] <> '(') then
    Dec(FBracketPos);

  Delete(LineText, Length(LineText), 1); // Delete bracket open

  for i := Length(LineText) downto 1 do
  begin
    FMethodPos := FBracketPos - (Length(LineText) - i);

    if (LineText[i] in [' ', ',', '.']) then
      Exit(Copy(LineText, i + 1, High(Int32)))
    else
    if (i = 1) then
      Exit(LineText);
  end;

  Exit('');
end;

function TNalaParameterHint.GetMethodTree: String;
var
  i: Int32;
  InParameters: Boolean = False;
begin
  Result := '';

  i := FMethodPos - 1;
  while (FEditor.Text[i] = ' ') do
    Dec(i);

  if (FEditor.Text[i] <> '.') then
    Exit;

  for i := i downto 1 do
    case FEditor.Text[i] of
      '(':
        if (not InParameters) then
          Break
        else
          InParameters := False;
      ')':
        InParameters := True;
      ' ':
        if (not InParameters) then
          Break;
      else
        if (not InParameters) then
          Result := FEditor.Text[i] + Result;
    end;

  Result := Lowercase(Result);
end;

procedure TNalaParameterHint.Execute;
var
  Decl: TDeclaration;
  r: TRect;
  p, Size: Types.TPoint;

  Method: record Name, Tree: String; end;
begin
  if (FParser <> nil) then
    FreeAndNil(FParser);

  Method.Name := GetMethodName();
  Method.Tree := GetMethodTree();

  if (Method.Name <> '') then
  begin
    FParser := TNalaSynEdit(FEditor).Parse(False);

    if (Method.Tree <> '') then
    begin
      try
        Decl := FParser.TypeOfExpression(Method.Tree);
      except
        Exit;

      end;
      if (Decl.ClassType = TCPType) and (FParser.Dictionary.HasMethod(TCPType(Decl).Name + '.' + Method.Name)) then
        FMethod := FParser.Dictionary.GetMethod(TCPType(Decl).Name + '.' + Method.Name);

    end else
    if (FParser.Dictionary.HasMethod(Method.Name)) then
      FMethod := FParser.Dictionary.GetMethod(Method.Name);

    if (FMethod <> nil) then
    begin
      P.X := FEditor.CaretX - (FEditor.SelStart - FMethodPos);
      P.Y := FEditor.CaretYPix;
      P := FEditor.ClientToScreen(Point(FEditor.RowColumnToPixels(P).X, P.Y));

      Size := Draw(False);

      r.Left := p.x - 2;
      r.Top := (p.y - Size.Y);
      r.Right := (r.Left + Size.X);
      r.Bottom := p.y;

      ActivateWithBounds(r, '');
      Invalidate;
    end;
  end;
end;

constructor TNalaParameterHint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FParser := nil;
end;

destructor TNalaParameterHint.Destroy;
begin
  if (FParser <> nil) then
    FreeAndNil(FParser);

  inherited Destroy;
end;

end.

