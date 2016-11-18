unit nala.Types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  TPoint = Classes.TPoint;
  PPoint = ^TPoint;

  TBox = record X1, Y1, X2, Y2: Int32; end;
  PBox = ^TBox;

  PRGB32 = ^TRGB32;
  TRGB32 = packed record
    B, G, R, A: Byte;
  end;
  TRGB32Array = array of TRGB32;

  TRGB = packed record
    R, G, B: Byte;
  end;
  TRGBArray = array of TRGB;

  THSB = packed record
    H, S, B: Single;
  end;
  THSBArray = array of THSB;

  TWindowData = packed record
    Width, Height: Integer;
    BufferPtr: PRGB32;
    BufferWidth, BufferHeight: Integer;
  end;

  TVirtualKey = UInt32;

type
  // Point
  TPointArray = array of TPoint;
  T2DPointArray = array of TPointArray;
  T3DPointArray = array of T2DPointArray;

  PPointArray = ^TPointArray;
  P2DPointArray = ^T2DPointArray;

  // Boolean
  TBoolArray = array of Boolean;
  T2DBoolArray = array of TBoolArray;
  T3DBoolArray = array of T2DBoolArray;

  PBoolArray = ^TBoolArray;
  P2DBoolArray = ^T2DBoolArray;

  // String
  TStringArray = array of String;
  T2DStringArray = array of TStringArray;

  PStringArray = ^TStringArray;
  P2DStringArray = ^T2DStringArray;

  // Integer
  TIntArray = array of Int32;
  T2DIntArray = array of TIntArray;

  PIntArray = ^TIntArray;
  P2DIntArray = ^T2DIntArray;

  // Double
  TDoubleArray = array of Double;
  T2DDoubleArray = array of TDoubleArray;

  PDoubleArray = ^TDoubleArray;
  P2DDoubleArray = ^T2DDoubleArray;

  // Box
  TBoxArray = array of TBox;
  T2DBoxArray = array of TBoxArray;

  PDBoxArray = ^TBoxArray;
  P2DBoxArray = ^T2DBoxArray;

  // Char
  TCharArray = array of Char;
  T2DCharArray = array of TCharArray;

  PCharArray = ^TCharArray;
  P2DCharArray = ^T2DCharArray;

const
  clListOdd = $E7E7E7;
  clListEven = $CFCFCF;
  clListSelected = $D69500;

implementation

end.

