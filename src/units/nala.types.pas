unit nala.Types;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils;

{ Records }

type
  PPoint = ^TPoint;
  TPoint = packed record X, Y: Int32; end;

  PBox = ^TBox;
  TBox = packed record X1, Y1, X2, Y2: Int32; end;

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
  P3DPointArray = ^T3DPointArray;

  // Boolean
  TBoolArray = array of Boolean;
  T2DBoolArray = array of TBoolArray;
  T3DBoolArray = array of T2DBoolArray;

  PBoolArray = ^TBoolArray;
  P2DBoolArray = ^T2DBoolArray;
  P3DBoolArray = ^T3DBoolArray;

  // String
  TStringArray = array of String;
  T2DStringArray = array of TStringArray;
  T3DStringArray = array of T2DStringArray;

  PStringArray = ^TStringArray;
  P2DStringArray = ^T2DStringArray;
  P3DStringArray = ^T3DStringArray;

  // Integer
  TIntArray = array of Int32;
  T2DIntArray = array of TIntArray;
  T3DIntArray = array of T2DIntArray;

  PIntArray = ^TIntArray;
  P2DIntArray = ^T2DIntArray;
  P3DIntArray = ^T3DIntArray;

  // Double
  TDoubleArray = array of Double;
  T2DDoubleArray = array of TDoubleArray;
  T3DDoubleArray = array of T2DDoubleArray;

  PDoubleArray = ^TDoubleArray;
  P2DDoubleArray = ^T2DDoubleArray;
  P3DDoubleArray = ^T3DDoubleArray;

  // Box
  TBoxArray = array of TBox;
  T2DBoxArray = array of TBoxArray;
  T3DBoxArray = array of T2DBoxArray;

  PDBoxArray = ^TBoxArray;
  P2DBoxArray = ^T2DBoxArray;
  P3DBoxArray = ^T3DBoxArray;

implementation

end.

