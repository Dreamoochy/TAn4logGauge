unit An4logGauge;

//{$DEFINE TICKER}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls;

type
  TGaugeStyle = ( gsLeft, gsRight, gsCenter );
  TFaceOption = ( foShowFrame, foShowMargin, foShowCaption, foShowCircles,
                  foShow3D, foShowValues, foValuesOutside, foShowMainTicks,
                  foShowSubTicks, foShowIndicatorMin, foShowIndicatorMid,
                  foShowIndicatorMax, foShowCenter, foChangeArrowColor,
                  foChangeValuesColor, foChangeCirclesColor, foAutoUpdate );

  TFaceOptions = set of TFaceOption;
  TAntialiased = (aaNone, aaBiline, aaTriline, aaQuadral);

  TCoord = record
    W, H, M, X, Y, JW, JH, dK: Integer;
    A, C, dF: Double;
  end;

  TAn4logGauge = class(TCustomControl)
  private
    // face elements colors
    FMinColor     : TColor;
    FMidColor     : TColor;
    FMaxColor     : TColor;
    FFaceColor    : TColor;
    FTicksColor   : TColor;
    FValueColor   : TColor;
    FCaptionColor : TColor;
    FArrowColor   : TColor;
    FMarginColor  : TColor;
    FCenterColor  : TColor;
    FCircleColor  : TColor;
    // face elements sizes, etc.
    FCoord           : TCoord;
    FCenterRadius    : Integer;
    FCircleRadius    : Integer;
    FScaleAngle      : Integer;
    FMargin          : Integer;
    FStyle           : TGaugeStyle;
    FArrowWidth      : Integer;
    FNumMainTicks    : Integer;
    FNumSubTicks     : Integer;
    FLengthMainTicks : Integer;
    FLengthSubTicks  : Integer;
    FFaceOptions     : TFaceOptions;
    FValuesFormat    : array[0..4] of Char;
    FAutoUpdate      : Boolean;
    // values
    FScaleValue    : Integer; // internal = 100%
    FPosition      : Double;
    FValueDecimals : Byte;
    FMinimum       : Double;
    FMaximum       : Double;
    FMinMid        : Double;
    FMidMax        : Double;
    FCaption       : string;
    // event handlers
    FOverMax : TNotifyEvent;
    FOverMin : TNotifyEvent;
    // anti-aliasing mode
    FAntiAliased : TAntialiased;
    // internal bitmaps
    FBackBitmap : TBitmap;
    FFaceBitmap : TBitmap;
    FAABitmap   : TBitmap;
{$IFDEF TICKER}
    // performance tracking
    FTicker   : Int64;
    FPeriod   : Int64;
    FFrames   : Integer;
    FOnFrames : TNotifyEvent;
{$ENDIF}
    // set properties
    procedure SetFMinColor(C: TColor);
    procedure SetFMidColor(C: TColor);
    procedure SetFMaxColor(C: TColor);
    procedure SetFFaceColor(C: TColor);
    procedure SetFTicksColor(C: TColor);
    procedure SetFValueColor(C: TColor);
    procedure SetFCaptionColor(C: TColor);
    procedure SetFArrowColor(C: TColor);
    procedure SetFMarginColor(C: TColor);
    procedure SetFCenterColor(C: TColor);
    procedure SetFCircleColor(C: TColor);
    procedure SetFCenterRadius(I: Integer);
    procedure SetFCircleRadius(I: Integer);
    procedure SetFScaleAngle(I: Integer);
    procedure SetFMargin(I: Integer);
    procedure SetFStyle(S: TGaugeStyle);
    procedure SetFArrowWidth(I: Integer);
    procedure SetFNumMainTicks(I: Integer);
    procedure SetFNumSubTicks(I: Integer);
    procedure SetFLengthMainTicks(I: Integer);
    procedure SetFLengthSubTicks(I: Integer);
    procedure SetFFaceOptions(O: TFaceOptions);
    procedure SetFPosition(F: Double);
//    procedure SetFScaleValue(I: Integer);
    procedure SetFValueDecimals(I: Byte);
    procedure SetFMaximum(I: Double);
    procedure SetFMinimum(I: Double);
    procedure SetFMinMid(I: Double);
    procedure SetFMidMax(I: Double);
    procedure SetFCaption(const S: string);
    procedure SetFAntiAliased(V: TAntialiased);
    function  GetAAMultipler: Integer;
    function  GetPositionColor(F: Double): TColor;
    procedure CalcCoords(const Bitmap: TBitmap; const K: Integer);
  protected
    procedure DrawScale(Bitmap: TBitmap; K: Integer);
    procedure DrawArrow(Bitmap: TBitmap; K: Integer);
    procedure RedrawScale(bDraw: Boolean = False);
    procedure RedrawArrow;
    procedure FastAntiAliasPicture;
    procedure Paint; override;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var Msg: TMessage); message CM_FontChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   ReDraw;
  published
    property Font;
    // face elements colors
    property MinColor     : TColor read FMinColor     write SetFMinColor;
    property MidColor     : TColor read FMidColor     write SetFMidColor;
    property MaxColor     : TColor read FMaxColor     write SetFMaxColor;
    property FaceColor    : TColor read FFaceColor    write SetFFaceColor;
    property TicksColor   : TColor read FTicksColor   write SetFTicksColor;
    property ValueColor   : TColor read FValueColor   write SetFValueColor;
    property CaptionColor : TColor read FCaptionColor write SetFCaptionColor;
    property ArrowColor   : TColor read FArrowColor   write SetFArrowColor;
    property MarginColor  : TColor read FMarginColor  write SetFMarginColor;
    property CenterColor  : TColor read FCenterColor  write SetFCenterColor;
    property CircleColor  : TColor read FCircleColor  write SetFCircleColor;
    // face elements sizes, etc.
    property CenterRadius    : Integer      read FCenterRadius    write SetFCenterRadius;
    property CircleRadius    : Integer      read FCircleRadius    write SetFCircleRadius;
    property Angle           : Integer      read FScaleAngle      write SetFScaleAngle;
    property Margin          : Integer      read FMargin          write SetFMargin;
    property Style           : TGaugeStyle  read FStyle           write SetFStyle;
    property ArrowWidth      : Integer      read FArrowWidth      write SetFArrowWidth;
    property NumberMainTicks : Integer      read FNumMainTicks    write SetFNumMainTicks;
    property NumberSubTicks  : Integer      read FNumSubTicks     write SetFNumSubTicks;
    property LengthMainTicks : Integer      read FLengthMainTicks write SetFLengthMainTicks;
    property LengthSubTicks  : Integer      read FLengthSubTicks  write SetFLengthSubTicks;
    property FaceOptions     : TFaceOptions read FFaceOptions     write SetFFaceOptions;
    // values
    property Position: Double read FPosition write SetFPosition;
//    property Scale: Integer read FScaleValue write SetFScaleValue;
    property Maximum       : Double  read FMaximum       write SetFMaximum;
    property Minimum       : Double  read FMinimum       write SetFMinimum;
    property MiddleMin     : Double  read FMinMid        write SetFMinMid;
    property MiddleMax     : Double  read FMidMax        write SetFMidMax;
    property Caption       : string  read FCaption       write SetFCaption;
    property ValueDecimals : Byte    read FValueDecimals write SetFValueDecimals;
    // anti-aliasing mode
    property AntiAliased: TAntialiased read FAntiAliased write SetFAntiAliased;
    // event handlers
    property OnOverMax : TNotifyEvent read FOverMax write FOverMax;
    property OnOverMin : TNotifyEvent read FOverMin write FOverMin;
{$IFDEF TICKER}
    // ticker
    property OnFrames : TNotifyEvent read FOnFrames write FOnFrames;
    property Frames   : Integer      read FFrames;
{$ENDIF}
  end;

procedure Register;

implementation

uses Math;

{ ========================================================================= }
constructor TAn4logGauge.Create(AOwner: TComponent);
begin
  inherited;

  FBackBitmap := TBitmap.Create();
  FFaceBitmap := TBitmap.Create();
  FAABitmap   := nil;

  //*****************************defaults:****************************************
  Width  := 225;
  Height := 180;

  FBackBitmap.Width  := Width;
  FBackBitmap.Height := Height;
  FFaceBitmap.Width  := Width;
  FFaceBitmap.Height := Height;
  FBackBitmap.Canvas.Brush.Style := bsClear;
  FBackBitmap.Canvas.Brush.Color := Self.Color;

  FFaceColor    := clBlack;
  FTicksColor   := clWhite;
  FValueColor   := clWhite;
  FCaptionColor := clWhite;
  FArrowColor   := clWhite;
  FMarginColor  := clWhite;
  FCenterColor  := clWhite;
  FCircleColor  := clBlue;
  FMinColor     := clNavy;
  FMidColor     := clGreen;
  FMaxColor     := clRed;

  FArrowWidth      := 1;
//  FLineWidth       := 1;
  FMargin          := 10;
  FStyle           := gsCenter;
  FScaleValue      := 100;
  FMaximum         := 80;
  FMinimum         := 30;
  FMinMid          := 50;
  FMidMax          := 60;
  FPosition        := FMinimum;
  FScaleAngle      := 120;
  FCircleRadius    := 3;
  FCenterRadius    := 8;
  FNumMainTicks    := 10;
  FNumSubTicks     := 5;
  FLengthMainTicks := 15;
  FLengthSubTicks  := 8;
  FValueDecimals   := 2;
  FValuesFormat    := '%1.2f';

  FCaption := 'Gauge';

  FFaceOptions := [foShowMargin, foShowMainTicks, foShowSubTicks, foShowIndicatorMax,
                   foShowValues, foShowCenter, foShowFrame, foShow3D, foShowCaption];

  FAntiAliased := aaNone;
{$IFDEF TICKER}
  FTicker := -1; FFrames := 0;
  if not QueryPerformanceFrequency(FPeriod) then FPeriod := 0;
{$ENDIF}
end;

destructor TAn4logGauge.Destroy;
begin
  FBackBitmap.Free();
  FFaceBitmap.Free();
  FAABitmap.Free();

  inherited;
end;
{ ------------------------------------------------------------------------- }
procedure SetPenStyles(Pen: TPen; Width: Integer; Color: TColor);
var
  HP : HPen;
  LB : TLOGBRUSH;

begin
  LB.lbStyle := BS_SOLID;
  LB.lbColor := Color;
  LB.lbHatch := 0;

  HP := ExtCreatePen(PS_GEOMETRIC or PS_SOLID or PS_ENDCAP_FLAT or
                     PS_JOIN_ROUND, Width, LB, 0, nil);

  if ( HP = 0 ) then
  begin
    Pen.Width := Width;
    Pen.Color := Color;
  end
  else
    Pen.Handle := HP;
end;

procedure TAn4logGauge.CalcCoords(const Bitmap: TBitmap; const K: Integer);
var
  TW, TH, S, O : Integer;
  dJ, A1 : Double;
  
begin
  with FCoord do
  begin
    W := Bitmap.Width;
    H := Bitmap.Height;

    M := FMargin * K;
    dK := K * FLengthMainTicks;

    O := Ord( foValuesOutside in FFaceOptions );
    if ( foShowValues in FFaceOptions ) and ( O > 0 ) then
    begin
      TW := ( Max(Bitmap.Canvas.TextWidth(Format(FValuesFormat, [RoundTo(FMaximum, -FValueDecimals)])),
                  Bitmap.Canvas.TextWidth(Format(FValuesFormat, [RoundTo(FMinimum, -FValueDecimals)]))) + 12)*K;

      TH := ( Bitmap.Canvas.TextHeight(' ') + 12)*K;

      TW := Max(TW, TH);
    end
    else
      TW := 0;

    TH := TW;

    if ( FStyle <> gsCenter ) then
    begin
      FScaleAngle := EnsureRange(FScaleAngle, 0, 90);

      Y := H - M - K;

      S := Ord(FStyle = gsRight);
      A1 :=(90 - FScaleAngle)/2;
      A := ((1 - S)*90 + A1);
      X := Round(W*S - Sign(2*S - 1)*(M + K));
      C := X;

      dJ := cos(DegToRad(A1));

      JW := Round((W - 2*M)/dj - TW*O*(2-cos(DegToRad(A1))));
      JH := Round((H - 2*M)/dj - TH*O);
    end
    else begin
      X := W div 2;
      A := (180 - FScaleAngle)/2;
      C := W/2;

      if ( fScaleAngle >= 180 ) then
      begin
        JW := (W - 2*M) div 2 - TW;
        JH := (H - 2*M) div 2 - TH;

        Y := Round((H + JH*(1 - Abs(Sin(DegToRad(A)))))/2);
      end
      else begin
        TH := (Canvas.TextHeight(' '))*K*O;

        JW := Min(Round((W/2 - M - TW)/Cos(DegToRad(A))),
                  Round((H/2 - M - TH)/Sin(DegToRad(A))));

        JH := JW;

        Y := (H + JH + TH - K*FCenterRadius) div 2;
      end;
    end;

    JH := JH - K;
    JW := JW - K;

    dF := FScaleAngle/FScaleValue;
  end;
end;

function TAn4logGauge.GetPositionColor(F: Double): TColor;
begin
  if ( F < FMinMid ) then
    Result := FMinColor
  else if ( F > FMidMax ) then
    Result := FMaxColor
  else
    Result := FMidColor;
end;

procedure TAn4logGauge.DrawScale(Bitmap: TBitmap; K: Integer);
var
  I, II, N, R, TL, S, VX, VY, VXX, VYY : Integer;
  A1, P, Max, Min : Double;
  T : string;

//**********************
  procedure DrawRange(P1, P2: TPoint; RC: TColor);
  begin
    if not ( (P1.X = P2.X) and (P1.Y = P2.Y) ) then with FCoord, Bitmap do
    begin
      SetPenStyles(Canvas.Pen, dK, RC);
      Canvas.Arc(X - JW + dK div 2, Y - JH + dK div 2, X + JW - dK div 2, Y + JH - dK div 2, P1.X, P1.Y, P2.X, P2.Y);
    end;
  end;
//**********************

begin
  Bitmap.Canvas.Font := Font;
  CalcCoords(Bitmap, K);

  Min := (FMinMid - FMinimum)*FScaleValue/(FMaximum - FMinimum);
  Max := (FMidMax - FMinimum)*FScaleValue/(FMaximum - FMinimum);

  N := (FNumMainTicks - 1)*(FNumSubTicks + 1);

  with Bitmap, FCoord do
  begin
    R := FCircleRadius * K;

    Canvas.Brush.Color := FFaceColor;
    Canvas.FillRect(Canvas.ClipRect);
    Canvas.Font.Height := Canvas.Font.Height * K;
    // ***************************** Out Frame **************************
    if ( foShowFrame in fFaceOptions ) then
    begin
      if ( foShow3D in fFaceOptions ) then
      begin
        Canvas.Pen.Width := 2*K;
        Canvas.Pen.Color := clBtnShadow;
        Canvas.MoveTo(W, 0); Canvas.LineTo(0, 0); Canvas.LineTo(0, H);
        Canvas.Pen.Color := clBtnHighlight;
        Canvas.LineTo(W, H); Canvas.LineTo(W, 0);
      end
      else begin
        Canvas.Pen.Width := K;
        Canvas.Pen.Color := clBtnText;
        Canvas.Rectangle(0, 0, W, H);
      end;
    end;
    //************************* Out Margins **************************
    if ( foShowMargin in fFaceOptions ) then
    begin
      Canvas.Pen.Color := FMarginColor;
      Canvas.Pen.Width := K;
      Canvas.Rectangle(M, M, W - M, H - M);
    end;
    //******************************** Out Caption *******************
    if ( foShowCaption in FFaceOptions ) then
    begin
      S := 1 - 2 * Ord( (FStyle <> gsCenter) or (FScaleAngle < 260) );

      Canvas.Font.Color := FCaptionColor;
{
      Canvas.TextOut(Round(C - JW/2 * Cos(DegToRad(A + FScaleAngle/2))) -
                     Canvas.TextWidth(FCaption) div 2,
                     Round(Y + S * JH/3 * Sin(DegToRad(A + FScaleAngle/2))),
                     FCaption);
{}
      Canvas.TextOut(Round(C - JW/2 * Cos(DegToRad(A + FScaleAngle/2))) -
                     Canvas.TextWidth(FCaption) div 2,
                     Round(Y + S * (JH/2 + Canvas.TextHeight(' ') + K)),
                     FCaption);
    end;
    //********************************** Out MinMaxLines *************************************

    if ( foShowIndicatorMax in FFaceOptions ) and ( FMinimum <= FMidMax ) and ( FMidMax < FMaximum ) then
      DrawRange(Point(Round(C - JW * Cos(DegToRad(A + FScaleAngle))),
                      Round(Y - JH * Sin(DegToRad(A + FScaleAngle)))),
                Point(Round(C - JW * Cos(DegToRad(A + Max*dF))),
                      Round(Y - JH * Sin(DegToRad(A + Max*dF)))),
                FMaxColor);

    if ( foShowIndicatorMid in FFaceOptions ) and ( FMidMax > FMinMid ) and ( FMinMid >= FMinimum ) then
      DrawRange(Point(Round(C - JW * Cos(DegToRad(A + Max*dF))),
                      Round(Y - JH * Sin(DegToRad(A + Max*dF)))),
                Point(Round(C - JW * Cos(DegToRad(A + Min*dF))),
                      Round(Y - JH * Sin(DegToRad(A + Min*dF)))),
                FMidColor);

    if ( foShowIndicatorMin in FFaceOptions ) and ( FMinimum < FMinMid ) and ( FMinMid <= FMaximum ) then
      DrawRange(Point(Round(C - JW * Cos(DegToRad(A + Min*dF))),
                      Round(Y - JH * Sin(DegToRad(A + Min*dF)))),
                Point(Round(C - JW * Cos(DegToRad(A))),
                      Round(Y - JH * Sin(DegToRad(A)))),
                FMinColor);

    Canvas.Font.Color := FValueColor;
    Canvas.Pen.Width  := K;

    I := 0;

    Canvas.Pen.Color := FTicksColor;

    while ( I <= N ) do
    begin
      // ********************************* Main Ticks Position *********************************
      if ( I mod (FNumSubTicks + 1) = 0 ) then
      begin
        II := I div (FNumSubTicks + 1);
        P := FMinimum + II*(FMaximum - FMinimum)/(FNumMainTicks - 1);
        A1 := DegToRad(A + II*FScaleAngle/(FNumMainTicks - 1));

        // ************************************* Out Values *************************************
        if ( foShowMainTicks in fFaceOptions ) then
          TL := FLengthMainTicks + 1;

        if ( foShowValues in fFaceOptions ) then
        begin
          if ( foChangeValuesColor in FFaceOptions ) then
            Canvas.Font.Color := GetPositionColor(P);

          Canvas.Brush.Color := FFaceColor;
          T := Format(FValuesFormat, [RoundTo(P, -FValueDecimals)]);

          VX := -Canvas.TextWidth(T);
          VY := -Canvas.TextHeight(T);
          S  := Ord(foValuesOutside in FFaceOptions);

          if ( FStyle <> gsCenter ) then
          begin
            VYY := (VY - dK)*(1 - S) - 3*K*(1 - 2*S);
            VXX := (VX - dK)*(1 - S) - 6*K*(1 - 2*S);
            VX  := VX *(Ord(FStyle = gsRight));
          end
          else begin
            VX  := VX div 2;
            VY  := VY div 2;
            VYY := -(VY - 4*K)*sign(2*S - 1) - dK*(1 - S);
            VXX := -(VX - 6*K)*sign(2*S - 1) - dK*(1 - S);
          end;

          Canvas.TextOut(Round(C - (JW + VXX)*cos(A1) + VX),
                         Round(Y - (JH + VYY)*sin(A1) + VY), T);
        end;

        // ************************************ Out Circles ************************************
        if ( foShowCircles in FFaceOptions ) then
        begin
          TL := TL - FCircleRadius;

          if ( foChangeCirclesColor in FFaceOptions ) then
            Canvas.Brush.Color := GetPositionColor(P)
          else
            Canvas.Brush.Color := FCircleColor;

          Canvas.Ellipse(Round(C - JW*Cos(A1)) - R,
                         Round(Y - JH*Sin(A1)) - R,
                         Round(C - JW*Cos(A1)) + R,
                         Round(Y - JH*Sin(A1)) + R);
        end;
      end
      else if ( foShowSubTicks in fFaceOptions ) then begin
        A1 := DegToRad(A + I*FScaleAngle/N);
        TL := FLengthSubTicks;
      end;

      if ( foShowSubTicks in fFaceOptions ) then
        Inc(I)
      else
        Inc(I, FNumSubTicks + 1);

      //********************************** Out Ticks *************************************
      if ( foShowMainTicks in fFaceOptions ) or ( foShowSubTicks in fFaceOptions ) then
      begin
        Canvas.MoveTo(Round(C - (JW - dK + K)*Cos(A1)),
                      Round(Y - (JH - dK + K)*Sin(A1)));

        Canvas.LineTo(Round(C - (JW - (FLengthMainTicks + 1 - TL)*K)*Cos(A1)),
                      Round(Y - (JH - (FLengthMainTicks + 1 - TL)*K)*Sin(A1)));
      end;
    end;
  end
end;

procedure TAn4logGauge.DrawArrow(Bitmap: TBitmap; K: Integer);
var
  R, AW, PW : Integer;
  P : Double;
  L : TColor;

begin
  CalcCoords(Bitmap, K);
  P := (FPosition - FMinimum)*FScaleValue/(FMaximum - FMinimum);

  with Bitmap, FCoord do
  begin
    R := FCenterRadius*K;

    if ( foChangeArrowColor in FFaceOptions ) then
      L := GetPositionColor(FPosition)
    else
      L := FArrowColor;

    Canvas.Pen.Color := L;
    Canvas.Brush.Color := L;

    A := DegToRad(A + P*dF);

    AW := (FArrowWidth - 1) * K;

    Canvas.Polygon([Point(Round(X - (JW - dK)*Cos(A)), Round(Y - (JH - dK)*Sin(A))),
                    Point(Round(C - AW * Sin(A)), Round(Y + AW * Cos(A))),
                    Point(Round(C + AW * Sin(A)), Round(Y - AW * Cos(A)))]);
    //********************************* Out Center ***************************************
    if ( foShowCenter in FFaceOptions ) then
    begin
      Canvas.Pen.Width := FArrowWidth*k;
      PW := Canvas.Pen.Width div 2;
      Canvas.Brush.Color := FCenterColor;
      Canvas.Ellipse(X - R + PW, Y - R + PW, X + R - PW, Y + R - PW);
      Canvas.Pen.Width := 1;
    end;
  end;
end;

procedure TAn4logGauge.RedrawArrow;
{$IFDEF TICKER}
var
  F: Integer;
  Ticker: Int64;

begin
  if ( FTicker < 0 ) then
    if ( FPeriod = 0 ) then
      FTicker := GetTickCount()
    else
      QueryPerformanceCounter(FTicker);
{$ELSE}
begin
{$ENDIF}
  BitBlt(FFaceBitmap.Canvas.Handle, 0, 0, FBackBitmap.Width,
         FBackBitmap.Height, FBackBitmap.Canvas.Handle, 0, 0, SRCCOPY);
  DrawArrow(FFaceBitmap, GetAAMultipler);
  if ( FAntialiased <> aaNone ) then
    FastAntiAliasPicture();

  Paint();
{$IFDEF TICKER}
  if ( FPeriod = 0 ) then
  begin
    Ticker := GetTickCount();

    if ( Ticker < FTicker ) then
      Ticker := Ticker + $100000000;

    F := 1000 div (Ticker - FTicker)
  end
  else begin
    QueryPerformanceCounter(Ticker);
    F := FPeriod div (Ticker - FTicker)
  end;

  if ( F <> FFrames ) then
  begin
    FFrames := F;

    if Assigned(FOnFrames) then
      FOnFrames(Self)
  end;
  
  FTicker := -1;
{$ENDIF}
end;

procedure TAn4logGauge.RedrawScale(bDraw: Boolean = False);
begin
  if not ( bDraw or FAutoUpdate or (csDesigning in ComponentState) ) then
    Exit;

{$IFDEF TICKER}
  if ( FPeriod = 0 ) then
    FTicker := GetTickCount()
  else
    QueryPerformanceCounter(FTicker);
{$ENDIF}
  DrawScale(FBackBitmap, GetAAMultipler);
  RedrawArrow();
end;

procedure TAn4logGauge.ReDraw;
begin
  RedrawScale(True);
end;

const
  MaxPixelCount = MaxInt div SizeOf(TRGBTriple);

type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount-1] of TRGBTriple;

procedure TAn4logGauge.FastAntiAliasPicture;
var
  i, k, x, y, cx, cy, cxi, totr, totg, totb : Integer;
  Row1, Row2, Row3, Row4, DestRow : PRGBArray;

begin
  // For each row
  K    := GetAAMultipler;
  Row2 := nil;
  Row3 := nil;
  Row4 := nil;

  for Y := 0 to FAABitmap.Height - 1 do
  begin
    // We compute samples of K x K pixels
    cy := y*K;
    // Get pointers to actual, previous and next rows in supersampled bitmap
    Row1 := FFaceBitmap.ScanLine[cy];

    if ( K > 1 ) then
      Row2 := FFaceBitmap.ScanLine[cy+1];

    if ( K > 2 ) then
      Row3 := FFaceBitmap.ScanLine[cy+2];

    if ( K > 3 ) then
      Row4 := FFaceBitmap.ScanLine[cy+3];

    // Get a pointer to destination row in output bitmap
    DestRow := FAABitmap.ScanLine[y];
    // For each column...
    for x := 0 to FAABitmap.Width - 1 do
    begin
      // We compute samples of 3 x 3 pixels
      cx := x*K;
      // Initialize result color
      totr := 0;
      totg := 0;
      totb := 0;
      
      if ( K > 3 ) then
      begin
        for i := 0 to 3 do
        begin
          cxi  := cx + i;
          totr := totr + Row1[cxi].rgbtRed   + Row2[cxi].rgbtRed   + Row3[cxi].rgbtRed   + Row4[cxi].rgbtRed;
          totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen + Row3[cxi].rgbtGreen + Row4[cxi].rgbtGreen;
          totb := totb + Row1[cxi].rgbtBlue  + Row2[cxi].rgbtBlue  + Row3[cxi].rgbtBlue  + Row4[cxi].rgbtBlue;
        end;

        DestRow[x].rgbtRed   := totr div 16;
        DestRow[x].rgbtGreen := totg div 16;
        DestRow[x].rgbtBlue  := totb div 16;
      end
      else if ( K > 2 ) then
      begin
        for i := 0 to 2 do
        begin
          cxi  := cx + i;
          totr := totr + Row1[cxi].rgbtRed   + Row2[cxi].rgbtRed   + Row3[cxi].rgbtRed;
          totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen + Row3[cxi].rgbtGreen;
          totb := totb + Row1[cxi].rgbtBlue  + Row2[cxi].rgbtBlue  + Row3[cxi].rgbtBlue;
        end;

        DestRow[x].rgbtRed   := totr div 9;
        DestRow[x].rgbtGreen := totg div 9;
        DestRow[x].rgbtBlue  := totb div 9;
      end
      else if ( K > 1 ) then
      begin
        for i := 0 to 1 do
        begin
          cxi  := cx + i;
          totr := totr + Row1[cxi].rgbtRed   + Row2[cxi].rgbtRed;
          totg := totg + Row1[cxi].rgbtGreen + Row2[cxi].rgbtGreen;
          totb := totb + Row1[cxi].rgbtBlue  + Row2[cxi].rgbtBlue;
        end;

        DestRow[x].rgbtRed   := totr div 4;
        DestRow[x].rgbtGreen := totg div 4;
        DestRow[x].rgbtBlue  := totb div 4;
      end
      else begin
        DestRow[x].rgbtRed   := Row1[cx].rgbtRed;
        DestRow[x].rgbtGreen := Row1[cx].rgbtGreen;
        DestRow[x].rgbtBlue  := Row1[cx].rgbtBlue;
      end;
    end;
  end
end;

procedure TAn4logGauge.Paint;
begin
  if ( FAntiAliased = aaNone ) then
    BitBlt(Canvas.Handle, 0, 0, FFaceBitmap.Width,
      FFaceBitmap.Height, FFaceBitmap.Canvas.Handle, 0, 0, SRCCOPY)
  else
    BitBlt(Canvas.Handle, 0, 0, FAABitmap.Width,
      FAABitmap.Height, FAABitmap.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TAn4logGauge.WMSize(var Message : TWMSize);
var
  K : Integer;

begin
  if ( Width  < 60 ) then
    Width := 60;

  if ( Height < 50 ) then
    Height := 50;

  if ( FAntiAliased = aaNone ) then
  begin
    FBackBitmap.Width := Width; FBackBitmap.Height := Height;
    FFaceBitmap.Width := Width; FFaceBitmap.Height := Height;
  end
  else begin
    K := GetAAMultipler;
    FBackBitmap.Width := Width * K; FBackBitmap.Height := Height * K;
    FFaceBitmap.Width := Width * K; FFaceBitmap.Height := Height * K;
    FAABitmap.Width   := Width;
    FAABitmap.Height  := Height;
  end;

  RedrawScale();
  inherited;
end;

procedure TAn4logGauge.CMFontChanged(var Msg:TMessage);
begin
  RedrawScale();
end;

{ ------------------------------------------------------------------------- }
procedure TAn4logGauge.SetFMinColor(C: TColor);
begin
  if ( C <> FMinColor ) then
  begin
    FMinColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFMidColor(C: TColor);
begin
  if ( C <> FMidColor ) then
  begin
    FMidColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFMaxColor(C: TColor);
begin
  if ( C <> FMaxColor ) then
  begin
    FMaxColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFFaceColor(C: TColor);
begin
  if ( C <> FFaceColor ) then
  begin
    FFaceColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFTicksColor(C: TColor);
begin
  if ( C <> FTicksColor ) then
  begin
    FTicksColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFValueColor(C: TColor);
begin
  if ( C <> FValueColor ) then
  begin
    FValueColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFCaptionColor(C: TColor);
begin
  if ( C <> FCaptionColor ) then
  begin
    FCaptionColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFArrowColor(C: TColor);
begin
  if ( C <> FArrowColor ) then
  begin
    FArrowColor := C;
    RedrawArrow();
  end;
end;

procedure TAn4logGauge.SetFMarginColor(C: TColor);
begin
  if ( C <> FMarginColor ) then
  begin
    FMarginColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFCenterColor(C: TColor);
begin
  if ( C <> FCenterColor ) then
  begin
    FCenterColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFCircleColor(C: TColor);
begin
  if ( C <> FCircleColor ) then
  begin
    FCircleColor := C;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFCenterRadius(I: Integer);
begin
  if ( I <> FCenterRadius ) then
  begin
    if ( FStyle = gsCenter ) then
      FCenterRadius := EnsureRange(I, 1, Min(Height div 4 - FMargin, Width div 4 - FMargin))
    else
      FCenterRadius := FMargin;

    if ( FCenterRadius <= FArrowWidth ) then
      FArrowWidth := FCenterRadius;

    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFCircleRadius(I: Integer);
begin
  if ( I <> FCircleRadius ) then
  begin
    FCircleRadius := I;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFScaleAngle(I: Integer);
begin
  if ( I <> FScaleAngle ) then
  begin
    if ( 10 <= I ) and ( I <= 360 ) then
      FScaleAngle := I;

    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFMargin(I: Integer);
var
  K : Integer;

begin
  if ( I <> FMargin ) then
  begin
    K := GetAAMultipler()*2;
    FMargin := EnsureRange(I, 1, Min(Height div 4 - K, Width div 4 - K));

    I := FCenterRadius;
    FCenterRadius := 1;

    SetFCenterRadius(I);
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFStyle(S: TGaugeStyle);
begin
  if ( S <> FStyle ) then
  begin
    FStyle := S;

    if ( FStyle <> gsCenter ) and ( FCenterRadius > FMargin ) then
      SetFCenterRadius(FMargin)
    else
      RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFArrowWidth(I: Integer);
begin
  if ( I <> FArrowWidth ) then
  begin
    FArrowWidth := EnsureRange(I, 1, FCenterRadius);
    RedrawArrow();
  end
end;

procedure TAn4logGauge.SetFNumMainTicks(I: Integer);
begin
  if ( I <> FNumMainTicks ) and ( I > 1 ) then
  begin
    FNumMainTicks := I;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFNumSubTicks(I: Integer);
begin
  if ( I <> FNumMainTicks ) and ( I > 0 ) then
  begin
    FNumSubTicks := I;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFLengthMainTicks(I: Integer);
begin
  if ( I <> FLengthMainTicks ) then
  begin
    if ( I <= FLengthSubTicks ) then
      FLengthMainTicks := FLengthSubTicks + 1
    else
      FLengthMainTicks := I;

    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFLengthSubTicks(I: Integer);
begin
  if ( I <> FLengthSubTicks ) then
  begin
    FLengthSubTicks := EnsureRange(I, 1, FLengthMainTicks - 1);
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFFaceOptions(O: TFaceOptions);
begin
  if ( O <> FFaceOptions ) then
  begin
    FFaceOptions := O;
    FAutoUpdate  := ( foAutoUpdate in FFaceOptions );
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFPosition(F: Double);
begin
  if ( F <> FPosition ) then
  begin
    FPosition := EnsureRange(F, FMinimum, FMaximum);

    if ( F > fMaximum ) and Assigned(FOverMax) then
      OnOverMax(Self);

    if ( F < FMinimum ) and Assigned(FOverMin) then
      OnOverMin(Self);

    RedrawArrow();
  end
end;

{
procedure TAn4logGauge.SetFScaleValue(I: Integer);
begin
  if ( I <> FScaleValue ) then
  begin
    if ( I > 1 ) then
    begin
      FScaleValue := I;
      if ( FMaximum >= FScaleValue ) then
        FMaximum := FScaleValue - 1;

      if ( FMinimum > FScaleValue - FMaximum ) then
        FMinimum := FScaleValue - fMaximum;
    end;

    RedrawScale();
  end;
end;
{}

procedure TAn4logGauge.SetFMaximum(I: Double);
begin
  if ( I <> FMaximum ) and ( I > FMinimum ) then
  begin
    FMaximum := I;

    FMinMid := EnsureRange(FMinMid, FMinimum, FMaximum);
    FMidMax := EnsureRange(FMidMax, FMinMid,  FMaximum);

    if ( FPosition > I ) then
      SetFPosition(I);

    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFMinimum(I: Double);
begin
  if ( I <> FMinimum ) and ( I < FMaximum ) then
  begin
    FMinimum := I;

    FMidMax := EnsureRange(FMidMax, FMinimum, FMaximum);
    FMinMid := EnsureRange(FMinMid, FMinimum, FMidMax);

    if ( FPosition < I ) then
      SetFPosition(I);

    RedrawScale();
  end
end;

procedure TAn4logGauge.SetFMinMid(I: Double);
begin
  if ( I <> FMinMid ) then
  begin
    FMinMid := EnsureRange(I, FMinimum, FMidMax);
    FMidMax := EnsureRange(FMidMax, FMinMid, FMaximum);

    RedrawScale();
  end
end;

procedure TAn4logGauge.SetFMidMax(I: Double);
begin
  if ( I <> FMidMax ) then
  begin
    FMidMax := EnsureRange(I, FMinMid, FMaximum);
    FMinMid := EnsureRange(FMinMid, FMinimum, FMidMax);

    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFCaption(const S: string);
begin
  if ( S <> FCaption ) then
  begin
    Canvas.Font := Font;
    FCaption := S;
    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFValueDecimals(I: Byte);
begin
  if ( I <> FValueDecimals ) then
  begin
    FValueDecimals  := EnsureRange(I, 0, 5);
    FValuesFormat[3]:= Chr(Ord('0') + FValueDecimals);

    RedrawScale();
  end;
end;

procedure TAn4logGauge.SetFAntiAliased(V: TAntialiased);
var
  K : Integer;

begin
  if ( V <> FAntiAliased ) then
  begin
    FAntiAliased := V;

    if ( FAntiAliased = aaNone ) then
    begin
      FreeAndNil(FAABitmap);
      FreeAndNil(FBackBitmap);
      FreeAndNil(FFaceBitmap);

      FBackBitmap := TBitmap.Create;
      FFaceBitmap := TBitmap.Create;

      FBackBitmap.Width  := Width;
      FFaceBitmap.Width  := Width;
      FBackBitmap.Height := Height;
      FFaceBitmap.Height := Height;
    end
    else begin
      K := GetAAMultipler;
      FBackBitmap.PixelFormat := pf24bit;
      FFaceBitmap.PixelFormat := pf24bit;
      FBackBitmap.Width  := Width * K;
      FFaceBitmap.Width  := Width * K;
      FBackBitmap.Height := Height * K;
      FFaceBitmap.Height := Height * K;

      if not Assigned(FAABitmap) then
        FAABitmap := TBitmap.Create;

      FAABitmap.PixelFormat := pf24bit;
      FAABitmap.Width  := Width;
      FAABitmap.Height := Height;
    end;

    RedrawScale();
  end
end;

function TAn4logGauge.GetAAMultipler: Integer;
begin
  Result := Ord(FAntiAliased) + 1;
end;

{ ------------------------------------------------------------------------- }
procedure Register;
begin
  RegisterComponents('Additional', [TAn4logGauge]);
end;
{ ========================================================================= }
end.


