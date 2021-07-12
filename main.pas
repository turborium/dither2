unit main;

{$mode delphi}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs, ComCtrls, ComboEx, Types;

type

  { TFormMain }

  TFormMain = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    ButtonLoad: TButton;
    ButtonSave: TButton;
    CheckBoxScale: TCheckBox;
    ComboBoxPalette: TComboBox;
    ImageDither: TImage;
    ImageListPalette: TImageList;
    ImageOrigin: TImage;
    Label1: TLabel;
    Label2: TLabel;
    LabelOrigin: TLabel;
    LabelDither: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    PanelMain: TPanel;
    PanelOrigin: TPanel;
    PanelDither: TPanel;
    SavePictureDialog: TSavePictureDialog;
    ScrollBoxOrigin: TScrollBox;
    ScrollBoxDither: TScrollBox;
    TrackBarDitherPower: TTrackBar;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure CheckBoxScaleChange(Sender: TObject);
    procedure ComboBoxPaletteChange(Sender: TObject);
    procedure ComboBoxPaletteDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure PanelMainResize(Sender: TObject);
    procedure ScrollBoxDitherPaint(Sender: TObject);
    procedure ScrollBoxOriginPaint(Sender: TObject);
    procedure TrackBarDitherPowerChange(Sender: TObject);
  private
    procedure DitherImage;
    procedure UpdateImagesSize;
  public

  end;

var
  FormMain: TFormMain;

implementation

uses
  Image32;

type
  TDitherPalette = array of TColor32;

const
  SolarEclipsePalette: array [0..3] of TColor32 = (
    $FFFFFCFE,
    $FFFFC200,
    $FFFF2A00,
    $FF11070A
  );

  Gray4Palette: array [0..3] of TColor32 = (
    $FFFFFFFF,
    $FFAAAAAA,
    $FF777777,
    $FF000000
  );

  Pollen8Palette: array [0..7] of TColor32 = (
    $FF73464C,
    $FFAB5675,
    $FFEE6A7C,
    $FFFFA7A5,
    $FFFFE07E,
    $FFFFE7D6,
    $FF72DCBB,
    $FF34ACBA
  );

  Pc88LowPalette: array [0..7] of TColor32 = (
    $FF0000DB,
    $FF00B6DB,
    $FF00DB6D,
    $FFFFB600,
    $FFFF926D,
    $FFDB0000,
    $FFDBDBDB,
    $FF000000
  );

  LemonLimegbPalette: array [0..3] of TColor32 = (
    $FFcdb81b,
    $FFafb525,
    $FF87b133,
    $FF5fad41
  );


type
  TPaletteEntry = record
    Name: string;
    Palette: array of TColor32;
  end;

const
  Palettes: array of TPaletteEntry = [
    (
      Name: 'Solar Eclipse';
      Palette:
      [
        $FFFFFCFE,
        $FFFFC200,
        $FFFF2A00,
        $FF11070A
      ]
    ),
    (
      Name: 'Monochrome';
      Palette:
      [
        $FFFFFFFF,
        $FF000000
      ]
    ),
    (
      Name: 'Gray 4';
      Palette:
      [
        $FF000000,
        $FF676767,
        $FFb6b6b6,
        $FFffffff
      ]
    ),
    (
      Name: 'Lemon Lime GB';
      Palette:
      [
        $FFcdb81b,
        $FFafb525,
        $FF87b133,
        $FF5fad41
      ]
    ),
    (
      Name: 'Pollen 8';
      Palette:
      [
        $FF73464C,
        $FFAB5675,
        $FFEE6A7C,
        $FFFFA7A5,
        $FFFFE07E,
        $FFFFE7D6,
        $FF72DCBB,
        $FF34ACBA
      ]
    ),
    (
      Name: 'PC 88';
      Palette:
      [
        $FF0000DB,
        $FF00B6DB,
        $FF00DB6D,
        $FFFFB600,
        $FFFF926D,
        $FFDB0000,
        $FFDBDBDB,
        $FF000000
      ]
    ),
    (
      Name: 'Pico 8';
      Palette:
      [
        $FF000000,
        $FF1D2B53,
        $FF7E2553,
        $FF008751,
        $FFAB5236,
        $FF5F574F,
        $FFC2C3C7,
        $FFFFF1E8,
        $FFFF004D,
        $FFFFA300,
        $FFFFEC27,
        $FF00E436,
        $FF29ADFF,
        $FF83769C,
        $FFFF77A8,
        $FFFFCCAA
      ]
    ),
    (
      Name: 'Apple II Lo Res';
      Palette:
      [
        $FF000000,
        $FF515c16,
        $FF843d52,
        $FFea7d27,
        $FF514888,
        $FFe85def,
        $FFf5b7c9,
        $FF006752,
        $FF00c82c,
        $FF919191,
        $FFc9d199,
        $FF00a6f0,
        $FF98dbc9,
        $FFc8c1f7,
        $FFffffff
      ]
    ),
    (
      Name: 'Commodore 64';
      Palette:
      [
        $FF000000,
        $FF626262,
        $FF898989,
        $FFadadad,
        $FFffffff,
        $FF9f4e44,
        $FFcb7e75,
        $FF6d5412,
        $FFa1683c,
        $FFc9d487,
        $FF9ae29b,
        $FF5cab5e,
        $FF6abfc6,
        $FF887ecb,
        $FF50459b,
        $FFa057a3
      ]
    ),
    (
      Name: 'Funky Future';
      Palette:
      [
        $FF2b0f54,
        $FFab1f65,
        $FFff4f69,
        $FFfff7f8,
        $FFff8142,
        $FFffda45,
        $FF3368dc,
        $FF49e7ec
      ]
    ),
    (
      Name: 'ArgeeBey';
      Palette:
      [
        $FF000000,
        $FF1f246a,
        $FF8a1181,
        $FFd14444,
        $FF2ca53e,
        $FF68cbcb,
        $FFe3c72d,
        $FFffffff
      ]
    ),
    (
      Name: 'FuzzyFour';
      Palette:
      [
        $FF302387,
        $FFff3796,
        $FF00faac,
        $FFfffdaf
      ]
    ),
    (
      Name: 'Ceral GB';
      Palette:
      [
        $FF2b061e,
        $FF875053,
        $FFd8c86e,
        $FFffeed6
      ]
    )
  ];



{$R *.lfm}

function CalcColorDist(const Color1, Color2: TColor32): Integer;
var
  DistR, DistG, DistB: Integer;
begin
  DistR := TARGB(Color1).R - TARGB(Color2).R;
  DistG := TARGB(Color1).G - TARGB(Color2).G;
  DistB := TARGB(Color1).B - TARGB(Color2).B;

  Result := DistR * DistR + DistG * DistG + DistB * DistB;
end;

function FindClosestColor(const Color: TColor32; const Palette: array of TColor32): TColor32;
var
  MinDist: Integer;
  PaletteColor: TColor32;
begin
  if Length(Palette) = 0 then
    Exit($FF000000);

  MinDist := High(Integer);
  for PaletteColor in Palette do
  begin
    if MinDist > CalcColorDist(Color, PaletteColor) then
    begin
      Result := PaletteColor;
      MinDist := CalcColorDist(Color, PaletteColor);
    end;
  end;
end;

type
  TDitherColorError = record
    R: Integer;
    G: Integer;
    B: Integer;
  end;

function ClipByte(const Value: Integer): Byte;
begin
  if Value > 255 then
    Exit(255)
  else if Value < 0 then
    Exit(0);

  Result := Value;
end;

procedure FloydSteinbergDithering(const Image: TImage32; const Palette: array of TColor32; const Power: Byte);
var
  X, Y, I: Integer;
  Color, NewColor: TColor32;
  Error: TDitherColorError;
  CurrLineError: array of TDitherColorError;
  NextLineError: array of TDitherColorError;
begin
  SetLength(CurrLineError, Image.Width + 2);
  SetLength(NextLineError, Image.Width + 2);

  for Y := 0 to Image.Height - 1 do
  begin
    for X := 0 to Image.Width - 1 do
    begin
      Color := Image.Pixel[X, Y];

      Color := Color32(
        255,// alpha
        ClipByte(TARGB(Color).R + (CurrLineError[X + 1].R * Power div 16) div 255),// red
        ClipByte(TARGB(Color).G + (CurrLineError[X + 1].G * Power div 16) div 255),// green
        ClipByte(TARGB(Color).B + (CurrLineError[X + 1].B * Power div 16) div 255)// blue
      );

      NewColor := FindClosestColor(Color, Palette);

      Image.Pixel[X, Y] := NewColor;

      Error.R := TARGB(Color).R - TARGB(NewColor).R;
      Error.G := TARGB(Color).G - TARGB(NewColor).G;
      Error.B := TARGB(Color).B - TARGB(NewColor).B;

      // [             *     7/16(0) ]
      // [ 3/16(1)  5/16(2)  1/16(3) ]
      // 0
      CurrLineError[X + 2].R := CurrLineError[X + 2].R + 7 * Error.R;
      CurrLineError[X + 2].G := CurrLineError[X + 2].G + 7 * Error.G;
      CurrLineError[X + 2].B := CurrLineError[X + 2].B + 7 * Error.B;
      // 1
      NextLineError[X + 0].R := NextLineError[X + 0].R + 3 * Error.R;
      NextLineError[X + 0].G := NextLineError[X + 0].G + 3 * Error.G;
      NextLineError[X + 0].B := NextLineError[X + 0].B + 3 * Error.B;
      // 2
      NextLineError[X + 1].R := NextLineError[X + 1].R + 5 * Error.R;
      NextLineError[X + 1].G := NextLineError[X + 1].G + 5 * Error.G;
      NextLineError[X + 1].B := NextLineError[X + 1].B + 5 * Error.B;
      // 3
      NextLineError[X + 2].R := NextLineError[X + 2].R + 1 * Error.R;
      NextLineError[X + 2].G := NextLineError[X + 2].G + 1 * Error.G;
      NextLineError[X + 2].B := NextLineError[X + 2].B + 1 * Error.B;
    end;

    for I := 0 to High(CurrLineError) do
    begin
      CurrLineError[I] := NextLineError[I];
      NextLineError[I] := Default(TDitherColorError);
    end;
  end;
end;

{ TFormMain }

procedure TFormMain.ButtonLoadClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    ImageOrigin.Picture.LoadFromFile(OpenPictureDialog.FileName);
    DitherImage;
    UpdateImagesSize;
  end;
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  if SavePictureDialog.Execute then
  begin
    ImageDither.Picture.Bitmap.Canvas.Changed;// HACK (recreate DC)
    ImageDither.Picture.SaveToFile(SavePictureDialog.FileName);
  end;
end;

procedure TFormMain.CheckBoxScaleChange(Sender: TObject);
begin
  UpdateImagesSize;
end;

procedure TFormMain.ComboBoxPaletteChange(Sender: TObject);
begin
  DitherImage;
end;

procedure TFormMain.ComboBoxPaletteDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
const
  MaxColorCount = 64;
var
  Count, I: Integer;
  Color: TARGB;
  OldColor: TColor;
begin
  TComboBox(Control).Canvas.FillRect(ARect);

  OldColor := TComboBox(Control).Canvas.Brush.Color;
  Count := Length(Palettes[Index].Palette);
  for I := 0 to Count - 1 do
  begin
    Color := TARGB(Palettes[Index].Palette[I]);
    TComboBox(Control).Canvas.Brush.Color := RGBToColor(Color.R, Color.G, Color.B);
    TComboBox(Control).Canvas.FillRect(
      ARect.Left + I * (MaxColorCount div Count) + 2,
      ARect.Top + 1,
      ARect.Left + (I + 1) * (MaxColorCount div Count) + 2,
      ARect.Bottom - 1
    );
  end;

  TComboBox(Control).Canvas.Brush.Color := OldColor;
  ARect.Left := ARect.Left + MaxColorCount + 2;
  TComboBox(Control).Canvas.TextOut(ARect.Left, ARect.Top, TComboBox(Control).Items[Index]);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  PaletteEntry: TPaletteEntry;
begin
  for PaletteEntry in Palettes do
  begin
    ComboBoxPalette.Items.Add(PaletteEntry.Name);
  end;

  // defaults
  ComboBoxPalette.ItemIndex := 0;
  TrackBarDitherPower.Position := 200;
  CheckBoxScale.Checked := False;

  DitherImage;
  UpdateImagesSize;
end;

procedure TFormMain.PanelMainResize(Sender: TObject);
begin
  PanelOrigin.Width :=
    (PanelMain.ClientWidth - PanelMain.ChildSizing.HorizontalSpacing - PanelMain.BorderWidth * 2) div 2 - 1;
end;

procedure TFormMain.ScrollBoxDitherPaint(Sender: TObject);
begin
  // это ужастный хак
  ScrollBoxOrigin.VertScrollBar.Position := ScrollBoxDither.VertScrollBar.Position;
  ScrollBoxOrigin.HorzScrollBar.Position := ScrollBoxDither.HorzScrollBar.Position;
end;

procedure TFormMain.ScrollBoxOriginPaint(Sender: TObject);
begin
  // это ужастный хак
  ScrollBoxDither.VertScrollBar.Position := ScrollBoxOrigin.VertScrollBar.Position;
  ScrollBoxDither.HorzScrollBar.Position := ScrollBoxOrigin.HorzScrollBar.Position;
end;

procedure TFormMain.TrackBarDitherPowerChange(Sender: TObject);
begin
  DitherImage;
end;

procedure TFormMain.DitherImage;
var
  Image: TImage32;
begin
  Image := TImage32.Create;
  try
    // load form origin image
    Image.SetSize(ImageOrigin.Picture.Bitmap.Width, ImageOrigin.Picture.Bitmap.Height);
    Image.CopyFromDC(
      ImageOrigin.Picture.Bitmap.Canvas.Handle,
      Rect(0, 0, Image.Width, Image.Height)
    );
    // dither
    FloydSteinbergDithering(Image, Palettes[ComboBoxPalette.ItemIndex].Palette, TrackBarDitherPower.Position);
    // load to dither image
    ImageDither.Picture.Bitmap.SetSize(Image.Width, Image.Height);
    Image.CopyToDc(ImageDither.Picture.Bitmap.Canvas.Handle, 0, 0, False);
  finally
    Image.Free;
  end;
  ImageDither.Refresh;
end;

procedure TFormMain.UpdateImagesSize;
var
  Scale: Integer;
begin
  if CheckBoxScale.Checked then
    Scale := 2
  else
    Scale := 1;

  ImageOrigin.Width := ImageOrigin.Picture.Width * Scale;
  ImageOrigin.Height := ImageOrigin.Picture.Height * Scale;
  ImageDither.Width := ImageDither.Picture.Width * Scale;
  ImageDither.Height := ImageDither.Picture.Height * Scale;
end;

end.

