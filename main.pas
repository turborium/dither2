unit main;

{$mode objfpc}{$H+}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ExtDlgs;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonLoad: TButton;
    ButtonSave: TButton;
    ImageDither: TImage;
    ImageOrigin: TImage;
    Label1: TLabel;
    Label2: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    PanelMain: TPanel;
    PanelOrigin: TPanel;
    PanelDither: TPanel;
    SavePictureDialog: TSavePictureDialog;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanelMainResize(Sender: TObject);
  private
    procedure DitherImage;
  public

  end;

var
  FormMain: TFormMain;

implementation

uses
  Image32;

{$R *.lfm}

function GetBrightness(Color: TColor32): Byte;
begin
  Result := ((Red(Color) * 299) + (Green(Color) * 587) + (Blue(Color) * 114)) div 1000;
end;

procedure FloydSteinbergDithering(const Image: TImage32);
var
  X, Y, I: Integer;
  Color: TColor32;
  Brightness: Byte;
  Error: Integer;
  CurrLineError: array of Integer;
  NextLineError: array of Integer;
begin
  SetLength(CurrLineError, Image.Width + 2);
  SetLength(NextLineError, Image.Width + 2);

  for Y := 0 to Image.Height - 1 do
  begin
    for I := 0 to High(CurrLineError) do
    begin
      CurrLineError[I] := NextLineError[I];
      NextLineError[I] := 0;
    end;

    for X := 0 to Image.Width - 1 do
    begin
      Color := Image.Pixel[X, Y];

      Brightness := GetBrightness(Color);

      if Brightness + CurrLineError[X + 1] < 127 then
      begin
        Color := Color32(255, 0, 0, 0);
        Error := Brightness + CurrLineError[X + 1];
      end else
      begin
        Color := Color32(255, 255, 255, 255);
        Error := Brightness - 255 + CurrLineError[X + 1];
      end;

      CurrLineError[X + 2] := CurrLineError[X + 2] + (7 * Error) div 16;
      NextLineError[X] := NextLineError[X] + (3 * Error) div 16;
      NextLineError[X + 1] := NextLineError[X + 1] + (5 * Error) div 16;
      NextLineError[X + 2] := NextLineError[X + 2] + (1 * Error) div 16;

      Image.Pixel[X, Y] := Color;
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

procedure TFormMain.FormCreate(Sender: TObject);
begin
  DitherImage;
end;

procedure TFormMain.PanelMainResize(Sender: TObject);
begin
  PanelOrigin.Width := PanelMain.ClientWidth div 2;
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
    FloydSteinbergDithering(Image);
    // load to dither image
    ImageDither.Picture.Bitmap.SetSize(Image.Width, Image.Height);
    Image.CopyToDc(ImageDither.Picture.Bitmap.Canvas.Handle, 0, 0, True);
  finally
    Image.Free;
  end;
end;

end.

