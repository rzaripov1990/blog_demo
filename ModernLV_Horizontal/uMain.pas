unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.ListView.Types, FMX.Controls.Presentation, FMX.ListView, FMX.Objects,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, System.ImageList, FMX.ImgList;

type
  TmyLVConfig = record
    class procedure makeCategory(const aLV: TListView); static;
    class procedure makeFilms(const aLV: TListView); static;
  end;

  TFormMain = class(TForm)
    Label1: TLabel;
    lvCategory: TListView;
    StyleBook1: TStyleBook;
    Rectangle1: TRectangle;
    ListView1: TListView;
    ImageList1: TImageList;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  TmyLVConfig.makeCategory(lvCategory);
  TmyLVConfig.makeFilms(ListView1);
end;

{ TmyLVConfig }

class procedure TmyLVConfig.makeCategory(const aLV: TListView);
const
  CTitle: array [0 .. 5] of string = ('Лучшее', 'Игры', 'Категории', 'Бета', 'Для всей семьи', 'Выбор редакции');
var
  I: Integer;
begin
  aLV.Horizontal := true;
  aLV.ItemAppearance.ItemAppearance := 'ListItem';
  aLV.ItemAppearance.ItemHeight := 100;
  aLV.ItemAppearanceObjects.ItemObjects.Accessory.Visible := false;
  aLV.ItemAppearanceObjects.ItemObjects.Text.TextAlign := TTextAlign.Center;
  aLV.StyleLookup := 'listviewstyle_panel';
  aLV.ShowScrollBar := false;
  aLV.MakeSelectedItemVisible := false;

  for I := Low(CTitle) to High(CTitle) do
  begin
    with aLV.Items.Add do
      Text := CTitle[I];
  end;
end;

class procedure TmyLVConfig.makeFilms(const aLV: TListView);
const
  CTitle: array [0 .. 8] of string = ('ПИКСЕЛИ', 'The Martian', 'Третий лишний 2', 'Прогулка', 'Hitman: Agent 47',
    'Миссия Невыполнима', 'The Perfect Guy', 'Такие разные близнецы', 'Мир Юрского Периода');
var
  I: Integer;
begin
  aLV.Horizontal := true;
  aLV.ItemAppearance.ItemAppearance := 'ImageListItem';
  aLV.ItemAppearance.ItemHeight := 100;
  aLV.StyleLookup := 'listviewstyle_panel';
  aLV.ShowScrollBar := false;
  aLV.MakeSelectedItemVisible := false;

  with aLV.ItemAppearanceObjects.ItemObjects do
  begin
    Accessory.Visible := false;
    Image.Width := 100;
    Image.Height := 120;
    Image.PlaceOffset.X := 1;
    Image.PlaceOffset.Y := 5;
    Image.VertAlign := TListItemAlign.Leading;
    Text.TextAlign := TTextAlign.Center;
    Text.PlaceOffset.X := 1;
    Text.PlaceOffset.Y := Image.Height;
    Text.Height := aLV.Height - Image.Height;
  end;

  for I := Low(CTitle) to High(CTitle) do
  begin
    with aLV.Items.Add do
    begin
      ImageIndex := I;
      Text := CTitle[I];
    end;
  end;
end;

end.
