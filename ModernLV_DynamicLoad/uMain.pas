unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.ListView.Types, FMX.Controls.Presentation, FMX.ListView, FMX.Objects,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView.DynamicLoad;

type
  TFormMain = class(TForm)
    Label1: TLabel;
    Rectangle1: TRectangle;
    ListView1: TListView;
    lbText: TLabel;
    Switch1: TSwitch;
    btnPrev: TButton;
    btnNext: TButton;
    procedure Switch1Switch(Sender: TObject);
    procedure ListView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure FormShow(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
  private
    procedure SetLVMode(const aValue: Boolean);
    { Private declarations }
    procedure OnLVScrollEnd(Sender: TObject);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Math;

procedure TFormMain.btnPrevClick(Sender: TObject);
begin
  // загружаем предыдущую страницу
  TmyLVDynamicLoad.LoadPage(ListView1, TmyLVDynamicLoad.Page - 1, lbText);
end;

procedure TFormMain.btnNextClick(Sender: TObject);
begin
  // загружаем следующую страницу
  TmyLVDynamicLoad.LoadPage(ListView1, TmyLVDynamicLoad.Page + 1, lbText);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  // настраиваем отображение ListView
  TmyLVDynamicLoad.configList(ListView1);
  // выбираем режим подгрузки Scrolling
  SetLVMode(false);
end;

procedure TFormMain.ListView1Paint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
var
  I: integer;
  aLV: TListView;
  aFirst, aLast: integer;
begin
  aLV := (Sender as TListView);
  if (aLV.Items.Count <= 0) then
    exit;

  // получаем первый видимый элемент
  aFirst := Max(0, aLV.getFirstVisibleItemIndex);
  // получаем кол-во видимых элементов
  aLast := aFirst + aLV.getVisibleCount;
  for I := aFirst to aLast do
  begin
    // находится ли наш индекс в нужном диапозоне
    if InRange(I, 0, aLV.Items.Count - 1) then
    begin
      // если картинка еще не была загружена, то начинаем её грузить
      if aLV.Items[I].Data[sign_Loaded].AsInteger = 0 then
      begin
        // начали загружать!
        aLV.Items[I].Data[sign_Loaded] := 1;
        // собственно загрузка картинки, в доп. потоке
        LoadBitmapFromURL(aLV.Items[I].Data[sign_URL].AsString, aLV.Items[I].Bitmap);
      end;
    end;
  end;
end;

procedure TFormMain.OnLVScrollEnd(Sender: TObject);
begin
  if TmyLVDynamicLoad.Page < TmyLVDynamicLoad.PageCount then
  begin
    // в режиме скролинга всегда грузим следующую страницу
    TmyLVDynamicLoad.LoadPage(ListView1, TmyLVDynamicLoad.Page + 1, lbText);
    sleep(1500); // делаем паузу, имитируем диалог ожидания
  end;
end;

procedure TFormMain.SetLVMode(const aValue: Boolean);
begin
  ListView1.ItemsClearTrue;

  if aValue then
  begin
    // меняем режим подгрузки на паджинацию
    TmyLVDynamicLoad.LoadMode := TmyLVLoadMode.Pages;
    lbText.Text := 'Pages / page = 1';
    // в режиме паджинации обнуляем событие
    ListView1.OnScrollEnd := nil;
    // показываем контролы управления паджинацией
    btnNext.Visible := true;
    btnPrev.Visible := true;
  end
  else
  begin
    // меняем режим подгрузки на бесконечный скроллинг
    TmyLVDynamicLoad.LoadMode := TmyLVLoadMode.Scrolling;
    lbText.Text := 'Scrolling / page = 1';
    // назначаем событие при котором будет вызываться подгрузка следующей порции
    ListView1.OnScrollEnd := OnLVScrollEnd;
    // скрываем контролы управления паджинацией
    btnPrev.Visible := false;
    btnNext.Visible := false;
  end;

  // после смены режима подгрузки, загружаем первую страницу
  TmyLVDynamicLoad.LoadPage(ListView1, 1, lbText);
end;

procedure TFormMain.Switch1Switch(Sender: TObject);
begin
  SetLVMode(Switch1.IsChecked);
end;

end.
