unit FMX.ListView.DynamicLoad;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Graphics, FMX.StdCtrls, FMX.ListView, FMX.ListView.Types;

type
  TmyJSONData = record
    struct: TArray<string>;
  end;

  TmyLVLoadMode = (Scrolling, Pages);

  TmyLVDynamicLoad = record
    // настраиваем LV
    class procedure configList(const aLV: TListView); static;

    // режим подгрузки данных
    class var LoadMode: TmyLVLoadMode;

    // кол-во страниц
    class function PageCount: integer; static;
    // текущая страница
    class var Page: integer;

    // загрузка нужной страницы
    class procedure LoadPage(const aLV: TListView; const aPage: integer; const aLabel: TLabel); static;
  end;

const
  // имена полей в Data[]
  sign_URL = 'URL';
  sign_Loaded = 'Loaded';

  // адрес сервера с картинками
  cHost = 'http://rzaripov.kz/';
  cStatic = cHost + 'blog_demo/';
  cRequest = cHost + 'fmx.php?method=getList&page=';

procedure LoadBitmapFromURL(const AURL: string; aBitmap: TBitmap);

implementation

uses
  System.Math, System.Threading, System.Net.HTTPClient,
  XSuperJSON, XSuperObject,
  FMX.HTTP.Request;

procedure LoadBitmapFromURL(const AURL: string; aBitmap: TBitmap);
var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(
    procedure
    var
      HTTP: THTTPClient;
      Result: TMemoryStream;
    begin
      Result := TMemoryStream.Create;
      HTTP := THTTPClient.Create;
      try
        try
          HTTP.Get(AURL, Result);
          TThread.Synchronize(TThread.CurrentThread,
            procedure
            var
              aSourceBmp: TBitmap;
            begin
              aSourceBmp := TBitmap.Create;
              aSourceBmp.LoadFromStream(Result);
              if not aSourceBmp.IsEmpty then
              begin
                aBitmap.SetSize(aSourceBmp.Width, aSourceBmp.Height);
                aBitmap.CopyFromBitmap(aSourceBmp);
              end;
              FreeAndNil(aSourceBmp);
            end);
        except
          FreeAndNil(Result);
        end;
      finally
        FreeAndNil(Result);
        FreeAndNil(HTTP);
      end;
    end);
  thread.FreeOnTerminate := true;
  thread.start;
end;

{ TmyLVDynamicLoad }

class function TmyLVDynamicLoad.PageCount: integer;
begin
  Result := 12;
end;

class procedure TmyLVDynamicLoad.configList(const aLV: TListView);
begin
  aLV.ItemAppearance.ItemAppearance := 'Custom';
  aLV.CanSwipeDelete := false;
  aLV.ItemAppearance.ItemHeight := 120;
  aLV.ItemSpaces.Left := 0;
  aLV.ItemSpaces.Right := 0;

  // отключаем, чтобы не было лишних срабатывании OnScrollEnd
  if aLV.getAniCalc <> nil then
    aLV.getAniCalc.BoundsAnimation := false;

  with aLV.ItemAppearanceObjects.ItemObjects do
  begin
    Accessory.Visible := false;
    TextButton.Visible := false;
    Detail.Visible := false;

    Image.Align := TListItemAlign.Center;
    Image.VertAlign := TListItemAlign.Leading;
    Image.PlaceOffset.X := 0;
    Image.PlaceOffset.Y := 1;
    Image.Width := 96;
    Image.Height := 96;
    Image.Visible := true;

    Text.TextVertAlign := TTextAlign.Leading;
    Text.TextAlign := TTextAlign.Center;
    Text.PlaceOffset.X := 0;
    Text.PlaceOffset.Y := Image.Height + 1;
    Text.Visible := true;
  end;

  // инициализируем переменные
  TmyLVDynamicLoad.Page := 0;
  TmyLVDynamicLoad.LoadMode := TmyLVLoadMode.Scrolling;
end;

class procedure TmyLVDynamicLoad.LoadPage(const aLV: TListView; const aPage: integer; const aLabel: TLabel);
var
  aJSON: string;
begin
  if not InRange(aPage, 1, TmyLVDynamicLoad.PageCount) then
    exit;

  // очищаем в режиме паджинации
  if TmyLVDynamicLoad.LoadMode = TmyLVLoadMode.Pages then
    aLV.ItemsClearTrue;

  TTask.Run(
    procedure
    begin
      // используем доп. поток для загрузки контента
      aJSON := TmyAPI.Get(cRequest + IntToStr(aPage));

      TThread.Synchronize(TThread.CurrentThread,
        procedure
        var
          aJSONData: TmyJSONData;
          I: integer;
        begin
          // если ошибки нет, то продолжаем работу
          if TmyAPI.IsOK(aJSON) then
          begin
            // обновляем значение
            TmyLVDynamicLoad.Page := aPage;

            if TmyLVDynamicLoad.LoadMode = TmyLVLoadMode.Scrolling then
              aLabel.Text := 'Scrolling / page = ' + TmyLVDynamicLoad.Page.ToString
            else
              aLabel.Text := 'Pages / page = ' + TmyLVDynamicLoad.Page.ToString;

            // используем десериализацию чтобы собрать структуру из данных
            // используется библиотека XSuperObject
            aJSONData := TJSON.Parse<TmyJSONData>(aJSON);

            // заполняем данными наш список
            for I := Low(aJSONData.struct) to High(aJSONData.struct) do
            begin
              with aLV.Items.Add do
              begin
                Text := aJSONData.struct[I];
                Data[sign_URL] := cStatic + aJSONData.struct[I];
                Data[sign_Loaded] := 0;
              end;
            end;
          end;
        end);
    end);
end;

end.
