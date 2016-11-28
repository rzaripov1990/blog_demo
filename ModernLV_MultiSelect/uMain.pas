unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.ListView.Types, FMX.Controls.Presentation, FMX.ListView, FMX.Objects,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base;

type
  TmyLVConfig = record
    // заполняется выбранный TListView данными
    class procedure makeList(const aLV: TListView); static;
    // выделения всего списка
    class procedure SelectAll(const aLV: TListView); static;
    // снятие выделения со всего списка
    class procedure UnSelectAll(const aLV: TListView); static;
    // выделяем конкретный элемент
    class procedure SelectItem(const aLV: TListView; const aIndex: integer); static;
    // снимаем выделение с конкретного элемента
    class procedure UnSelectItem(const aLV: TListView; const aIndex: integer); static;

    // получаем количество выделенных элементов
    class function getSelectedCount(const aLV: TListView): integer; static;
    // проверяем на выделение конкретный элемент
    class function IsSelectedItem(const aLV: TListView; const aIndex: integer): boolean; static;

    // возвращает цвет для выделения
    class function SelectedColor: TAlphaColor; static;
  end;

  TFormMain = class(TForm)
    Label1: TLabel;
    Rectangle1: TRectangle;
    ListView1: TListView;
    Label2: TLabel;
    Switch1: TSwitch;
    procedure FormCreate(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure Switch1Switch(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

var
  // переменная которая отвечает за переключение режимов
  FMultiSelectMode: boolean = false;

const
  // имя нашего поля в Data[]
  sign_IsSelected = 'IsSelected';

implementation

{$R *.fmx}

uses
  System.Math;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  TmyLVConfig.makeList(ListView1);
end;

procedure TFormMain.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
begin
  if AItem.HasData[sign_IsSelected] and (FMultiSelectMode) then
  begin
    if (AItem.Data[sign_IsSelected].AsInteger = 0) then
      TmyLVConfig.SelectItem(ListView1, AItem.Index)
    else
      TmyLVConfig.UnSelectItem(ListView1, AItem.Index);
  end;

  if not FMultiSelectMode then
    Label2.Text := 'Multi Select Mode'
  else
    Label2.Text := 'Selected: ' + TmyLVConfig.getSelectedCount(ListView1).ToString;
end;

procedure TFormMain.Switch1Switch(Sender: TObject);
begin
  FMultiSelectMode := Switch1.IsChecked;
  if not FMultiSelectMode then
  begin
    Label2.Text := 'Multi Select Mode';
    TmyLVConfig.UnSelectAll(ListView1);
  end;
  ListView1.ShowSelection := not FMultiSelectMode;
end;

{ TmyLVConfig }

class procedure TmyLVConfig.makeList(const aLV: TListView);
var
  I: integer;
begin
  aLV.ItemAppearance.ItemAppearance := 'ListItem';
  aLV.ItemAppearanceObjects.ItemObjects.Accessory.Visible := false;

  for I := 0 to 14 do
  begin
    with aLV.Items.Add do
    begin
      Text := 'Item ' + IntTostr(I);
      Data[sign_IsSelected] := 0; // не ставим выделение при добавлении
    end;
  end;
end;

class function TmyLVConfig.getSelectedCount(const aLV: TListView): integer;
var
  I: integer;
begin
  Result := 0;
  if not FMultiSelectMode then
    exit;

  for I := 0 to aLV.Items.Count - 1 do
  begin
    if aLV.Items[I].Data[sign_IsSelected].AsInteger = 1 then
      Result := Result + 1;
  end;
end;

class function TmyLVConfig.IsSelectedItem(const aLV: TListView; const aIndex: integer): boolean;
begin
  Result := false;
  if not FMultiSelectMode then
    exit;
  if not InRange(aIndex, 0, aLV.Items.Count - 1) then
    exit;

  Result := (aLV.Items[aIndex].Data[sign_IsSelected].AsInteger = 1) and (aLV.IsCustomColorUsed(aIndex));
end;

class function TmyLVConfig.SelectedColor: TAlphaColor;
begin
  Result := TAlphaColorRec.Lightsalmon;
end;

class procedure TmyLVConfig.SelectItem(const aLV: TListView; const aIndex: integer);
begin
  if not InRange(aIndex, 0, aLV.Items.Count - 1) then
    exit;

  aLV.SetCustomColorForItem(aIndex, TmyLVConfig.SelectedColor);
  aLV.Items[aIndex].Data[sign_IsSelected] := 1;
end;

class procedure TmyLVConfig.UnSelectItem(const aLV: TListView; const aIndex: integer);
begin
  if not InRange(aIndex, 0, aLV.Items.Count - 1) then
    exit;

  aLV.SetDefaultColorForItem(aIndex);
  aLV.Items[aIndex].Data[sign_IsSelected] := 0;
end;

class procedure TmyLVConfig.SelectAll(const aLV: TListView);
var
  I: integer;
begin
  for I := 0 to aLV.Items.Count - 1 do
  begin
    if aLV.Items[I].Data[sign_IsSelected].AsInteger = 0 then
      TmyLVConfig.SelectItem(aLV, I);
  end;
end;

class procedure TmyLVConfig.UnSelectAll(const aLV: TListView);
var
  I: integer;
begin
  for I := 0 to aLV.Items.Count - 1 do
  begin
    if aLV.Items[I].Data[sign_IsSelected].AsInteger = 1 then
      TmyLVConfig.UnSelectItem(aLV, I);
  end;
end;

end.
