unit FMX.HTTP.Request;

{
  author: ZuBy
  email: rzaripov1990@gmail.com

  https://github.com/rzaripov1990
}

interface

uses
  System.Classes, System.SysUtils, System.Net.URLClient, FMX.Types;

type
  // Post Params
  TmyPostParam = record
    Key: string;
    Value: string;
    IsFile: boolean;
    constructor Create(const aKey, aValue: string; const aIsFile: boolean = false);
  end;

  // HTTP Helper
  TmyAPI = record
  private
    class var FProxy: TProxySettings;
    class function CheckInet: boolean; static;
    class function urlEnc(const aValue: string): string; static;
    class function GetDefaultProxyParams: TProxySettings; static;
  public
    class var FNeedCheckInet: boolean;
    class var FNeedUseDefaultWindowsProxy: boolean;

    class function IsOK(const aData: string): boolean; static;
    class function ErrorText(const aData: string): string; static;
    class procedure SetProxySettings(const aHost: string; aPort: Integer; const aUserName: string = '';
      const aPassword: string = ''; const aScheme: string = ''); static;
    class function Get(const aURL: string): string; overload; static;
    class function Get(const aURL: string; const aEncoding: TEncoding): string; overload; static;
    class function Post(const aURL: string; const aFields: TArray<TmyPostParam>): string; overload; static;
    class function Post(const aURL: string; const aFields: TArray<TmyPostParam>; const aEncoding: TEncoding): string;
      overload; static;
  end;

implementation

uses
  System.Net.HTTPClient, System.Net.HTTPClientComponent,
  System.Net.Mime, System.NetEncoding
{$IFDEF MSWINDOWS} , WinAPI.Windows, WinAPI.WinInet {$ENDIF};

type
  TmyCert = record
    procedure OnValidateServerCertificate(const Sender: TObject; const ARequest: TURLRequest;
      const Certificate: TCertificate; var Accepted: boolean);
  end;

const
  SIGNATURE = '[TmyAPI]';
  SIGNATURE_LEN = Length(SIGNATURE);
  MSGERR = SIGNATURE + 'Error';
  MSGERR_I = SIGNATURE + 'No Internet Connection';

var
  FmyCert: TmyCert;

procedure TmyCert.OnValidateServerCertificate(const Sender: TObject; const ARequest: TURLRequest;
  const Certificate: TCertificate; var Accepted: boolean);
begin
  Accepted := Now <= Certificate.Expiry; // Certificate Valid?
end;

{$IFDEF MSWINDOWS}

function GetProxyInformation: string;
var
  ProxyInfo: PInternetProxyInfo;
  Len: DWORD;
begin
  Result := '';
  Len := 4096;
  GetMem(ProxyInfo, Len);
  try
    if InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len) then
    begin
      if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
        Result := ProxyInfo^.lpszProxy;
    end;
  finally
    FreeMem(ProxyInfo);
  end;
end;

procedure GetProxyServer(protocol: string; var ProxyServer: string; var ProxyPort: Integer);
var
  i: Integer;
  ProxyInfo, ps: string;
begin
  ProxyServer := '';
  ProxyPort := 0;
  ProxyInfo := GetProxyInformation;
  if ProxyInfo = '' then
    Exit;
  protocol := protocol + '=';
  i := Pos(protocol, ProxyInfo);
  if i > 0 then
  begin
    Delete(ProxyInfo, 1, i + Length(protocol));
    i := Pos(';', ProxyServer);
    if i > 0 then
      ProxyInfo := Copy(ProxyInfo, 1, i - 1);
  end;
  i := Pos(':', ProxyInfo);
  if i > 0 then
  begin
    ProxyPort := StrToIntDef(Copy(ProxyInfo, i + 1, Length(ProxyInfo) - i), 0);
    ProxyServer := Copy(ProxyInfo, 1, i - 1)
  end;
end;
{$ENDIF}

class function TmyAPI.GetDefaultProxyParams: TProxySettings;
{$IFDEF MSWINDOWS}
var
  aHost: string;
  aPort: Integer;
{$ENDIF}
begin
  Result.Host := '';
  Result.Port := 0;
{$IFDEF MSWINDOWS}
  GetProxyServer('http', aHost, aPort);
  Result := TProxySettings.Create(aHost, aPort);
{$ENDIF}
end;

class function TmyAPI.CheckInet: boolean;
var
  aResp: IHTTPResponse;
  aHTTP: TNetHTTPClient;
begin
  Result := false;
  aHTTP := TNetHTTPClient.Create(nil);
  try
    try
      aResp := aHTTP.Head('http://google.com');
      Result := aResp.StatusCode < 400;
    except
      Result := false;
    end;
  finally
    FreeAndNil(aHTTP);
  end;
end;

class function TmyAPI.urlEnc(const aValue: string): string;
begin
  Result := TNetEncoding.URL.Encode(aValue);
end;

class function TmyAPI.ErrorText(const aData: string): string;
begin
  Result := aData.Substring(SIGNATURE_LEN);
end;

class function TmyAPI.IsOK(const aData: string): boolean;
begin
  Result := not aData.StartsWith(SIGNATURE);
end;

class function TmyAPI.Post(const aURL: string; const aFields: TArray<TmyPostParam>; const aEncoding: TEncoding): string;
var
  aData: TMultipartFormData;
  aHTTP: TNetHTTPClient;
  aResp: TStringStream;
  i: Integer;
begin
  if TmyAPI.FNeedCheckInet then
  begin
    // Check Internet Connection
    if not TmyAPI.CheckInet then
    begin
      Result := MSGERR_I;
      Exit;
    end;
  end;

  Result := MSGERR;
  // Request
  aResp := TStringStream.Create('', aEncoding);
  aData := TMultipartFormData.Create();
  aHTTP := TNetHTTPClient.Create(nil);
  aHTTP.OnValidateServerCertificate := FmyCert.OnValidateServerCertificate;

{$IFDEF MSWINDOWS}
  if TmyAPI.FNeedUseDefaultWindowsProxy then
    TmyAPI.FProxy := GetDefaultProxyParams;
{$ENDIF}
  aHTTP.ProxySettings := TmyAPI.FProxy;
  try
    for i := Low(aFields) to High(aFields) do
    begin
      if aFields[i].IsFile then
        aData.AddFile(aFields[i].Key, aFields[i].Value)
      else
        aData.AddField(aFields[i].Key, aFields[i].Value);
    end;

    try
      aHTTP.Post(aURL, aData, aResp);
      Result := aResp.DataString;
    except
      Result := MSGERR;
    end;
  finally
    FreeAndNil(aHTTP);
    FreeAndNil(aData);
    FreeAndNil(aResp);
  end;
end;

class procedure TmyAPI.SetProxySettings(const aHost: string; aPort: Integer;
  const aUserName, aPassword, aScheme: string);
begin
  TmyAPI.FProxy := TProxySettings.Create(aHost, aPort, aUserName, aPassword, aScheme);
end;

class function TmyAPI.Post(const aURL: string; const aFields: TArray<TmyPostParam>): string;
begin
  Result := TmyAPI.Post(aURL, aFields, TEncoding.UTF8);
end;

class function TmyAPI.Get(const aURL: string): string;
begin
  Result := TmyAPI.Get(aURL, TEncoding.UTF8);
end;

class function TmyAPI.Get(const aURL: string; const aEncoding: TEncoding): string;
var
  aHTTP: TNetHTTPClient;
  aResp: TStringStream;
begin
  if TmyAPI.FNeedCheckInet then
  begin
    // Check Internet Connection
    if not TmyAPI.CheckInet then
    begin
      Result := MSGERR_I;
      Exit;
    end;
  end;

  Result := MSGERR;
  // Request
  aResp := TStringStream.Create('', aEncoding);
  aHTTP := TNetHTTPClient.Create(nil);
  aHTTP.OnValidateServerCertificate := FmyCert.OnValidateServerCertificate;

{$IFDEF MSWINDOWS}
  if TmyAPI.FNeedUseDefaultWindowsProxy then
    TmyAPI.FProxy := GetDefaultProxyParams;
{$ENDIF}
  aHTTP.ProxySettings := TmyAPI.FProxy;
  try
    try
      aHTTP.Get(aURL, aResp);
      Result := aResp.DataString;
    except
      Result := MSGERR;
    end;
  finally
    FreeAndNil(aHTTP);
    FreeAndNil(aResp);
  end;
end;

{ TmyPostParam }

constructor TmyPostParam.Create(const aKey, aValue: string; const aIsFile: boolean = false);
begin
  Self.Key := aKey;
  Self.Value := aValue;
  Self.IsFile := aIsFile;
end;

end.
