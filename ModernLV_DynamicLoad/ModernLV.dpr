program ModernLV;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FormMain},
  FMX.ListView in 'FMX.ListView.pas',
  FMX.ListView.Types in 'FMX.ListView.Types.pas',
  XSuperJSON in 'comps\XSO\XSuperJSON.pas',
  XSuperObject in 'comps\XSO\XSuperObject.pas',
  FMX.HTTP.Request in 'comps\FMX.HTTP.Request.pas',
  FMX.ListView.DynamicLoad in 'FMX.ListView.DynamicLoad.pas';

{$R *.res}

begin
   ReportMemoryLeaksOnShutdown := true;

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
