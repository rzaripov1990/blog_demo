program ModernLV;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FormMain},
  FMX.ListView in 'FMX.ListView.pas',
  FMX.ListView.Types in 'FMX.ListView.Types.pas';

{$R *.res}

begin
   ReportMemoryLeaksOnShutdown := true;

  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;

end.
