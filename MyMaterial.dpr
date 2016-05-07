program MyMaterial;



uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  Core in 'Core.pas',
  LUX.FMX.Material in '_LIBRARY\LUXOPHIA\LUX.FMX\LUX.FMX.Material.pas',
  LUX in '_LIBRARY\LUXOPHIA\LUX\LUX.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
