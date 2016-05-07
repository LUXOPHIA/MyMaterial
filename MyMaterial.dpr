program MyMaterial;



uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LUX in '_LIBRARY\LUX.idea\LUX.pas',
  LUX.FMX.Material in '_LIBRARY\LUX.FMX.Material.pas',
  Core in 'Core.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
