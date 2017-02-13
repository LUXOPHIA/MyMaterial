program MyMaterial;



uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LUX.FMX.Material in '_LIBRARY\LUXOPHIA\LUX.FMX\LUX.FMX.Material.pas',
  LUX in '_LIBRARY\LUXOPHIA\LUX\LUX.pas',
  LIB.Material in '_LIBRARY\LIB.Material.pas',
  LUX.D4.V4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4.V4.pas',
  LUX.Lattice.T1 in '_LIBRARY\LUXOPHIA\LUX\LUX.Lattice.T1.pas',
  LUX.Lattice.T2 in '_LIBRARY\LUXOPHIA\LUX\LUX.Lattice.T2.pas',
  LUX.Lattice.T3 in '_LIBRARY\LUXOPHIA\LUX\LUX.Lattice.T3.pas',
  LUX.M2 in '_LIBRARY\LUXOPHIA\LUX\LUX.M2.pas',
  LUX.M3 in '_LIBRARY\LUXOPHIA\LUX\LUX.M3.pas',
  LUX.M4 in '_LIBRARY\LUXOPHIA\LUX\LUX.M4.pas',
  LUX.D1 in '_LIBRARY\LUXOPHIA\LUX\LUX.D1.pas',
  LUX.D2.M4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2.M4.pas',
  LUX.D2 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2.pas',
  LUX.D2.V4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D2.V4.pas',
  LUX.D3.M4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3.M4.pas',
  LUX.D3 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3.pas',
  LUX.D3.V4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D3.V4.pas',
  LUX.D4.M4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4.M4.pas',
  LUX.D4 in '_LIBRARY\LUXOPHIA\LUX\LUX.D4.pas',
  LUX.FMX.Types3D in '_LIBRARY\LUXOPHIA\LUX.FMX\LUX.FMX.Types3D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
