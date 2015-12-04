program MyMaterial;



uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  LIB.Material in '_LIBRARY\LIB.Material.pas',
  LUX in '_LIBRARY\LUX.idea\LUX.pas',
  LUX.FireMonkey.Material in '_LIBRARY\LUX.idea\» FireMonkey\LUX.FireMonkey.Material.pas',
  LUX.DirectX.d3dcommon in '_LIBRARY\LUX.idea\» DirectX\LUX.DirectX.d3dcommon.pas',
  LUX.DirectX.d3dcompiler in '_LIBRARY\LUX.idea\» DirectX\LUX.DirectX.d3dcompiler.pas',
  LUX.DirectX.d3d11shader in '_LIBRARY\LUX.idea\» DirectX\LUX.DirectX.d3d11shader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
