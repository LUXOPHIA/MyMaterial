unit Main;

interface //#################################################################### ■

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Math.Vectors,
  FMX.Types3D, FMX.Controls3D, FMX.MaterialSources, FMX.Objects3D, FMX.Viewport3D,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.TabControl,
  LIB.Material;

type
  TForm1 = class(TForm)
    TabControl1: TTabControl;
      TabItemV: TTabItem;
        Viewport3D1: TViewport3D;
          Dummy1: TDummy;
            Dummy2: TDummy;
              Camera1: TCamera;
          Light1: TLight;
          Grid3D1: TGrid3D;
          Sphere1: TSphere;
            LightMaterialSource1: TLightMaterialSource;
          Sphere2: TSphere;
        Timer1: TTimer;
      TabItemS: TTabItem;
        TabControlS: TTabControl;
          TabItemSV: TTabItem;
            TabControlSV: TTabControl;
              TabItemSVC: TTabItem;
                MemoSVC: TMemo;
              TabItemSVE: TTabItem;
                MemoSVE: TMemo;
          TabItemSP: TTabItem;
            TabControlSP: TTabControl;
              TabItemSPC: TTabItem;
                MemoSPC: TMemo;
              TabItemSPE: TTabItem;
                MemoSPE: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure Timer1Timer(Sender: TObject);
  private
    { private 宣言 }
    _MouseS :TShiftState;
    _MouseP :TPointF;
  public
    { public 宣言 }
  end;

var
  Form1: TForm1;

implementation //############################################################### ■

{$R *.fmx}

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

procedure TForm1.FormCreate(Sender: TObject);
var
   M :TMyMaterialSource;
   T :String;
begin
     with LightMaterialSource1 do
     begin
          Emissive  := TAlphaColors.Null;

          Ambient   := $FF202020;

          Diffuse   := TAlphaColors.White;

          Specular  := TAlphaColors.White;
          Shininess := 50;

          Texture.LoadFromFile( '..\..\_DATA\Earth.jpg' );
     end;

     M := TMyMaterialSource.Create( Self );

     with Sphere2 do
     begin
          MaterialSource := M;
          TwoSide := True;
     end;

     MemoSVC.Lines.LoadFromFile( '..\..\_DATA\ShaderV.hlsl' );
     MemoSPC.Lines.LoadFromFile( '..\..\_DATA\ShaderP.hlsl' );

     with M do
     begin
          EmisLight := TAlphaColorF.Create( 0, 0, 0 );

          AmbiLight := TAlphaColorF.Create( 0.1, 0.1, 0.1 );

          DiffRatio := TAlphaColorF.Create( 1, 1, 1 );

          SpecRatio := TAlphaColorF.Create( 1, 1, 1 );
          SpecShiny := 50;

          DiffImage.LoadFromFile( '..\..\_DATA\Earth.jpg' );

          with ShaderV do
          begin
               Source.Text := MemoSVC.Text;

               for T in Errors.Keys do
               begin
                    with MemoSVE.Lines do
                    begin
                         Add( '▼ ' + T   );
                         Add( Errors[ T ] );
                    end;
               end;
          end;

          with ShaderP do
          begin
               Source.Text := MemoSPC.Text;

               for T in Errors.Keys do
               begin
                    with MemoSPE.Lines do
                    begin
                         Add( '▼ ' + T   );
                         Add( Errors[ T ] );
                    end;
               end;
          end;
     end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Viewport3D1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     _MouseS := Shift;
     _MouseP := TPointF.Create( X, Y );
end;

procedure TForm1.Viewport3D1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var
   P :TPointF;
begin
     if ssLeft in _MouseS then
     begin
          P := TPointF.Create( X, Y );

          with Dummy1.RotationAngle do Y := Y + ( P.X - _MouseP.X ) / 2;
          with Dummy2.RotationAngle do X := X - ( P.Y - _MouseP.Y ) / 2;

          _MouseP := P;
     end;
end;

procedure TForm1.Viewport3D1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
     Viewport3D1MouseMove( Sender, Shift, X, Y );

     _MouseS := [];
end;

////////////////////////////////////////////////////////////////////////////////

procedure TForm1.Timer1Timer(Sender: TObject);
begin
     with Sphere1.RotationAngle do Y := Y + 1;
     with Sphere2.RotationAngle do Y := Y + 1;
end;

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

     //GlobalUseDX := False;

end. //######################################################################### ■
