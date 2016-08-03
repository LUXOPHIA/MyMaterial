﻿unit LUX.FMX;

interface //#################################################################### ■

uses System.Types, System.UITypes, System.Math.Vectors, System.Generics.Collections, System.Classes,
     FMX.Types, FMX.Graphics,
     FMX.Types3D, FMX.Controls3D, FMX.Objects3D, FMX.Viewport3D, FMX.MaterialSources,
     LUX;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HBitmapData

     HBitmapData = record helper for TBitmapData
     private
       ///// アクセス
       function GetPixels( const X_,Y_:Integer ) :TAlphaColor;
       procedure SetPixels( const X_,Y_:Integer; const Pixels_:TAlphaColor );
     public
       ///// プロパティ
       property Pixels[ const X_,Y_:Integer ] :TAlphaColor read GetPixels write SetPixels;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HCanvas

     HCanvas = class helper for TCanvas
     private
     protected
       ///// アクセス
       function GetMatrix :TMatrix;
       procedure SetMatrix( const Matrix_:TMatrix );
     public
       property Matrix :TMatrix read GetMatrix write SetMatrix;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HMeshData

     HMeshData = class helper for TMeshData
     private
     protected
     public
       ///// メソッド
       procedure SaveToFileBinSTL( const FileName_:String; const Text_:AnsiString = '' );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HControl3D

     HControl3D = class helper for TControl3D
     private
     protected
       ///// アクセス
       function GetAbsolMatrix :TMatrix3D;
       procedure SetAbsoluteMatrix( const AbsoluteMatrix_:TMatrix3D ); virtual;
       function GetLocalMatrix :TMatrix3D; virtual;
       procedure SetLocalMatrix( const LocalMatrix_:TMatrix3D ); virtual;
       ///// メソッド
       procedure RecalcFamilyAbsolute;
       procedure RecalcChildrenAbsolute;
     public
       ///// プロパティ
       property AbsoluteMatrix :TMatrix3D read GetAbsolMatrix write SetAbsoluteMatrix;
       property LocalMatrix    :TMatrix3D read GetLocalMatrix write SetLocalMatrix   ;
       ///// メソッド
       procedure RenderInternalTo( const Context_:TContext3D );
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HCustomMesh

     HCustomMesh = class helper for TCustomMesh
     private
     protected
       ///// アクセス
       function GetMeshData :TMeshData;
     public
       ///// プロパティ
       property MeshData :TMeshData read GetMeshData;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TLuxViewport3D

     TLuxViewport3D = class( TViewport3D )
     private
       _Bitmap        :TBitmap;
       _DrawOK        :Boolean;
       _RenderingList :TList<TControl3D>;
     protected
       ///// メソッド
       procedure Paint; override;
       procedure Resize; override;
     public
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// メソッド
       procedure RebuildRenderingList;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TTensors

     TTensors = class( TControl3D )
     private
     protected
       _GeometryX :TMeshData;
       _GeometryY :TMeshData;
       _GeometryZ :TMeshData;
       _MaterialX :TColorMaterialSource;
       _MaterialY :TColorMaterialSource;
       _MaterialZ :TColorMaterialSource;
       _MeshData  :TMeshData;
       _AxisSize  :Single;
       ///// アクセス
       procedure SetMeshData( const MeshData_:TMeshData );
       procedure SetAxisSize( const AxisSize_:Single );
       ///// メソッド
       procedure Render; override;
     public
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property MeshData :TMeshData read _MeshData write SetMeshData;
       property AxisSize :Single    read _AxisSize write SetAxisSize;
       ///// メソッド
       procedure MakeModel;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils, System.RTLConsts, System.AnsiStrings,
     FMX.Controls,
     LUX.D3;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HBitmapData

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function HBitmapData.GetPixels( const X_,Y_:Integer ) :TAlphaColor;
begin
     Result := GetPixel( X_, Y_ );
end;

procedure HBitmapData.SetPixels( const X_,Y_:Integer; const Pixels_:TAlphaColor );
begin
     SetPixel( X_, Y_, Pixels_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HCanvas

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function HCanvas.GetMatrix :TMatrix;
begin
     with Self do
     begin
          Result := FMatrix;
     end;
end;

procedure HCanvas.SetMatrix( const Matrix_:TMatrix );
begin
     inherited SetMatrix( Matrix_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HMeshData

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

/////////////////////////////////////////////////////////////////////// メソッド

procedure HMeshData.SaveToFileBinSTL( const FileName_:String; const Text_:AnsiString = '' );
var
   Cs :array [ 0..80-1 ] of AnsiChar;
   N, I :Cardinal;
   Face :packed record
           Nor  :TSingle3D;
           Pos1 :TSingle3D;
           Pos2 :TSingle3D;
           Pos3 :TSingle3D;
           temp :Word;
         end;
begin
     with TFileStream.Create( FileName_, fmCreate ) do
     begin
          try
             System.AnsiStrings.StrLCopy( Cs, PAnsiChar( Text_ ), Length( Cs )-1 );

             Write( Cs, 80 );

             N := IndexBuffer.Length div 3;

             Write( N, SizeOf( N ) );

             for I := 0 to N-1 do
             begin
                  with Face do
                  begin
                       Nor  := TSingle3D.Create( 0, 0, 0 );
                       Pos1 := VertexBuffer.Vertices[ IndexBuffer.Indices[ 3*I+0 ] ];
                       Pos2 := VertexBuffer.Vertices[ IndexBuffer.Indices[ 3*I+1 ] ];
                       Pos3 := VertexBuffer.Vertices[ IndexBuffer.Indices[ 3*I+2 ] ];
                       temp := 0;
                  end;

                  Write( Face, SizeOf( Face ) );
             end;

          finally
                 Free;
          end;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HControl3D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function HControl3D.GetAbsolMatrix :TMatrix3D;
begin
     if FRecalcAbsolute then
     begin
          if FParent is TControl3D then FAbsoluteMatrix := FLocalMatrix * TControl3D(FParent).AbsoluteMatrix
                                   else FAbsoluteMatrix := FLocalMatrix;

          Result := FAbsoluteMatrix;

          FInvAbsoluteMatrix := FAbsoluteMatrix.Inverse;

          FRecalcAbsolute := False;
     end
     else Result := FAbsoluteMatrix;
end;

procedure HControl3D.SetAbsoluteMatrix( const AbsoluteMatrix_:TMatrix3D );
begin
        FAbsoluteMatrix := AbsoluteMatrix_;
     FInvAbsoluteMatrix := AbsoluteMatrix_.Inverse;

     if Assigned( FParent ) and ( FParent is TControl3D )
     then FLocalMatrix := FAbsoluteMatrix * TControl3D( FParent ).AbsoluteMatrix.Inverse
     else FLocalMatrix := FAbsoluteMatrix;

     RecalcChildrenAbsolute;

     Repaint;
end;

function HControl3D.GetLocalMatrix :TMatrix3D;
begin
     Result := FLocalMatrix;
end;

procedure HControl3D.SetLocalMatrix( const LocalMatrix_:TMatrix3D );
begin
     FLocalMatrix := LocalMatrix_;

     RecalcFamilyAbsolute;

     Repaint;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure HControl3D.RecalcFamilyAbsolute;
begin
     RecalcAbsolute;
end;

procedure HControl3D.RecalcChildrenAbsolute;
var
   F :TFmxObject;
begin
     FRecalcAbsolute := False;

     if Assigned( Children ) then
     begin
          for F in Children do
          begin
               if F is TControl3D then TControl3D( F ).RecalcFamilyAbsolute;
          end;
     end;
end;

procedure HControl3D.RenderInternalTo( const Context_:TContext3D );
begin
     TempContext := Context_;

     RenderInternal;

     TempContext := nil;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% HCustomMesh

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function HCustomMesh.GetMeshData :TMeshData;
begin
     Result := Data;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TLuxViewport3D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TLuxViewport3D.Paint;
var
   R :TRectF;
   I :Integer;
   C :TControl3D;
begin
     if ( csDesigning in ComponentState ) then
     begin
          R := LocalRect;

          InflateRect( R, -0.5, -0.5 );

          Canvas.DrawDashRect( R, 0, 0, AllCorners, AbsoluteOpacity, $A0909090 );
     end;

     if _DrawOK then
     begin
          _DrawOK := False;

          try
             if Assigned( Context ) then
             begin
                  Canvas.Flush;

                  with Context do
                  begin
                       if BeginScene then
                       try
                          SetContextState( TContextState.csScissorOff );
                          Clear( [ TClearTarget.Color, TClearTarget.Depth ], Color, 1.0, 0 );
                          SetCameraMatrix( Camera.CameraMatrix );
                          SetCameraAngleOfView( Camera.AngleOfView );

                          Lights.Clear;
                          for I := 0 to Camera.Viewport.LightCount-1
                          do Lights.Add( Camera.Viewport.Lights[ I ].LightDescription );

                          for C in _RenderingList do
                          begin
                               with C do
                               begin
                                    if Visible or ( not Visible and ( csDesigning in ComponentState ) and not Locked )
                                    then RenderInternalTo( Self.Context );
                               end;
                          end;

                       finally
                              EndScene;
                       end;

                       CopyToBitmap( _Bitmap, _Bitmap.Bounds );
                  end;
             end;
          finally
                 _DrawOK := True;
          end;

          inherited Canvas.DrawBitmap( _Bitmap, _Bitmap.BoundsF, LocalRect, AbsoluteOpacity, True );
     end;
end;

procedure TLuxViewport3D.Resize;
begin
     inherited;

     FreeAndNil( _Bitmap );

     with Context do _Bitmap := TBitmap.Create( Width, Height );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TLuxViewport3D.Create( Owner_:TComponent );
begin
     inherited;

     _DrawOK        := True;
     _RenderingList := TList<TControl3D>.Create;

     UsingDesignCamera := False;
end;

destructor TLuxViewport3D.Destroy;
begin
     FreeAndNil( _Bitmap        );
     FreeAndNil( _RenderingList );

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TLuxViewport3D.RebuildRenderingList;
var
   F :TFmxObject;
 //CompareFunc :TRenderingCompare;
begin
     with TViewport3D( Camera.Viewport ) do
     begin
          if Assigned( Children ) and ( Children.Count > 0 ) and ( FUpdating = 0 ) then
          begin
               _RenderingList.Clear;

               for F in Children do
               begin
                    if ( F is TControl3D ) then _RenderingList.Add( F as TControl3D );
               end;

               {
               CompareFunc := TRenderingCompare.Create;

               try
                  _RenderingList.Sort( CompareFunc );

               finally
                      CompareFunc.Free;
               end;
               }
          end;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TTensors

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

procedure TTensors.SetMeshData( const MeshData_:TMeshData );
begin
     _MeshData := MeshData_;  MakeModel;
end;

procedure TTensors.SetAxisSize( const AxisSize_:Single );
begin
     _AxisSize := AxisSize_;  MakeModel;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TTensors.Render;
begin
     with Context do
     begin
          SetMatrix( AbsoluteMatrix );

          DrawLines( _GeometryX.VertexBuffer, _GeometryX.IndexBuffer, _MaterialX.Material, AbsoluteOpacity );
          DrawLines( _GeometryY.VertexBuffer, _GeometryY.IndexBuffer, _MaterialY.Material, AbsoluteOpacity );
          DrawLines( _GeometryZ.VertexBuffer, _GeometryZ.IndexBuffer, _MaterialZ.Material, AbsoluteOpacity );
     end;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TTensors.Create( Owner_:TComponent );
begin
     inherited;

     HitTest := False;

     _GeometryX := TMeshData.Create;
     _GeometryY := TMeshData.Create;
     _GeometryZ := TMeshData.Create;

     _MaterialX := TColorMaterialSource.Create( Self );
     _MaterialY := TColorMaterialSource.Create( Self );
     _MaterialZ := TColorMaterialSource.Create( Self );

     _MaterialX.Color := TAlphaColors.Red;
     _MaterialY.Color := TAlphaColors.Lime;
     _MaterialZ.Color := TAlphaColors.Blue;

     _AxisSize := 0.05;
end;

destructor TTensors.Destroy;
begin
     _GeometryX.Free;
     _GeometryY.Free;
     _GeometryZ.Free;

     inherited;
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TTensors.MakeModel;
var
   N, I, J :Integer;
   P, EX, EY, EZ :TPoint3D;
begin
     with _MeshData.VertexBuffer do
     begin
          N := Length * 2{Poin};

          _GeometryX.VertexBuffer.Length := N;
          _GeometryY.VertexBuffer.Length := N;
          _GeometryZ.VertexBuffer.Length := N;

          _GeometryX.IndexBuffer .Length := N;
          _GeometryY.IndexBuffer .Length := N;
          _GeometryZ.IndexBuffer .Length := N;

          J := 0;
          for I := 0 to Length-1 do
          begin
               P  := Vertices [ I ];
               EX := Tangents [ I ];
               EY := BiNormals[ I ];
               EZ := Normals  [ I ];

               _GeometryX.VertexBuffer.Vertices[ J ] := P;
               _GeometryY.VertexBuffer.Vertices[ J ] := P;
               _GeometryZ.VertexBuffer.Vertices[ J ] := P;

               _GeometryX.IndexBuffer .Indices [ J ] := J;
               _GeometryY.IndexBuffer .Indices [ J ] := J;
               _GeometryZ.IndexBuffer .Indices [ J ] := J;

               Inc( J );

               _GeometryX.VertexBuffer.Vertices[ J ] := P + _AxisSize * EX;
               _GeometryY.VertexBuffer.Vertices[ J ] := P + _AxisSize * EY;
               _GeometryZ.VertexBuffer.Vertices[ J ] := P + _AxisSize * EZ;

               _GeometryX.IndexBuffer .Indices [ J ] := J;
               _GeometryY.IndexBuffer .Indices [ J ] := J;
               _GeometryZ.IndexBuffer .Indices [ J ] := J;

               Inc( J );
          end;
     end;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
