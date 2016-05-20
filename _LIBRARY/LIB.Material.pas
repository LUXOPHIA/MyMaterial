unit LIB.Material;

interface //#################################################################### ■

uses System.Classes, System.UITypes, System.Messaging,
     FMX.Graphics, FMX.Types3D,
     LUX, LUX.FMX.Material;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterial

     TMyMaterial = class( TLuxMaterial )
     private
     protected
       _FMatrixMVP :TShaderVarMatrix3D;
       _FMatrixMV  :TShaderVarMatrix3D;
       _TIMatrixMV :TShaderVarMatrix3D;
       _Light      :TShaderVarLight;
       _EyePos     :TShaderVarVector3D;
       _Opacity    :TShaderVarSingle;
       _EmisLight  :TShaderVarColorF;
       _AmbiLight  :TShaderVarColorF;
       _DiffRatio  :TShaderVarColorF;
       _SpecRatio  :TShaderVarColorF;
       _SpecShiny  :TShaderVarSingle;
       _DiffImage  :TShaderVarTexture;
       ///// メソッド
       procedure DoApply( const Context_:TContext3D ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property EmisLight :TShaderVarColorF  read _EmisLight;
       property AmbiLight :TShaderVarColorF  read _AmbiLight;
       property DiffRatio :TShaderVarColorF  read _DiffRatio;
       property SpecRatio :TShaderVarColorF  read _SpecRatio;
       property SpecShiny :TShaderVarSingle  read _SpecShiny;
       property DiffImage :TShaderVarTexture read _DiffImage;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterialSource

     TMyMaterialSource = class( TLuxMaterialSource<TMyMaterial> )
     private
       _ContextResetId :Integer;
       ///// メソッド
       procedure ContextResetHandler( const Sender_:TObject; const Msg_:TMessage );
     protected
       _DiffImage :TBitmap;
       ///// アクセス
       function GetEmisLight :TAlphaColorF;
       procedure SetEmisLight( const EmisLight_:TAlphaColorF );
       function GetAmbiLight :TAlphaColorF;
       procedure SetAmbiLight( const AmbiLight_:TAlphaColorF );
       function GetDiffRatio :TAlphaColorF;
       procedure SetDiffRatio( const DiffRatio_:TAlphaColorF );
       function GetSpecRatio :TAlphaColorF;
       procedure SetSpecRatio( const SpecRatio_:TAlphaColorF );
       function GetSpecShiny :Single;
       procedure SetSpecShiny( const SpecShiny_:Single );
       procedure SetDiffImage( const DiffImage_:TBitmap );
       ///// メソッド
       procedure DoDiffImageChanged( Sender_:TObject );
     public
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property EmisLight :TAlphaColorF read GetEmisLight write SetEmisLight;
       property AmbiLight :TAlphaColorF read GetAmbiLight write SetAmbiLight;
       property DiffRatio :TAlphaColorF read GetDiffRatio write SetDiffRatio;
       property SpecRatio :TAlphaColorF read GetSpecRatio write SetSpecRatio;
       property SpecShiny :Single       read GetSpecShiny write SetSpecShiny;
       property DiffImage :TBitmap      read   _DiffImage write SetDiffImage;
     end;

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

implementation //############################################################### ■

uses System.SysUtils;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterial

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMyMaterial.DoApply( const Context_:TContext3D );
begin
     inherited;

     with Context_ do
     begin
          SetShaders( _ShaderV.Shader, _ShaderP.Shader );

          _FMatrixMVP.Value := CurrentModelViewProjectionMatrix;
          _FMatrixMV .Value := CurrentMatrix;
          _TIMatrixMV.Value := CurrentMatrix.Inverse.Transpose;
          _Light     .Value := Lights[ 0 ];
          _EyePos    .Value := CurrentCameraInvMatrix.M[ 3 ];
          _Opacity   .Value := CurrentOpacity;
     end;

     _ShaderV.SendVars( Context_ );
     _ShaderP.SendVars( Context_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyMaterial.Create;
begin
     inherited;

     _FMatrixMVP := TShaderVarMatrix3D.Create( 'FMatrixMVP'  );
     _FMatrixMV  := TShaderVarMatrix3D.Create( 'FMatrixMV'   );
     _TIMatrixMV := TShaderVarMatrix3D.Create( 'IMatrixMV'   );
     _Light      := TShaderVarLight   .Create( '_Light'      );
     _EyePos     := TShaderVarVector3D.Create( '_EyePos'     );
     _Opacity    := TShaderVarSingle  .Create( '_Opacity'    );
     _EmisLight  := TShaderVarColorF  .Create( '_EmisLight'  );
     _AmbiLight  := TShaderVarColorF  .Create( '_AmbiLight'  );
     _DiffRatio  := TShaderVarColorF  .Create( '_DiffRatio'  );
     _SpecRatio  := TShaderVarColorF  .Create( '_SpecRatio'  );
     _SpecShiny  := TShaderVarSingle  .Create( '_SpecShiny'  );
     _DiffImage  := TShaderVarTexture .Create( '_DiffImage'  );

     _ShaderV.Vars := [ _FMatrixMVP,
                        _FMatrixMV ,
                        _TIMatrixMV ];

     _ShaderP.Vars := [ _FMatrixMVP,
                        _FMatrixMV ,
                        _TIMatrixMV,
                        _Light     ,
                        _EyePos    ,
                        _Opacity   ,
                        _EmisLight ,
                        _AmbiLight ,
                        _DiffRatio ,
                        _SpecRatio ,
                        _SpecShiny ,
                        _DiffImage  ];

     _EmisLight.Value := TAlphaColorF.Create( 0, 0, 0 );
     _AmbiLight.Value := TAlphaColorF.Create( 0.1, 0.1, 0.1 );
     _DiffRatio.Value := TAlphaColorF.Create( 1, 1, 1 );
     _SpecRatio.Value := TAlphaColorF.Create( 1, 1, 1 );
     _SpecShiny.Value := 50;
end;

destructor TMyMaterial.Destroy;
begin
     _FMatrixMVP.Free;
     _FMatrixMV .Free;
     _TIMatrixMV.Free;
     _Light     .Free;
     _EyePos    .Free;
     _Opacity   .Free;
     _EmisLight .Free;
     _AmbiLight .Free;
     _DiffRatio .Free;
     _SpecRatio .Free;
     _SpecShiny .Free;
     _DiffImage .Free;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterialSource

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMyMaterialSource.ContextResetHandler( const Sender_:TObject; const Msg_:TMessage );
begin
     DoDiffImageChanged( Self );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TMyMaterialSource.GetEmisLight :TAlphaColorF;
begin
     Result := _Material.EmisLight.Value;
end;

procedure TMyMaterialSource.SetEmisLight( const EmisLight_:TAlphaColorF );
begin
     _Material.EmisLight.Value := EmisLight_;
end;

function TMyMaterialSource.GetAmbiLight :TAlphaColorF;
begin
     Result := _Material.AmbiLight.Value;
end;

procedure TMyMaterialSource.SetAmbiLight( const AmbiLight_:TAlphaColorF );
begin
     _Material.AmbiLight.Value := AmbiLight_;
end;

function TMyMaterialSource.GetDiffRatio: TAlphaColorF;
begin
     Result := _Material.DiffRatio.Value;
end;

procedure TMyMaterialSource.SetDiffRatio( const DiffRatio_:TAlphaColorF );
begin
     _Material.DiffRatio.Value := DiffRatio_;
end;

function TMyMaterialSource.GetSpecRatio :TAlphaColorF;
begin
     Result := _Material.SpecRatio.Value;
end;

procedure TMyMaterialSource.SetSpecRatio( const SpecRatio_:TAlphaColorF );
begin
     _Material.SpecRatio.Value := SpecRatio_;
end;

function TMyMaterialSource.GetSpecShiny :Single;
begin
     Result := _Material.SpecShiny.Value;
end;

procedure TMyMaterialSource.SetSpecShiny( const SpecShiny_:Single );
begin
     _Material.SpecShiny.Value := SpecShiny_;
end;

procedure TMyMaterialSource.SetDiffImage( const DiffImage_:TBitmap );
begin
     _DiffImage.Assign( DiffImage_ );
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMyMaterialSource.DoDiffImageChanged( Sender_:TObject );
begin
     if not _DiffImage.IsEmpty then _Material.DiffImage.Value := TTextureBitmap( _DiffImage ).Texture;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyMaterialSource.Create( Owner_:TComponent );
begin
     inherited;

     _ContextResetId := TMessageManager.DefaultManager.SubscribeToMessage( TContextResetMessage, ContextResetHandler );

     _DiffImage := TTextureBitmap.Create;

     _DiffImage.OnChange := DoDiffImageChanged;
end;

destructor TMyMaterialSource.Destroy;
begin
     FreeAndNil( _DiffImage );

     TMessageManager.DefaultManager.Unsubscribe( TContextResetMessage, _ContextResetId );

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //######################################################## 初期化

finalization //########################################################## 最終化

end. //######################################################################### ■
