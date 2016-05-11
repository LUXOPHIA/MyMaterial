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
       _FMatrixMVP :TShaderVarMatrix;
       _FMatrixMV  :TShaderVarMatrix;
       _TIMatrixMV :TShaderVarMatrix;
       _EyePos     :TShaderVarVector;
       _Opacity    :TShaderVarFloat;
       _EmisColor  :TShaderVarColor;
       _AmbiColor  :TShaderVarColor;
       _DiffColor  :TShaderVarColor;
       _SpecColor  :TShaderVarColor;
       _SpecShiny  :TShaderVarFloat;
       _Light      :TShaderVarLight;
       _Texture    :TShaderVarTexture;
       ///// メソッド
       procedure DoApply( const Context_:TContext3D ); override;
     public
       constructor Create; override;
       destructor Destroy; override;
       ///// プロパティ
       property EmisColor :TShaderVarColor   read _EmisColor;
       property AmbiColor :TShaderVarColor   read _AmbiColor;
       property DiffColor :TShaderVarColor   read _DiffColor;
       property SpecColor :TShaderVarColor   read _SpecColor;
       property SpecShiny :TShaderVarFloat   read _SpecShiny;
       property Texture   :TShaderVarTexture read _Texture  ;
     end;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterialSource

     TMyMaterialSource = class( TLuxMaterialSource<TMyMaterial> )
     private
     protected
       _Texture        :TBitmap;
       _ContextResetId :Integer;
       ///// アクセス
       function GetEmisColor :TAlphaColor;
       procedure SetEmisColor( const EmisColor_:TAlphaColor );
       function GetAmbiColor :TAlphaColor;
       procedure SetAmbiColor( const AmbiColor_:TAlphaColor );
       function GetDiffColor :TAlphaColor;
       procedure SetDiffColor( const DiffColor_:TAlphaColor );
       function GetSpecColor :TAlphaColor;
       procedure SetSpecColor( const SpecColor_:TAlphaColor );
       function GetSpecShiny :Single;
       procedure SetSpecShiny( const SpecShiny_:Single );
       procedure SetTexture( const Texture_:TBitmap );
       procedure ContextResetHandler( const Sender_:TObject; const Msg_:TMessage );
       ///// メソッド
       procedure DoTextureChanged( Sender_:TObject );
     public
       constructor Create( Owner_:TComponent ); override;
       destructor Destroy; override;
       ///// プロパティ
       property EmisColor :TAlphaColor read GetEmisColor write SetEmisColor;
       property AmbiColor :TAlphaColor read GetAmbiColor write SetAmbiColor;
       property DiffColor :TAlphaColor read GetDiffColor write SetDiffColor;
       property SpecColor :TAlphaColor read GetSpecColor write SetSpecColor;
       property SpecShiny :Single      read GetSpecShiny write SetSpecShiny;
       property Texture   :TBitmap     read   _Texture   write SetTexture  ;
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
          _EyePos    .Value := CurrentCameraInvMatrix.M[ 3 ];
          _Opacity   .Value := CurrentOpacity;
          _Light     .Value := Lights[ 0 ];
     end;

     _ShaderV.SendVars( Context_ );
     _ShaderP.SendVars( Context_ );
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyMaterial.Create;
begin
     inherited;

     _FMatrixMVP := TShaderVarMatrix .Create( 'FMatrixMVP'  );
     _FMatrixMV  := TShaderVarMatrix .Create( 'FMatrixMV'   );
     _TIMatrixMV := TShaderVarMatrix .Create( 'IMatrixMV'   );
     _EyePos     := TShaderVarVector .Create( '_EyePos'     );
     _Opacity    := TShaderVarFloat  .Create( '_Opacity'    );
     _EmisColor  := TShaderVarColor  .Create( '_EmisColor'  );
     _AmbiColor  := TShaderVarColor  .Create( '_AmbiColor'  );
     _DiffColor  := TShaderVarColor  .Create( '_DiffColor'  );
     _SpecColor  := TShaderVarColor  .Create( '_SpecColor'  );
     _SpecShiny  := TShaderVarFloat  .Create( '_SpecShiny'  );
     _Light      := TShaderVarLight  .Create( '_Light'      );
     _Texture    := TShaderVarTexture.Create( '_Texture'    );

     _ShaderV.Vars := [ _FMatrixMVP,
                        _FMatrixMV ,
                        _TIMatrixMV ];

     _ShaderP.Vars := [ _FMatrixMVP,
                        _FMatrixMV ,
                        _TIMatrixMV,
                        _EyePos    ,
                        _Opacity   ,
                        _EmisColor ,
                        _AmbiColor ,
                        _DiffColor ,
                        _SpecColor ,
                        _SpecShiny ,
                        _Light     ,
                        _Texture    ];

     _EmisColor.Value := TAlphaColors.Null;
     _AmbiColor.Value := $FF202020;
     _DiffColor.Value := $FFFFFFFF;
     _SpecColor.Value := $FF606060;
     _SpecShiny.Value := 30;
end;

destructor TMyMaterial.Destroy;
begin
     _FMatrixMVP.Free;
     _FMatrixMV .Free;
     _TIMatrixMV.Free;
     _EyePos    .Free;
     _Opacity   .Free;
     _EmisColor .Free;
     _AmbiColor .Free;
     _DiffColor .Free;
     _SpecColor .Free;
     _SpecShiny .Free;
     _Light     .Free;
     _Texture   .Free;

     inherited;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TMyMaterialSource

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& protected

/////////////////////////////////////////////////////////////////////// アクセス

function TMyMaterialSource.GetEmisColor :TAlphaColor;
begin
     Result := _Material.EmisColor.Value;
end;

procedure TMyMaterialSource.SetEmisColor( const EmisColor_:TAlphaColor );
begin
     _Material.EmisColor.Value := EmisColor_;
end;

function TMyMaterialSource.GetAmbiColor :TAlphaColor;
begin
     Result := _Material.AmbiColor.Value;
end;

procedure TMyMaterialSource.SetAmbiColor( const AmbiColor_:TAlphaColor );
begin
     _Material.AmbiColor.Value := AmbiColor_;
end;

function TMyMaterialSource.GetDiffColor: TAlphaColor;
begin
     Result := _Material.DiffColor.Value;
end;

procedure TMyMaterialSource.SetDiffColor( const DiffColor_:TAlphaColor );
begin
     _Material.DiffColor.Value := DiffColor_;
end;

function TMyMaterialSource.GetSpecColor :TAlphaColor;
begin
     Result := _Material.SpecColor.Value;
end;

procedure TMyMaterialSource.SetSpecColor( const SpecColor_:TAlphaColor );
begin
     _Material.SpecColor.Value := SpecColor_;
end;

function TMyMaterialSource.GetSpecShiny :Single;
begin
     Result := _Material.SpecShiny.Value;
end;

procedure TMyMaterialSource.SetSpecShiny( const SpecShiny_:Single );
begin
     _Material.SpecShiny.Value := SpecShiny_;
end;

procedure TMyMaterialSource.SetTexture( const Texture_:TBitmap );
begin
     _Texture.Assign( Texture_ );
end;

procedure TMyMaterialSource.ContextResetHandler( const Sender_:TObject; const Msg_:TMessage );
begin
     DoTextureChanged( Self );
end;

/////////////////////////////////////////////////////////////////////// メソッド

procedure TMyMaterialSource.DoTextureChanged( Sender_:TObject );
begin
     if not _Texture.IsEmpty then _Material.Texture.Value := TTextureBitmap( _Texture ).Texture;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TMyMaterialSource.Create( Owner_:TComponent );
begin
     inherited;

     _Texture := TTextureBitmap.Create;

     _Texture.OnChange := DoTextureChanged;

     _ContextResetId := TMessageManager.DefaultManager.SubscribeToMessage( TContextResetMessage, ContextResetHandler );
end;

destructor TMyMaterialSource.Destroy;
begin
     TMessageManager.DefaultManager.Unsubscribe( TContextResetMessage, _ContextResetId );

     FreeAndNil( _Texture );

     inherited;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//############################################################################## □

initialization //######################################################## 初期化

finalization //########################################################## 最終化

end. //######################################################################### ■
