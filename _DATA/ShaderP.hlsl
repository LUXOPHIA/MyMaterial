//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

static const float Pi = 3.141592653589793;

static const float Pi2 = Pi * 2.0;

static const float P2i = Pi / 2.0;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【設定】

SamplerState _SamplerState {};

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

float Pow2( float X_ )
{
    return X_ * X_;
}

float Roo2( float X_ )
{
    return sqrt( X_ );
}

float2 VectorToSky( float3 Vector_ )
{
    float2 _Result;

    _Result.x = ( Pi - atan2( -Vector_.z, -Vector_.x ) ) / Pi2;
    _Result.y =        acos ( -Vector_.y             )   / Pi ;

    return _Result;
}

float Fresnel( float3 EyeVec_, float3 NorVec_, float RefI_ )
{
    float N = Pow2( RefI_ );
    float C = dot( EyeVec_, NorVec_ );
    float G = sqrt( N + Pow2( C ) - 1 );
    float NC = N * C;
    return ( Pow2( (  C - G ) / (  C + G ) )
           + Pow2( ( NC - G ) / ( NC + G ) ) ) / 2;

    /* 近似
    float R = pow( ( RefI_ - 1 ) / ( RefI_ + 1 ), 2 );
    float C = dot( EyeVec_, NorVec_ );
    return R + ( 1 - R ) * pow( 1 - C, 5 );
    */
}

//##############################################################################

struct TSenderP               //フラグメントの変数型
{
    float4 Scr :SV_Position;  //位置（スクリーン）
    float4 Pos :TEXCOORD0  ;  //位置（グローバル）
    float4 Tan :TANGENT    ;  //接線（グローバル）
    float4 Bin :BINORMAL   ;  //従法線（グローバル）
    float4 Nor :NORMAL     ;  //法線（グローバル）
    float4 Tex :TEXCOORD1  ;  //テクスチャ座標
};

struct TResultP               //ピクセルの変数型
{
    float4 Col :SV_Target  ;  //色
};

////////////////////////////////////////////////////////////////////////////////

TResultP MainP( TSenderP _Sender )
{
    TResultP _Result;

    float3 N = normalize( _Sender.Nor.xyz );                                    //表面法線（グローバル）
    float3 T = normalize( _Sender.Tan.xyz );                                    //表面接線（グローバル）
    float3 B = normalize( _Sender.Bin.xyz );                                    //表面従法線（グローバル）
    float3 L = -_Light.Dir.xyz;                                                 //光線方向（グローバル）
    float3 V = normalize( _EyePos.xyz - _Sender.Pos.xyz );                      //視線方向（グローバル）
    float3 H = normalize( L + V );                                              //ハーフベクトル

    //--------------------------------------------------------------------------

    if ( dot( N, V ) < 0 ) N = -N;                                              //裏面の法線を反転

    clip( -_Sender.Pos.z );                                                     //半球の描画をキャンセル

    //--------------------------------------------------------------------------

    float LD = max( dot( N, L ), 0.0 );                                         //光線拡散反射率

    float4 D = _DiffImage.Sample( _SamplerState, _Sender.Tex.xy );              //拡散反射率マップ色

    float4 CD = ( _AmbiLight + _Light.Col * LD ) * ( _DiffRatio * D );          //拡散反射光

    //--------------------------------------------------------------------------

    float LS = pow( max( dot( N, H ), 0.0 ), _SpecShiny );                      //光線鏡面反射率

    float4 CS = ( _Light.Col * LS ) * _SpecRatio;                               //光線鏡面反射光

    //--------------------------------------------------------------------------

    _Result.Col = _EmisLight + CD + CS;

    //--------------------------------------------------------------------------

    _Result.Col.a = 1;

    _Result.Col = _Opacity * _Result.Col;

    return _Result;
}

//##############################################################################
