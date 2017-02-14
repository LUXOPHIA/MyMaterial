﻿                                                                //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【設定】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

//##############################################################################

struct TSenderV               //頂点の変数型
{
    float4 Pos :POSITION   ;  //位置（ローカル）
    float4 Tan :TANGENT    ;  //接線（ローカル）
    float4 Bin :BINORMAL   ;  //従法線（ローカル）
    float4 Nor :NORMAL     ;  //法線（ローカル）
    float4 Tex :TEXCOORD   ;  //テクスチャ座標
};

struct TResultV               //フラグメントの変数型
{
    float4 Scr :SV_Position;  //位置（スクリーン）
    float4 Pos :TEXCOORD0  ;  //位置（グローバル）
    float4 Tan :TANGENT    ;  //接線（グローバル）
    float4 Bin :BINORMAL   ;  //従法線（グローバル）
    float4 Nor :NORMAL     ;  //法線（グローバル）
    float4 Tex :TEXCOORD1  ;  //テクスチャ座標
};

////////////////////////////////////////////////////////////////////////////////

TResultV MainV( TSenderV _Sender )
{
    TResultV _Result;

    _Result.Scr = mul( _Sender.Pos, _FMatrixMVP );
    _Result.Pos = mul( _Sender.Pos, _FMatrixMV  );
    _Result.Tan = mul( _Sender.Tan, _IMatrixMV  );
    _Result.Bin = mul( _Sender.Bin, _IMatrixMV  );
    _Result.Nor = mul( _Sender.Nor, _IMatrixMV  );
    _Result.Tex =      _Sender.Tex               ;

    return _Result;
}

//##############################################################################
