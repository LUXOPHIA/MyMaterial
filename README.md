# MyMaterial
FireMonkey の 3Dモデル へ適用できる独自のマテリアルを作る方法。

HLSL @ Direct3D でシェーダを自由に記述することができます。
> ####【図1】 実行画面  
> ![MyMaterial.png](https://raw.githubusercontent.com/LUXOPHIA/MyMaterial/master/--------/_ScreenShot/MyMaterial.png)  
> ・左：標準の TLightMaterialSource  
> ・右：自作の TMyMaterialSource

シェーダへの外部変数の提供は、独自の変数クラス「TShaderVar」によって管理されるので、シェーダソース内にも変数宣言を記述する必要はありません。
> ####【図2】 頂点シェーダソース
> ![_02-VertexShader.png](https://raw.githubusercontent.com/LUXOPHIA/MyMaterial/master/--------/_ScreenShot/MyMaterial_02-VertexShader.png)  
> ####【図3】 ピクセルシェーダソース
> ![MyMaterial_03-PixelShader.png](https://raw.githubusercontent.com/LUXOPHIA/MyMaterial/master/--------/_ScreenShot/MyMaterial_03-PixelShader.png)  

ピクセルシェーダの実行を中断させる [clip](https://msdn.microsoft.com/ja-jp/library/bb509579(v=vs.85).aspx) 関数を用いて、描画を部分的にキャンセルすることもできます。
> ####【図4】 切断表示
> ![MyMaterial_01-View.png](https://raw.githubusercontent.com/LUXOPHIA/MyMaterial/master/--------/_ScreenShot/MyMaterial_01-View.png)  
