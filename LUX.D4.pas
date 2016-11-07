﻿unit LUX.D4;

interface //#################################################################### ■

uses System.Math.Vectors,
     LUX, LUX.D1, LUX.D2, LUX.D3;

type //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【型】

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TSingle4D

     TSingle4D = record
     private
       ///// アクセス
       function GetSiz2 :Single; inline;
       procedure SetSiz2( const Siz2_:Single ); inline;
       function GetSize :Single; inline;
       procedure SetSize( const Size_:Single ); inline;
       function GetUnitor :TSingle4D; inline;
       procedure SetUnitor( const Unitor_:TSingle4D ); inline;
     public
       constructor Create( const V_:Single ); overload;
       constructor Create( const X_,Y_,Z_,W_:Single ); overload;
       ///// プロパティ
       property Siz2   :Single    read GetSiz2   write SetSiz2;
       property Size   :Single    read GetSize   write SetSize;
       property Unitor :TSingle4D read GetUnitor write SetUnitor;
       ///// 演算子
       class operator Negative( const V_:TSingle4D ) :TSingle4D;
       class operator Positive( const V_:TSingle4D ) :TSingle4D;
       class operator Add( const A_,B_:TSingle4D ) :TSingle4D;
       class operator Subtract( const A_,B_:TSingle4D ) :TSingle4D;
       class operator Multiply( const A_:TSingle4D; const B_:Single ) :TSingle4D;
       class operator Multiply( const A_:Single; const B_:TSingle4D ) :TSingle4D;
       class operator Divide( const A_:TSingle4D; const B_:Single ) :TSingle4D;
       ///// 型変換
       class operator Implicit( const V_:TSingle3D ) :TSingle4D;
       class operator Explicit( const V_:TSingle4D ) :TSingle3D;
       class operator Implicit( const V_:TPoint3D ) :TSingle4D;
       class operator Explicit( const V_:TSingle4D ) :TPoint3D;
       class operator Implicit( const V_:TVector3D ) :TSingle4D;
       class operator Explicit( const V_:TSingle4D ) :TVector3D;
       ///// 定数
       class function IdentityX :TSingle4D; inline; static;
       class function IdentityY :TSingle4D; inline; static;
       class function IdentityZ :TSingle4D; inline; static;
       class function IdentityW :TSingle4D; inline; static;
       ///// メソッド
       function VectorTo( const P_:TSingle4D ) :TSingle4D;
       function UnitorTo( const P_:TSingle4D ) :TSingle4D;
       function DistanTo( const P_:TSingle4D ) :Single;
       class function RandG :TSingle4D; static;
       class function RandBS1 :TSingle4D; static;
       class function RandBS2 :TSingle4D; static;
       class function RandBS4 :TSingle4D; static;

     case Integer of
      0:(  X :Single;
           Y :Single;
           Z :Single;
           W :Single;                  );
      1:( _1 :Single;
          _2 :Single;
          _3 :Single;
          _4 :Single;                  );
      2:( _  :array [ 1..4 ] of Single );
     end;

     TSinglePos4D = TSingle4D;
     TSingleVec4D = TSingle4D;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDouble4D

     TDouble4D = record
     private
       ///// アクセス
       function GetSiz2 :Double; inline;
       procedure SetSiz2( const Siz2_:Double ); inline;
       function GetSize :Double; inline;
       procedure SetSize( const Size_:Double ); inline;
       function GetUnitor :TDouble4D; inline;
       procedure SetUnitor( const Unitor_:TDouble4D ); inline;
     public
       constructor Create( const V_:Double ); overload;
       constructor Create( const X_,Y_,Z_,W_:Double ); overload;
       ///// プロパティ
       property Siz2   :Double    read GetSiz2   write SetSiz2;
       property Size   :Double    read GetSize   write SetSize;
       property Unitor :TDouble4D read GetUnitor write SetUnitor;
       ///// 演算子
       class operator Negative( const V_:TDouble4D ) :TDouble4D;
       class operator Positive( const V_:TDouble4D ) :TDouble4D;
       class operator Add( const A_,B_:TDouble4D ) :TDouble4D;
       class operator Subtract( const A_,B_:TDouble4D ) :TDouble4D;
       class operator Multiply( const A_:TDouble4D; const B_:Double ) :TDouble4D;
       class operator Multiply( const A_:Double; const B_:TDouble4D ) :TDouble4D;
       class operator Divide( const A_:TDouble4D; const B_:Double ) :TDouble4D;
       ///// 型変換
       class operator Implicit( const V_:TDouble3D ) :TDouble4D;
       class operator Explicit( const V_:TDouble4D ) :TDouble3D;
       class operator Implicit( const V_:TPoint3D ) :TDouble4D;
       class operator Explicit( const V_:TDouble4D ) :TPoint3D;
       class operator Implicit( const V_:TVector3D ) :TDouble4D;
       class operator Explicit( const V_:TDouble4D ) :TVector3D;
       ///// 定数
       class function IdentityX :TDouble4D; inline; static;
       class function IdentityY :TDouble4D; inline; static;
       class function IdentityZ :TDouble4D; inline; static;
       class function IdentityW :TDouble4D; inline; static;
       ///// メソッド
       function VectorTo( const P_:TDouble4D ) :TDouble4D;
       function UnitorTo( const P_:TDouble4D ) :TDouble4D;
       function DistanTo( const P_:TDouble4D ) :Double;
       class function RandG :TDouble4D; static;
       class function RandBS1 :TDouble4D; static;
       class function RandBS2 :TDouble4D; static;
       class function RandBS4 :TDouble4D; static;

     case Integer of
      0:(  X :Double;
           Y :Double;
           Z :Double;
           W :Double;                  );
      1:( _1 :Double;
          _2 :Double;
          _3 :Double;
          _4 :Double;                  );
      2:( _  :array [ 1..4 ] of Double );
     end;

     TDoublePos4D = TDouble4D;
     TDoubleVec4D = TDouble4D;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TdSingle4D

     TdSingle4D = record
     private
       ///// アクセス
       function Geto :TSingle4D; inline;
       procedure Seto( const o_:TSingle4D ); inline;
       function Getd :TSingle4D; inline;
       procedure Setd( const d_:TSingle4D ); inline;
       function GetSiz2 :TdSingle; inline;
       procedure SetSiz2( const Siz2_:TdSingle ); inline;
       function GetSize :TdSingle; inline;
       procedure SetSize( const Size_:TdSingle ); inline;
       function GetUnitor :TdSingle4D; inline;
       procedure SetUnitor( const Unitor_:TdSingle4D ); inline;
     public
       constructor Create( const X_,Y_,Z_,W_:TdSingle );
       ///// プロパティ
       property o      :TSingle4D  read Geto      write Seto     ;
       property d      :TSingle4D  read Getd      write Setd     ;
       property Siz2   :TdSingle   read GetSiz2   write SetSiz2  ;
       property Size   :TdSingle   read GetSize   write SetSize  ;
       property Unitor :TdSingle4D read GetUnitor write SetUnitor;
       ///// 演算子
       class operator Negative( const V_:TdSingle4D ) :TdSingle4D; inline;
       class operator Positive( const V_:TdSingle4D ) :TdSingle4D; inline;
       class operator Add( const A_,B_:TdSingle4D ) :TdSingle4D; inline;
       class operator Subtract( const A_,B_:TdSingle4D ) :TdSingle4D; inline;
       class operator Multiply( const A_:TdSingle4D; const B_:TdSingle ) :TdSingle4D; inline;
       class operator Multiply( const A_:TdSingle; const B_:TdSingle4D ) :TdSingle4D; inline;
       class operator Divide( const A_:TdSingle4D; const B_:TdSingle ) :TdSingle4D; inline;
       ///// 型変換
       class operator Implicit( const V_:TSingle4D ) :TdSingle4D;
       class operator Implicit( const V_:TdSingle4D ) :TSingle4D;

     case Integer of
      0:(  X :TdSingle;
           Y :TdSingle;
           Z :TdSingle;
           W :TdSingle;                  );
      1:( _1 :TdSingle;
          _2 :TdSingle;
          _3 :TdSingle;
          _4 :TdSingle;                  );
      2:( _  :array [ 1..4 ] of TdSingle );
     end;

     TdSinglePos4D = TdSingle4D;
     TdSingleVec4D = TdSingle4D;

     //%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TdDouble4D

     TdDouble4D = record
     private
       ///// アクセス
       function Geto :TDouble4D; inline;
       procedure Seto( const o_:TDouble4D ); inline;
       function Getd :TDouble4D; inline;
       procedure Setd( const d_:TDouble4D ); inline;
       function GetSiz2 :TdDouble; inline;
       procedure SetSiz2( const Siz2_:TdDouble ); inline;
       function GetSize :TdDouble; inline;
       procedure SetSize( const Size_:TdDouble ); inline;
       function GetUnitor :TdDouble4D; inline;
       procedure SetUnitor( const Unitor_:TdDouble4D ); inline;
     public
       constructor Create( const X_,Y_,Z_,W_:TdDouble );
       ///// プロパティ
       property o      :TDouble4D  read Geto      write Seto     ;
       property d      :TDouble4D  read Getd      write Setd     ;
       property Siz2   :TdDouble   read GetSiz2   write SetSiz2  ;
       property Size   :TdDouble   read GetSize   write SetSize  ;
       property Unitor :TdDouble4D read GetUnitor write SetUnitor;
       ///// 演算子
       class operator Negative( const V_:TdDouble4D ) :TdDouble4D; inline;
       class operator Positive( const V_:TdDouble4D ) :TdDouble4D; inline;
       class operator Add( const A_,B_:TdDouble4D ) :TdDouble4D; inline;
       class operator Subtract( const A_,B_:TdDouble4D ) :TdDouble4D; inline;
       class operator Multiply( const A_:TdDouble4D; const B_:TdDouble ) :TdDouble4D; inline;
       class operator Multiply( const A_:TdDouble; const B_:TdDouble4D ) :TdDouble4D; inline;
       class operator Divide( const A_:TdDouble4D; const B_:TdDouble ) :TdDouble4D; inline;
       ///// 型変換
       class operator Implicit( const V_:TDouble4D ) :TdDouble4D;
       class operator Implicit( const V_:TdDouble4D ) :TDouble4D;

     case Integer of
      0:(  X :TdDouble;
           Y :TdDouble;
           Z :TdDouble;
           W :TdDouble;                  );
      1:( _1 :TdDouble;
          _2 :TdDouble;
          _3 :TdDouble;
          _4 :TdDouble;                  );
      2:( _  :array [ 1..4 ] of TdDouble );
     end;

     TdDoublePos4D = TdDouble4D;
     TdDoubleVec4D = TdDouble4D;

     //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//const //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【定数】

//var //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【変数】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function DotProduct( const A_,B_:TSingleVec4D ) :Single; inline; overload;
function DotProduct( const A_,B_:TDoubleVec4D ) :Double; inline; overload;
function DotProduct( const A_,B_:TdSingleVec4D ) :TdSingle; inline; overload;
function DotProduct( const A_,B_:TdDoubleVec4D ) :TdDouble; inline; overload;

function CrossProduct( const A_,B_,C_:TSingleVec4D ) :TSingleVec4D; inline; overload;
function CrossProduct( const A_,B_,C_:TDoubleVec4D ) :TDoubleVec4D; inline; overload;
function CrossProduct( const A_,B_,C_:TdSingleVec4D ) :TdSingleVec4D; inline; overload;
function CrossProduct( const A_,B_,C_:TdDoubleVec4D ) :TdDoubleVec4D; inline; overload;

function Dista2( const A_,B_:TSinglePos4D ) :Single; inline; overload;
function Dista2( const A_,B_:TDoublePos4D ) :Double; inline; overload;
function Dista2( const A_,B_:TdSinglePos4D ) :TdSingle; inline; overload;
function Dista2( const A_,B_:TdDoublePos4D ) :TdDouble; inline; overload;

function Distan( const A_,B_:TSinglePos4D ) :Single; inline; overload;
function Distan( const A_,B_:TDoublePos4D ) :Double; inline; overload;
function Distan( const A_,B_:TdSinglePos4D ) :TdSingle; inline; overload;
function Distan( const A_,B_:TdDoublePos4D ) :TdDouble; inline; overload;

function Ave( const P1_,P2_:TSingle4D ) :TSingle4D; inline; overload;
function Ave( const P1_,P2_:TDouble4D ) :TDouble4D; inline; overload;
function Ave( const P1_,P2_:TdSingle4D ) :TdSingle4D; inline; overload;
function Ave( const P1_,P2_:TdDouble4D ) :TdDouble4D; inline; overload;

function Ave( const P1_,P2_,P3_:TSingle4D ) :TSingle4D; inline; overload;
function Ave( const P1_,P2_,P3_:TDouble4D ) :TDouble4D; inline; overload;
function Ave( const P1_,P2_,P3_:TdSingle4D ) :TdSingle4D; inline; overload;
function Ave( const P1_,P2_,P3_:TdDouble4D ) :TdDouble4D; inline; overload;

function Ave( const P1_,P2_,P3_,P4_:TSingle4D ) :TSingle4D; inline; overload;
function Ave( const P1_,P2_,P3_,P4_:TDouble4D ) :TDouble4D; inline; overload;
function Ave( const P1_,P2_,P3_,P4_:TdSingle4D ) :TdSingle4D; inline; overload;
function Ave( const P1_,P2_,P3_,P4_:TdDouble4D ) :TdDouble4D; inline; overload;

implementation //############################################################### ■

uses System.SysUtils, System.Math;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【レコード】

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TSingle4D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TSingle4D.GetSiz2 :Single;
begin
     Result := Pow2( X ) + Pow2( Y ) + Pow2( Z ) + Pow2( W );
end;

procedure TSingle4D.SetSiz2( const Siz2_:Single );
begin
     Self := Roo2( Siz2_ / Siz2 ) * Self;
end;

function TSingle4D.GetSize :Single;
begin
     Result := Roo2( GetSiz2 );
end;

procedure TSingle4D.SetSize( const Size_:Single );
begin
     Self := Size_ * Unitor;
end;

function TSingle4D.GetUnitor :TSingle4D;
begin
     Result := Self / Size;
end;

procedure TSingle4D.SetUnitor( const Unitor_:TSingle4D );
begin
     Self := Size * Unitor_;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TSingle4D.Create( const V_:Single );
begin
     X := V_;
     Y := V_;
     Z := V_;
     W := V_;
end;

constructor TSingle4D.Create( const X_,Y_,Z_,W_:Single );
begin
     X := X_;
     Y := Y_;
     Z := Z_;
     W := W_;
end;

///////////////////////////////////////////////////////////////////////// 演算子

class operator TSingle4D.Negative( const V_:TSingle4D ) :TSingle4D;
begin
     with Result do
     begin
          X := -V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W := -V_.W;
     end;
end;

class operator TSingle4D.Positive( const V_:TSingle4D ) :TSingle4D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := +V_.Y;
          Z := +V_.Z;
          W := +V_.W;
     end;
end;

class operator TSingle4D.Add( const A_,B_:TSingle4D ) :TSingle4D;
begin
     with Result do
     begin
          X := A_.X + B_.X;
          Y := A_.Y + B_.Y;
          Z := A_.Z + B_.Z;
          W := A_.W + B_.W;
     end;
end;

class operator TSingle4D.Subtract( const A_,B_:TSingle4D ) :TSingle4D;
begin
     with Result do
     begin
          X := A_.X - B_.X;
          Y := A_.Y - B_.Y;
          Z := A_.Z - B_.Z;
          W := A_.W - B_.W;
     end;
end;

class operator TSingle4D.Multiply( const A_:TSingle4D; const B_:Single ) :TSingle4D;
begin
     with Result do
     begin
          X := A_.X * B_;
          Y := A_.Y * B_;
          Z := A_.Z * B_;
          W := A_.W * B_;
     end;
end;

class operator TSingle4D.Multiply( const A_:Single; const B_:TSingle4D ) :TSingle4D;
begin
     with Result do
     begin
          X := A_ * B_.X;
          Y := A_ * B_.Y;
          Z := A_ * B_.Z;
          W := A_ * B_.W;
     end;
end;

class operator TSingle4D.Divide( const A_:TSingle4D; const B_:Single ) :TSingle4D;
begin
     with Result do
     begin
          X := A_.X / B_;
          Y := A_.Y / B_;
          Z := A_.Z / B_;
          W := A_.W / B_;
     end;
end;

///////////////////////////////////////////////////////////////////////// 型変換

class operator TSingle4D.Implicit( const V_:TSingle3D ) :TSingle4D;
begin
     with Result do
     begin
          X := V_.X;
          Y := V_.Y;
          Z := V_.Z;
          W :=    0;
     end;
end;

class operator TSingle4D.Explicit( const V_:TSingle4D ) :TSingle3D;
begin
     with Result do
     begin
          X := V_.X;
          Y := V_.Y;
          Z := V_.Z;
     end;
end;

class operator TSingle4D.Implicit( const V_:TPoint3D ) :TSingle4D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W :=     0;
     end;
end;

class operator TSingle4D.Explicit( const V_:TSingle4D ) :TPoint3D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
     end;
end;

class operator TSingle4D.Implicit( const V_:TVector3D ) :TSingle4D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W :=  V_.W;
     end;
end;

class operator TSingle4D.Explicit( const V_:TSingle4D ) :TVector3D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W :=  V_.W;
     end;
end;

/////////////////////////////////////////////////////////////////////////// 定数

class function TSingle4D.IdentityX :TSingle4D;
begin
     with Result do
     begin
          X := 1;
          Y := 0;
          Z := 0;
          W := 0;
     end;
end;

class function TSingle4D.IdentityY :TSingle4D;
begin
     with Result do
     begin
          X := 0;
          Y := 1;
          Z := 0;
          W := 0;
     end;
end;

class function TSingle4D.IdentityZ :TSingle4D;
begin
     with Result do
     begin
          X := 0;
          Y := 0;
          Z := 1;
          W := 0;
     end;
end;

class function TSingle4D.IdentityW :TSingle4D;
begin
     with Result do
     begin
          X := 0;
          Y := 0;
          Z := 0;
          W := 1;
     end;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TSingle4D.VectorTo( const P_:TSingle4D ) :TSingle4D;
begin
     Result := P_ - Self;
end;

function TSingle4D.UnitorTo( const P_:TSingle4D ) :TSingle4D;
begin
     Result := VectorTo( P_ ).Unitor;
end;

function TSingle4D.DistanTo( const P_:TSingle4D ) :Single;
begin
     Result := VectorTo( P_ ).Size;
end;

//------------------------------------------------------------------------------

class function TSingle4D.RandG :TSingle4D;
begin
     with Result do
     begin
          X := System.Math.RandG( 0, 1 );
          Y := System.Math.RandG( 0, 1 );
          Z := System.Math.RandG( 0, 1 );
          W := System.Math.RandG( 0, 1 );
     end;
end;

//------------------------------------------------------------------------------

class function TSingle4D.RandBS1 :TSingle4D;
begin
     with Result do
     begin
          X := TSingle.RandBS1;
          Y := TSingle.RandBS1;
          Z := TSingle.RandBS1;
          W := TSingle.RandBS1;
     end;
end;

class function TSingle4D.RandBS2 :TSingle4D;
begin
     with Result do
     begin
          X := TSingle.RandBS2;
          Y := TSingle.RandBS2;
          Z := TSingle.RandBS2;
          W := TSingle.RandBS2;
     end;
end;

class function TSingle4D.RandBS4 :TSingle4D;
begin
     with Result do
     begin
          X := TSingle.RandBS4;
          Y := TSingle.RandBS4;
          Z := TSingle.RandBS4;
          W := TSingle.RandBS4;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TDouble4D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TDouble4D.GetSiz2 :Double;
begin
     Result := Pow2( X ) + Pow2( Y ) + Pow2( Z ) + Pow2( W );
end;

procedure TDouble4D.SetSiz2( const Siz2_:Double );
begin
     Self := Roo2( Siz2_ / Siz2 ) * Self;
end;

function TDouble4D.GetSize :Double;
begin
     Result := Roo2( GetSiz2 );
end;

procedure TDouble4D.SetSize( const Size_:Double );
begin
     Self := Size_ * Unitor;
end;

function TDouble4D.GetUnitor :TDouble4D;
begin
     Result := Self / Size;
end;

procedure TDouble4D.SetUnitor( const Unitor_:TDouble4D );
begin
     Self := Size * Unitor_;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TDouble4D.Create( const V_:Double );
begin
     X := V_;
     Y := V_;
     Z := V_;
     W := V_;
end;

constructor TDouble4D.Create( const X_,Y_,Z_,W_:Double );
begin
     X := X_;
     Y := Y_;
     Z := Z_;
     W := W_;
end;

///////////////////////////////////////////////////////////////////////// 演算子

class operator TDouble4D.Negative( const V_:TDouble4D ) :TDouble4D;
begin
     with Result do
     begin
          X := -V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W := -V_.W;
     end;
end;

class operator TDouble4D.Positive( const V_:TDouble4D ) :TDouble4D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := +V_.Y;
          Z := +V_.Z;
          W := +V_.W;
     end;
end;

class operator TDouble4D.Add( const A_,B_:TDouble4D ) :TDouble4D;
begin
     with Result do
     begin
          X := A_.X + B_.X;
          Y := A_.Y + B_.Y;
          Z := A_.Z + B_.Z;
          W := A_.W + B_.W;
     end;
end;

class operator TDouble4D.Subtract( const A_,B_:TDouble4D ) :TDouble4D;
begin
     with Result do
     begin
          X := A_.X - B_.X;
          Y := A_.Y - B_.Y;
          Z := A_.Z - B_.Z;
          W := A_.W - B_.W;
     end;
end;

class operator TDouble4D.Multiply( const A_:TDouble4D; const B_:Double ) :TDouble4D;
begin
     with Result do
     begin
          X := A_.X * B_;
          Y := A_.Y * B_;
          Z := A_.Z * B_;
          W := A_.W * B_;
     end;
end;

class operator TDouble4D.Multiply( const A_:Double; const B_:TDouble4D ) :TDouble4D;
begin
     with Result do
     begin
          X := A_ * B_.X;
          Y := A_ * B_.Y;
          Z := A_ * B_.Z;
          W := A_ * B_.W;
     end;
end;

class operator TDouble4D.Divide( const A_:TDouble4D; const B_:Double ) :TDouble4D;
begin
     with Result do
     begin
          X := A_.X / B_;
          Y := A_.Y / B_;
          Z := A_.Z / B_;
          W := A_.W / B_;
     end;
end;

///////////////////////////////////////////////////////////////////////// 型変換

class operator TDouble4D.Implicit( const V_:TDouble3D ) :TDouble4D;
begin
     with Result do
     begin
          X := V_.X;
          Y := V_.Y;
          Z := V_.Z;
          W :=    0;
     end;
end;

class operator TDouble4D.Explicit( const V_:TDouble4D ) :TDouble3D;
begin
     with Result do
     begin
          X := V_.X;
          Y := V_.Y;
          Z := V_.Z;
     end;
end;

class operator TDouble4D.Implicit( const V_:TPoint3D ) :TDouble4D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W :=     0;
     end;
end;

class operator TDouble4D.Explicit( const V_:TDouble4D ) :TPoint3D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
     end;
end;

class operator TDouble4D.Implicit( const V_:TVector3D ) :TDouble4D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W :=  V_.W;
     end;
end;

class operator TDouble4D.Explicit( const V_:TDouble4D ) :TVector3D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W :=  V_.W;
     end;
end;

/////////////////////////////////////////////////////////////////////////// 定数

class function TDouble4D.IdentityX :TDouble4D;
begin
     with Result do
     begin
          X := 1;
          Y := 0;
          Z := 0;
          W := 0;
     end;
end;

class function TDouble4D.IdentityY :TDouble4D;
begin
     with Result do
     begin
          X := 0;
          Y := 1;
          Z := 0;
          W := 0;
     end;
end;

class function TDouble4D.IdentityZ :TDouble4D;
begin
     with Result do
     begin
          X := 0;
          Y := 0;
          Z := 1;
          W := 0;
     end;
end;

class function TDouble4D.IdentityW :TDouble4D;
begin
     with Result do
     begin
          X := 0;
          Y := 0;
          Z := 0;
          W := 1;
     end;
end;

/////////////////////////////////////////////////////////////////////// メソッド

function TDouble4D.VectorTo( const P_:TDouble4D ) :TDouble4D;
begin
     Result := P_ - Self;
end;

function TDouble4D.UnitorTo( const P_:TDouble4D ) :TDouble4D;
begin
     Result := VectorTo( P_ ).Unitor;
end;

function TDouble4D.DistanTo( const P_:TDouble4D ) :Double;
begin
     Result := VectorTo( P_ ).Size;
end;

//------------------------------------------------------------------------------

class function TDouble4D.RandG :TDouble4D;
begin
     with Result do
     begin
          X := System.Math.RandG( 0, 1 );
          Y := System.Math.RandG( 0, 1 );
          Z := System.Math.RandG( 0, 1 );
          W := System.Math.RandG( 0, 1 );
     end;
end;

//------------------------------------------------------------------------------

class function TDouble4D.RandBS1 :TDouble4D;
begin
     with Result do
     begin
          X := TDouble.RandBS1;
          Y := TDouble.RandBS1;
          Z := TDouble.RandBS1;
          W := TDouble.RandBS1;
     end;
end;

class function TDouble4D.RandBS2 :TDouble4D;
begin
     with Result do
     begin
          X := TDouble.RandBS2;
          Y := TDouble.RandBS2;
          Z := TDouble.RandBS2;
          W := TDouble.RandBS2;
     end;
end;

class function TDouble4D.RandBS4 :TDouble4D;
begin
     with Result do
     begin
          X := TDouble.RandBS4;
          Y := TDouble.RandBS4;
          Z := TDouble.RandBS4;
          W := TDouble.RandBS4;
     end;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TdSingle4D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TdSingle4D.Geto :TSingle4D;
begin
     Result.X := X.o;
     Result.Y := Y.o;
     Result.Z := Z.o;
     Result.W := W.o;
end;

procedure TdSingle4D.Seto( const o_:TSingle4D );
begin
     X.o := o_.X;
     Y.o := o_.Y;
     Z.o := o_.Z;
     W.o := o_.W;
end;

function TdSingle4D.Getd :TSingle4D;
begin
     Result.X := X.d;
     Result.Y := Y.d;
     Result.Z := Z.d;
     Result.W := W.d;
end;

procedure TdSingle4D.Setd( const d_:TSingle4D );
begin
     X.d := d_.X;
     Y.d := d_.Y;
     Z.d := d_.Z;
     W.d := d_.W;
end;

function TdSingle4D.GetSiz2 :TdSingle;
begin
     Result := Pow2( X ) + Pow2( Y ) + Pow2( Z ) + Pow2( W );
end;

procedure TdSingle4D.SetSiz2( const Siz2_:TdSingle );
begin
     Self := Roo2( Siz2_ / Siz2 ) * Self;
end;

function TdSingle4D.GetSize :TdSingle;
begin
     Result := Roo2( Siz2 );
end;

procedure TdSingle4D.SetSize( const Size_:TdSingle );
begin
     Self := Size_ * Unitor;
end;

function TdSingle4D.GetUnitor :TdSingle4D;
begin
     Result := Self / Size;
end;

procedure TdSingle4D.SetUnitor( const Unitor_:TdSingle4D );
begin
     Self := Size * Unitor_;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TdSingle4D.Create( const X_,Y_,Z_,W_:TdSingle );
begin
     X := X_;
     Y := Y_;
     Z := Z_;
     W := W_;
end;

///////////////////////////////////////////////////////////////////////// 演算子

class operator TdSingle4D.Negative( const V_:TdSingle4D ) :TdSingle4D;
begin
     with Result do
     begin
          X := -V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W := -V_.W;
     end;
end;

class operator TdSingle4D.Positive( const V_:TdSingle4D ) :TdSingle4D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := +V_.Y;
          Z := +V_.Z;
          W := +V_.W;
     end;
end;

class operator TdSingle4D.Add( const A_,B_:TdSingle4D ) :TdSingle4D;
begin
     with Result do
     begin
          X := A_.X + B_.X;
          Y := A_.Y + B_.Y;
          Z := A_.Z + B_.Z;
          W := A_.W + B_.W;
     end;
end;

class operator TdSingle4D.Subtract( const A_,B_:TdSingle4D ) :TdSingle4D;
begin
     with Result do
     begin
          X := A_.X - B_.X;
          Y := A_.Y - B_.Y;
          Z := A_.Z - B_.Z;
          W := A_.W - B_.W;
     end;
end;

class operator TdSingle4D.Multiply( const A_:TdSingle4D; const B_:TdSingle ) :TdSingle4D;
begin
     with Result do
     begin
          X := A_.X * B_;
          Y := A_.Y * B_;
          Z := A_.Z * B_;
          W := A_.W * B_;
     end;
end;

class operator TdSingle4D.Multiply( const A_:TdSingle; const B_:TdSingle4D ) :TdSingle4D;
begin
     with Result do
     begin
          X := A_ * B_.X;
          Y := A_ * B_.Y;
          Z := A_ * B_.Z;
          W := A_ * B_.W;
     end;
end;

class operator TdSingle4D.Divide( const A_:TdSingle4D; const B_:TdSingle ) :TdSingle4D;
begin
     with Result do
     begin
          X := A_.X / B_;
          Y := A_.Y / B_;
          Z := A_.Z / B_;
          W := A_.W / B_;
     end;
end;

///////////////////////////////////////////////////////////////////////// 型変換

class operator TdSingle4D.Implicit( const V_:TSingle4D ) :TdSingle4D;
const
     _d :TSingle4D = ( X:0; Y:0; Z:0; W:0 );
begin
     with Result do
     begin
          o := V_;
          d := _d;
     end;
end;

class operator TdSingle4D.Implicit( const V_:TdSingle4D ) :TSingle4D;
begin
     Result := V_.o;
end;

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TdDouble4D

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& private

/////////////////////////////////////////////////////////////////////// アクセス

function TdDouble4D.Geto :TDouble4D;
begin
     Result.X := X.o;
     Result.Y := Y.o;
     Result.Z := Z.o;
     Result.W := W.o;
end;

procedure TdDouble4D.Seto( const o_:TDouble4D );
begin
     X.o := o_.X;
     Y.o := o_.Y;
     Z.o := o_.Z;
     W.o := o_.W;
end;

function TdDouble4D.Getd :TDouble4D;
begin
     Result.X := X.d;
     Result.Y := Y.d;
     Result.Z := Z.d;
     Result.W := W.d;
end;

procedure TdDouble4D.Setd( const d_:TDouble4D );
begin
     X.d := d_.X;
     Y.d := d_.Y;
     Z.d := d_.Z;
     W.d := d_.W;
end;

function TdDouble4D.GetSiz2 :TdDouble;
begin
     Result := Pow2( X ) + Pow2( Y ) + Pow2( Z ) + Pow2( W );
end;

procedure TdDouble4D.SetSiz2( const Siz2_:TdDouble );
begin
     Self := Roo2( Siz2_ / Siz2 ) * Self;
end;

function TdDouble4D.GetSize :TdDouble;
begin
     Result := Roo2( Siz2 );
end;

procedure TdDouble4D.SetSize( const Size_:TdDouble );
begin
     Self := Size_ * Unitor;
end;

function TdDouble4D.GetUnitor :TdDouble4D;
begin
     Result := Self / Size;
end;

procedure TdDouble4D.SetUnitor( const Unitor_:TdDouble4D );
begin
     Self := Size * Unitor_;
end;

//&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& public

constructor TdDouble4D.Create( const X_,Y_,Z_,W_:TdDouble );
begin
     X := X_;
     Y := Y_;
     Z := Z_;
     W := W_;
end;

///////////////////////////////////////////////////////////////////////// 演算子

class operator TdDouble4D.Negative( const V_:TdDouble4D ) :TdDouble4D;
begin
     with Result do
     begin
          X := -V_.X;
          Y := -V_.Y;
          Z := -V_.Z;
          W := -V_.W;
     end;
end;

class operator TdDouble4D.Positive( const V_:TdDouble4D ) :TdDouble4D;
begin
     with Result do
     begin
          X := +V_.X;
          Y := +V_.Y;
          Z := +V_.Z;
          W := +V_.W;
     end;
end;

class operator TdDouble4D.Add( const A_,B_:TdDouble4D ) :TdDouble4D;
begin
     with Result do
     begin
          X := A_.X + B_.X;
          Y := A_.Y + B_.Y;
          Z := A_.Z + B_.Z;
          W := A_.W + B_.W;
     end;
end;

class operator TdDouble4D.Subtract( const A_,B_:TdDouble4D ) :TdDouble4D;
begin
     with Result do
     begin
          X := A_.X - B_.X;
          Y := A_.Y - B_.Y;
          Z := A_.Z - B_.Z;
          W := A_.W - B_.W;
     end;
end;

class operator TdDouble4D.Multiply( const A_:TdDouble4D; const B_:TdDouble ) :TdDouble4D;
begin
     with Result do
     begin
          X := A_.X * B_;
          Y := A_.Y * B_;
          Z := A_.Z * B_;
          W := A_.W * B_;
     end;
end;

class operator TdDouble4D.Multiply( const A_:TdDouble; const B_:TdDouble4D ) :TdDouble4D;
begin
     with Result do
     begin
          X := A_ * B_.X;
          Y := A_ * B_.Y;
          Z := A_ * B_.Z;
          W := A_ * B_.W;
     end;
end;

class operator TdDouble4D.Divide( const A_:TdDouble4D; const B_:TdDouble ) :TdDouble4D;
begin
     with Result do
     begin
          X := A_.X / B_;
          Y := A_.Y / B_;
          Z := A_.Z / B_;
          W := A_.W / B_;
     end;
end;

///////////////////////////////////////////////////////////////////////// 型変換

class operator TdDouble4D.Implicit( const V_:TDouble4D ) :TdDouble4D;
const
     _d :TDouble4D = ( X:0; Y:0; Z:0; W:0 );
begin
     with Result do
     begin
          o := V_;
          d := _d;
     end;
end;

class operator TdDouble4D.Implicit( const V_:TdDouble4D ) :TDouble4D;
begin
     Result := V_.o;
end;

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【クラス】

//$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$【ルーチン】

function DotProduct( const A_,B_:TSingleVec4D ) :Single;
begin
     Result := A_.X * B_.X
             + A_.Y * B_.Y
             + A_.Z * B_.Z
             + A_.W * B_.W;
end;

function DotProduct( const A_,B_:TDoubleVec4D ) :Double;
begin
     Result := A_.X * B_.X
             + A_.Y * B_.Y
             + A_.Z * B_.Z
             + A_.W * B_.W;
end;

function DotProduct( const A_,B_:TdSingleVec4D ) :TdSingle;
begin
     Result := A_.X * B_.X
             + A_.Y * B_.Y
             + A_.Z * B_.Z
             + A_.W * B_.W;
end;

function DotProduct( const A_,B_:TdDoubleVec4D ) :TdDouble;
begin
     Result := A_.X * B_.X
             + A_.Y * B_.Y
             + A_.Z * B_.Z
             + A_.W * B_.W;
end;

//------------------------------------------------------------------------------

function CrossProduct( const A_,B_,C_:TSingleVec4D ) :TSingleVec4D;
begin
     with Result do
     begin
          X := A_.Y * B_.Z * C_.W - A_.W * B_.Z * C_.Y
             + B_.Y * C_.Z * A_.W - B_.W * C_.Z * A_.Y
             + C_.Y * A_.Z * B_.W - C_.W * A_.Z * B_.Y;

          Y := A_.X * B_.W * C_.Z - A_.Z * B_.W * C_.X
             + B_.X * C_.W * A_.Z - B_.Z * C_.W * A_.X
             + C_.X * A_.W * B_.Z - C_.Z * A_.W * B_.X;

          Z := A_.W * B_.X * C_.Y - A_.Y * B_.X * C_.W
             + B_.W * C_.X * A_.Y - B_.Y * C_.X * A_.W
             + C_.W * A_.X * B_.Y - C_.Y * A_.X * B_.W;

          W := A_.Z * B_.Y * C_.X - A_.X * B_.Y * C_.Z
             + B_.Z * C_.Y * A_.X - B_.X * C_.Y * A_.Z
             + C_.Z * A_.Y * B_.X - C_.X * A_.Y * B_.Z;
     end;
end;

function CrossProduct( const A_,B_,C_:TDoubleVec4D ) :TDoubleVec4D;
begin
     with Result do
     begin
          X := A_.Y * B_.Z * C_.W - A_.W * B_.Z * C_.Y
             + B_.Y * C_.Z * A_.W - B_.W * C_.Z * A_.Y
             + C_.Y * A_.Z * B_.W - C_.W * A_.Z * B_.Y;

          Y := A_.X * B_.W * C_.Z - A_.Z * B_.W * C_.X
             + B_.X * C_.W * A_.Z - B_.Z * C_.W * A_.X
             + C_.X * A_.W * B_.Z - C_.Z * A_.W * B_.X;

          Z := A_.W * B_.X * C_.Y - A_.Y * B_.X * C_.W
             + B_.W * C_.X * A_.Y - B_.Y * C_.X * A_.W
             + C_.W * A_.X * B_.Y - C_.Y * A_.X * B_.W;

          W := A_.Z * B_.Y * C_.X - A_.X * B_.Y * C_.Z
             + B_.Z * C_.Y * A_.X - B_.X * C_.Y * A_.Z
             + C_.Z * A_.Y * B_.X - C_.X * A_.Y * B_.Z;
     end;
end;

function CrossProduct( const A_,B_,C_:TdSingleVec4D ) :TdSingleVec4D;
begin
     with Result do
     begin
          X := A_.Y * B_.Z * C_.W - A_.W * B_.Z * C_.Y
             + B_.Y * C_.Z * A_.W - B_.W * C_.Z * A_.Y
             + C_.Y * A_.Z * B_.W - C_.W * A_.Z * B_.Y;

          Y := A_.X * B_.W * C_.Z - A_.Z * B_.W * C_.X
             + B_.X * C_.W * A_.Z - B_.Z * C_.W * A_.X
             + C_.X * A_.W * B_.Z - C_.Z * A_.W * B_.X;

          Z := A_.W * B_.X * C_.Y - A_.Y * B_.X * C_.W
             + B_.W * C_.X * A_.Y - B_.Y * C_.X * A_.W
             + C_.W * A_.X * B_.Y - C_.Y * A_.X * B_.W;

          W := A_.Z * B_.Y * C_.X - A_.X * B_.Y * C_.Z
             + B_.Z * C_.Y * A_.X - B_.X * C_.Y * A_.Z
             + C_.Z * A_.Y * B_.X - C_.X * A_.Y * B_.Z;
     end;
end;

function CrossProduct( const A_,B_,C_:TdDoubleVec4D ) :TdDoubleVec4D;
begin
     with Result do
     begin
          X := A_.Y * B_.Z * C_.W - A_.W * B_.Z * C_.Y
             + B_.Y * C_.Z * A_.W - B_.W * C_.Z * A_.Y
             + C_.Y * A_.Z * B_.W - C_.W * A_.Z * B_.Y;

          Y := A_.X * B_.W * C_.Z - A_.Z * B_.W * C_.X
             + B_.X * C_.W * A_.Z - B_.Z * C_.W * A_.X
             + C_.X * A_.W * B_.Z - C_.Z * A_.W * B_.X;

          Z := A_.W * B_.X * C_.Y - A_.Y * B_.X * C_.W
             + B_.W * C_.X * A_.Y - B_.Y * C_.X * A_.W
             + C_.W * A_.X * B_.Y - C_.Y * A_.X * B_.W;

          W := A_.Z * B_.Y * C_.X - A_.X * B_.Y * C_.Z
             + B_.Z * C_.Y * A_.X - B_.X * C_.Y * A_.Z
             + C_.Z * A_.Y * B_.X - C_.X * A_.Y * B_.Z;
     end;
end;

//------------------------------------------------------------------------------

function Dista2( const A_,B_:TSinglePos4D ) :Single;
begin
     Result := Pow2( B_.X - A_.X )
             + Pow2( B_.Y - A_.Y )
             + Pow2( B_.Z - A_.Z )
             + Pow2( B_.W - A_.W );
end;

function Dista2( const A_,B_:TDoublePos4D ) :Double;
begin
     Result := Pow2( B_.X - A_.X )
             + Pow2( B_.Y - A_.Y )
             + Pow2( B_.Z - A_.Z )
             + Pow2( B_.W - A_.W );
end;

function Dista2( const A_,B_:TdSinglePos4D ) :TdSingle;
begin
     Result := Pow2( B_.X - A_.X )
             + Pow2( B_.Y - A_.Y )
             + Pow2( B_.Z - A_.Z )
             + Pow2( B_.W - A_.W );
end;

function Dista2( const A_,B_:TdDoublePos4D ) :TdDouble;
begin
     Result := Pow2( B_.X - A_.X )
             + Pow2( B_.Y - A_.Y )
             + Pow2( B_.Z - A_.Z )
             + Pow2( B_.W - A_.W );
end;

//------------------------------------------------------------------------------

function Distan( const A_,B_:TSinglePos4D ) :Single;
begin
     Result := Roo2( Dista2( A_, B_ ) );
end;

function Distan( const A_,B_:TDoublePos4D ) :Double;
begin
     Result := Roo2( Dista2( A_, B_ ) );
end;

function Distan( const A_,B_:TdSinglePos4D ) :TdSingle;
begin
     Result := Roo2( Dista2( A_, B_ ) );
end;

function Distan( const A_,B_:TdDoublePos4D ) :TdDouble;
begin
     Result := Roo2( Dista2( A_, B_ ) );
end;

//------------------------------------------------------------------------------

function Ave( const P1_,P2_:TSingle4D ) :TSingle4D;
begin
     Result := ( P1_ + P2_ ) / 2;
end;

function Ave( const P1_,P2_:TDouble4D ) :TDouble4D;
begin
     Result := ( P1_ + P2_ ) / 2;
end;

function Ave( const P1_,P2_:TdSingle4D ) :TdSingle4D;
begin
     Result := ( P1_ + P2_ ) / 2;
end;

function Ave( const P1_,P2_:TdDouble4D ) :TdDouble4D;
begin
     Result := ( P1_ + P2_ ) / 2;
end;

//------------------------------------------------------------------------------

function Ave( const P1_,P2_,P3_:TSingle4D ) :TSingle4D;
begin
     Result := ( P1_ + P2_ + P3_ ) / 3;
end;

function Ave( const P1_,P2_,P3_:TDouble4D ) :TDouble4D;
begin
     Result := ( P1_ + P2_ + P3_ ) / 3;
end;

function Ave( const P1_,P2_,P3_:TdSingle4D ) :TdSingle4D;
begin
     Result := ( P1_ + P2_ + P3_ ) / 3;
end;

function Ave( const P1_,P2_,P3_:TdDouble4D ) :TdDouble4D;
begin
     Result := ( P1_ + P2_ + P3_ ) / 3;
end;

//------------------------------------------------------------------------------

function Ave( const P1_,P2_,P3_,P4_:TSingle4D ) :TSingle4D;
begin
     Result := ( P1_ + P2_ + P3_ + P4_ ) / 4;
end;

function Ave( const P1_,P2_,P3_,P4_:TDouble4D ) :TDouble4D;
begin
     Result := ( P1_ + P2_ + P3_ + P4_ ) / 4;
end;

function Ave( const P1_,P2_,P3_,P4_:TdSingle4D ) :TdSingle4D;
begin
     Result := ( P1_ + P2_ + P3_ + P4_ ) / 4;
end;

function Ave( const P1_,P2_,P3_,P4_:TdDouble4D ) :TdDouble4D;
begin
     Result := ( P1_ + P2_ + P3_ + P4_ ) / 4;
end;

//############################################################################## □

initialization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 初期化

finalization //$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 最終化

end. //######################################################################### ■
