3 ;;
(* 四則演算 *)
3 + 2 ;;
(3 + 4) * 2 ;;
(* 実数での計算は演算子に「.」をつける *)
3.4 +. 2.5 ;;
(* べき乗は実数のみに実装されている *)
2.5 ** 2.0 ;;

(* 無限大 *)
infinity ;;
infinity +. 5.0 ;;
2.0 /. infinity ;;
(* マイナス無限大 *)
neg_infinity ;;
1.0 -. neg_infinity ;;

(* 文字列 *)
"Tokyo" ;;
"東京" ;;
"東京" ^ "駅" ;;

(* 真偽値 *)
true ;;
false ;;
true && true ;;
true && false ;;
true || true ;;
false || false ;;
not true ;;
not (false || not false && not false) ;;

(* 比較 *)
(* 比較演算子は違う型のデータを比較できない *)
2 < 3 ;;

(* 関数 *)
let num = 5 ;;
let f x = 3 * x ;;
f 10 ;;

(* if文 *)
if true then 1 else 2 ;;
let kyuyo x = if x > 30 then x * 100 else x ;;
kyuyo 50 ;;
kyuyo 10 ;;
let kyuyo x = x * (if x > 30 then 100 else 10) ;;
kyuyo 50 ;;
kyuyo 10 ;;
let kion t = if t < 15 then "samui" else if t <= 25 then "hutuu" else "atui" ;;
kion 10 ;;
kion 20 ;;
kion 30 ;;
let kion t = if t >= 15 && t < 25 then "kaiteki" else "hukai" ;;
kion 10 ;;
kion 20 ;;
kion 30 ;;

(* 組 *)
(3.14, 2.71) ;;
(3, true) ;;
((23, "a"), 3.14) ;;

(* パターンマッチ *)
match (3,5) with
  (a,b) -> a + b ;;
(* 下記のように1つの組の引数か、2つの整数の引数で同じ処理にできる
   組を使うときは意味的にひとまとめにすることができるものにするのが自然（X座標、Y座標など） *)
let add pair = match pair with
    (a,b) -> a + b ;;
add (3, 5) ;;
let add2 a b = a + b ;;
add2 3 5 ;;
(* パターンマッチでなはmatch内の式が先に実行される *)

(* レコード *)
(* レコードを使うときは必ず自分で新しい型を定義し、使いたいフィールド名を指定しなければならない *)
type  gakusei_t = {
  name : string;
  tensuu : int;
  seiseki : string;
} ;;
{name = "asai"; tensuu = 70; seiseki = "B"} ;;
(* フィールド名があっていれば順番は関係ない *)
{tensuu = 70; seiseki = "B"; name = "asai"; } ;;

let tuuti gakusei = match gakusei with
    {name = n; tensuu = t; seiseki = s} ->
    n ^ "さんは" ^ string_of_int t ^ "点で、成績は" ^ s ^ "です。" ;;
tuuti {name = "asai"; tensuu = 70; seiseki = "B"} ;;

let hyouka gakusei = match gakusei with
    {name = n; tensuu = t; seiseki =s} ->
    {
      name = n;
      tensuu = t;
      seiseki = if t >= 80 then "A"
        else if t >= 70 then "B"
        else if t >= 60 then "C" else "D"
    } ;;
tuuti (hyouka {name = "asai"; tensuu = 80; seiseki = ""}) ;;

(* リスト *)
(* 常に :: の左には要素が、右にはリストがくる *)
(* [] :: 2 ;;  This expression has type int but an expression was expected of type 'a list list *)
(* :: は右に結合する。下記だと 2 :: ( 1 :: [] ) になる *)
2 :: 1 :: [] ;;
1 :: 2 :: 3 :: [] ;;
[1; 2; 3;] ;;
1 :: [2; 3;] ;;
match [] with
  [] -> 0
| first :: rest -> first ;;
(* first は先頭の 1 にマッチし、rest は [2; 3;] にマッチする *)
match [1; 2; 3;] with
  [] -> 0
| first :: rest -> first ;;
(* match を書くとき、空リストとそうでない場合を記述しないと警告が出る *)

(* 再帰関数 *)
(* 再帰関数を定義する問は先頭を let rec にする *)
let rec contain_zero lst = match lst with
    [] -> false
  | first :: rest -> if first = 0 then true else contain_zero rest ;;
let rec sum lst = match lst with
    [] -> 0
  | first :: rest -> first + sum rest ;;
sum [1; 2; 3; 4; 5;] ;;
let rec length lst = match lst with
    [] -> 0
  | first :: rest -> 1 + length rest ;;
length [2; 7; 5; 9;] ;;
let rec even lst = match lst with
    [] -> []
  | first :: rest -> if first mod 2 = 0 then first :: even rest else even rest ;;
even [2; 1; 6; 4; 7] ;;

let lst = [
  {name = "yoshida"; tensuu = 80; seiseki = "A"};
  {name = "asai"; tensuu = 70; seiseki = "B"};
  {name = "kaneko"; tensuu = 85; seiseki = "A"};
]
let rec count_A lst = match lst with
    [] -> 0
  | ({name = n; tensuu = t; seiseki = s;}) :: rest ->
    if s = "A" then 1 + count_A rest else count_A rest ;;
count_A lst ;;
(* (パターン as パターン変数) で左のパターン全体を右のパターン変数で参照できるようになる *)
let rec test lst = match lst with
    [] -> []
  | ({name = n; tensuu = t; seiseki = s;} as first) :: rest -> first :: test rest

(* 関数のネスト *)
let rec add_to_each n lst = match lst with
    [] -> []
  | first :: rest -> (n :: first) :: add_to_each n rest ;;
let rec prefix lst = match lst with
    [] -> []
  | first :: rest -> [first] :: add_to_each first (prefix rest) ;;
prefix [1; 2; 3; 4;] ;;

(* 昇順となる位置にnを挿入する *)
let rec insert n lst = match lst with
    [] -> [n]
  | first :: rest -> if first < n then first :: insert n rest else n :: lst ;;
insert 5 [1; 3; 6; 8;] ;;

(* 昇順に整列したリストを返す *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert first (ins_sort rest) ;;
ins_sort [3; 1;] ;;
ins_sort [5; 3; 8; 1; 7; 4;] ;;

max_int ;;
min_int ;;

(* 局所変数定義 *)
(* 変数のスコープはinに続く式のみ *)
let x = 2 in x + x ;;
let x = 3 in let y = 4 in x + y ;;
let rec minimum lst = match lst with
    [] -> max_int
  | first :: rest -> let min_rest = minimum rest in if first <= min_rest then first else min_rest ;;
minimum [2; 7; 1; 8; 4;] ;;

(* パターンマッチ付き局所変数定義 *)
let rec shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {name = n; tensuu = t; seiseki = s;} :: rest ->
    let (a, b, c, d) = shukei rest in
      if s = "A" then (a + 1, b, c, d)
      else if s = "B" then (a, b + 1, c, d)
      else if s = "C" then (a, b, c + 1, d)
      else (a, b, c, d + 1) ;;

(* リスト結合 *)
[1; 2; 3;] @ [4; 5; 6;] ;;
(* p106 *)
