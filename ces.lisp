;;; ---------------------------------------------------------------------------
;;; マップを記述するいくつかの変数を定義
;;; ---------------------------------------------------------------------------

;; マップの大きさ:横
(defparameter *width* 100)
;; マップの大きさ:縦
(defparameter *height* 30)
;; ジャングルの位置(left top width height)
(defparameter *jungle* '(45 10 10 10))
;; 植物の生命力(動物がこの植物を食べたら生命力の日数だけ生きられる)
(defparameter *plant-energy* 80)


;;; ---------------------------------------------------------------------------
;;; マップに草を生やす
;;; ---------------------------------------------------------------------------

;; 植物用のハッシュテーブル（存在する座標がキー）
;; キー比較関数を、デフォルトのeql(シンボルと文字列比較用)からequalに変更する
(defparameter *plants* (make-hash-table :test #'qeual))

(defun random-plant (left top width height)
  "マップの指定された領域の中でランダムに植物を配置する"
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    ;; 植物が配置された座標のキーに対する値をtにする
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  "ジャングル内に植物を配置し、マップ内にも植物を配置する"
  ;; ジャングル内に植物をランダムに配置する
  (apply #'random-plant *jungle*)
  ;; マップ内に植物をランダムに配置する
  (random-plant 0 0 *width* *height*))


;;; ---------------------------------------------------------------------------
;;; 動物を作る
;;; ---------------------------------------------------------------------------

;; 動物の構造体を定義する
(defstruct animal
  "動物"
  ;; マップ上での動物の位置(x座標)
  x
  ;; マップ上での動物の位置(y座標)
  y
  ;; 動物が生命活動できる日数
  energy
  ;; 方向(左上0、時計まわりに0->7)
  ;; 0 1 2
  ;; 7 x 3
  ;; 6 5 4
  dir
  ;; 動物の遺伝子(各方向に行く確率を持つ。大きいほどその方向に行きやすい。)
  genes)

;; 最初の動物を一体作る
(defparameter *animals*
  ;; *animals*は単に全要素をスキャンして処理するのみのため単純にリスト
  (list (make-animal
          :x         (ash *width* -1)    ; マップ中央(横方向)
          :y         (ash *height* -1)   ; マップ中央(縦方向)
          :energy    1000                ; 生命力（1000日間）
          :dir       0                   ; 初期の方向（左上）
          :genes     (loop repeat 8 collect (1+ (random 10))))))    ; 各方向への行きやすさ

(defun move (animal)
  "動物を動かす"
  (let ((dir (animal-dir animal))
        (x (anima-x animal))
        (y (anima-y animal)))
    ;; animalのx座標を更新する
    (setf (animal-x animal)
          (mod (+ x
                  (cond ((and (>= dir 2) (< dir 5)) 1)
                        ((or (= dir 1) (= dir 5)) 0)
                        (t -1)))
               *width*))
    ;; animalのy座標を更新する
    (setf (anima-y animal)
          (mod (+ y
                  (cond ((and (>= dir 0) (< dir 3)) -1)
                        ((and (>= dir 4) (< dir 7)) 1)
                        (t 0)))
               *height*))
    ;; animalの生命力を減らす
    (decf (animal-energy animal))))

