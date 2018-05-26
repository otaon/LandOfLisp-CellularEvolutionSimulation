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
                  (cond ((and (>= dir 2) (< dir 5)) 1)  ; 方向が234のときx座標を1増やす
                        ((or (= dir 1) (= dir 5)) 0)    ; 方向が1か5のときx座標を変えない
                        (t -1)))                        ; 方向が067のときx座標を1減らす
               *width*))
    ;; animalのy座標を更新する
    (setf (anima-y animal)
          (mod (+ y
                  (cond ((and (>= dir 0) (< dir 3)) -1) ; 方向が012のときy座標を1減らす
                        ((and (>= dir 4) (< dir 7)) 1)  ; 方向が456のときy座標を1増やす
                        (t 0)))                         ; 方向が3か7のときy座標を変えない
               *height*))
    ;; animalの生命力を減らす
    (decf (animal-energy animal))))

(defun turn (animal)
  "動物の向きを変える"
  ;; 各方向の確率を足し合わせ、0からその数値未満の整数値をランダムに取る
  ;; これが、累積ヒストグラムにおける頻度を表す
  (let ((x (random (apply #'+ (animal-genes animal)))))
    ;; angle関数
    ;; summary: 各方向の確率とランダム値を元にして進む方向を算出する
    ;; genes: 各方向の確率のリスト
    ;; x: ランダム値。累積ヒストグラムにおける頻度を表す。
    ;; ret: 次に進む方向
    (labels ((angle (genes x)
               ;; ランダム値から、リストの先頭から確率の値を引く
               (let ((xnu (- x (car genes))))
                 ;; 差分が0未満なら0を返す
                 ;; 差分が0以上なら次の方向の評価に進める
                 (if (< xnu 0)
                     0
                     (1+ (angle (cdr genes) xnu))))))
      ;; 動物の向きを変える(0(左上方向)が前であることに注意)
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
              8)))))

(defun eat (animal)
  "動物に食べさせる"
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      ;; 植物が持つ生命力を動物に与える
      (incf (animal-energy animal) *plant-energy*)
      ;; 食べられた植物を消す
      (remhash pos *plants*))))


;;; ---------------------------------------------------------------------------
;;; 繁殖に関する処理
;;; ---------------------------------------------------------------------------

;; 繁殖可能な生命力の下限
(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  "繁殖する"
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      ;; 繁殖するために生命力を半減させる
      (setf (animal-energy animal) (ash e -1))
      (let ((animal-nu  (copy-structure animal))    ; shallow copyのため実体は共有される
            (genes      (copy-list (animal-genes animal)))  ; 
            (mutation   (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*)))))



