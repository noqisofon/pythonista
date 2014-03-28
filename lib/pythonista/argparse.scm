;; -*- coding: utf-8; -*-
(define-module argparse)

(select-module argparse)

;; <help-formatter> は --help 用の文字列をフォーマットするやつだと思います。
;; <argument-section> がその一部を管理する気がします。
(define-class <help-formatter> ()
  ;;  prog はプログラムの名前を保持します。
  ;;  help 用の文字列に使われたり、version 情報にも使われます。
  ((prog :allocation :instance :init-keyword :prog :init-value "" :accessor prog-of)
   ;; インデントをインクリメントした際に width がどのくらい増えるかを示します。
   (indent-increment :allocation :instance :init-keyword :indent-increment :init-value 2 :accessor indent-increment-of)
   ;; 不明
   (max-help-position :allocation :instance :init-keyword :max-help-position
                      :init-value 24 :init-thunk (lambda (self)
                                                   (min (ref self max-help-position)
                                                        (max (- (ref self width) 20)
                                                             (* (ref self indent-increment) 2)))) :accessor max-help-position-of)
   ;; ヘルプ文字列の幅？
   (width :allocation :instance :init-keyword :width :init-thunk (lambda (self)
                                                                   (let ((width# (get-environment-variable "COLUMNS")))
                                                                     (if width#
                                                                         (set! width# 80))
                                                                     (set! width# (- width# 2))
                                                                     width#)) :accessor width-of)
   ;; 現在のインデントレベルです？
   (current-indent :allocation :instance :init-value 0)
   ;; れべる？
   (level :allocation :instance :init-value 0)
   ;; ？？？
   (action-max-length :allocation :instance :init-value 0)
   ;; 現在のセクションを示します。
   (current-section :allocation :instance :init-thunk (lambda (self)
                                                       (make <argument-section> self)))
   ;; ホワイトスペースにマッチする正規表現オブジェクトです。
   (whitespace-matcher :allocation :instance :init-value #/\s+/)
   ;; 
   (long-break-matcher :allocation :instance :init-value #/\n\n\n+/)))

;; ( ˘⊖˘) ｡o( 待てよ、export する時はクラスも export しなきゃいけないのかな……？

;; インデントします。
(define-method help-formatter-indent (self <help-formatter>)
  ;; ref を書くのがめんどくさいので、let を使います。
  (let ((current-indent# (ref self current-indent))
        (level# (ref self level)))
    ;; ( ˘⊖˘) ｡o( current-indent-of でも使えばよかったか…
    (set! (ref self current-indent) (+ current-indent# current-indent#))
    (set! (ref self level) (+ level# 1))))

;; アンインデントを行います。
(define-method help-formatter-unindent (self <help-formatter>)
  ;; ref を書くのがめんどくさいので、let を使います。
  (let ((current-indent# (ref self current-indent))
        (level# (ref self level)))
    (set! (ref self current-indent) (- current-indent# current-indent#))
    (set! (ref self level) (- level# 1))))

;; プログラム引数の？？？
(define-class <argument-section> ()
  ;;  <help-formatter> クラスのインスタンスなんだぜ。
  ;;  相互参照するんだぜ？
  ((formatter :allocation :instance :init-keyword :formatter :accessor formatter-of)
   ;; 親のセクションを表します。通常は formatter スロットの current-section が入ります。
   (parent :allocation :instance :init-keyword :parent :init-value '() :accessor parent-of)
   ;; ヘッダー？
   (heading :allocation :instance :init-keyword :heading :init-value '() :accessor heading-of)
   ;; 何に使うのか不明
   (items :allocation :instance :init-value (make-hash-table))))

(define-method argument-section-format-help ((self <argument-section>))
  (if (not (null? (ref self parent)))
      ((formatter-of self) indent))
  (define join ((ref self formatter) join-parts))
  ;; func がキーで、args が値のハッシュ？ じゃなくて、[[func args] ...] だった？ｗ
  ;; おそらく前者のような気がする…
  (hash-table-map (ref self items) (lambda (func args)
                                     (apply func args)))
  ;; 突然のリスト内表記にビビりっぱなしである…
  ;;(define item-help (string-join ))
  ;; ここで、format-help を実装するには情報が足りないことに気づく。
  ;; これは後回しにして、<help-formatter> のメソッドの実装を行うこととしよう。
  )

;; <help-formatter> に <argument-section> と リストのペアを追加します。
(define help-formatter-add-item! ((self <help-formatter>) (func <argument-section>) (args <list>))
  (append (ref (current-section-of self) items) `(func . args)))

;; セクションを初めます。
(define-method help-formatter-start-section ((self <help-formatter>) heading)
  ;; セクションってつまり:
  ;;
  ;;     Global options:
  ;;       --verbose                    やかましく喋るようにします。
  ;;       -q, --quiet                  寡黙に作業を行います。
  ;;       -h, --help                   このヘルプを表示します。
  ;;
  ;; っていうアレだと思う。
  ;; heading は "Global options:" の部分かな。
  (help-formatter-indent self)
  (let ((section# (make <argument-section> :formatter self :parent (ref self current-section) :heading heading)))
    (add-item! (argument-section-format-help section#) '())
    (set! (ref current-section) section)))

;; セクションを終わります。
(define-method help-formatter-end-section ((self <help-formatter>))
  ;; 次のセクションに移るためにカレントセクションを戻してアンインデントします。
  (set! (ref current-section) (ref (ref self current-section) parent))
  (help-formatter-unindent self))

;; 
;; メソッドには、プレフィックスとしてクラス名を付けておいたほうがいいんだろうね…
;; ということでつけました。