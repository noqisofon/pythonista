;; -*- coding: utf-8; -*-
(define-module argparse
  (use pythonista.textwrap)
  (export <argument-parser> make-argument-parser parser-on parser-parse))

(select-module argparse)

(define-constant +suppess+ "==SUPPRESS==")
(define-constant +optional+ "?")

;; (define-class <attribute-holder> ()
;;   ())

;; (define-method attribute-holder-get-args ((self <attribute-holder>))
;;   '())

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
   ;; 現在のインデントカラム。
   (current-indent :allocation :instance :init-value 0)
   ;; 現在のインデントレベルです？
   (level :allocation :instance :init-value 0)
   ;; アクション(オプションの名前)の最大長。
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
(define-method formatter-indent (self <help-formatter>)
  ;; ref を書くのがめんどくさいので、let を使います。
  (let ((current-indent% (ref self current-indent))
        (indent-increment% (ref self indent-increment))
        (level% (ref self level)))
    ;; ( ˘⊖˘) ｡o( current-indent-of でも使えばよかったか…
    (set! (ref self current-indent) (+ current-indent% indent-increment%))
    (set! (ref self level) (+ level% 1))))

;; アンインデントを行います。
(define-method formatter-unindent (self <help-formatter>)
  ;; ref を書くのがめんどくさいので、let を使います。
  (let ((current-indent% (ref self current-indent))
        (indent-increment% (ref self indent-increment))
        (level% (ref self level)))
    (set! (ref self current-indent) (- current-indent% indent-increment%))
    (set! (ref self level) (- level% 1))))

;; ヘルプ文字列における
;;
;;     Global options:
;;       --verbose                    やかましく喋るようにします。
;;       -q, --quiet                  寡黙に作業を行います。
;;       -h, --help                   このヘルプを表示します。
;;
;; のようなヘッダーと引数と説明のペアの一部分を表します。
(define-class <argument-section> ()
  ;;  <help-formatter> クラスのインスタンスなんだぜ。
  ;;  相互参照するんだぜ？
  ((formatter :allocation :instance :init-keyword :formatter :accessor formatter-of)
   ;; 親のセクションを表します。通常は formatter スロットの current-section が入ります。
   (parent :allocation :instance :init-keyword :parent :init-value '() :accessor parent-of)
   ;; セクション名などのセクションのはじめに表示される文字列を表します。
   (heading :allocation :instance :init-keyword :heading :init-value '() :accessor heading-of)
   ;; 引数と説明などを保持するバッグ的アソシエーションリストを表します。
   (items :allocation :instance :init-value (make-hash-table))))

(define-method section-format-help ((self <argument-section>))
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
(define formatter-add-item! ((self <help-formatter>) (func <argument-section>) (args <list>))
  (push! (ref (current-section-of self) items) `(func . args)))

;; セクションを初めます。
(define-method formatter-start-section ((self <help-formatter>) heading)
  (formatter-indent self)
  (let ((section% (make <argument-section> :formatter self :parent (ref self current-section) :heading heading)))
    (add-item! (section-format-help section%) '())
    (set! (ref current-section) section)))

;; セクションを終わります。
(define-method formatter-end-section ((self <help-formatter>))
  ;; 次のセクションに移るためにカレントセクションを戻してアンインデントします。
  (set! (ref current-section) (ref (ref self current-section) parent))
  (formatter-unindent self))

;; ヘルプフォーマッターにテキストを追加します。
(define-method formatter-add-text! ((self <help-formatter>) text)
  (when (and (not (equal? text +suppress+)) (not (null? text)))
        (formatter-add-item! self (ref self format-text) '(text))))

;; ヘルプフォーマッターに Usage を追加します。
;; Usage って:
;;
;;    prog [--version] [--help] [FILE1 [FILE2 ...]]
;;
;; みたいなやつね。
(define-method formatter-add-usage! ((self <help-formatter>) usage actions groups :optional prefix)
  (unless (equal? usage +suppress+)
          (let ((args (list usage actions groups prefix)))
            (formatter-add-item! self (ref self format-usage) args))))

;; ヘルプフォーマッターにアクションを追加します。
;; argument って書いてあるけど、実際は 引数フラグのインスタンスですね。
(define-method formatter-add-argument! ((self <help-formatter>) (action <argument-action>))
  (unless (equal? (help-of action) +suppress+)
          (let* ((get-invocation% (ref self format-action-invocation))
                 (invocations% (hash-table-get get-invocation% action)))
            ;; iter が含まれているメソッドは何を返すんだかわからないんだけど、リストだろと思って map を
            ;; 使ってる。
            (map (lambda (subaction)
                   (push! invocations% (hash-table-get get-invocation% subaction)))
                 (formatter-iter-indented-subactions self action))
            ;; アイテムの最大長を更新します。
            (let* ((invocation-length% (fold max 0 (map string-length invocations%)))
                   (action-length% (+ invocation-length% (ref current-indent self)))
                   (action-max-length% (ref action-max-length self)))
              (set! (ref action-max-length self) (max action-max-length% action-length%)))
            (formatter-add-item! self (ref format-action self) `(action)))))

;; action のリストをヘルプフォーマッターに全て追加します。
(define-method formatter-add-arguments! ((self <help-formatter>) (actions <list>))
  (map (lambda (action)
         (formatter-add-argument! self action))
       actions))

(define-method formatter-format-action-invocation ((self <help-formatter>) (action <argument-action>))
  (if (not (null? (option-strings-of action)))
      (let* ((default% (formatter-metavar-for-positional self action))
             (metavar% (first (formatter-metavar-formatter self action default))))
        metavar%)
      ;; else
      (let ((parts% '()))
        (if (zero? (action-nargs-count action))
            (append parts% (option-strings-of action)))
            ;; else
            (let* ((default% (formatter-metavar-for-optional self action))
                   (args-string% (formatter-format-args self action default%)))
              (map (lambda (option-string)
                     (push! parts% (format "~A ~A" option-string args-string%)))
                   (option-strings-of action))

              (string-join parts% ", "))))))

(define-method formatter-metavar-formatter ((self <help-formatter>) (action <argument-action>) default-metavar)
  (let ((result (cond ((not (null? (ref metavar action))) (ref metavar action))
                      ((not (null? (ref choices action))) (let ((choice-strs% (map x->string (ref choices action))))
                                                            (string-append "'" (string-join choice-strs% ",") "'")))
                      (else default-metavar))))
    (lambda (tuple-size)
      (apply values (if (list? result)
                        result
                        ;; else
                        (make-list result tuple-size))))))


(define-method formatter-format-args ((self <help-formatter>) (action <argument-action>) default-metavar)
  (let ((get-metavar% (formatter-metavar-formatter self action default-metavar))
        (nargs% (ref action nargs)))
    (cond ((null? nargs%)                    (receive (a) (get-metavar 1)
                                                      (format "~A" a)))
          ((equal? nargs% +optional+)        (receive (a) (get-metavar 1)
                                                      (format "[~A]" a)))
          ((equal? nargs% +zero-or-more+)    (receive (a b) (get-metavar 2)
                                                      (format "[~A [~A ...]]" a b)))
          ((equal? nargs% +one-or-more+)     (receive (a b) (get-metavar 2)
                                                      (format "~A [~A ...]" a b)))
          ((equal? nargs% +reminder+)        "...")
          ((equal? nargs% +parser+)          (receive (a) (get-metavar 1)
                                                      (format "~A ..." a)))
          ;; ここまでくると、nargs% は数字だということになるらしい。
          (else (let* ((formats% (map (lambda (x)
                                       (format "~A" x))
                                     (iota nargs%))))
                  (string-join formats% " " (call-with-values ((get-metavar nargs%) list))))))))

(define-method formatter-expand-help ((self <help-formatter>) (action <argument-action>))
  (let ((params (hash-table (cons '(:prog . (prog-of self)) (action-vars action)))))
    ;; value が +suppress+ だった場合は key を削除。
    (map (lambda (name)
           (if (string-equal? name +suppress+)
               (hash-table-delete! params% name)))
         (hash-table-keys params%))
    ;; value が name スロットのある文字列以外のオブジェクト
    ;; である場合は name の値を value に設定します。
    (map (lambda (name)
           (let ((value% (ref params% name)))
             (if (slot-exists? value% name)
               (set! (ref params% name) (ref value % name)))))
         (hash-table-keys params%))
    ;; params% はハッシュテーブルなので format 出来ない感じある。
    (format (formatter-get-help-string self action) params%)))

(define-method formatter-iter-indented-subactions ((self <help-formatter>) (action <argument-action>))
  (let ((get-subactions% (action-get-subactions action)))
    (formatter-indent self)
    (map (lambda (subaction)
           (action-yield subaction))
         ;; get-subactions% はリストを返すことを前提に書いているが、
         ;; get-subactions% はジェネレータのようである。
         (get-subactions%))
    (formatter-unindent self)))

(define-method formatter-split-lines ((self <help-formatter>) (text <string>) (width <integer>))
  (let* ((whitespace-matcher% (ref whitespace-matcher self))
         (text% (regexp-replace-all* (regexp-replace whitespace-matcher% text " ")
                                     #/^\s+/ ""
                                     #/\s+$/ "")))
    ))
