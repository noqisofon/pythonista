;; -*- coding: utf-8; -*-
(define-module textwrap
  (use gauche.generator)
  (export textwrap-wrap textwrap-fill textwrap-unindent textwrap-indent
          <text-wrapper> wrapper-wrap wrapper-fill wrapper-unindent wrapper-indent))

(select-module textwrap)

;; ホワイトスペースを表す正規表現な定数。
(define-constant +whitespace+ #/\s/)
;; 単語の区切りを表す正規表現。
(define-constant +word-separator-simple+ #/(\s+)/)

(define-constant +word-separator+ #/(\s+|[^\s\w]*\w[-^0-9\W]-(?=\w+[^0-9\W])|(?<=[\w\!\"'&\.\,\?])-{2,}(?=\w))/)



(define-class <text-wrapper> ()
  ;;  width は wrap するテキストの幅を表すんじゃないかな。
  ;;  デフォルトは 70。
  ((width :allocation :instance :init-keyword :width :init-value 70)
   ;; 最初のインデント幅を表します。と思ったんだけど、実際のインデント用の文字列の模様。
   (initial-indent :allocation :instance :init-keyword :initial-indent :init-value "")
   ;;
   (subsequent-indent :allocation :instance :init-keyword :subsequent-indent :init-value "")
   ;;
   (expand-tabs :allocation :instance :init-keyword :expand-tabs :init-value #t)
   ;;
   (replace-whitespace :allocation :instance :init-keyword :replace-whitespace :init-value #t)
   ;;
   (fix-sentence-endings :allocation :instance :init-keyword :fix-sentence-endings :init-value #t)
   ;;
   (drop-whitespace :allocation :instance :init-keyword :drop-whitespace :init-value #t)
   ;;
   (break-on-hypens :allocation :instance :init-keyword :break-on-hypens :init-value #t)
   ;;
   (tab-size :allocation :instance :init-keyword :tab-size :init-value 8)))


;;
;; Private Method
;; ================================================================
;; text を色々置換する。
(define-method wrapper-munge-whitespace ((self <text-wrapper>) (text <string>))
  (let ((text% text))
    (when (ref expand-tabs self)
          (set! text% (regexp-replace-all #/\t/ text% "    ")))
    ;; ホワイトスペースっぽいのはスペースにしてるけど、何かの変数で置換してる。
    (when (ref replace-whitespace self)
          (set! text% (regexp-replace-all #/\s/ text% " ")))
    text%))


(define-method wrapper-split ((self <text-wrapper>) (text <string>))
  ;; gauche には python の RE#split のようなものはないので、ジェネレーターを使う必要がある。
  (let ((chunks% ($ generator->list
                    $ gmap rxmatch-substring
                    $ grxmatch (if (ref break-on-hypens self)
                                   +word-separator+
                                   +word-separator-simple+) text)))
    (filter (lambda (c)
              (> (string-length c) 0)) chunks%))))
  
  
(define-method wrapper-fix-sentence-endings ((self <text-wrapper>) (chunks <sequence>))
  (define (loop chunks)
    (if (null? chunks)
        '()
        (let ((first% (car chunks))
              (second% (cdr (car chunks))))
          (cons first% (if (and (string=? second% " ") (pat-search first%))
                           (cons "  " (loop (cdr (cdr chunk))))
                           (cons second$ (loop (cdr chunk))))))))
  (loop chunks))


(define-method wrapper-handle-long-word ((self <text-wrapper>) (reversed-chunks <sequence>) (cur-line <sequence>) (cur-len <integer>) (width <integer>))
  (let ((space-left% (if (< width 1)
                         1
                         (- width cur-len))))
    ;; ここら辺、よくわかんない＞＜
    (cond ((ref break-long-words self)  (begin (push! cur-line (revesed-chunks -1 :space-left))
                                               (revesed-chunks -1 (revesed-chunks -1 :space-left))))
          ((not (null? cur-line))       (push! cur-line (pop! revesed-chunks))))))

(define (strip s)
  (regexp-repace-all s
                     #/^\s+/ ""
                     #/\s+$/ ""))

(define-method wrapper-wrap-chunks ((self <text-wrapper>) (chunks <sequence>))
  (let ((line% '()))
    (when (<= (ref width self) 0)
          (errorf "invalid width ~A (must be > 0)" (ref width self)))
    (reverse! chunks)
    (define (loop chunks%)
      (let* ((cur-line% '())
             (cur-len% 0)
             (indent% (if (null? lines%)
                          (ref subsequent-indent self)
                          ;; else
                          (ref initial-indent self)))
             (width% (- (ref width self) indent%)))
        (when (and (ref drop-whitespace) (string=? )))))
    (loop chunks)))
