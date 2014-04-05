;; -*- coding: utf-8; -*-
(define-module textwrap
  (export textwrap-wrap textwrap-fill textwrap-unindent textwrap-indent
          <text-wrapper> wrapper-wrap wrapper-fill wrapper-unindent wrapper-indent))

(select-module textwrap)

;; ホワイトスペースを表す正規表現な定数。
(define-constant +whitespace+ #/\s/)


(define-class <text-wrapper> ()
  ;;  width は wrap するテキストの幅を表すんじゃないかな。
  ;;  デフォルトは 70。
  ((width :allocation :instance :init-keyword :width :init-value 70)
   ;; 最初のインデント幅を表します。と思ったんだけど、実際のインデント用の文字列の模様。
   (initial-indent :allocation :instance :init-keyword :initial-indent :init-value "")
   ))
