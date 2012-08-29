;; ディレクトリ構成
;; .emacs.d
;; |-- init.el         ;; 基本的な設定を記述
;; |-- config          ;; 特定のモードや非標準のElispの設定をこの下に置く
;; |   |-- builtins.el ;; 標準Elispの設定
;; |   |-- packages.el ;; 非標準Elispの設定
;; |   `-- packages    ;; 非標準Elispのうち、設定が多くなるものはこの下に置く
;; |       `-- sdic.el ;; （例）非標準Elispであるsdicの設定
;; `-- packages        ;; 非標準Elispをこの下に置く

;;; ロードパスの追加
(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/packages"
                   "~/.emacs.d/packages/navi2ch")
                 load-path))

;; 文字コードの設定
;;; Localeに合わせた環境の設定
;(set-locale-environment nil)
;; (set-clipboard-coding-system 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (setq file-name-coding-system 'utf-8)
;; ;ansi-termを日本語表示に必要
(setq locale-coding-system 'utf-8)
;; for Japanese users.emacs21
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8) ; クリップボードやputtyのIME直入力
;; (setq file-name-coding-system 'utf-8) ; C-x C-f 時のファイル名
;; ;文字コード自動判定utf-8優先に
(prefer-coding-system 'utf-8)

;; N9では、diredが使えない。busybox lsのせい？
(setq dired-use-ls-dired nil)

;;; キーバインド
(define-key global-map (kbd "C-z") 'undo)                 ; undo
(define-key global-map (kbd "C-c i") 'indent-region)      ; インデント
(define-key global-map (kbd "C-c C-i") 'hippie-expand)    ; 補完

;;; FileやEditといったメニューを非表示にする
(menu-bar-mode -1)
;;; ツールバーを消す
;;(tool-bar-mode nil) ;;Non-x emacsだとエラー
;;; welcome splash 非表示
(setq inhibit-splash-screen t)
;;; 折り返し記号を表示しないようにする
(set-display-table-slot standard-display-table 'wrap ?\ )
;;; xterm上でマウスが効くようにする
(xterm-mouse-mode t)
(mouse-wheel-mode t) ;;Non-x emacsだとエラー
(setq mouse-wheel-follow-mouse t)
;;; 個人用info(~/info)の参照追加 (個人優先)
(setq Info-default-directory-list
      (cons (expand-file-name "~/info") Info-default-directory-list ))
;;; 対応するカッコをハイライト
(show-paren-mode t)
(set-face-background 'show-paren-match "green")
;;; 改行と同時にインデントも行う
(global-set-key "\C-m" 'newline-and-indent)
;; find-functionをキー割当する
(find-function-setup-keys)
;;; ミニバッファ入力履歴を次回Emacs起動時にも保存する
(savehist-mode 1)
;;; ファイル内のカーソル位置を記憶する
(setq-default save-place t)
(require 'saveplace)
;;; 行番号・桁番号を表示する
(line-number-mode t)
(column-number-mode t)
;;; バックアップファイルを作らない
(setq backup-inhibited t)
;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
;; ログの記録行数を増やす
(setq message-log-max 10000)
;; 履歴をたくさん保存する
(setq history-length 1000)
;;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 1000)
;; キーストロークをエコーエリアに早く表示する
(setq echo-keystrokes 0.1)
;; yesと入力するのは面倒なのでyで十分
(defalias 'yes-or-no-p 'y-or-n-p)
;; 4.1 ffap.el 現在位置のファイル・URLを開く
(ffap-bindings)
;; 4.13 wdired.el ディレクトリ内のファイル名を自由自在に編集する
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
;; 4.14 TRAMP sshやftp先のファイルを編集する
;;  C-x C-f /ssh:kuma@192.168.24.43:.emacsx
(require 'tramp)
(setq tramp-default-method "sshx")
;; C-x C-f /sudo:root@localhost:/etc/passwd root権限で編集
;; 4.15 sudoedit root権限でファイルを編集する emacsclient screen:0でのみ動作
(if (string= (getenv "WINDOW") "0")
    (progn (server-start)
	   ))
;; M(shift+m)した時のブラウザの設定
(setq browse-url-netscape-program "~/bin/netscape")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;全角スペースとかに色を付ける
;(defface my-face-b-1 '((t (:background "medm aquamarine"))) nil)
;(defface my-face-b-1 '((t (:background "dark turquoise"))) nil)
(defface my-face-b-1 '((t (:background "blue"))) nil)
;(defface my-face-b-2 '((t (:background "cyan"))) nil)
(defface my-face-b-2 '((t (:background "brightblack"))) nil) ;TAB
;(defface my-face-b-2 '((t (:background "SeaGreen"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
(defadvice font-lock-mode (before my-font-lock-mode ())
            (font-lock-add-keywords
                 major-mode
                    '(
                           ("　" 0 my-face-b-1 append)
                           ("\t" 0 my-face-b-2 append)
                           ("[ ]+$" 0 my-face-u-1 append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks '(lambda ()
                             (if font-lock-mode
                               nil
                               (font-lock-mode t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 再帰的にgrep
(define-key global-map (kbd "M-C-g") 'grep)               ; grep
(define-key global-map (kbd "C-[ M-C-g") 'goto-line)      ; 指定行へ移動
(require 'grep)
(setq grep-command-before-query "grep -nH -r -e ")
(defun grep-default-command ()
  (if current-prefix-arg
      (let ((grep-command-before-target
             (concat grep-command-before-query
                     (shell-quote-argument (grep-tag-default)))))
        (cons (if buffer-file-name
                  (concat grep-command-before-target
                          " *."
                          (file-name-extension buffer-file-name))
                (concat grep-command-before-target " ."))
              (+ (length grep-command-before-target) 1)))
    (car grep-command)))
(setq grep-command (cons (concat grep-command-before-query " .")
                         (+ (length grep-command-before-query) 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ansi-termでterm-char-modeとterm-line-modeをトグル切替する
;;http://d.hatena.ne.jp/mooz/?of=15
;; term-char-mode キーマップは term-raw-map
;;  C-n, C-p などのキーが, シェルへ直接渡る. zsh の zle など, シェルのエディタ機能を叩ける.
;;  ansi-term の醍醐味.
;;  中々 Emacs のような操作ができない. 
;; term-line-mode キーマップは term-mode-map
;;  文字入力は完全に Emacs の方で行う. Enter を押したときに, 入力された文字列がシェルへ渡る.
;;  shell-mode に近い.
(defvar my-ansi-term-toggle-mode-key (kbd "S-<F5>"))
(defun my-term-switch-line-char ()
  "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
  (interactive)
  (cond
   ((term-in-line-mode)
    (term-char-mode)
    (hl-line-mode -1))
   ((term-in-char-mode)
    (term-line-mode)
    (hl-line-mode 1))))
;ansi-term上でバッファ移動する shellのヒストリにはM-p M-nを使用。;ansi-termが動いていないと定義できない？
;ので、フックに変更
(add-hook 'term-mode-hook '(lambda ()
  (define-key term-raw-map "\C-p" 'previous-line)
  (define-key term-raw-map "\C-n" 'next-line)
  ;; これがないと M-x できなかったり
  (define-key term-raw-map (kbd "M-x") 'nil)
  (define-key term-raw-map (kbd "C-t") 'nil)
  (define-key term-raw-map (kbd "S-<f5>") 'nil)
;  (define-key term-raw-map (kbd "<f5>") 'nil)
  ;; コピー, 貼り付け
  (define-key term-raw-map (kbd "C-k")
    (lambda (&optional arg) (interactive "P") (funcall 'kill-line arg) (term-send-raw)))
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  (define-key term-raw-map (kbd "M-y") 'anything-show-kill-ring)
  ;; C-t で line-mode と char-mode を切り替える
  (define-key term-raw-map  my-ansi-term-toggle-mode-key 'my-term-switch-line-char)
  (define-key term-mode-map my-ansi-term-toggle-mode-key 'my-term-switch-line-char)
  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ファンクションキーが動作 xterm.elが読み込まれる
;; ;; defvar xterm-function-map
;; ;; >>> http://www.dennougedougakkai-ndd.org/~delmonta/emacs/26.html
;; ;; define-key function-key-map "\e[21~" [f10]
;; (add-hook 'term-setup-hook '(lambda ()	
;; 					;    (define-key xterm-function-map "\e[23~" [S-f1])
;; 			      (define-key input-decode-map "\e[23~" [S-f1])
;; 			      (define-key input-decode-map "\e[29~" [S-f6])
;; 			      (define-key function-key-map "\e[23~" [S-f1])
;; 			      (define-key function-key-map "\e[24~" [S-f2])
;; 			      (define-key function-key-map "\e[25~" [S-f3])
;; 			      (define-key function-key-map "\e[26~" [S-f4])
;; 			      (define-key function-key-map "\e[28~" [S-f5])
;; 			      (define-key function-key-map "\e[29~" [S-f6])
;; 			      (define-key function-key-map "\e[31~" [S-f7])
;; 			      (define-key function-key-map "\e[32~" [S-f8])
;; 			      (define-key function-key-map "\e[33~" [S-f9])
;; 			      (define-key function-key-map "\e[34~" [S-f10])
;; 			      　　　　　))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 標準Elispの設定
(load "config/builtins")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 非標準Elispの設定
(load "config/packages.el")

;;;
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
;(package-activate 'magit '(1 1 1))
(require 'magit)