;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; パッケージ管理システム
;;; http://www.clear-code.com/blog/2011/2/16.html
(require 'cl)

(defvar package-base-dir "~/.emacs.d/packages")

(defun package-path-basename (path)
  (file-name-sans-extension (file-name-nondirectory path)))

(defun package-directory (files)
  (concat package-base-dir "/"
          (package-path-basename (car files))))

(defun package-run-shell-command (command)
  (message (format "running...: %s" command))
  (shell-command command))

(defun package-install-from-emacswiki (files)
  (shell-command
   (format "mkdir -p %s" (package-directory files)))
  (package-run-shell-command
   (format "wget --directory-prefix %s %s"
           (package-directory files)
           (mapconcat (lambda (name)
                        (concat "http://www.emacswiki.org/emacs/download/"
                                name))
                      files
                      " "))))

(defun package-install-from-github (files)
  (package-run-shell-command
   (format (concat "git clone https://github.com/%s.git %s")
           (car files)
           (package-directory files))))

(defun package-install-from-repo.or.cz (files)
  (package-run-shell-command
   (format (concat "git clone git://repo.or.cz/%s.git %s")
           (car files)
           (package-directory files))))

(defun package-alist-value (alist key default-value)
  (if (listp alist)
      (let ((alist-item (assoc key alist)))
        (if alist-item
            (cdr alist-item)
          default-value))
    default-value))

(defun package-install (type package-spec require-name &optional force)
  (let ((files (package-alist-value package-spec 'files
                                    (if (listp package-spec)
                                        package-spec
                                      (list package-spec))))
        (base-path (package-alist-value package-spec 'base-path "."))
        (additional-paths (package-alist-value package-spec 'additional-paths
                                               nil))
        (install-proc (case type
                        (emacswiki
                         'package-install-from-emacswiki)
                        (github
                         'package-install-from-github)
                        (repo.or.cz
                         'package-install-from-repo.or.cz)
                        (t
                         (error "unknown package type: <%s>(%s)"
                                type package)))))
    (add-to-list 'load-path
                 (format "%s/%s"
                         (package-directory files)
                         base-path))
    (dolist (additional-path additional-paths)
      (add-to-list 'load-path (format "%s/%s"
                                      (package-directory files)
                                      additional-path)))
    (condition-case err
        (require require-name)
      (error
       (message (format "installing %s..." files))
       (funcall install-proc files)))
    (require require-name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-install
;;; (install-elisp-from-emacswiki "auto-install.el")
;; auto-installのインストール先
(setq auto-install-directory "~/elisp/auto-install/")
(add-to-list 'load-path auto-install-directory)
(require 'auto-install)
;; 起動時にEmacsWikiのページ名を補完候補に加える
(auto-install-update-emacswiki-package-name t)
;; install-elisp.el互換モードにする
(auto-install-compatibility-setup)
;; ediff関連のバッファを一つのフレームにまとめる
(setq ediff-windows-setup-function 'ediff-setup-windows-plain)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 14 org-mode 総合文章作成環境
(require 'org)
;; org と howm-modeとの連携
(add-hook 'org-mode-hook 'howm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|txt\\|howm\\)$" . org-mode))
(setq howm-view-title-header "*")	; <- howmのロードより前に書くこと
(setq org-startup-folded "nofold")	; org開いた際、全て表示

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;(define-key global-map "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/howm/work.org"
                               "~/howm/home.org"
			       "~/howm/memo.org"))
;; M-x org-remember
;; install https://gna.org/p/remember-el
(add-to-list 'load-path "~/elisp/remember")
(require 'remember)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

(org-remember-insinuate)	; org-rememberの初期化
;; org メモを格納するorgrファイルの設定
(setq org-directory "~/howm/")
(setq org-default-notes-file (expand-file-name "memo.org" org-directory))
(define-key global-map "\C-cr" 'org-remember)

;; テンプレートの設定
(setq org-remember-templates
      '(("Note" ?n "** %?\n %i\n %a\n %t" nil "Inbox")
;;        ("Task" ?t "** TASK %?\n %i\n %a\n %t" nil "Tasks")
        ("Task" ?t "** TASK %^{Brief Description} %?\nAdded: %U" nil "Tasks")
	("Idea" ?i "** %?\n %i\n %a\n %t" nil "New Ideas")))
;; 14.6 C-c C-t,<S-up>,<S-down> TODOリストを作成する
(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
	'((sequence "TASK(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(x)" "CANCEL(c)")
	  (sequence "APPT(a)" "|" "DONE(x)" "CANCEL(c)")))
;; GTD手法
;; TASK 次にとるべき行動	PROJECT プロジェクト次にとるべき行動が複数あるもの
;; APPT 人と会う約束		STARTED 今行動している状態
;; WAITING 他人からの応答待ち、行動できる状態になるまで待機
;; | ここから右は終了状態を表すメタキーワード
;; SOMEDAY いつかやると決めた行動
;; CANCEL とりやめた状態
(setq org-startup-truncated nil)	; org-modeでウィンドウ右端で折り返すように変更

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; howm
(setq howm-menu-lang 'ja)
(global-set-key "\C-c,," 'howm-menu)
(autoload 'howm-menu "howm" "Hitori Otegaru Wiki Modoki" t)
;バックアップファイル,CVS,バイナリファイルは検索対象外にする
(setq howm-excluded-file-regexp
 (concat "\\(^\\|/\\)\\([.]\\|CVS/\\)\\|[~#]$\\|\\.\\(bak\\|elc\\|gz\\|aux\\|toc\\|idx\\|dvi\\)$\\|\\.\\(
GIF\\|JP\\(?:E?G\\)\\|P\\(?:BM\\|GM\\|NG\\|PM\\)\\|TIFF?\\|X\\(?:[BP]M\\)\\|gif\\|jp\\(?:e?g\\)\\|p\\(?:b
m\\|gm\\|ng\\|pm\\)\\|tiff?\\|x\\(?:[bp]m\\)\\)\\'"
 "\\|\\.xls$\\|\\.doc$\\|\\.ppt$\\|\\.tgz$\\|\\.pdf$\\|\\.bak$"))
;;日付フォーマットはorgに合わせる
(setq howm-date-separator "-") ;; "-" ==> 2003-10-21
(setq howm-date-format
  (concat "%Y" howm-date-separator "%m" howm-date-separator "%d %a")) ; => "%Y-%m-%d %a"
(setq howm-dtime-body-format
  (concat howm-date-format " %H:%M")) 	; => "%Y-%m-%d %a %H:%M"
(setq howm-dtime-format
  (concat "<" howm-dtime-body-format ">"))  ; => "<%Y-%m-%d %a %H:%M>"
(setq howm-insert-date-format "<%s>") ; => "<%s>"
(setq howm-date-regexp-grep
  (concat "[1-2][0-9][0-9][0-9]" howm-date-separator
          "[0-1][0-9]" howm-date-separator
          "[0-3][0-9]"
;;	  " *\\(?:Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\)*"
	  " *(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun|月|火|水|木|金|土|日)*"
	  ))
(setq howm-date-regexp
  (concat "\\([1-2][0-9][0-9][0-9]\\)" howm-date-separator
          "\\([0-1][0-9]\\)" howm-date-separator
          "\\([0-3][0-9]\\)"
	  " *\\(?:Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\|月\\|火\\|水\\|木\\|金\\|土\\|日\\)*"))
;;　想定日付フォーマット
;;<2010-12-19 Sun> <2010-12-20 Mon> <2010-12-21 Tue> <2010-12-22 Wed> <2010-12-23 Thu>
;;<2010-12-24 Fri> <2010-12-25 Sat> <2010-12-23 Thu 11:08>

;;(setq howm-reminder-regexp-format
;;      (concat "\\(\\[" howm-date-regexp "[- :0-9]*\\]\\)\\(\\(%s\\)\\([0-9]*\\)\\)"))
;; howm-reminder.el
(setq howm-reminder-regexp-grep-format
      (concat "<" howm-date-regexp-grep "[ :0-9]*>%s"))
(setq howm-reminder-regexp-format
  (concat "\\(<" howm-date-regexp "[ :0-9]*>\\)\\(\\(%s\\)\\([0-9]*\\)\\)"))

;; (setq howm-reminder-font-lock-keywords
;;   `(
;;     (,(howm-reminder-regexp "[-]") (0 howm-reminder-normal-face prepend))
;;     (,(howm-reminder-regexp "[+]") (0 howm-reminder-todo-face prepend))
;;     (,(howm-reminder-regexp "[~]") (0 howm-reminder-defer-face prepend))
;;     (,(howm-reminder-regexp "[!]") (0 howm-reminder-deadline-face prepend))
;;     (,(howm-reminder-regexp "[@]") (0 howm-reminder-schedule-face prepend))
;;     (,(howm-reminder-regexp "[.]") (0 howm-reminder-done-face prepend))
;;     ))
(setq howm-menu-expiry-hours 2) ;; メニューを 2 時間キャッシュ
(setq howm-menu-refresh-after-save nil) ;; メモ保存時のメニュー更新も止める
;howm検索高速化(grep使用)
(setq howm-view-use-grep t)
;(setq howm-view-grep-command "egrep")
;(setq howm-view-fgrep-command "fgrep")
(setq howm-process-coding-system 'utf-8)
;howmのデフォルトエンコードをutf-8にする。
(add-hook 'howm-create-file-hook
          (lambda ()
            (set-buffer-file-coding-system 'utf-8)))
;; org-remember-code-reading
;; http://d.hatena.ne.jp/rubikitch/20090121/1232468026
(defvar org-code-reading-software-name nil)
;; ~/howm/code-reading.org に記録する
(defvar org-code-reading-file "code-reading.org")
(defun org-code-reading-read-software-name ()
  (set (make-local-variable 'org-code-reading-software-name)
       (read-string "Code Reading Software: " 
                    (or org-code-reading-software-name
                        (file-name-nondirectory
                         (buffer-file-name))))))
(defun org-code-reading-get-prefix (lang)
  (concat "[" lang "]"
          "[" (org-code-reading-read-software-name) "]"))
(defun org-remember-code-reading ()
  (interactive)
  (let* ((prefix (org-code-reading-get-prefix (substring (symbol-name major-mode) 0 -5)))
         (org-remember-templates
          `(("CodeReading" ?r "** %(identity prefix)%?\n   \n   %a\n   %t"
             ,org-code-reading-file "Memo"))))
    (org-remember)))
(define-key global-map "\C-cc" 'org-remember-code-reading)

;;(setq howm-todo-menu-types (quote ("" "-" "+" "~" "!" "")))
;;(setq howm-todo-types (quote ("" "-" "+" "~" "!" "")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;navi2ch
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
(setq navi2ch-display-splash-screen nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;lookup(EPWING閲覧)
     ;; ;; オートロードの設定
     ;; (autoload 'lookup "lookup" nil t)
     ;; (autoload 'lookup-region "lookup" nil t)
     ;; (autoload 'lookup-pattern "lookup" nil t)

     ;; ;; キーバインドの設定
     ;; (define-key ctl-x-map "l" 'lookup)              ; C-x l - lookup
     ;; (define-key ctl-x-map "y" 'lookup-region)       ; C-x y - lookup-region
     ;; (define-key ctl-x-map "\C-y" 'lookup-pattern)   ; C-x C-y - lookup-pattern

     ;; ;; 検索エージェントの設定
     ;; (setq lookup-search-agents '((ndeb "/home/user/MyDocs/eb_dict/WIKIP")
     ;; 				  (ndeb "/home/user/MyDocs/eb_dict/genius")
     ;; 				  (ndeb "/home/user/MyDocs/eb_dict/super2002")
     ;; 				  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;cperl-mode利用 >>>http://www.namazu.org/~tsuchiya/elisp/perl-mode.html
;; (defalias 'perl-mode 'cperl-mode)
;; (autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
;; (setq cperl-indent-level 4)
;; (setq cperl-continued-statement-offset 4)
;; (setq cperl-comment-column 40)
;; ;perl Debugger C-c d
;; ;wget http://www.namazu.org/~tsuchiya/elisp/perl-debug.el
;; (setq perl-debug-coding-system 'utf-8)
;; (add-hook 'cperl-mode-hook
;;           (lambda ()
;;             (require 'perl-debug)
;;             (perl-debug-set-coding-system)
;; ;            (define-key cperl-mode-map "\C-cc" 'perl-debug-lint)
;;             (define-key cperl-mode-map "\C-cd" 'perl-debug)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;mode-info
;; (add-to-list 'load-path "/home/user/mode-info/")
;; (require 'mi-config)
;; (define-key global-map "\C-hf" 'mode-info-describe-function)
;; (define-key global-map "\C-hv" 'mode-info-describe-variable)
;; (define-key global-map "\M-." 'mode-info-find-tag)
;; ;カラフルな Info が好みの人は，以下の設定も追加してください．
;; (require 'mi-fontify)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;sdic C-c C-w
(when (require 'sdic nil t)
  (global-set-key "\C-c\C-w" 'sdic))
;; sdic inline
(require 'sdic-inline)
;; http://github.com/m2ym/auto-complete
(require 'popup)
(sdic-inline-mode t)   ; sdic-inline モードの起動
;; 辞書ファイルの設定
(setq sdic-inline-eiwa-dictionary "/home/user/MyDocs/sdic/gene.sdic")
;(setq sdic-inline-waei-dictionary "/home/user/MyDocs/sdic/jedict.sdic")
(setq sdic-inline-enable-modes nil)                ; major-mode を基準にオン・オフを判断しなくする。
;; (add-to-list 'sdic-inline-enable-modes 'w3m-mode)  ; w3m-mode を対象に追加
;; (add-to-list 'sdic-inline-enable-modes 'text-mode)  ; howmを対象に追加
;; (add-to-list 'sdic-inline-enable-modes 'help-mode)
;; (add-to-list 'sdic-inline-enable-modes 'info-mode)
;; (add-to-list 'sdic-inline-enable-modes 'org-mode)
;(setq sdic-inline-enable-faces nil)            ; テキスプロパティを基準にオン・オフを判断しなくする。
(setq sdic-inline-not-search-style 'word)  ; カーソル下の単語が前回辞書で引いた単語と同じである限り、再度辞書ではひかない
(setq sdic-inline-search-func 'sdic-inline-search-word-with-stem) ;過去形などを除去
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Wanderlust その他設定 ~/.wl,.addresses,.folers
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;text-translator (google translator http://www.emacswiki.org/emacs/TextTranslator
(require 'text-translator-load)
(global-set-key "\C-x\M-t" 'text-translator)
;(require 'popup) ;要 popup.el
(setq text-translator-display-popup t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 03 キーバインドについて
;; 3.8 sequental-command.el 同じコマンドを連続実行したときの振舞いを変更する
;; C-a C-eを連続動作させた時にファイル先頭や終端へ移動
;; M-x auto-install-batch sequential-command
(require 'sequential-command-config)
(sequential-command-setup-keys)
;; 3.9 key-chord.el キーボード同時押しでコマンドを実行する
;; key-chord.el キーボード同時押しでコマンドを実行する
;; (install-elisp-from-emacswiki "key-chord.el")
(require 'key-chord)
(setq key-chord-two-keys-delay 0.08) ; 許容誤差範囲は、0.04秒が妥当と判断 ->倍にしてみた
(key-chord-mode 1)
;; kj同時押しでview-mode切替
(key-chord-define-global "kj" 'view-mode)
;; yu で auto-complete-modeの有効/無効を切り替える
(key-chord-define-global "yu" 'auto-complete-mode)
;; ui で 次ウィンドウへ
(key-chord-define-global "ui" 'other-window)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recentf.el 最近使ったファイルを開く 
;; (install-elisp-from-emacswiki "recentf-ext.el")
;; 最近のファイル保存数
(setq recentf-max-saved-items 500)
;; 最近使ったファイルに加えないファイルを正規表現で指定
(setq recentf-exclude '("/TAGS$" "/var/tmp/" "\.bmk$"))
(require 'recentf-ext)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tempbuf.el 使わないバッファを自動的に消す 
;; (install-elisp-from-emacswiki "tempbuf.el")
(require 'tempbuf)
;; ファイルを開いたら自動的にtempbufを有効にする
;(add-hook 'find-file-hooks 'turn-on-tempbuf-mode)
;; diredバッファに対してtempbufを有効にする
(add-hook 'dired-mode-hooks 'turn-on-tempbuf-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.5 migemo use cmigemo ローマ字のまま日本語をインクリメントサーチする
;; CMIGEMO http://www.kaoriya.net/#CMIGEMO
;; migemo.el (install-elisp-from-gist 457761)
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
;; (load-library "migemo")
;; (migemo-init)
(setq migemo-isearch-enable-p nil)	;デフォルトOff/M-mでisearch<->migimoのトグル
;; emacs終了時にプロセスがいるけどどうする?と聞いてこなくする
(defadvice save-buffers-kill-terminal (before my-save-buffers-kill-terminal activate)
  (when (process-list)
    (dolist (p (process-list))
      (set-process-query-on-exit-flag p nil))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.6 point-undo.el カーソル位置を戻す F7 S-F7
;; (install-elisp-from-emacswiki "point-undo.el")
(require 'point-undo)
(global-set-key (kbd "<f7>") 'point-undo)
(global-set-key (kbd "S-<f7>") 'point-redo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.7 bm.el カーソル位置に目に見える印をつける http://www.nongnu.org/bm/
;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")
(setq-default bm-buffer-persistence nil)
(setq bm-restore-repository-on-load t)
(require 'bm)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)
(global-set-key (kbd "M-SPC") 'bm-toggle)
(global-set-key (kbd "<print>") 'bm-previous); <print>=S-<f6>?
(global-set-key (kbd "S-<f6>") 'bm-previous); <print>=S-<f6>?
(global-set-key (kbd "<f6>") 'bm-next)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5.8 goto-chg.el 最後の変更箇所にジャンプする F8 S-F8
;; (install-elisp-from-emacswiki "goto-chg.el")
(require 'goto-chg)
(global-set-key (kbd "<f8>")  'goto-last-change)
(global-set-key (kbd "S-<f8>") 'goto-last-change-reverse)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.1 autoinsert.el
;(auto-insert-mode)			;.elで確定できなくなるのでコメントアウト
(setq auto-insert-directory "~/.emacs.d/templates/") ; 最後の/は必須
(define-auto-insert "\\.rb$" "ruby-template.rb")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.2 redo+.el やり直しundoをC-M-/に割り当て
;; (install-elisp-from-emacswiki "redo+.el")
(require 'redo+)
(global-set-key (kbd "C-M-/") 'redo) ; dabbrev-completionは使わない
(global-set-key (kbd "C-M-_") 'redo) ; dabbrev-completionは使わない
(setq undo-no-redo t) ;過去のundoがredoされないようにする
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.3 sense-region.el 矩形を選択しやすくする C-SPC C-SPCで矩形、単語へ拡張
;; (install-elisp "http://taiyaki.org/elisp/sense-region/src/sense-region.el")
(require 'sense-region)
;;Warning: `mapcar' called for effect; use `mapc' or `dolist' instead を防ぐ
(setq byte-compile-warnings '(free-vars unresolved callargs redefine obsolete noruntime cl-functions interactive-only make-local))
(sense-region-on) ; ワーニングが出る
;; なにかの拍子に region Faceが壊れるので sense-region-to-rectangleを上書きする @ emacs23
;; http://d.hatena.ne.jp/sonota88/20101005/1286230129
(defun sense-region-to-rectangle ()
  (interactive)
  (setq sense-region-status 'rectangle)
  ;; フェイスまわりのデフォルトの挙動が変なので
  ;; この2行を追加
  (copy-face mell-region-face 'sense-region-face)
  (copy-face 'region 'sense-region-region-face)
  (mell-sign-reset-face mell-region-face))
;; Faceがおかしくなったら EVAL IT (reset-region-face)
(defun reset-region-face ()
  "This function reset region face."
  (interactive)
  (custom-set-faces '(region ((t (:background "lightgoldenrod2" :foreground "black")))))
  (copy-face 'region 'sense-region-face)
  (copy-face 'region 'sense-region-region-face)
  (copy-face 'region 'sense-region-rectangle-face)
  (copy-face 'region 'sense-region-offset-face)
  (copy-face 'region 'sense-region-selection-base-face)
  (copy-face 'highlight 'sense-region-selection-highlight-face))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.5 yasnippet.el 略語から定型文を入力する
;; (install-elisp-from-emacswiki "yasnippet-config.el")
(add-to-list 'load-path (expand-file-name "~/elisp/yasnippet"))
(require 'yasnippet-config)
(yas/setup "~/elisp/yasnippet")
;; 6.6 スニペットを定義する
;; M-x yas/new-snippet
;; key:の行は削除 name:はスニペットの1行説明文
;; ${1:デフォルト値} がプレースフォルダー $0 はスニペット展開後に置かれるカーソル位置を示す
;; C-c C-t スニペットをテストする
;; C-c C-c スニペットと登録する
;; 6.9 スニペットをその場で定義して使う
;(global-set-key (kbd "C-x y") 'yas/registor-oneshot-snippet) ;リージョンをスニペットとして記憶
;(global-set-key (kbd "C-x C-y") 'yas/expand-oneshot-snippet)
;; 6.10 M-x hippie-expand 略語展開・補完を行うコマンドをまとめる
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-file-name-partially 	;ファイル名の一部
;; 	try-complete-file-name			;ファイル名
;; 	try-expand-all-abbrevs			;静的略語展開
;; 	try-expand-dabbrev			;動的略語展開(カレントバッファ)
;; 	try-expand-dabbrev-all-buffers		;動的略語展開(全バッファ)
;; 	try-expand-dabbrev-from-kill		;動的略語展開(キリング)
;; 	try-complete-lisp-symbol-partially	;lispシンボル名の一部
;; 	try-complete-lisp-symbol		;lispシンボル名全体
;; 	))
;; 6.11 キーボードマクロで操作を自動化する F3 F4
;; <F3> (kmacro-start-macro-or-insert-counter) キーボードマクロ 記録開始
;; <F4> (kmacro-end-or-call-macro) マクロ記録終了
;; <F4> キーボードマクロ実行 C-u 回数 <F4> 回数分実行 C-u 0 <F4> エラーが出るまで実行繰返し
;; C-u <F3> キーボードマクロに追加記録
;; キーボードマクロ定義途中で C-x q(kbd-macro-query)を押しておくと実行時に続行可否のプロンプト表示
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6.14 auto-complite IDEのような入力支援をする
;; auto-install-batch "auto-complete"
(package-install 'github "m2ym/auto-complete" 'auto-complete-config)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (format "%s/auto-complete/dict" package-base-dir)) ;補完辞書の置場
(ac-config-default)			;初期設定?
(setq ac-modes (append ac-modes '(howm-mode)))
(setq ac-modes (append ac-modes '(text-mode)));howm用
(global-auto-complete-mode t) 		;グローバルにauto-complete-modeを利用する
(setq ac-menu-height 3) 		;補完メニューの行数
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 07 検索・置換
;; color-moccur.el バッファ内(複数可)を検索する
;; (install-elisp-from-emacswiki "color-moccur.el")
(require 'color-moccur)
(setq moccur-split-word t) ;スペースで区切られた複数の単語にマッチさせる
;; M-x occur-by-moccur
;; moccur-edit.el color-moccur.elの結果を編集する
;; (install-elisp-from-emacswiki "moccur-edit.el")
(require 'moccur-edit)
;; M-x occur 等の後 *Moccur*バッファ内で r (moccur-edit-mode-in)で検索結果を編集状態できその状態で
;; M-% (query-replace)等で自由に編集。C-cで変更をファイルに反映

;; igrepl.el grepのコマンドラインを打たずにgrep検索する
;; (install-elisp-from-emacswiki "igrep.el")
(require 'igrep)
(igrep-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -Ou8"))
(igrep-find-define lgrep (igrep-use-zgrep nil)(igrep-regex-option "-n -Ou8"))
;; grep-edit.el grep検索結果を編集する
;; (install-elisp-from-emacswiki "grep-edit.el")
(require 'grep-edit)
;; M-x grep で変更すべき行を抜き出し M-% や C-M-% で文字列置換し、C-c C-e で変更を反映し、
;; C-x s(save-some-buffers)で変更されたファイルを保存する。このコマンドは変更されたファイルをまとめて保存
;; C-c C-r でリージョンの変更点を破棄 C-c C-uで変更点全破棄
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; twittering-mode.el
;; >>>http://twmode.sourceforge.net/ja/
;(add-to-list 'load-path "~/elisp/twittering-mode-2.0.0/")
;; (install-elisp "http://github.com/hayamiz/twittering-mode/raw/master/twittering-mode.el")
(require 'twittering-mode)
;(setq twittering-auth-method 'xauth)
(setq twittering-username "komecha")
(setq twittering-use-master-password t)	; OAuthのPINを保存する
;; 使い方) M-x twitで実行
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; viewer.el view-mode拡張
;; (install-elisp-from-emacswiki "viewer.el")
(require 'viewer)
(viewer-stay-in-setup)
(setq viewer-modeline-color-unwritable "tomato"
      viewer-modeline-color-view "red")
(viewer-change-modeline-color-setup)
(setq view-read-only t) ; 読取専用はview-modeで開く (読取属性はC-x C-q でトグル切替)
(viewer-stay-in-setup) ; 書込み不能ファイルでview-modeから抜けなくする
(require 'view)
;;less感覚の操作
(define-key view-mode-map (kbd "N") 'View-search-last-regexp-backward)
(define-key view-mode-map (kbd "?") 'View-search-regexp-backward)
(define-key view-mode-map (kbd "G") 'View-goto-line-last)
(define-key view-mode-map (kbd "b") 'View-scroll-page-backward)
(define-key view-mode-map (kbd "f") 'View-scroll-page-forward)
;; vi/w3m感覚の操作
(define-key view-mode-map (kbd "h") 'backward-char)
(define-key view-mode-map (kbd "j") 'next-line)
(define-key view-mode-map (kbd "k") 'previous-line)
(define-key view-mode-map (kbd "l") 'forward-char)
(define-key view-mode-map (kbd "J") 'View-scroll-line-forward)
(define-key view-mode-map (kbd "K") 'View-scroll-line-backward)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit.el 括弧の対応を取りながらS式を編集する
;;(install-elisp "http://mumble.net/~campbell/emacs/paredit.el")
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc.el EmacsLisp関数・変数のヘルプをエコーエリアに表示する
;; (install-elisp-from-emacswiki "eldoc-extension.el")
(require 'eldoc-extension)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
(setq eldoc-idle-delay 0.2) ;すぐに表示
(setq eldoc-minor-mode-string "")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; usage-memo.el *Help*バッファにメモを書き込めるようにする
;;(install-elisp-from-emacswiki "usage-memo.el")
;; 使い方) 境界線以下に直接メモを記入して C-x C-sで保存
(require 'usage-memo)
(setq umemo-base-directory "~/elisp/umemo") ;メモを保存するディレクトリ
;(define-usage-memo info "elisp" 0 "*info*<elisp> (elisp-jp) %s")
;(define-usage-memo mode-info-describe-function "elisp" 0 "*info*<elisp> (elisp-jp) %s")
(umemo-initialize)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lispxmp.el EmacsLisp式の値をコメントで注釈する
;; (install-elisp-from-emacswiki "lispxmp.el")
(require 'lispxmp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-junk-file.el 使い捨てのファイルを開く
;; (install-elisp-from-emacswiki "open-junk-file.el")
(require 'open-junk-file)
;; ファイル名入力時に~/junk/年-月-日-時分秒.が出てくる
(setq open-junk-file-format "~/junk/%Y-%m-%d-%H%M%S.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elscreen
;; wget ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-1.4.6.tar.gz ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-server-0.2.0.tar.gz ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-speedbar-0.0.0.tar.gz ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-wl-0.8.0.tar.gz ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-w3m-0.2.2.tar.gz ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-howm-0.1.3.tar.gz ftp://ftp.morishima.net/pub/morishima.net/naoto/ElScreen/elscreen-dired-0.1.0.tar.gz
(setq elscreen-prefix-key "\C-t")
;; C-t ?ヘルプ表示 C-t C-k 現在のウィンドウをkill
(setq elscreen-display-tab 8) ; タブの幅（６以上じゃないとダメ）t:自動幅(画面の外にいっちゃう
(setq elscreen-tab-display-kill-screen   nil)       ; スクリーンを消すボタン[X]を配置しない
(require 'elscreen)
;; C-t SPCで次タブへ
(define-key elscreen-map (kbd "SPC") 'elscreen-next)
;; ElScreen-server
(require 'elscreen-server)
;; ElScreen-Speedbar
(require 'elscreen-speedbar)
;; ElScreen-WL
;(require 'elscreen-wl)
;; ElScreen-W3M
(require 'elscreen-w3m)
;; ElScreen-howm
;(require 'elscreen-howm)
;; ElScreen-dired
(require 'elscreen-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;w3m-mode
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")
(require 'w3m-load)
(autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
(setq newsticker-html-renderer 'w3m-region
      browse-url-browser-function 'w3m-browse-url)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ipa.el ファイルに直接書き込まずメモをする メモインサート
;; (install-elisp-from-emacswiki "ipa.el")
;; M-x ipa-insert 現在位置にメモを書き込む
;; M-x ipa-eidt カーソルより後ろにあるメモを編集する
;; M-x ipa-move メモの位置を移動 hでヘルプ
;; M-x ipa-previos,ipa-next で前後のメモへジャンプ
;; M-x ipa-show すべてのメモ一覧を閲覧
;; M-x anything-ipa でカレントバッファの一覧
;; M-x anything-ipa-global は全ファイルのしおり一覧
(require 'ipa)
(define-key ipa-mode-map (kbd "RET") 'ipa-go-to-annotation)
(define-key ipa-overriding-map (kbd "RET") 'ipa-move-finish)
(define-key ipa-overriding-map (kbd "C-j") 'ipa-move-finish)
(define-key ipa-overriding-map (kbd "ESC") 'ipa-move-cancel)
(define-key ipa-overriding-map (kbd "C-p") 'ipa-move-line-up)
(define-key ipa-overriding-map (kbd "C-n") 'ipa-move-line-down)
(define-key ipa-overriding-map (kbd "C-f") 'ipa-move-right)
(define-key ipa-overriding-map (kbd "C-b") 'ipa-move-left)
(define-key ipa-overriding-map (kbd "C-v") 'ipa-move-page-down)
;(define-key ipa-overriding-map (kbd "M-v") 'ipa-move-page-up)
(defun ipa-move-end-of-line ()
  (interactive)
  (ipa-move-overlay 'end-of-line))
(defun ipa-move-beginnig-of-line ()
  (interactive)
  (ipa-move-overlay 'beginning-of-line))
(define-key ipa-overriding-map (kbd "C-e") 'ipa-move-end-of-line)
(define-key ipa-overriding-map (kbd "C-a") 'ipa-move-beginnig-of-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything
;; (auto-install-batch "anything")
(require 'anything-startup)
;(define-key ctl-x-map (kbd "C-f") 'anything-filelist+)
;(define-key ctl-x-map (kbd "C-f") 'anything-for-files)
;;(install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-moccur/trunk/anything-c-moccur.el")
;anything top なぜか1文字分行あふれしてレイアウトが乱れる対策
(defun anything-c-top-init ()
  (with-current-buffer (anything-candidate-buffer 'global)
    (call-process-shell-command
     (format anything-c-top-command
             (- (frame-width) (if anything-enable-digit-shortcuts 5 1)))
     nil (current-buffer))))

(require 'anything-c-moccur)
(setq moccur-split-word t)
(global-set-key (kbd "M-s") 'anything-c-moccur-occur-by-moccur)
;; インクリメントサーチから移行出来る様に
(define-key isearch-mode-map (kbd "C-o") 'anything-c-moccur-from-isearch)
;; 旧来のisearch-occurはC-M-oへ引越し
(define-key isearch-mode-map (kbd "C-M-o") 'isearch-occur)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; shell-pop shellバッファをポップアップする
;; (install-elisp-from-emacswiki "shell-pop.el")
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "/bin/zsh")
(global-set-key [f5] 'shell-pop)
(global-set-key (kbd "S-<f5>") 'my-term-switch-line-char);
(shell-pop-set-window-height 50)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x flymake-mode リアルタイムにプログラム言語の文法をチェックする
;; C,C++
(add-hook 'c-modde-common-hook (lambda () (flymake-mode t)))
;; Shell
;; 要 flymake-shell.el http://www.emacswiki.org/emacs/FlymakeShell
(require 'flymake-shell)
(add-hook 'sh-mode-hook 'flymake-shell-load)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; html関係
;; for Javascript .js
;; (install-elisp "http://js2-mode.googlecode.com/files/js2-20090723b.el")
   (autoload 'js2-mode "js2" nil t)
   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
   (setq c-basic-offset 4) ; インデント幅 4 
;; for javascript
;; (install-elisp "http://www.brgeight.se/downloads/emacs/javascript.el")
(autoload 'javascript-mode "javascript" nil t)

;; for css　.css
;; (install-elisp "http://www.garshol.priv.no/download/software/css-mode/css-mode.el") 
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css$'" . css-mode))

;; mmm-mode
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face "navy")	;色設定
;(set-face-background 'mmm-default-submode-face nil)	;submodeの背景色は不要

;; define mmm submodes
(mmm-add-classes
  '(
    (embedded-css
    :submode css-mode
    :face mmm-code-submode-face
    :front "<style[^>]*>"
    :back "</style>")
  (embedded-js
;;    :submode js2-mode
    :submode javascript-mode
    :face mmm-code-submode-face
    :front "<script[^>]*>"
    :back "</script>"))
)
;;

;; add mmm submodes to *.html files
(mmm-add-mode-ext-class nil "\\.s?html?\\'" 'embedded-js)
(mmm-add-mode-ext-class nil "\\.s?html?\\'" 'embedded-css)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evernote-mode
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; optional
;(add-to-list 'load-path "<your load path>")
(require 'evernote-mode)
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)
(setq evernote-username "komecha")
(setq evernote-password-cache t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
