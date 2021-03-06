;;;diredで"V"を入力するとそのディレクトリで使っているバージョン 管理システム用のモードを起動
(defun dired-vc-status (&rest args)
  (interactive)
  (cond ((file-exists-p (concat (dired-current-directory) ".svn"))
         (svn-status (dired-current-directory)))
        ((file-exists-p (concat (dired-current-directory) ".git"))
         (magit-status (dired-current-directory)))
        (t
         (message "version controlled?"))))

(define-key dired-mode-map "V" 'dired-vc-status)

; 個人用Infoを 追加
(setq Info-default-directory-list
      (cons (expand-file-name "~/MyDocs/info") Info-default-directory-list ))

