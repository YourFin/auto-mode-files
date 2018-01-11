(defvar yf/major-mode-hooks-dir
  ;; Support for no-littering.el without any interference 
  (if (file-exists-p (concat user-emacs-directory "etc/"))
      (concat user-emacs-directory "etc/major-hooks/")
    (concat user-emacs-directory "major-hooks/"))
  "The directory that is used for major mode configuration files.")

(defun yf-major-mode-hooks--make-table ()
  "Builds the hash table for `yf/major-mode-hooks--table'
It should be noted that this function does nothing to actually populate
said table; that should be done with `yf-major-mode-hooks--populate-table',
usually called through `yf-major-mode-hooks-refresh'."
  (unless (file-exists-p yf/major-mode-hooks-dir) (make-directory yf/major-mode-hooks-dir))
  (let ((dir-list (directory-files yf/major-mode-hooks-dir)))
    ;;Weakness determines the garbage collection rate;
    ;;key-and-value requires that both a key and value
    ;;be present in order to avoid garbage collection
    ;;if they are unused in any other context
    (make-hash-table :weakness nil
		     :test 'equal
		     :size (length dir-list)
		     :rehash-size 1.1
		     :rehash-threshold 0.9)))

(defvar yf/major-mode-hooks--table
  (yf-major-mode-hooks--make-table)
  "The table to keep track of which modes have
files that should be loaded after they open

Load precedence is minor mode files then major mode files;
at this point in time there isn't any way to customize this,
but hopefully in the future this will be part of the file
name, as a way to allow multiple files to be loaded in
whatever order the user desires.")

(defun yf-major-mode-hooks--populate-table ()
  "Populates `yf/major-mode-hooks--table' with the files in `yf/major-mode-hooks-dir'"
  (cl-flet* ((get-mode-symbol (lambda (filename)
				(intern
				 (file-name-nondirectory
				  (file-name-sans-extension filename)))))
	     (put-hash-hooks-table (lambda (filename)
				     (puthash (get-mode-symbol filename)
					      (file-truename filename)
					      yf/major-mode-hooks--table))))
    (let* ((dir-list (directory-files yf/major-mode-hooks-dir))
	   (el-list (seq-filter (lambda (filtee)
				  (string= "el" (file-name-extension filtee)))
				dir-list))
	   (elc-list (seq-filter (lambda (filtee)
				   (string= "elc" (file-name-extension filtee)))
				 dir-list)))
      ;; We want to use the compiled version if it exists;
      ;; hence the explicit split between the found el files
      ;; and elc files
      ;;
      ;; TODO: figure out how to access the function portion
      ;; of put-hash-hooks-table to get rid of these stupid
      ;; lambdas, and why symbol-function doesn't work for this
      (mapc (lambda (a) (put-hash-hooks-table a)) el-list)
      (mapc (lambda (a) (put-hash-hooks-table a)) elc-list))))

(defun yf-major-mode-hooks-refresh ()
  "Refresh the state of the files in 
Clears `yf/major-mode-hooks--table' and then populates it
with `yf-major-mode-hooks--populate-table'."
  (interactive)
  ;; Flush out the old values to make
  ;; available for garbage collection
  (clrhash yf/major-mode-hooks--table) 
  ;; Make a new table to avoid a bunch
  ;; of resizing if the new set is massively bigger
  (setq yf/major-mode-hooks--table (yf-major-mode-hooks--make-table))
  ;; Populate the new table
  (yf-major-mode-hooks--populate-table))

;; To allow the user to set `yf/major-mode-hooks-dir' after
;; loading this file and not have to worry about running
;; the refresh function twice on startup
(add-hook 'after-init-hook 'yf-major-mode-hooks-refresh)

(defun yf-major-mode-hooks--run-symbol-internal (symbol)
  "Not to be used externally!

Essentialy the same thing as `yf-major-mode-hooks-run-symbol',
however it doesn't throw an error on a non-existant file"
  (let ((file-name (gethash symbol yf/major-mode-hooks--table)))
    (if file-name
	(load-file file-name)
      nil)))

(defun yf-major-mode-hooks-run-symbol (symbol)
  "Runs the file matching SYMBOL in `yf/major-mode-hooks-dir'
Note: this function cannot be called before startup without
running `yf-major-mode-hooks-refresh' somewhere along the
line in your .emacs || init.el

This is to allow you to manually set `yf/major-mode-hooks-dir'
without slowing down startup by calling `yf-major-mode-hooks-refresh',
and to make clear that this should not be used in place of `require' on startup"
  (or (yf-major-mode-hooks--run-symbol-internal symbol)
      (error (concat
	      "Error: `yf-major-mode-hooks-run-symbol' could not find a file to match symbol "
	      (symbol-name symbol)))))

;; symbol-name converts symbol to string nitwit
(defun yf-edit-major-mode-hook (&optional mode)
  "Edit the hook file for the MODE major mode. 
Current major mode if called by default or
if called interactively

If the file does not already exist, create it, add a
requirement statement to major-mode-hooks.el and 
create the file with a newline followed by an appropriate 
provide statement. 

The files take the form {name of MODE (sans -mode)}-hook.el
in ${yf/major-mode-hooks-dir}/majorHooks

Note that this function does not check for the existance
of require statements in major-mode-hooks.el"
  (interactive (list major-mode))
  (let* ((acting-mode-name (symbol-name mode))
	 (major-mode-file-dir (file-truename yf/major-mode-hooks-dir))
	 (major-hooks-file (concat major-mode-file-dir "/major-mode-hooks.el"))
	 (hook-file-path (concat major-mode-file-dir "/" acting-mode-name ".el")))
    (if (file-exists-p hook-file-path)
	(find-file hook-file-path)
      ;; create the hook file dir if it doesn't exist
      (if (not (file-exists-p major-mode-file-dir))
	  (make-directory major-mode-file-dir t))
      (find-file hook-file-path)
      (insert (concat
	       ";;;;;;;;;;Automatically generated by yf-edit-major-mode-hook;;;;;;;;;;;\n\n\n;;"
	       acting-mode-name ".el ends here"))
      (forward-line -2)
      (save-buffer)
      (if (fboundp 'make-thread)
	  (make-thread #'yf-major-mode-hooks--populate-table)
	(yf-major-mode-hooks--populate-table)))))

(defun yf-major-mode-hooks--hook-func ()
  "The function called by various hooks to load files from `yf/major-mode-hooks-dir'"
  (mapc 'yf-major-mode-hooks--run-symbol-internal (append minor-mode-list (list major-mode))))
(add-hook 'find-file-hook #'yf-major-mode-hooks--hook-func)
(add-hook 'after-change-major-mode-hook #'yf-major-mode-hooks--hook-func)

(defun helm-edit-mode-hook ()
  "A helmized way of editing a mode hook"
  (interactive)
  (helm :sources
	(let ((action (helm-make-actions "Edit mode hook" 'yf-edit-major-mode-hook))
	      (build-helm-alist (lambda (sym) (cons (symbol-name sym) sym))))
	  (list (helm-build-sync-source "Current Major Mode"
		  :candidates
		  (list (funcall build-helm-alist major-mode))
		  :action action)
		(helm-build-sync-source "Minor modes in use"
		  :candidates
		  (mapcar (lambda (sym) (funcall build-helm-alist sym)) minor-mode-list)
		  :action action)))
	:buffer "*Helm edit hook*"))

(provide 'auto-mode-files-setup)
