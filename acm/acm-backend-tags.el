;;; Code:
(defcustom acm-enable-tags t
  "Popup tag completions when this option is turn on."
  :type 'boolean)

(defun acm-backend-tags-candidates (keyword)
  (when acm-enable-tags
    (let* ((candidates (list))
	   (tags (tags-completion-table (get-buffer-create "*acm-backends-tags-completion-table*"))))
      (dolist (tag tags)
	(when (and
	       (< (length keyword) (length tag))
	       (acm-candidate-fuzzy-search keyword tag))
	  (add-to-list 'candidates (list :key tag
					 :icon "tag"
					 :label tag
					 :display-label tag
					 :annotate (capitalize "tag")
					 :backend "tag")
		       t)))
      (acm-candidate-sort-by-prefix keyword candidates))))

(defun acm-backend-tags-candidate-expand (candidate-info bound-start)
  (let* ((tag (plist-get candidate-info :label)))
    (delete-region bound-start (point))
    (insert tag)))

(provide 'acm-backend-tags)

;;; acm-backend-tags.el ends here
