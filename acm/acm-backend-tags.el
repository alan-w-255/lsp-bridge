;;; Code:
(defun acm-backend-tags-filter-by-prefix (tag)
  (string-prefix-p (downcase keyword) (downcase tag)))

(defcustom acm-enable-tags t
  "Popup tag completions when this option is turn on."
  :type 'boolean)

(defun acm-backend-tags-candidates (keyword)
  (when (and
	 acm-enable-tags
	 tags-table-list
	 (length> keyword 1))
    (let* ((candidates (list))
	   (tags (tags-completion-table)))
      (dolist (tag tags)
	(when (acm-backend-tags-filter-by-prefix tag)
	  (add-to-list 'candidates (list :key tag
					 :icon "tag"
					 :label tag
					 :display-label tag
					 :annotate (capitalize "tag")
					 :backend "tag")
		       t)))
      candidates)))

(defun acm-backend-tags-candidate-expand (candidate-info bound-start)
  (let* ((tag (plist-get candidate-info :label)))
    (delete-region bound-start (point))
    (insert tag)))

(provide 'acm-backend-tags)

;;; acm-backend-tags.el ends here
