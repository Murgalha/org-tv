(defun org-tv-write-movie (movie-data)
  (let* ((year (substring (assoc-default "release_date" movie-data) 0 4))
		 (name (assoc-default "original_title" movie-data))
		 (first-genre (car (assoc-default "genres" movie-data)))
		 (genre (assoc-default "name" first-genre)))
	(with-current-buffer (find-file-noselect "~/org/movies.org")
	  (beginning-of-buffer)
	  (if (not (search-forward genre nil t))
		  (progn
			(end-of-buffer)
			(org-insert-heading)
			(insert (concat genre "\n"))))
	  (org-narrow-to-subtree)
	  (end-of-buffer)
	  (insert (format "- [ ] %s (%s)\n" name year))
	  (widen)
	  (beginning-of-buffer)
	  (save-buffer)
	  (message "%s added!" name))))


(defun org-tv-set-movie-display-name (choice item-list)
  (mapcar (lambda (x)
            (let* ((title (assoc-default "original_title" x))
                   (id (assoc-default "id" x))
                   (date (assoc-default "release_date" x))
                   (year (if (= (length date) 0)
                             "unknown" (substring date 0 4)))
					         (string (format "%s (%s) - ID:%s" title year id)))
              (append x (list (cons "display_name" string)))))
          item-list))


(defun org-tv-add-movie ()
  (interactive)
  (let* ((choice "movie")
         (results (assoc-default "results" (org-tv-search choice)))
         (movie-summary
          (org-tv-choose-item
           choice (org-tv-set-movie-display-name choice results)
           "display_name")))
    (org-tv-write-movie
     (org-tv-get-from-id choice (assoc-default "id" movie-summary)))))
