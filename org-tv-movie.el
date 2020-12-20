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


(defun org-tv-add-movie ()
  (interactive)
  (org-tv-write-movie
   (org-tv-get-from-id
	"movie"
	(car (reverse
		  (org-tv-choose-item
		   "movie"
		   (assoc-default "results" (org-tv-search "movie"))))))))
