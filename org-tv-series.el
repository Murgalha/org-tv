(defun org-tv-get-season-url (id season)
  (format "https://api.themoviedb.org/3/tv/%s/season/%s" id season))


(defun org-tv-write-season (series-name season-data)
  (let* ((episodes (assoc-default "episodes" season-data))
		 (season-number (assoc-default "season_number" season-data))
		 (formatted-name (replace-regexp-in-string " " "-" series-name))
		 (filename (format "~/org/series/%s.org" (downcase formatted-name))))
	(with-current-buffer (find-file-noselect filename)
	  (end-of-buffer)
	  (insert "\n\n")
	  (org-insert-heading-respect-content)
	  (insert (format "Season %s [/]\n" season-number))
	  (dolist (episode episodes)
		(let* ((name (assoc-default "name" episode))
			   (number (assoc-default "episode_number" episode)))
		  (insert (format "- [ ] Episode %s - %s\n" number name))))
	  (save-buffer))))


(defun org-tv-write-series (name id seasons-list)
  (dolist (season seasons-list)
	(let* ((full-url (org-tv-get-query-url
					  (org-tv-get-season-url id season)
					  (list (cons "api_key" (getenv "TMDB_API_KEY")))))
		   (season-data (org-tv-retrieve-json full-url)))
	  (org-tv-write-season name season-data)))
  (message "%s added!" name))


(defun org-tv-get-seasons-list (input)
  (let* ((tokens (split-string input "-"))
		 (first (string-to-number (car tokens)))
		 (last (string-to-number (car (last tokens)))))
	(number-sequence first last)))


(defun org-tv-choose-seasons (series-data)
  (print series-data)
  (org-tv-get-seasons-list
   (ivy-read (format "Choose seasons (%s available): "
					 (assoc-default "number_of_seasons" series-data))
			 '())))

(defun org-tv-add-series ()
  (interactive)
  (let* ((data (org-tv-choose-item
				"series"
				(assoc-default "results" (org-tv-search "series"))))
		 (name (car data))
		 (id (car (reverse data))))
	(print data)
	(print name)
	(print id)
	(org-tv-write-series name id (org-tv-choose-seasons
							 (org-tv-get-from-id "series" id)))))
