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


(defun org-tv-get-seasons-list (series-data)
  (let* ((input
          (ivy-read (format "Choose seasons (%s available): "
					                  (assoc-default "number_of_seasons" series-data))
                    '()))
         (tokens (split-string input "-"))
		 (first (string-to-number (car tokens)))
		 (last (string-to-number (car (last tokens)))))
	(number-sequence first last)))


(defun org-tv-set-series-display-name (choice item-list)
  (mapcar (lambda (x)
            (let* ((title (assoc-default "original_name" x))
                   (id (assoc-default "id" x))
                   (date (assoc-default "first_air_date" x))
                   (year (if (= (length date) 0)
                             "unknown" (substring date 0 4)))
					         (string (format "%s (%s) - ID:%s" title year id)))
              (append x (list (cons "display_name" string)))))
          item-list))


(defun org-tv-add-series ()
  (interactive)
  (let* ((choice "series")
         (results (assoc-default "results" (org-tv-search choice)))
         (series-summary
          (org-tv-choose-item
           choice (org-tv-set-series-display-name choice results)
           "display_name"))
         (seasons
               (org-tv-get-seasons-list
                (org-tv-get-from-id choice (assoc-default "id" series-summary))))
         (name (assoc-default "name" series-summary))
         (id (assoc-default "id" series-summary)))
    (org-tv-write-series name id seasons)))

;; TODO: Create finished directory if it does not exist
;; TODO: Make a nicer selection of series (without extensions?)
(defun org-tv-move-finished-series ()
  "Move the chosen series to the 'finished' dir."
  (interactive)
  (let* ((base-dir "~/org/series/")
        (series (directory-files base-dir nil "\\.org$"))
        (basename (ivy-read "Select finished series: " series)))
    (rename-file (concat (file-name-as-directory base-dir) basename)
                 (concat (file-name-as-directory base-dir)
                         (file-name-as-directory "finished")))))
