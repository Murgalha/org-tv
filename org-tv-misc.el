(defun org-tv-retrieve-json (url)
  "Make a GET request on given url and parse the JSON result."
  (parse-json-from-string
   (shell-command-to-string (format "curl -s '%s'" url))))


(defun org-tv-get-id-url (choice id)
  (if (string= choice "movie")
	  (format "https://api.themoviedb.org/3/%s/%s" choice id)
	(format "https://api.themoviedb.org/3/tv/%s" id)))


(defun org-tv-get-search-url (choice)
  (if (string= choice "movie")
	  "https://api.themoviedb.org/3/search/movie"
	"https://api.themoviedb.org/3/search/tv"))


(defun org-tv-get-query-url (base-url query-list)
  "Concatenates BASE-URL with key/values on given QUERY-LIST,
a list of dotted pairs (key . value)."
  (let* ((full-url (concat base-url "?")))
	(loop for pair in query-list do
		  (let* ((key (car pair))
				 (value (replace-regexp-in-string " " "+" (cdr pair))))
			(setq full-url (format "%s%s=%s&" full-url key value))))
	(substring full-url 0 -1)))


(defun org-tv-search (choice)
  "Prompt to enter the name of the choice to search on IMDb.
CHOICE must be either 'movie' or 'series'"
  (let* ((search-string
		  (read-string (format "%s to search: " (capitalize choice)))))
	(org-tv-retrieve-json
	 (org-tv-get-query-url (org-tv-get-search-url choice)
						   (list (cons "api_key" (getenv "TMDB_API_KEY"))
								 (cons "query" search-string))))))


(defun org-tv-choose-item (choice itemlist)
  (let ((choices
		 (mapcar (lambda (x)
				   (let* ((title-key (if (string= choice "movie")
										 "original_title"
									   "original_name"))
						  (title (assoc-default title-key x))
              (id (assoc-default "id" x)))
					 (format "%s - %s" title id)))
				 itemlist)))
	(let ((choice (ivy-read "Choose: " choices)))
	  (split-string choice " - "))))

;; TODO show error message if TMDB_API_KEY is empty
(defun org-tv-get-from-id (choice id)
  "Fetch data from movie or series from given ID."
  (let* ((query-url (org-tv-get-query-url
					 (org-tv-get-id-url choice id)
					 (list (cons "api_key" (getenv "TMDB_API_KEY"))))))
	(org-tv-retrieve-json query-url)))
