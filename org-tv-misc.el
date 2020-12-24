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


(defun org-tv-get-element-from-list (list field-to-compare search-string)
  "Compare SEARCH-STRING with each FIELD-TO-COMPARE element from each
list item."
  (when list '() list)
  (if (string= (assoc-default field-to-compare (car list)) search-string)
      (car list)
    (org-tv-get-element (cdr list) field-to-compare search-string)))


(defun org-tv-choose-item (choice item-list field-to-display)
  (let* ((choices
          (mapcar (lambda (x) (assoc-default field-to-display x)) item-list))
         (selection (ivy-read "Choose: " choices)))
    (org-tv-get-element-from-list item-list field-to-display selection)))


;; TODO show error message if TMDB_API_KEY is empty
(defun org-tv-get-from-id (choice id)
  "Fetch data from movie or series from given ID."
  (let* ((query-url (org-tv-get-query-url
					 (org-tv-get-id-url choice id)
					 (list (cons "api_key" (getenv "TMDB_API_KEY"))))))
	(org-tv-retrieve-json query-url)))
