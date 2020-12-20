# Org TV

Org TV is a set of functions to manage your TV Shows and Movies watch list.

## Requirements
Org TV needs my [https://github.com/Murgalha/json-parser-el](JSON Parser), since it relies on having a JSON as a key/value dotted pair on elisp.
It also needs a valid [https://www.themoviedb.org](TMDb) API Key on the environment variable `TMDB_API_KEY`.

## Usage
Org Tv uses 2 main functions, `org-tv-add-movie` and `org-tv-add-series`, they provide a search and select interface to add something to your watchlist.

### Movies
Movies are added to `~/org/movies.org` as a checklist item inside the gender header for the specific movie being add.

### Series
Series are added to `~/org/series/<series-name>.org`. Each series has its own file, containing a header for each season and a checklist item for each episode.
