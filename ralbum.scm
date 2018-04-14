(require-extension mpd-client)
(require-extension getopt-long)

; Command line options
(define clgrammar
  '((add "Add an album"
	 (single-char #\a)
	 (required #f)
	 (value #f))
    (verbose "Verbose mode"
	     (required #f)
	     (single-char #\v)
	     (value #f))
    ))
(define clopts (getopt-long (argv) clgrammar))

; define mpd as the connection
(define mpd (connect))

; List of albums in the database
(define albums (list-metadata mpd 'album))

; [choose-album a] chooses randomly an album among those in [a]
(define choose-album
  (lambda (albums)
    (let* ([card (length albums)]
	   [ind (random card)])
      (list-ref albums ind))))

; [album-to-filelist a] returns the paths of songs (relatively to mpd
; database) of album [a]
(define album-to-filelist
  (lambda (album)
    (let ([songprops (find-songs mpd 'album album)])
      (map (lambda (sp) (cdr (assq 'file sp))) songprops))))

; [enqueue-songpaths s] enequeues songs in [s] into the playlist. [s] contains
; the paths relative to mpd database
(define enqueue-songpaths
  (lambda (sgps)
    (if (null? sgps)
      '()
      (begin (add-song! mpd (car sgps)) (enqueue-songpaths (cdr sgps))))))

(if (pair? (assq 'add clopts))
  (let* ([chosen (choose-album albums)]
	 [songs (album-to-filelist chosen)])
      (begin
	 (if (pair? (assq 'verbose clopts))
	   (begin
	     (display "Adding album: ")
	     (display chosen)
	     (newline))
	 '())
	 (enqueue-songpaths songs)))
  '())
