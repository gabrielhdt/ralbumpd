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

; [behead-songprop a s] removes all songs from alist [s] appearing before
; the album [a] and all songs of album [a].
(define behead-songprop
  (lambda (album songprops)
    (letrec ([clear-ante-album
	       (lambda (rsps)
		 (if (eq? (assq 'album (car rsps)) album)
		   (clear-ante-album (cdr rsps))
		   rsps))]
	     [skip-album
	       (lambda (rsps)
		 (if (eq? (assq 'album (car rsps)) album)
		   (skip-album (cdr rsps))
		   rsps))])
      (skip-album (clear-ante-album songprops)))))

; [next-album! '()] plays the first track in
; [behead-songprop ((album playing now) (playlist))]
(define next-album!
  (lambda (conn)
    (let* ([current-album (cdr (assq 'album (get-current-song conn)))]
	   [rsongprops (behead-songprop (current-album (get-playlist conn)))]
	   [next-id (cdr (assq 'id (car rsongprops)))])
      (play-song! conn next-id))))
      


; Clear playlist if play state is stop. Happens when all playlist has been
; read (avoid going back to the top.
(if (eq? (cdr (assq 'state (get-status mpd))) 'stop)
  (clear-playlist! mpd)
  '())

; Looks for options and triggers actions
(cond ((pair? (assq 'add clopts))
       (let* ([chosen (choose-album albums)]
	      [songs (album-to-filelist chosen)])
	 (begin
	   (if (pair? (assq 'verbose clopts))
	   (begin
	     (display "Adding album: ")
	     (display chosen)
	     (newline)))
	   (enqueue-songpaths songs))))
      ((pair? (assq 'next clopts))
       (next-album! '()))
      (else (display "No arg given")))
