(require-extension mpd-client)
(require-extension getopt-long)

; Command line options
(define clgrammar
  '((add "Add an album"
         (single-char #\a)
         (required #f)
         (value #f))
    (next "Play next album in the playlist"
          (single-char #\n)
          (required #f)
          (value #f))
    (refill "Adds an album if playlist nearly exhausted"
            (single-char #\r)
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
; Currently playing album
(define current-album (cdr (assq 'Album (get-current-song mpd))))
; Current playlist
(define current-playlist (get-playlist mpd))

; [choose-album a] chooses randomly an album among those in [a]
(define choose-album
  (lambda (albums)
    (let* ([card (length albums)]
           [ind (random card)])
      (list-ref albums ind))))

; List of albums in the database
(define albums (list-metadata mpd 'album))

; [album-to-filelist a] returns the paths of songs (relatively to mpd
; database) of album [a]
(define album-to-filelist
  (lambda (album)
    (let ([songprops (find-songs mpd 'album album)])
      (map (lambda (sp) (cdr (assq 'file sp))) songprops))))

; [enqueue-songpaths s] enequeues songs in [s] into the playlist. [s] contains
; the paths relative to mpd database
(define enqueue-songpaths!
  (lambda (sgps)
    (if (null? sgps)
      '()
      (begin (add-song! mpd (car sgps)) (enqueue-songpaths! (cdr sgps))))))

; [clear-ante-album a s] removes all songs from songprops [s] before the first
; song of album [a]
(define clear-ante-album
  (lambda (album songprops)
    (letrec ([loop (lambda (rsgps)
                     (let ([calb (cdr (assq 'Album (car rsgps)))])
                              (if (equal? calb album)
                                rsgps
                                (loop (cdr rsgps)))))])
      (loop songprops))))

; [skip-album a s] removes all songs from [s] following the first one and
; which are from the same album
(define skip-album
  (lambda (album songprops)
    (letrec ([loop (lambda (rsgps)
                     (let ([calb (cdr (assq 'Album (car rsgps)))])
                       (if (equal? calb album)
                         (loop (cdr rsgps))
                         rsgps)))])
      (loop songprops))))

; [tonext-album a s] removes all songs from alist [s] appearing before
; the album [a] and all songs of album [a].
(define tonext-album
  (lambda (album songprops)
    (skip-album album (clear-ante-album album songprops))))

; [next-album! '()] plays the first track in
; [tonext-album ((album playing now) (playlist))]
(define next-album!
  (lambda (conn)
    (let* ([rsongprops (tonext-album current-album current-playlist)]
           [next-id (cdr (assq 'Id (car rsongprops)))])
      (play-song! conn next-id))))

; Clear playlist if play state is stop. Happens when all playlist has been
; read (avoid going back to the top).
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
           (enqueue-songpaths! songs))))
      ((pair? (assq 'next clopts))
       (next-album! mpd))
      (else (display (usage clgrammar))))
