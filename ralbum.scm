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
(define current-album (if (equal? (cdr (assq 'state (get-status mpd)))
                                  "play")
                        (cdr (assq 'Album (get-current-song mpd)))
                        ""))
; Current playlist
(define current-playlist (get-playlist mpd))
; List of albums in the database
(define albums (list-metadata mpd 'album))

; [random-elt l] chooses a random element in list [l]
(define random-elt
  (lambda (elts)
    (let* ([card (length elts)]
           [ind (random card)])
      (list-ref elts ind))))


; [album-to-filelist a] returns the paths of songs (relatively to mpd
; database) of album [a]
(define album-to-filelist
  (lambda (album)
    (let ([songprops (find-songs mpd 'album album)])
      (map (lambda (sp) (cdr (assq 'file sp))) songprops))))

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
; which are from the same album. Returns 'exhausted if no next album
(define skip-album
  (lambda (album songprops)
    (letrec ([loop (lambda (rsgps)
                     (if (null? rsgps)
                       'exhausted
                       (let ([calb (cdr (assq 'Album (car rsgps)))])
                         (if (equal? calb album)
                           (loop (cdr rsgps))
                           rsgps))))])
      (loop songprops))))

; [tonext-album a s] removes all songs from alist [s] appearing before
; the album [a] and all songs of album [a].
(define tonext-album
  (lambda (album songprops)
    (skip-album album (clear-ante-album album songprops))))

; [count-albums s] counts albums in alist of songs [s]
(define count-albums
  (lambda (songprops)
    (letrec ([loop
               (lambda (rsprops lalb cnt)
                 (if (null? rsprops)
                   cnt
                   (let ([calb (cdr (assq 'Album (car rsprops)))])
                     (if (equal? calb lalb)
                       (loop (cdr rsprops) lalb cnt)
                       (loop (cdr rsprops) calb (+ cnt 1))))))])
      (loop songprops current-album 1)))) ; 1 to count current album

; [enqueue-songpaths s] enequeues songs in [s] into the playlist. [s] contains
; the paths relative to mpd database
(define enqueue-songpaths!
  (lambda (sgps)
    (if (null? sgps)
      '()
      (begin (add-song! mpd (car sgps)) (enqueue-songpaths! (cdr sgps))))))

; [enqueue-random-album! m] adds a random album to the playlist of server [m]
(define enqueue-random-album!
  (lambda (conn)
    (enqueue-songpaths! (album-to-filelist (random-elt albums)))))

; [next-album! c] plays the first track in [tonext-album
; ((album playing now) (playlist))] of server [c]
(define next-album!
  (lambda (conn)
    (let ([next-songs (tonext-album current-album current-playlist)])
      (if (eq? next-songs 'exhausted)
        (begin (enqueue-random-album! conn) ; If playlist exhausted, add album
               (let* ([nnext-songs (tonext-album current-album
                                                  (get-playlist conn))]
                      [next-id (cdr (assq 'Id (car nnext-songs)))])
                 (play! conn next-id)))
                 ;(display (cdr (assq 'Title (car nnext-songs))))))
        (let ((next-id (cdr (assq 'Id (car next-songs)))))
          (play! conn next-id))))))


; Clear playlist if play state is stop. Happens when all playlist has been
; read (avoid going back to the top).
(if (eq? (cdr (assq 'state (get-status mpd))) 'stop)
  (clear-playlist! mpd)
  '())

; Looks for options and triggers actions
(cond ((pair? (assq 'add clopts))
       (let* ([chosen (random-elt albums)]
              [songs (album-to-filelist chosen)])
         (begin
           (if (pair? (assq 'verbose clopts))
             (begin
               (display "Adding album: ")
               (display chosen)
               (newline)))
           (enqueue-songpaths! songs)
           (if (equal? (cdr (assq 'state (get-status mpd))) 'stop)
             (play! mpd)))))
      ((pair? (assq 'next clopts))
       (next-album! mpd))
      ((pair? (assq 'refill clopts))
       (if (<= (count-albums (clear-ante-album current-album current-playlist))
               1)
         (enqueue-songpaths! (album-to-filelist (random-elt albums)))
         '()))
      (else (begin
              (display "ralbum")
              (newline)
              (display (usage clgrammar)))))
