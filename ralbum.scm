(require-extension mpd-client)

(define mpd (connect))

(define albums (list-metadata mpd 'album))

(define get-songs-of-album
  (lambda (album)
    (list-metadata mpd 'song 'album album)))

(define choose-album
  (lambda (albums)
    (let* ([card (length albums)]
	   [ind (random card)])
      (list-ref albums ind))))

(define add-album-plst
  (lambda (album)
    (let ([songs (get-songs-of-album album)])
      letrec ([loop
		(lambda (rsongs)
		  (if (null? rsongs)
		    #t
		    (begin (add-song mpd (car rsongs)) (loop (cdr rsongs)))))])
      loop songs)))
(display "chosen album: ")
(display (choose-album albums))
(newline)
