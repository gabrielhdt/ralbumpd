(require-extension mpd-client)

(define mpd (connect))

(define albums (list-metadata mpd 'album))

(define choose-album
  (lambda (albums)
    (let* ([card (length albums)]
	   [ind (random card)])
      (list-ref albums ind))))

(define album-to-filelist
  (lambda (album)
    (let ([songprops (find-songs mpd 'album album)]
	  [songprop-to-file (lambda (songprop)
			      (assv 'file songprop))])
      map songprop-to-file (songprops album))))

(define add-album-plst
  (lambda (album)
    letrec ([loop
	      (lambda (rsongs)
		(if (null? rsongs)
		  #t
		  (begin (add-song mpd (car rsongs)) (loop (cdr rsongs)))))])
    loop (album-to-filelist album)))
(display "chosen album: ")
(display (choose-album albums))
(newline)
