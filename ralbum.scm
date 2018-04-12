(require-extension mpd-client)

(define mpd (connect))

(define albums (list-metadata mpd 'album))

(define choose-album
  (lambda (albums)
    (let* ([card (length albums)]
	   [ind (random card)])
      (list-ref albums ind))))
(display "chosen album: ")
(display (choose-album albums))
(newline)
