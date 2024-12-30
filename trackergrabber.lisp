(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:lisp-magick-wand :alexandria :com.inuoe.jzon :dexador :lparallel) :silent t))

(cffi:defcfun "MagickReadImageBlob" :boolean
  (wand :pointer)
  (blob :pointer)
  (length :int))

(cffi:defcfun "MagickCompareImages" :pointer
  (wand :pointer)
  (reference :pointer)
  (type :int)
  (distortion (:pointer :double)))

(defun compare-images (wand1 wand2)
  (let ((dist (cffi:foreign-alloc :double)))
    (let ((new-wand (magickcompareimages wand1 wand2 1 dist)))
      (prog1 (cffi:mem-ref dist :double)
        (magick:destroy-magick-wand new-wand)
        (cffi:foreign-free dist)))))

(defun no-extension (path)
  (let ((str (file-namestring path)))
    (subseq str 0 (- (length str) 4))))

(defun load-dir (dir)
  (loop for file in (uiop:directory-files dir)
        for nsfile = (namestring file)
        when (search "png" nsfile)
          collect
          (let ((wand (magick:new-magick-wand)))
            (magick:read-image wand nsfile)
            (thresh wand 0.5)
            (cons wand (no-extension file)))))

(defparameter *fixedsys* (load-dir "/home/npfaro/Desktop/ss/imgs/fixedsys/"))
(defparameter *inst-num* (load-dir "/home/npfaro/Desktop/ss/imgs/inst-num/"))
(defparameter *note-num* (load-dir "/home/npfaro/Desktop/ss/imgs/note-num/"))
(defparameter *note* (load-dir "/home/npfaro/Desktop/ss/imgs/note/"))
(defparameter *vol* (load-dir "/home/npfaro/Desktop/ss/imgs/vol/"))


(defun bytes->int (b1 b2 b3 b4)
  (logior
   b1
   (ash b2 8)
   (ash b3 16)
   (ash b4 24)))

;; row num: 0 751 42 23
;; note: 44 751 91 23
;; instr: 132 751 48 23
;; vol?: 175 751 32 23
;; fx: 205 751 61 23

;; order num: 28 56 46 26

;; +228 is col width

(defparameter *note-rect* '(91 23 44 751))
(defparameter *instr-rect* '(48 23 132 751))
(defparameter *vol-rect* '(32 23 175 751))
(defparameter *fx-rect* '(61 23 205 751))

(defun wand-png (wand)
  (magick:set-option wand "png:color-type" "2")
  (magick:set-image-format wand "PNG")
  ;; (magick:write-image wand (format nil "/tmp/~a-~a.png" name num))
  (magick:get-image-blob wand))

(defun thresh (wand threshold)
  (magick:threshold-image wand (* threshold (nth-value 1 (magick:get-quantum-range)))))

(defun do-image (wand num)
  (let* ((wands nil)
         (imgs
           (loop for (w h x y) in (list *note-rect* *instr-rect* *vol-rect* *fx-rect*)
                 for name in '(note instr vol fx)
                 for threshold in '(0.50 0.53 0.46 0.55)
                 collect
                 (let ((wand (magick:clone-magick-wand wand)))
                   (push wand wands)
                   (magick:crop-image wand w h (+ x (* 228 num)) y)
                   (thresh wand threshold)
                   (wand-png wand)))))
    (mapcar #'magick:destroy-magick-wand wands)
    (mapcar #'funcall
            '(transcribe-note transcribe-inst transcribe-vol transcribe-fx)
            imgs)))

(defun do-whole-image (wand)
  (loop for i from 0 to 7
        collect (do-image wand i)))

(defun grab-crop (wand x y w h)
  (let ((new-wand (magick:clone-magick-wand wand)))
    (magick:crop-image new-wand w h x y)
    new-wand))

(defmacro with-crop (old name rect &body body)
  `(let ((,name (grab-crop ,old ,@rect)))
     ,@body
     (magick:destroy-magick-wand ,name)))

(defmacro with-crops (crops &body body)
  (cond
    ((null crops) `(progn ,@body))
    (t `(with-crop ,@(car crops)
            (with-crops ,(cdr crops) ,@body)))))

(defun process-wand (wand)
  (magick:with-pixel-wand (pw :string "white")
    (thresh wand 0.5)
    (magick:negate-image wand nil)
    (magick:border-image wand pw 255 255)))

(defun run (stream)
  (let (prev-wand)
    (loop for x from 0 to 15 do
      (let ((buf (make-array 6 :element-type '(unsigned-byte 8))))
        (read-sequence buf stream)
        (let ((data-size (apply #'bytes->int (cddr (coerce buf 'list)))))
          (when (zerop data-size)
            (return-from run))
          (setf buf (adjust-array buf data-size))
          (read-sequence buf stream :start 6 :end data-size)

          (magick:with-magick-wand (wand)
            (cffi:with-pointer-to-vector-data (ptr buf)
              (magickreadimageblob wand ptr (length buf)))

            (let ((cropped-wand (grab-crop wand 0 444 1920 636)))
              (cond
                ((or (not prev-wand) (> (compare-images cropped-wand prev-wand) 500000))
                 (with-crops ((wand order-num-wand (28 56 46 26))
                              (wand row-num-wand (0 751 42 23)))
                   (process-wand order-num-wand)
                   (process-wand row-num-wand)
                   (let ((order-num (transcribe-number (wand-png order-num-wand)))
                         (row-num (transcribe-number (wand-png row-num-wand))))
                     (magick:write-image order-num-wand "/tmp/ordnum.png")
                     (magick:write-image row-num-wand "/tmp/rownum.png")
                     (format t "order-num: ~a; row-num: ~a~%" order-num row-num)
                     (format t "~a~%" (do-whole-image wand)))))
                (t (format t "Skipping ~a~%" x)))
              (when prev-wand
                (magick:destroy-magick-wand prev-wand))
              (setf prev-wand cropped-wand))))))))

(defun drive ()
  (let ((process (uiop:launch-program
                  '("ffmpeg" "-i" "/home/npfaro/Desktop/ss/short.mp4"
                    "-f" "image2pipe" "-vcodec" "bmp" "-")
                  :output :stream)))
    (unwind-protect
         (let ((output-stream (uiop:process-info-output process)))
           (run output-stream))
      (uiop:close-streams process))))
