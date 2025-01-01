(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:lisp-magick-wand :alexandria :serapeum :lparallel) :silent t))

(cffi:defcfun "MagickReadImageBlob" :boolean
  (wand :pointer)
  (blob :pointer)
  (length :int))

(cffi:defcfun "MagickCompareImages" :pointer
  (wand :pointer)
  (reference :pointer)
  (type :int)
  (distortion (:pointer :double)))

(defun show (wand)
  (let ((name (format nil "/dev/shm/~a.png" (gensym))))
    (magick:write-image wand name)
    (swank:eval-in-emacs `(slime-media-insert-image (create-image ,name) ,name))
    wand))

(defun thresh (wand threshold)
  (magick:threshold-image wand (* threshold (nth-value 1 (magick:get-quantum-range))))
  wand)

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

(defparameter *fixedsys-imgs* (load-dir "/home/npfaro/Desktop/ss/imgs/fixedsys/"))
(defparameter *inst-num-imgs* (load-dir "/home/npfaro/Desktop/ss/imgs/inst-num/"))
(defparameter *note-num-imgs* (load-dir "/home/npfaro/Desktop/ss/imgs/note-num/"))
(defparameter *note-imgs* (load-dir "/home/npfaro/Desktop/ss/imgs/note/"))
(defparameter *vol-imgs* (load-dir "/home/npfaro/Desktop/ss/imgs/vol/"))
(defparameter *sharp-imgs* (load-dir "/home/npfaro/Desktop/ss/imgs/sharp/"))


(defun bytes->int (b1 b2 b3 b4)
  (logior
   b1
   (ash b2 8)
   (ash b3 16)
   (ash b4 24)))

(defparameter *col-offset* 95)
(defparameter *row-offset* 13)

(defparameter *order-num* '((*fixedsys-imgs* 13 33 8 11)
                            (*fixedsys-imgs* 21 33 8 11)))

(defparameter *row-num* '((*inst-num-imgs* 0 419 7 8)
                          (*inst-num-imgs* 8 419 7 8)))

(defparameter *note* '((*note-imgs* 22 419 8 8)
                       (*sharp-imgs* 34 419 8 9)
                       (*note-num-imgs* 46 419 8 8)))

(defparameter *inst* '((*inst-num-imgs* 57 419 7 8)
                       (*inst-num-imgs* 65 419 7 8)))

(defparameter *vol*' ((*vol-imgs* 76 420 3 7)
                      (*vol-imgs* 80 420 3 7)))

(defparameter *fx* '((*inst-num-imgs* 86 419 7 8)
                     (*inst-num-imgs* 94 419 7 8)
                     (*inst-num-imgs* 102 419 7 8)))

(defparameter *song-data* (make-hash-table))

(defun store-row (order-num row-num row)
  (let ((pattern (alexandria:ensure-gethash order-num *song-data* (make-array #x5F :initial-element nil))))
    (push row (aref pattern row-num))))

(defun read-row (order-num row-num)
  (let* ((data (aref (gethash order-num *song-data*) row-num))
         (assorted (serapeum:assort data :test #'equal)))
    (car (alexandria:extremum assorted #'> :key #'length))))

(defun read-all-rows (order-num row-num)
  (aref (gethash order-num *song-data*) row-num))

(defun grab-crop (wand x y w h)
  (magick:get-image-region wand w h x y))

(defmacro with-crop (old name rect &body body)
  `(let ((,name (grab-crop ,old ,@rect)))
     (prog1 (progn ,@body)
       (magick:destroy-magick-wand ,name))))

(defmacro with-crops (crops &body body)
  (cond
    ((null crops) `(progn ,@body))
    (t `(with-crop ,@(car crops)
            (with-crops ,(cdr crops) ,@body)))))

(defun classify (src rect &key (x-offset 0) (y-offset 0))
  (apply #'concatenate 'string
         (loop for entry in rect
               collect
               (destructuring-bind (img-set x y w h) entry
                 (with-crop src crop ((+ x (* x-offset *col-offset*))
                                      (+ y (* y-offset *row-offset*))
                                      w
                                      h)
                   ;; (magick:write-image crop (format nil "/tmp/durr/~a" (gensym)))
                   (let ((img-set (symbol-value img-set)))
                     (loop for (img . name) in img-set
                           with best = nil
                           with best-val = nil do
                             ;; (magick:write-image img "/tmp/durr/img.png")
                             ;; (magick:write-image crop "/tmp/durr/crop.png")
                             ;; (break)
                             (let ((diff (compare-images img crop)))
                               ;; (format t "Trying ~a ~a~%" name diff)
                               (when (or (not best) (< diff best-val))
                                 ;; (format t "Found new best: ~a~%" name)
                                 (setf best name
                                       best-val diff)))
                           finally (return best))))))))

#+nil
(magick:with-magick-wand (wand :load "/home/npfaro/Desktop/ss/frame_048.bmp")
  (magick:resize-image wand 800 600 :point)
  (thresh wand 0.48)
  (terpri)
  (show wand)
  (print (classify wand *order-num*))
  (print (classify wand *row-num*))
  (print (classify wand *note* :x-offset 1))
  (print (classify wand *inst* :x-offset 1))
  (print (classify wand *vol* :x-offset 1))
  (print (classify wand *fx* :x-offset 1))
  )

#+nil
(progn
  (setf lparallel:*kernel* (lparallel:make-kernel 10))
  (let ((chan (lparallel.kernel:make-channel :fixed-capacity 1)))
    (loop repeat 20 do
      (lparallel:submit-task chan (lambda () 1))
      (format t "added~%"))
    (format t "done ~%")

    (sleep 1)
    (loop for result = (lparallel:try-receive-result chan :timeout 1)
          while result do
            (format t "got one!~%"))))

(defun run (stream)
  (setf lparallel:*kernel* (lparallel:make-kernel 10))
  (clrhash *song-data*)
  (loop for frame from 1
        with prev-wand
        with sent = 0
        with received = 0
        with channel = (lparallel.kernel:make-channel) do
          (loop for result = (lparallel:try-receive-result channel)
                while result do
                  (loop for data in result do
                    (apply #'store-row data))
                  (incf received))

          (let ((buf (make-array 6 :element-type '(unsigned-byte 8))))
            (read-sequence buf stream)
            (let ((data-size (apply #'bytes->int (cddr (coerce buf 'list)))))
              (when (zerop data-size)
                (format t "FFmpeg said zero at frame ~a ~a ~a~%" frame sent received)
                (loop repeat (- sent received)
                      for result = (lparallel:receive-result channel) do
                        (loop for data in result do
                          (apply #'store-row data)))
                (return-from run))
              (setf buf (adjust-array buf data-size))
              (read-sequence buf stream :start 6 :end data-size)

              (let ((wand (magick:new-magick-wand)))
                (cffi:with-pointer-to-vector-data (ptr buf)
                  (magickreadimageblob wand ptr (length buf)))
                (magick:resize-image wand 800 600 :point)
                (thresh wand 0.5)

                (let ((cropped-wand (grab-crop wand 0 248 800 352)))
                  (cond
                    ((or (not prev-wand) (> (compare-images cropped-wand prev-wand) 600))
                     (incf sent)
                     (format t "Sending ~a~%" frame)
                     (lparallel:submit-task
                      channel
                      (let ((frame frame))
                        (lambda ()
                          (let* ((order-num (parse-integer (classify wand *order-num*) :radix 16))
                                 (row-num (parse-integer (classify wand *row-num*) :radix 16)))
                            (prog1
                                (loop for y from 0 downto (- (min row-num 6))
                                      for i from 0
                                      for row-num-2 = (parse-integer (classify wand *row-num* :y-offset y) :radix 16)
                                      for data = (loop for x from 0 to 7
                                                       collect
                                                       (loop for el in (list *note* *inst* *vol* *fx*)
                                                             collect (classify wand el :x-offset x :y-offset y)))
                                      when (zerop i) do
                                        (format t "~a: ~x ~x~%" frame order-num row-num-2)
                                        ;; (format t "~x ~x |" order-num row-num-2)
                                        ;; (loop for cell in data do
                                        ;; (format t "~{~a ~}" cell))
                                        ;; (terpri)
                                      end
                                      collect (list order-num row-num-2 data))
                              (magick:destroy-magick-wand wand)))))))
                    (t (format t "Skipping ~a~%" frame)
                       (magick:destroy-magick-wand wand)))
                  (when prev-wand
                    (magick:destroy-magick-wand prev-wand))
                  (setf prev-wand cropped-wand)))))))

(defun drive ()
  (let ((process (uiop:launch-program
                  '("ffmpeg" "-i" "/home/npfaro/Desktop/ss/short.mp4"
                    ;; "-vf" "select=gte(n\,50),setpts=PTS-STARTPTS"
                    ;; "-af" "aselect=gte(n\,50),asetpts=PTS-STARTPTS"
                    "-f" "image2pipe" "-vcodec" "bmp" "-")
                  :output :stream)))
    (unwind-protect
         (let ((output-stream (uiop:process-info-output process)))
           (run output-stream))
      (uiop:close-streams process))))
