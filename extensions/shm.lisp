;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: XLIB; -*-

(in-package :xlib)

(export '(shm-seg-error
          shm-query-version
          shm-attach
          shm-detach
          shm-put-image
          shm-get-image
          shm-array-8
          shm-array-16
          shm-array-32
          shm-array
          image-shm
          image-shm-format
          image-shm-bits-per-pixel
          image-shm-bit-lsb-first-p
          image-shm-byte-lsb-first-p
          image-shm-unit
          image-shm-pad
          image-shm-bytes-per-line
          image-shm-data
          image-shm-seg
          create-shm-image
          destroy-shm-image
          put-shm-image
          get-shm-image))

(pushnew :clx-ext-shm *features*)

(define-extension "MIT-SHM"
  :events (:shm-completion)
  :errors (shm-seg-error))

(defconstant +shm-query-version+ 0)
(defconstant +shm-attach+ 1)
(defconstant +shm-detach+ 2)
(defconstant +shm-put-image+ 3)
(defconstant +shm-get-image+ 4)
(defconstant +shm-create-pixmap+ 5)

(def-clx-class (shm-seg (:copier nil))
  (id 0 :type resource-id)
  (display nil :type (or null display))
  (shmid 0 :type card32)
  (addr #+sbcl (sb-sys:int-sap 0) #-sbcl 0
        :type #+sbcl sb-sys:system-area-pointer #-sbcl integer)
  (read-only nil :type boolean))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-accessor shm-seg (32)
    ((index) `(resource-id-get ,index))
    ((index thing) `(resource-id-put ,index (shm-seg-id ,thing)))))

(declare-event :shm-completion
  (card16 sequence)
  (drawable (drawable event-window))
  (card16 minor)
  (card8 major)
  (pad8 1)
  (shm-seg seg)
  (card32 offset))

(define-condition shm-seg-error (resource-error) ())
(define-error shm-seg-error decode-resource-error)

(defun shm-query-version (display)
  "Query the X server for MIT-SHM support and return a shared pixmaps flag,
major version, minor version, uid, gid, and shared pixmap format."
  (with-buffer-request-and-reply (display (extension-opcode display "MIT-SHM")
                                  nil :sizes (8 16))
    ((data +shm-query-version+))
    (values
     (boolean-get 0)
     (card16-get 8)
     (card16-get 10)
     (card16-get 12)
     (card16-get 14)
     (member8-get 16 :bitmap :xy-pixmap :z-pixmap))))

(defun shm-attach (shm-seg &key (read-only nil read-only-p))
  (let ((display (shm-seg-display shm-seg)))
    (when read-only-p (setf (shm-seg-read-only shm-seg) read-only))
    (setf (shm-seg-id shm-seg) (allocate-resource-id display shm-seg 'shm-seg))
    (with-buffer-request (display (extension-opcode display "MIT-SHM"))
      (data +shm-attach+)
      (shm-seg shm-seg)
      (card32 (shm-seg-shmid shm-seg))
      (boolean (shm-seg-read-only shm-seg)))))

(defun shm-detach (shm-seg)
  (let ((display (shm-seg-display shm-seg)))
    (with-buffer-request (display (extension-opcode display "MIT-SHM"))
      (data +shm-detach+)
      (shm-seg shm-seg))))

(defun shm-put-image (drawable gcontext shm-seg &key
                      src-x src-y x y width height depth format
                      total-width total-height
                      (offset 0) send-event)
  (let ((display (shm-seg-display shm-seg)))
    (with-buffer-request (display (extension-opcode display "MIT-SHM")
                                  :gc-force gcontext)
      (data +shm-put-image+)
      (drawable drawable)
      (gcontext gcontext)
      (card16 total-width total-height src-x src-y width height)
      (int16 x y)
      (card8 depth)
      ((member8 :bitmap :xy-pixmap :z-pixmap) format)
      (boolean send-event)
      (pad8 nil)
      (shm-seg shm-seg)
      (card32 offset))))

(defun shm-get-image (drawable shm-seg &key x y width height plane-mask
                      (format :z-pixmap) (offset 0))
  (let ((display (shm-seg-display shm-seg)))
    (with-buffer-request-and-reply (display (extension-opcode display "MIT-SHM")
                                    nil :sizes (8 32))
         ((data +shm-get-image+)
          (drawable drawable)
          (int16 x y)
          (card16 width height)
          (card32 plane-mask)
          ((member8 :bitmap :xy-pixmap :z-pixmap) format)
          (pad8 nil)
          (pad8 nil)
          (pad8 nil)
          (shm-seg shm-seg)
          (card32 offset))
      (values
       (card8-get 1) ; depth
       (visual-info display (resource-id-get 8)) ; visual
       (card32-get 12))))) ; size

;;; Shared memory images.

(deftype shm-array-8 ()
  #+sbcl '(sb-alien:alien (array (sb-alien:unsigned 8) nil)))

(deftype shm-array-16 ()
  #+sbcl '(sb-alien:alien (array (sb-alien:unsigned 16) nil)))

(deftype shm-array-32 ()
  #+sbcl '(sb-alien:alien (array (sb-alien:unsigned 32) nil)))

(deftype shm-array ()
  '(or shm-array-8 shm-array-16 shm-array-32))

#+sbcl
(defun make-shm-array (addr size n-bits)
  (declare (ignore size))
  (ecase n-bits
    ((1 4 8) (sb-alien:sap-alien addr (array (sb-alien:unsigned 8) nil)))
    (16 (sb-alien:sap-alien addr (array (sb-alien:unsigned 16) nil)))
    ((24 32) (sb-alien:sap-alien addr (array (sb-alien:unsigned 32) nil)))))

(declaim (type shm-array-8 *empty-shm-array*))
(defvar *empty-shm-array*
  #+sbcl (make-shm-array (sb-sys:int-sap 0) 0 8))

(def-clx-class (image-shm (:include image) (:copier nil)
                          (:print-function print-image))
  (format :z-pixmap :type (member :bitmap :xy-pixmap :z-pixmap))
  (bits-per-pixel 1 :type (member 1 4 8 16 24 32))
  (bit-lsb-first-p +image-bit-lsb-first-p+ :type generalized-boolean)
  (byte-lsb-first-p +image-byte-lsb-first-p+ :type generalized-boolean)
  (unit +image-unit+ :type (member 8 16 32))
  (pad +image-pad+ :type (member 8 16 32))
  (bytes-per-line 0 :type card16)
  (data *empty-shm-array* :type shm-array)
  (seg nil :type (or null shm-seg)))

(defun create-shm-image (display &key
                         (width (required-arg width))
                         (height (required-arg height))
                         (depth (required-arg depth))
                         (format :z-pixmap)
                         plist name x-hot y-hot
                         red-mask blue-mask green-mask
                         (mode #o600) read-only (attach t) (x-attach t))
  "Create and (by default) attach to a shared-memory image segment."
  (let ((bitmap-format (display-bitmap-format display)))
    (multiple-value-bind (pad bits-per-pixel)
        (ecase format
          ((:bitmap :xy-pixmap)
           (values (bitmap-format-pad bitmap-format) 1))
          (:z-pixmap
           (if (= depth 1)
               (values (bitmap-format-pad bitmap-format) 1)
               (let ((pixmap-format
                      (find depth (display-pixmap-formats display)
                            :key #'pixmap-format-depth)))
                 (declare (type (or null pixmap-format) pixmap-format))
                 (unless pixmap-format
                   (error "No server pixmap format with depth ~D." depth))
                 (values (pixmap-format-scanline-pad pixmap-format)
                         (pixmap-format-bits-per-pixel pixmap-format))))))
      (let* ((bytes-per-line
              (let* ((bits-per-line (index* width bits-per-pixel))
                     (padded-bits-per-line
                      (index* (index-ceiling bits-per-line pad) pad)))
                (index-ceiling padded-bits-per-line 8)))
             (image-size (* bytes-per-line height))
             (shmid
              #+sbcl (sb-posix:shmget sb-posix:ipc-private image-size mode)
              #-sbcl (error "Don't know how to create SHM segment."))
             (addr
              (typecase attach
                ((eql t)
                 #+sbcl (sb-posix:shmat shmid nil 0)
                 #-sbcl (error "Don't know how to attach SHM segment."))
                (integer attach)))
             (shm-seg
              (make-shm-seg :display display
                            :shmid shmid
                            :addr (or addr
                                      #+sbcl (sb-sys:int-sap 0)
                                      #-sbcl nil)
                            :read-only read-only))
             (image
              (make-image-shm
                :width width :height height :depth depth :plist plist
                :format format
                :bits-per-pixel bits-per-pixel
                :bit-lsb-first-p (bitmap-format-lsb-first-p bitmap-format)
                :byte-lsb-first-p (display-image-lsb-first-p display)
                :unit (bitmap-format-unit bitmap-format)
                :pad pad
                :bytes-per-line bytes-per-line
                :data (if addr
                          (make-shm-array addr image-size bits-per-pixel)
                          *empty-shm-array*)
                :seg (prog1 shm-seg
                       (when x-attach
                         (shm-attach shm-seg :read-only read-only))))))
        (declare (type image image))
        (when name (setf (image-name image) name))
        (when x-hot (setf (image-x-hot image) x-hot))
        (when y-hot (setf (image-y-hot image) y-hot))
        (when red-mask (setf (image-red-mask image) red-mask))
        (when blue-mask (setf (image-blue-mask image) blue-mask))
        (when green-mask (setf (image-green-mask image) green-mask))
        image))))

(defun destroy-shm-image (image &key (detach t) (x-detach t) (delete t))
  (declare (type image image))
  (let ((shm-seg (image-shm-seg image)))
    (when x-detach (shm-detach shm-seg))
    (when detach
      #+sbcl (sb-posix:shmdt (shm-seg-addr shm-seg))
      #-sbcl (error "Don't know how to detach SHM segment."))
    (when delete
      #+sbcl (sb-posix:shmctl (shm-seg-shmid shm-seg) sb-posix:ipc-rmid)
      #-sbcl (error "Don't know how to delete SHM segment."))))

(defun put-shm-image (drawable gcontext image &key
                      (src-x 0) (src-y 0)
                      (x (required-arg x)) (y (required-arg y))
                      (width (image-width image))
                      (height (image-height image))
                      (offset 0)
                      send-event)
  (shm-put-image drawable gcontext (image-shm-seg image)
                 :src-x src-x :src-y src-y :x x :y y
                 :width width :height height
                 :total-width (image-width image)
                 :total-height (image-height image)
                 :depth (image-depth image)
                 :format (image-shm-format image)
                 :offset offset
                 :send-event send-event))

(defun get-shm-image (drawable image &key
                      (x (required-arg x)) (y (required-arg y))
                      (width (image-width image))
                      (height (image-height image))
                      (plane-mask #xffffffff)
                      (offset 0))
  (multiple-value-bind (depth visual-info size)
      (shm-get-image drawable (image-shm-seg image)
                     :x x :y y :width width :height height
                     :plane-mask plane-mask
                     :format (image-shm-format image)
                     :offset offset)
    (declare (ignore size))
    (assert (= depth (image-depth image)))
    (when visual-info
      (unless (zerop (visual-info-red-mask visual-info))
        (setf (image-red-mask image) (visual-info-red-mask visual-info)))
      (unless (zerop (visual-info-green-mask visual-info))
        (setf (image-green-mask image) (visual-info-green-mask visual-info)))
      (unless (zerop (visual-info-blue-mask visual-info))
        (setf (image-blue-mask image) (visual-info-blue-mask visual-info))))
    (values image visual-info)))
