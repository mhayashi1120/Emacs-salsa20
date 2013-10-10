;;; salsa.el --- todo

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: https://github.com/mhayashi1120/Emacs-salsa/raw/master/salsa20.el
;; Emacs: GNU Emacs 24 or later (--with-wide-int)
;; Version: 0.0.1
;; Package-Requires: ()

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install:

;; Put this file into load-path'ed directory, and
;; !!!!!!!!!!!!!!! BYTE COMPILE IT !!!!!!!!!!!!!!!
;; And put the following expression into your .emacs.
;;
;; (require 'kaesar)
;;

;;; Commentary:

;;todo

;;; Usage:
;;todo

;;; TODO:
;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup salsa20 nil
  "todo"
  :prefix "salsa20-"
  :group 'data)


;;TODO clear secret data from function literal

;; http://cr.yp.to/snuffle/spec.pdf

(when (zerop (lsh 1 32))
  (error "TODO"))


(defconst salsa20-word-range
  (eval-when-compile
    (lsh 1 32)))

(defconst salsa20-word-max
  (eval-when-compile
    (1- salsa20-word-range)))

(defun salsa20--clone-16word (16word)
  (vector
   (vconcat (aref 16word 0))
   (vconcat (aref 16word 1))
   (vconcat (aref 16word 2))
   (vconcat (aref 16word 3))))

(defun salsa20--sum (word1 word2)
  (logand (+ word1 word2) salsa20-word-max))

(defun salsa20--xor (word1 word2)
  (logxor word1 word2))

;; 2. Words
(defun salsa20--left-shift (word shift)
  ;; shift must be `shift < 32'
  (logior
   (logand salsa20-word-max (lsh word shift))
   (logand salsa20-word-max (lsh word (- shift 32)))))

;; 3. The quarterround function (4-word -> 4-word)
(defun salsa20--quarterround (y0 y1 y2 y3)
  (let* ((z1 (salsa20--xor y1 (salsa20--left-shift (salsa20--sum y0 y3) 7)))
         (z2 (salsa20--xor y2 (salsa20--left-shift (salsa20--sum z1 y0) 9)))
         (z3 (salsa20--xor y3 (salsa20--left-shift (salsa20--sum z2 z1) 13)))
         (z0 (salsa20--xor y0 (salsa20--left-shift (salsa20--sum z3 z2) 18)))
         ;; literal vector (destructively changed)
         (res [nil nil nil nil]))
    (aset res 0 z0)
    (aset res 1 z1)
    (aset res 2 z2)
    (aset res 3 z3)
    res))

;; 4. The rowround function (16-word 4x4 -> 16-word 4x4)
(defsubst salsa20--rowround! (y)
  (let* ((y0 (aref y 0))
         (y1 (aref y 1))
         (y2 (aref y 2))
         (y3 (aref y 3))
         (generator (lambda (v j0 j1 j2 j3)
                      (salsa20--quarterround
                       (aref v j0) (aref v j1) (aref v j2) (aref v j3))))
         (setter (lambda (row v i0 i1 i2 i3)
                   (aset row 0 (aref v i0))
                   (aset row 1 (aref v i1))
                   (aset row 2 (aref v i2))
                   (aset row 3 (aref v i3)))))
    (funcall setter y0 (funcall generator y0 0 1 2 3) 0 1 2 3)
    (funcall setter y1 (funcall generator y1 1 2 3 0) 3 0 1 2)
    (funcall setter y2 (funcall generator y2 2 3 0 1) 2 3 0 1)
    (funcall setter y3 (funcall generator y3 3 0 1 2) 1 2 3 0)
    y))

;; 5. The columnround function (16-word 4x4 -> 16-word 4x4)
(defsubst salsa20--columnround! (x)
  (let* ((x0 (aref x 0))
         (x1 (aref x 1))
         (x2 (aref x 2))
         (x3 (aref x 3))
         (generator (lambda (v0 v1 v2 v3 i)
                      (salsa20--quarterround
                       (aref v0 i) (aref v1 i) (aref v2 i) (aref v3 i))))
         (setter (lambda (col v i0 i1 i2 i3)
                   (aset (aref x 0) col (aref v i0))
                   (aset (aref x 1) col (aref v i1))
                   (aset (aref x 2) col (aref v i2))
                   (aset (aref x 3) col (aref v i3)))))
    (funcall setter 0 (funcall generator x0 x1 x2 x3 0) 0 1 2 3)
    (funcall setter 1 (funcall generator x1 x2 x3 x0 1) 3 0 1 2)
    (funcall setter 2 (funcall generator x2 x3 x0 x1 2) 2 3 0 1)
    (funcall setter 3 (funcall generator x3 x0 x1 x2 3) 1 2 3 0)
    x))

;; 6. The doubleround function (16-word 4x4 -> 16-word 4x4)
(defsubst salsa20--doubleround! (x)
  (salsa20--rowround! (salsa20--columnround! x)))

;; 7. The littleendian function (4-byte -> 1-word)
(defconst salsa20--byte-table
  (eval-when-compile
    (vector (lsh ?\xff 0) (lsh ?\xff 8) (lsh ?\xff 16) (lsh ?\xff 24))))

(defun salsa20--littleendian (b0 b1 b2 b3)
  (logior
   (lsh b0 0)
   (lsh b1 8)
   (lsh b2 16)
   (lsh b3 24)))

;;TODO unibyte-text -> unibyte list to append by block algorithm.
;; 8. The Salsa20 hash function (64-byte unibyte -> 64-byte unibyte)
(defun salsa20--16word-to-bytes (16word)
  (apply 'unibyte-string
         (loop for word across 16word
               append (loop for x across word
                            append (salsa20-word-to-4bytes x)))))

(defun salsa20--read-16word (string)
  (loop with pos = 0
        ;; destructive literal vector
        with xs = [[nil nil nil nil] [nil nil nil nil] [nil nil nil nil] [nil nil nil nil]]
        for i from 0 below 4
        for xi across xs
        do (loop for j from 0 below 4
                 do (progn
                      (aset xi j (salsa20--littleendian
                                  (aref string (+ pos 0))
                                  (aref string (+ pos 1))
                                  (aref string (+ pos 2))
                                  (aref string (+ pos 3))))
                      (setq pos (+ pos 4))))
        finally return xs))

(defun salsa20-sum-16word! (words1 words2)
  (loop for ws1 across words1
        for ws2 across words2
        collect (loop for w1 across ws1
                      for w2 across ws2
                      for i from 0
                      do (aset ws1 i (salsa20--sum w1 w2)))
        finally return words1))

;;;###autoload
(defun salsa20-hash (x)
  (loop with xs = (salsa20--read-16word x)
        ;; clone working vector to preserve initial vector
        with tmp = (salsa20--clone-16word xs)
        repeat 10
        do (setq tmp (salsa20--doubleround! tmp))
        finally return
        (progn
          (salsa20-sum-16word! tmp xs)
          (salsa20--16word-to-bytes tmp))))

(defun salsa20-hash-many (x time)
  (loop repeat time
        do (setq x (salsa20-hash x))
        finally return x))

(defun salsa20-word-to-4bytes (word)
  (list (lsh (logand word (aref salsa20--byte-table 0)) 0)
        (lsh (logand word (aref salsa20--byte-table 1)) -8)
        (lsh (logand word (aref salsa20--byte-table 2)) -16)
        (lsh (logand word (aref salsa20--byte-table 3)) -24)))

;; 9. The Salsa20 expansion function
(defconst salsa20--sigma
  [
   [101 120 112 97]
   [110 100 32 51]
   [50 45 98 121]
   [116 101 32 107]])

(defconst salsa20--tau
  [
   [101 120 112 97]
   [110 100 32 49]
   [54 45 98 121]
   [116 101 32 107]])

(defun salsa20-expansion (k n)
  (unless (= (length n) 16)
    (error "TODO invalid n length 16-byte"))
  (cond
   ((= (length k) 16)
    (salsa20-hash
     (vconcat
      (aref salsa20--tau 0)
      k
      (aref salsa20--tau 1)
      n
      (aref salsa20--tau 2)
      k
      (aref salsa20--tau 3))))
   ((= (length k) 32)
    (salsa20-hash
     (vconcat
      (aref salsa20--sigma 0)
      (substring k 0 16)                ; k0
      (aref salsa20--sigma 1)
      n
      (aref salsa20--sigma 2)
      (substring k 16)                  ; k1
      (aref salsa20--sigma 3))))
   (t
    (error "Not supported key length (16 or 32)"))))
   
;; 10. The Salsa20 encryption function
;;;###autoload
(defun salsa20-encryption (k m &optional iv) 
  ;;TODO check k size 16 or 32
  (unless (or (null iv) (= (length iv) 8))
    (error "Invalid length of IV"))
  (when (multibyte-string-p m)
    (error "todo Invalid string not a unibyte string"))
  (unless iv
    (setq iv (salsa20--random-u8vector 8)))
  (let* ((i (vector 0 0 0 0 0 0 0 0))
         (n (vconcat iv i))
         (ms (string-to-list m))
         (res '()))
    (catch 'done
      (while t
        (let ((key (string-to-list (salsa20-expansion k n))))
          (setq res (nconc res (salsa20--xor-list key ms)))
          (setq ms (nthcdr 64 ms))
          (unless ms
            (throw 'done t))
          (salsa20-increment-8byte! i)
          (aset n 2 (aref i 0))
          (aset n 3 (aref i 1)))))
    (apply 'unibyte-string res)))

(defun salsa20--xor-list (list1 list2)
  (loop for x1 in list1
        for x2 in list2
        collect (logxor x1 x2)))

(defun salsa20--random-u8vector (size)
  (loop with v = (make-vector size nil)
        for i below size
        do (aset v i (random ?\x100))
        finally return v))

(defun salsa20-increment-8byte! (8byte)
  (loop for i from 0 below 8
        do (let ((n (logand (1+ (aref 8byte i)) ?\xff)))
             (aset 8byte i n))
        unless (zerop (aref 8byte i))
        return nil)
  8byte)

;; read 64-byte -> 16-word (4x4)
(defun salsa20-read-from-string (string pos)
  (when (multibyte-string-p string)
    (error "error todo"))
  (loop with v1 = (make-vector 4 nil)
        for i from 0 below 4
        do (loop with v2 = (make-vector 4 0)
                 for j from 0 below 4
                 do (progn
                      (aset v2 j (logior
                                  (lsh (aref string (+ pos 0)) 0)
                                  (lsh (aref string (+ pos 1)) 8)
                                  (lsh (aref string (+ pos 2)) 16)
                                  (lsh (aref string (+ pos 3)) 24)))
                      (setq pos (+ pos 4)))
                 finally (aset v1 i v2))
        finally return v1))

(provide 'salsa20)

;;; salsa20.el ends here
