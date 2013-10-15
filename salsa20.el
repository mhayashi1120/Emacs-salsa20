;;; salsa.el --- Salsa20 Encrypt/Hash/Expansion algorithm -*- lexical-binding: t -*-

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
;; (require 'salsa20)
;;

;;; Commentary:

;;todo

;; http://cr.yp.to/snuffle/spec.pdf

;; Salsa20/8 Salsa20/12

;;; Usage:
;;todo

;;; TODO:
;; * clear secret data from function literal

;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup salsa20 nil
  "Salsa20 encrypt/hash/expansion utilities."
  :prefix "salsa20-"
  :group 'data)

(when (zerop (lsh 1 32))
  (error "This Emacs doesn't support wide-int"))

;;
;; Utilities bytewise operation
;; 

(eval-and-compile
  (defconst salsa20-word-range
    (eval-when-compile
      (lsh 1 32))))

(eval-and-compile
  (defconst salsa20-word-max
    (eval-when-compile
      (1- salsa20-word-range))))

(defconst salsa20--byte-table
  (eval-when-compile
    (vector (lsh ?\xff 0) (lsh ?\xff 8) (lsh ?\xff 16) (lsh ?\xff 24))))

(defun salsa20--clone-16word (16word)
  (vconcat 16word))

(eval-when-compile
  (defsubst salsa20--word-to-4bytes (word)
    (list (lsh (logand word (aref salsa20--byte-table 0))   0)
          (lsh (logand word (aref salsa20--byte-table 1))  -8)
          (lsh (logand word (aref salsa20--byte-table 2)) -16)
          (lsh (logand word (aref salsa20--byte-table 3)) -24))))

(eval-when-compile
  (defsubst salsa20--sum (word1 word2)
    (logand (+ word1 word2) salsa20-word-max)))

(eval-when-compile
  (defsubst salsa20--xor (word1 word2)
    (logxor word1 word2)))

(eval-when-compile
  (defsubst salsa20--sum-16word! (words1 words2)
    (loop for w1 across words1
          for w2 across words2
          for i from 0
          do (aset words1 i (salsa20--sum w1 w2))
          finally return words1)))

(eval-when-compile
  (defsubst salsa20--xor-list (list1 list2)
    (loop for x1 in list1
          for x2 in list2
          collect (logxor x1 x2))))

;;
;; 2. Words
;;

(eval-when-compile
  (defsubst salsa20--left-shift (word shift)
    ;; shift must be `shift < 32'
    (logior
     (logand salsa20-word-max (lsh word shift))
     (logand salsa20-word-max (lsh word (- shift 32))))))

;;
;; 3. The quarterround function (4 bytes -> 4-word)
;;

;;TODO
(eval-when-compile
  (defsubst salsa20--one-sixteenth-round (base x0 x1 lshift)
    (salsa20--xor base (salsa20--left-shift (salsa20--sum x0 x1) lshift))))

(eval-when-compile
  (defsubst salsa20--quarterround (y0 y1 y2 y3)
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
      res)))

;;
;; 4. The rowround function (16-word 4x4 todo -> 16-word 4x4)
;;

(eval-when-compile
  (defsubst salsa20--rowround! (y0 y1 y2 y3)
    (let* ((generator (lambda (v j0 j1 j2 j3)
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
      (vector y0 y1 y2 y3))))

;;
;; 5. The columnround function (16-word 4x4 todo -> 16-word 4x4)
;;

(eval-when-compile
  (defsubst salsa20--columnround! (x0 x1 x2 x3)
    (let* ((generator (lambda (v0 v1 v2 v3 i)
                        (salsa20--quarterround
                         (aref v0 i) (aref v1 i) (aref v2 i) (aref v3 i))))
           (setter (lambda (col v i0 i1 i2 i3)
                     (aset x0 col (aref v i0))
                     (aset x1 col (aref v i1))
                     (aset x2 col (aref v i2))
                     (aset x3 col (aref v i3)))))
      (funcall setter 0 (funcall generator x0 x1 x2 x3 0) 0 1 2 3)
      (funcall setter 1 (funcall generator x1 x2 x3 x0 1) 3 0 1 2)
      (funcall setter 2 (funcall generator x2 x3 x0 x1 2) 2 3 0 1)
      (funcall setter 3 (funcall generator x3 x0 x1 x2 3) 1 2 3 0)
      (vector x0 x1 x2 x3))))

;;
;; 6. The doubleround function (16-word 4x4 -> 16-word 4x4)
;;

(eval-when-compile
  (defsubst salsa20--doubleround! (x)
    ;; (salsa20--columnround! (substring x 0 4) (substring x 4 8) (substring x 8 12) (substring x 12 16))
    ;; (salsa20--rowround! (substring x 0 4) (substring x 4 8) (substring x 8 12) (substring x 12 16))
    (let ((setter
           (lambda (target src1 src2 shift-count)
             (let* ((sum (salsa20--sum (aref x src1) (aref x src2)))
                    (shift (salsa20--left-shift sum shift-count))
                    (new (salsa20--xor (aref x target) shift)))
               (aset x target new)))))
      (funcall setter  4  0 12  7)
      (funcall setter  8  4  0  9)
      (funcall setter 12  8  4 13)
      (funcall setter  0 12  8 18)
      (funcall setter  9  5  1  7)
      (funcall setter 13  9  5  9)
      (funcall setter  1 13  9 13)
      (funcall setter  5  1 13 18)
      (funcall setter 14 10  6  7)
      (funcall setter  2 14 10  9)
      (funcall setter  6  2 14 13)
      (funcall setter 10  6  2 18)
      (funcall setter  3 15 11  7)
      (funcall setter  7  3 15  9)
      (funcall setter 11  7  3 13)
      (funcall setter 15 11  7 18)
      (funcall setter  1  0  3  7)
      (funcall setter  2  1  0  9)
      (funcall setter  3  2  1 13)
      (funcall setter  0  3  2 18)
      (funcall setter  6  5  4  7)
      (funcall setter  7  6  5  9)
      (funcall setter  4  7  6 13)
      (funcall setter  5  4  7 18)
      (funcall setter 11 10  9  7)
      (funcall setter  8 11 10  9)
      (funcall setter  9  8 11 13)
      (funcall setter 10  9  8 18)
      (funcall setter 12 15 14  7)
      (funcall setter 13 12 15  9)
      (funcall setter 14 13 12 13)
      (funcall setter 15 14 13 18)
      x)))

;;
;; 7. The littleendian function (4-byte -> 1-word)
;;

(defun salsa20--littleendian (b0 b1 b2 b3)
  (logior
   (lsh b0 0)
   (lsh b1 8)
   (lsh b2 16)
   (lsh b3 24)))

;;
;; 8. The Salsa20 hash function (64-byte unibyte list -> 64-byte unibyte list)
;;

(defun salsa20--16word-to-bytes (16word)
  (loop for x across 16word
        append (salsa20--word-to-4bytes x)))

(defun salsa20--load-16word! (xs list)
  ;; destructive literal vector
  (loop for i from 0 below 16
        for j from 0 by 4
        for top on list by (lambda (x) (nthcdr 4 x))
        do (aset xs i (salsa20--littleendian
                       (nth 0 top)
                       (nth 1 top)
                       (nth 2 top)
                       (nth 3 top)))
        finally return xs))

;;TODO consider about ROUNDS is odd/even
(defun salsa20--hash (x &optional rounds)
  (unless rounds
    (setq rounds 20))
  (loop with 16word = [nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]
        with xs = (salsa20--load-16word! 16word x)
        ;; clone working vector to preserve initial vector
        with tmp = (salsa20--clone-16word xs)
        for r from 0 below rounds by 2
        do (salsa20--doubleround! tmp)
        finally return
        (progn
          (salsa20--sum-16word! tmp xs)
          (salsa20--16word-to-bytes tmp))))

(defun salsa20--hash-many (x time)
  (loop repeat time
        do (setq x (salsa20--hash x))
        finally return x))

;;
;; 9. The Salsa20 expansion function
;;

(defconst salsa20--sigma
  [
   [101 120 112  97]
   [110 100  32  51]
   [ 50  45  98 121]
   [116 101  32 107]])

(defconst salsa20--tau
  [
   [101 120 112  97]
   [110 100  32  49]
   [ 54  45  98 121]
   [116 101  32 107]])

(defun salsa20-expansion (k n)
  (unless (= (length n) 16)
    (error "invalid `n' length (Must be 16-byte)"))
  ;;TODO remove `append'??
  (cond
   ((= (length k) 16)
    (salsa20--hash
     (append
      (aref salsa20--tau 0)
      k
      (aref salsa20--tau 1)
      n
      (aref salsa20--tau 2)
      k
      (aref salsa20--tau 3)
      nil)))
   ((= (length k) 32)
    (salsa20--hash
     (append
      (aref salsa20--sigma 0)
      (substring k 0 16)                ; k0
      (aref salsa20--sigma 1)
      n
      (aref salsa20--sigma 2)
      (substring k 16)                  ; k1
      (aref salsa20--sigma 3)
      nil)))
   (t
    (error "Not a supported key length (16 or 32 bytes)"))))

;;
;; 10. The Salsa20 encryption function
;;

(defun salsa20--inc-ushort! (vector start)
  (loop repeat 8 for i from start
        do (let ((n (logand (1+ (aref vector i)) ?\xff)))
             (aset vector i n))
        unless (zerop (aref vector i))
        return nil)
  vector)

;;;###autoload
(defun salsa20-generate-random-iv ()
  "Utility function to create randomized initial vector (IV)."
  (loop with size = 8
        with v = (make-vector size nil)
        for i below size
        do (aset v i (random ?\x100))
        finally return v))

;;;###autoload
(defun salsa20-generator (key iv)
  "Return a function which generate random sequence as byte list.
This function accept a integer arg indicate the length of the byte list.
TODO

"
  (unless (= (length iv) 8)
    (error "Invalid length of IV (Must be 8 byte)"))
  (unless (memq (length key) '(16 32))
    (error "Invalid length of KEY (Must be 16 or 32 byte)"))
  (let* ((i (vector 0 0 0 0 0 0 0 0))
         (n (vconcat iv i))
         remain)
    (lambda (size)
      (let ((done remain))
        (while (< (length done) size)
          (let ((hash (salsa20-expansion key n)))
            (setq done (nconc done hash))
            (salsa20--inc-ushort! n 8)))
        (setq remain (nthcdr size done))
        (setcdr (nthcdr (1- size) done) nil)
        done))))

;;;###autoload
(defun salsa20-encrypt (key iv bytes)
  "Encrypt/Decrypt BYTES (string) with IV by KEY.
KEY: 16 or 32 byte vector.
IV: 8 byte vector.
"
  (when (multibyte-string-p bytes)
    (error "Not a unibyte string"))
  (let* ((generator (salsa20-generator key iv))
         (hash (funcall generator (length bytes))))
    (apply 'unibyte-string
           (salsa20--xor-list (string-to-list bytes) hash))))

;;;###autoload
(defalias 'salsa20-decrypt 'salsa20-encrypt)

;; ;;;###autoload
;; (defun salsa20-encrypt-string (key iv text &optional coding-system)
;;   "TODO"
;;   (let ((bytes (if coding-system
;;                    (encode-coding-string text coding-system)
;;                  (string-as-unibyte text))))
;;     (salsa20-encrypt key iv bytes)))

;; ;;;###autoload
;; (defalias 'salsa20-decrypt-string 'salsa20-encrypt-string)

;;TODO
;; (defun salsa20-password-to-key (password salt)
;;   (secure-hash 'sha256 ))

;;TODO
;;;;###autoload
;; (defun salsa20-hash (object &optional coding-system start end)
;;   "todo Core function to create 64-byte hash like `secure-hash'"
;;   (salsa20--hash ))

;;TODO use?
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
                                  (lsh (aref string (+ pos 0))  0)
                                  (lsh (aref string (+ pos 1))  8)
                                  (lsh (aref string (+ pos 2)) 16)
                                  (lsh (aref string (+ pos 3)) 24)))
                      (setq pos (+ pos 4)))
                 finally (aset v1 i v2))
        finally return v1))

(provide 'salsa20)

;;; salsa20.el ends here
