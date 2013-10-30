;;; salsa.el --- Salsa20 Encrypt/Hash/Expansion algorithm -*- lexical-binding: t -*-

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: data
;; URL: TODO https://github.com/mhayashi1120/Emacs-salsa20/raw/master/salsa20.el
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

;; Salsa20 basic implementation
;; http://cr.yp.to/snuffle/spec.pdf

;; Salsa20/8 Salsa20/12
;; http://cr.yp.to/snuffle/812.pdf

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

(defun salsa20--copy-16word! (to from)
  (loop for i from 0 below 16
        do (aset to i (aref from i))))

(defun salsa20--memcpy! (dest dest-from src src-from byte)
  (loop repeat byte
        for di from dest-from
        for si from src-from
        do (aset dest di (aref src si))))

(eval-when-compile
  (defsubst salsa20--word-to-4bytes (word)
    (list (lsh (logand word (aref salsa20--byte-table 0))   0)
          (lsh (logand word (aref salsa20--byte-table 1))  -8)
          (lsh (logand word (aref salsa20--byte-table 2)) -16)
          (lsh (logand word (aref salsa20--byte-table 3)) -24))))

(defun salsa20--16word-to-bytes (16word)
  (loop for x across 16word
        append (salsa20--word-to-4bytes x)))

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

(defun salsa20--xor-vector! (vector list)
  (loop for i from 0
        for v1 across vector
        for l1 in list
        do (aset vector i (logxor v1 l1))
        finally return vector))

(defun salsa20--xor-list (list1 list2)
  (loop for x1 in list1
        for x2 in list2
        collect (logxor x1 x2)))

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
;; 3. The quarterround function
;;

;; TODO not used but test
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

(eval-when-compile
  (defsubst salsa20--quarter-quarterround! (x target src1 src2 shift-count)
    (let* ((sum (salsa20--sum (aref x src1) (aref x src2)))
           (shift (salsa20--left-shift sum shift-count))
           (new (salsa20--xor (aref x target) shift)))
      (aset x target new))))

;;
;; 4. The rowround function (16-word -> 16-word)
;;

(eval-when-compile
  (defsubst salsa20--rowround! (y)
    (salsa20--quarter-quarterround! y  1  0  3  7)
    (salsa20--quarter-quarterround! y  2  1  0  9)
    (salsa20--quarter-quarterround! y  3  2  1 13)
    (salsa20--quarter-quarterround! y  0  3  2 18)
    (salsa20--quarter-quarterround! y  6  5  4  7)
    (salsa20--quarter-quarterround! y  7  6  5  9)
    (salsa20--quarter-quarterround! y  4  7  6 13)
    (salsa20--quarter-quarterround! y  5  4  7 18)
    (salsa20--quarter-quarterround! y 11 10  9  7)
    (salsa20--quarter-quarterround! y  8 11 10  9)
    (salsa20--quarter-quarterround! y  9  8 11 13)
    (salsa20--quarter-quarterround! y 10  9  8 18)
    (salsa20--quarter-quarterround! y 12 15 14  7)
    (salsa20--quarter-quarterround! y 13 12 15  9)
    (salsa20--quarter-quarterround! y 14 13 12 13)
    (salsa20--quarter-quarterround! y 15 14 13 18)
    y))

;;
;; 5. The columnround function (16-word -> 16-word)
;;

(eval-when-compile
  (defsubst salsa20--columnround! (x)
    (salsa20--quarter-quarterround! x  4  0 12  7)
    (salsa20--quarter-quarterround! x  8  4  0  9)
    (salsa20--quarter-quarterround! x 12  8  4 13)
    (salsa20--quarter-quarterround! x  0 12  8 18)
    (salsa20--quarter-quarterround! x  9  5  1  7)
    (salsa20--quarter-quarterround! x 13  9  5  9)
    (salsa20--quarter-quarterround! x  1 13  9 13)
    (salsa20--quarter-quarterround! x  5  1 13 18)
    (salsa20--quarter-quarterround! x 14 10  6  7)
    (salsa20--quarter-quarterround! x  2 14 10  9)
    (salsa20--quarter-quarterround! x  6  2 14 13)
    (salsa20--quarter-quarterround! x 10  6  2 18)
    (salsa20--quarter-quarterround! x  3 15 11  7)
    (salsa20--quarter-quarterround! x  7  3 15  9)
    (salsa20--quarter-quarterround! x 11  7  3 13)
    (salsa20--quarter-quarterround! x 15 11  7 18)
    x))

;;
;; 6. The doubleround function (16-word -> 16-word)
;;

;; Not used
(eval-when-compile
  (defsubst salsa20--doubleround! (x)
    (salsa20--columnround! x)
    (salsa20--rowround! x)
    x))

;;
;; 7. The littleendian function (4-byte -> 1-word)
;;

(eval-and-compile
  (defun salsa20--littleendian (b0 b1 b2 b3)
    (logior
     (lsh b0 0)
     (lsh b1 8)
     (lsh b2 16)
     (lsh b3 24))))

;;
;; 8. The Salsa20 hash function (64-byte unibyte list -> 64-byte unibyte list)
;;

(defun salsa20--hash (16word &optional rounds)
  (unless rounds
    (setq rounds 20))
  (loop with initial = [nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]
        ;; clone working vector to preserve initial vector
        initially (salsa20--copy-16word! initial 16word)
        for r from 0 below rounds
        if (zerop (logand r 1))
        do (salsa20--columnround! 16word)
        else
        do (salsa20--rowround! 16word)
        finally return
        (progn
          (salsa20--sum-16word! 16word initial)
          (prog1
              (salsa20--16word-to-bytes 16word)
            (fillarray 16word 0)
            (fillarray initial 0)))))

;;
;; 9. The Salsa20 expansion function
;;

(defconst salsa20--sigma-word
  (eval-when-compile
    (vconcat
     (mapcar
      (lambda (x)
        (apply 'salsa20--littleendian x))
      '((101 120 112  97)
        (110 100  32  51)
        ( 50  45  98 121)
        (116 101  32 107))))))

(defconst salsa20--tau-word
  (eval-when-compile
    (vconcat
     (mapcar
      (lambda (x)
        (apply 'salsa20--littleendian x))
      '((101 120 112  97)
        (110 100  32  49)
        ( 54  45  98 121)
        (116 101  32 107))))))

(defun salsa20--read-sigma-16word! (16word k m)
  (let ((sigma salsa20--sigma-word))
    (aset 16word 0 (aref sigma 0))
    (salsa20--memcpy! 16word 1 k 0 4)
    (aset 16word 5 (aref sigma 1))
    (salsa20--memcpy! 16word 6 m 0 4)
    (aset 16word 10 (aref sigma 2))
    (salsa20--memcpy! 16word 11 k 4 4)
    (aset 16word 15 (aref sigma 3))))

(defun salsa20--read-tau-16word! (16word k m)
  (let ((tau salsa20--tau-word))
    (aset 16word 0 (aref tau 0))
    (salsa20--memcpy! 16word 1 k 0 4)
    (aset 16word 5 (aref tau 1))
    (salsa20--memcpy! 16word 6 m 0 4)
    (aset 16word 10 (aref tau 2))
    (salsa20--memcpy! 16word 11 k 0 4)
    (aset 16word 15 (aref tau 3))))

(defun salsa20--bytes-to-word (bytes)
  (loop with v = (make-vector (/ (length bytes) 4) nil)
        for i from 0 below (length bytes) by 4
        for j from 0
        do (aset v j (salsa20--littleendian
                      (aref bytes (+ i 0)) (aref bytes (+ i 1))
                      (aref bytes (+ i 2)) (aref bytes (+ i 3))))
        finally return v))

(defun salsa20-expansion (k n &optional rounds)
  (unless (= (length n) 16)
    (error "invalid `n' length (Must be 16-byte)"))
  (unless (memq (length k) '(16 32))
    (error "Not a supported key length (16 or 32 bytes)"))
  ;; TODO 16word will be cleared in `salsa20--hash'
  (let ((kw (salsa20--bytes-to-word k))
        (nw (salsa20--bytes-to-word n))
        (16word [nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil]))
    (cond
     ((= (length k) 16)
      (salsa20--read-tau-16word! 16word kw nw))
     ((= (length k) 32)
      (salsa20--read-sigma-16word! 16word kw nw))
     (t
      (error "assert")))
    (salsa20--hash 16word rounds)))

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


(defun salsa20--generate-random-bytes (size)
  (loop with v = (make-vector size nil)
        for i below size
        do (aset v i (random ?\x100))
        finally return v))

;;;###autoload
(defun salsa20-generate-random-iv ()
  "Utility function to create randomized initial vector (IV)."
  (salsa20--generate-random-bytes 8))

;;;###autoload
(defun salsa20-generator (key iv &optional rounds)
  "Return a function which generate random sequence as byte list.
This function accept following one of arg indicate the command of this function.

* length of the byte list.
* `t' means destruct this function.

Optional ROUNDS arg see `salsa20-encrypt' description.

TODO sample of using
"
  (unless (= (length iv) 8)
    (error "Invalid length of IV (Must be 8 byte)"))
  (unless (memq (length key) '(16 32))
    (error "Invalid length of KEY (Must be 16 or 32 byte)"))
  (let* ((i [0 0 0 0 0 0 0 0])
         (n (vconcat iv i))
         remain)
    (lambda (command)
      (cond
       ((eq command t)
        (fillarray i 0)
        (fillarray n 0)
        (loop for r on remain
              do (setcar r 0)))
       ((not (numberp command))
        (error "Not supported command %s" command))
       ((zerop command)
        '())
       (t
        (let ((done remain))
          (while (< (length done) command)
            (let ((hash (salsa20-expansion key n rounds)))
              (setq done (nconc done hash))
              (salsa20--inc-ushort! n 8)))
          (setq remain (nthcdr command done))
          (setcdr (nthcdr (1- command) done) nil)
          done))))))

;;;###autoload
(defun salsa20-encrypt (bytes key iv &optional rounds)
  "Encrypt/Decrypt BYTES (string) with IV by KEY.
KEY: 16 or 32 byte vector.
IV: 8 byte vector.
ROUNDS: Optional integer to reduce the number of rounds.
  See http://cr.yp.to/snuffle/812.pdf refering about Salsa20/8 Salsa20/12
  as a secure option.

TODO clear KEY?? but generator use this instance.
TODO BYTES is destructively changed
"
  (when (multibyte-string-p bytes)
    (error "Not a unibyte string"))
  (let* ((generator (salsa20-generator key iv rounds))
         (hash (funcall generator (length bytes))))
    (salsa20--xor-vector! bytes hash)))

;;;###autoload
(defalias 'salsa20-decrypt 'salsa20-encrypt)

;;;###autoload
(defun salsa20-encrypt-string (string &optional coding-system)
  "Encrypt STRING by password.
TODO"
  (let ((bytes (cond
                ((not (multibyte-string-p string))
                 string)
                (coding-system
                 (encode-coding-string string coding-system))
                (t
                 (string-as-unibyte string)))))
    (let* ((salt (salsa20--generate-random-bytes 8))
           (pass (read-passwd "Password to encrypt: " t)))
      ;;TODO
      (require 'kaesar)
      (destructuring-bind (raw-key iv)
          (kaesar--openssl-evp-bytes-to-key-0
           ;;TODO
           8 16
           ;;TODO
           (vconcat pass) salt)
        (apply 
         'unibyte-string
         (append
          (string-to-list "Salted__")
          salt
          (string-to-list (salsa20-encrypt bytes raw-key iv))))))))

;;;###autoload
(defun salsa20-decrypt-string (string &optional coding-system)
  "Decrypt STRING by password."
  (unless (string-match "\\`Salted__\\(.\\{8\\}\\)" string)
    (error "TODO Not salted string"))
  (let ((pass (read-passwd "Password to decrypt: "))
        (salt (match-string 1 string))
        (body (substring string (match-end 0))))
    ;;TODO
    (require 'kaesar)
    (destructuring-bind (raw-key iv)
        (kaesar--openssl-evp-bytes-to-key-0
         ;;TODO
         8 16
         ;;TODO
         (vconcat pass) salt)
      (let ((bytes (salsa20-decrypt body raw-key iv)))
        (cond
         (coding-system
          (decode-coding-string bytes coding-system))
         ((default-value 'enable-multibyte-characters)
          ;;TODO only multibyte environment
          (string-as-multibyte bytes))
         (t
          bytes))))))

;;TODO
;; (defun salsa20-password-to-key (password salt)
;;   (secure-hash 'sha256 ))

;;TODO
;;;;###autoload
;; (defun salsa20-hash (object &optional coding-system start end)
;;   "todo Core function to create 64-byte hash like `secure-hash'"
;;   (salsa20--hash ))

(provide 'salsa20)

;;; salsa20.el ends here
