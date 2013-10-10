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

(require 'ert)

(defun salsa20-test--hex (hex)
  (cond
   ((string-match "\\`0x\\([0-9a-fA-F]+\\)" hex)
    (string-to-number (match-string 1 hex)16))
   (t
    (error "Not supported"))))

(defun salsa20-test--concat-byteseq (text)
  (apply 'unibyte-string (mapcar 'string-to-number (split-string text "[; \n]" t))))

;; 0xc0a8787e + 0x9fd1161d = 0x60798e9b
(should (equal (salsa20--sum (salsa20-test--hex "0xc0a8787e") (salsa20-test--hex "0x9fd1161d"))
               (salsa20-test--hex "0x60798e9b")))

;; 0xc0a8787e (+) 0x9fd1161d = 0x5f796e63
(should (equal (salsa20--xor (salsa20-test--hex "0xc0a8787e") (salsa20-test--hex "0x9fd1161d"))
               (salsa20-test--hex "0x5f796e63")))

;; 0xc0a8787e <<< 5 = 0x150f0fd8
(should (equal (salsa20--left-shift (salsa20-test--hex "0xc0a8787e") 5)
               (salsa20-test--hex "0x150f0fd8")))

(defun salsa20-test--quarterround-read (text)
  (mapcar
   'salsa20-test--hex
   (split-string text "[; ]" t)))

(should (equal (apply 'salsa20--quarterround (salsa20-test--quarterround-read "0x00000000; 0x00000000; 0x00000000; 0x00000000"))
               (vconcat (salsa20-test--quarterround-read "0x00000000; 0x00000000; 0x00000000; 0x00000000"))))

(should (equal (apply 'salsa20--quarterround (salsa20-test--quarterround-read "0x00000001; 0x00000000; 0x00000000; 0x00000000"))
               (vconcat (salsa20-test--quarterround-read "0x08008145; 0x00000080; 0x00010200; 0x20500000"))))

(should (equal (apply 'salsa20--quarterround (salsa20-test--quarterround-read "0x00000000; 0x00000001; 0x00000000; 0x00000000"))
               (vconcat (salsa20-test--quarterround-read "0x88000100; 0x00000001; 0x00000200; 0x00402000"))))

(should (equal (apply 'salsa20--quarterround (salsa20-test--quarterround-read "0x00000000; 0x00000000; 0x00000001; 0x00000000"))
               (vconcat (salsa20-test--quarterround-read "0x80040000; 0x00000000; 0x00000001; 0x00002000"))))

(should (equal (apply 'salsa20--quarterround (salsa20-test--quarterround-read "0x00000000; 0x00000000; 0x00000000; 0x00000001"))
               (vconcat (salsa20-test--quarterround-read "0x00048044; 0x00000080; 0x00010000; 0x20100001"))))

(should (equal (apply 'salsa20--quarterround (salsa20-test--quarterround-read "0xd3917c5b; 0x55f1c407; 0x52a58a7a; 0x8f887a3b"))
               (vconcat (salsa20-test--quarterround-read "0x3e2f308c; 0xd90a8f36; 0x6ab2a923; 0x2883524c"))))

(defun salsa20-test--16word-4x4 (text)
  (vconcat
   (loop for xs on (split-string text "[; ]" t) by (lambda (x) (nthcdr 4 x))
         collect (vconcat
                  (loop repeat 4
                        for x in xs
                        collect (salsa20-test--hex x))))))

(should (equal (salsa20--rowround!
                (salsa20-test--16word-4x4 "0x00000001; 0x00000000; 0x00000000; 0x00000000; 0x00000001; 0x00000000; 0x00000000; 0x00000000; 0x00000001; 0x00000000; 0x00000000; 0x00000000; 0x00000001; 0x00000000; 0x00000000; 0x00000000"))
               (salsa20-test--16word-4x4 "0x08008145; 0x00000080; 0x00010200; 0x20500000;0x20100001; 0x00048044; 0x00000080; 0x00010000;0x00000001; 0x00002000; 0x80040000; 0x00000000;0x00000001; 0x00000200; 0x00402000; 0x88000100")))

(should (equal (salsa20--rowround!
                (salsa20-test--16word-4x4 "0x08521bd6; 0x1fe88837; 0xbb2aa576; 0x3aa26365;0xc54c6a5b; 0x2fc74c2f; 0x6dd39cc3; 0xda0a64f6;0x90a2f23d; 0x067f95a6; 0x06b35f61; 0x41e4732e;0xe859c100; 0xea4d84b7; 0x0f619bff; 0xbc6e965a"))
               (salsa20-test--16word-4x4 "0xa890d39d; 0x65d71596; 0xe9487daa; 0xc8ca6a86;0x949d2192; 0x764b7754; 0xe408d9b9; 0x7a41b4d1;0x3402e183; 0x3c3af432; 0x50669f96; 0xd89ef0a8;0x0040ede5; 0xb545fbce; 0xd257ed4f; 0x1818882d")))


(should (equal (salsa20--columnround!
                (salsa20-test--16word-4x4 "0x00000001; 0x00000000; 0x00000000; 0x00000000; 0x00000001; 0x00000000; 0x00000000; 0x00000000; 0x00000001; 0x00000000; 0x00000000; 0x00000000; 0x00000001; 0x00000000; 0x00000000; 0x00000000"))
               (salsa20-test--16word-4x4 "0x10090288; 0x00000000; 0x00000000; 0x00000000;0x00000101; 0x00000000; 0x00000000; 0x00000000;0x00020401; 0x00000000; 0x00000000; 0x00000000;0x40a04001; 0x00000000; 0x00000000; 0x00000000")))

(should (equal (salsa20--columnround!
                (salsa20-test--16word-4x4 "0x08521bd6; 0x1fe88837; 0xbb2aa576; 0x3aa26365;0xc54c6a5b; 0x2fc74c2f; 0x6dd39cc3; 0xda0a64f6;0x90a2f23d; 0x067f95a6; 0x06b35f61; 0x41e4732e;0xe859c100; 0xea4d84b7; 0x0f619bff; 0xbc6e965a"))
               (salsa20-test--16word-4x4"0x8c9d190a; 0xce8e4c90; 0x1ef8e9d3; 0x1326a71a;0x90a20123; 0xead3c4f3; 0x63a091a0; 0xf0708d69;0x789b010c; 0xd195a681; 0xeb7d5504; 0xa774135c;0x481c2027; 0x53a8e4b5; 0x4c1f89c5; 0x3f78c9c8")))

(should (equal (salsa20--doubleround!
                (salsa20-test--16word-4x4 "0x00000001; 0x00000000; 0x00000000; 0x00000000;0x00000000; 0x00000000; 0x00000000; 0x00000000;0x00000000; 0x00000000; 0x00000000; 0x00000000;0x00000000; 0x00000000; 0x00000000; 0x00000000"))
               (salsa20-test--16word-4x4 "0x8186a22d; 0x0040a284; 0x82479210; 0x06929051;0x08000090; 0x02402200; 0x00004000; 0x00800000;0x00010200; 0x20400000; 0x08008104; 0x00000000;0x20500000; 0xa0000040; 0x0008180a; 0x612a8020")))

(should (equal (salsa20--doubleround!
                (salsa20-test--16word-4x4 "0xde501066; 0x6f9eb8f7; 0xe4fbbd9b; 0x454e3f57;0xb75540d3; 0x43e93a4c; 0x3a6f2aa0; 0x726d6b36;0x9243f484; 0x9145d1e8; 0x4fa9d247; 0xdc8dee11;0x054bf545; 0x254dd653; 0xd9421b6d; 0x67b276c1"))
               (salsa20-test--16word-4x4 "0xccaaf672; 0x23d960f7; 0x9153e63a; 0xcd9a60d0;0x50440492; 0xf07cad19; 0xae344aa0; 0xdf4cfdfc;0xca531c29; 0x8e7943db; 0xac1680cd; 0xd503ca00;0xa74b2ad6; 0xbc331c5c; 0x1dda24c7; 0xee928277")))

(should (equal (salsa20--littleendian 0 0 0 0) (string-to-number "00000000" 16)))
(should (equal (salsa20--littleendian 86 75 30 9) (string-to-number "091e4b56" 16)))
(should (equal (salsa20--littleendian 255 255 255 250) (string-to-number "faffffff" 16)))

(should (equal (salsa20-hash
                (salsa20-test--concat-byteseq "0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0"))
               (salsa20-test--concat-byteseq "0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0")))

(should (equal (salsa20-hash
                (salsa20-test--concat-byteseq "211;159; 13;115; 76; 55; 82;183; 3;117;222; 37;191;187;234;136;49;237;179; 48; 1;106;178;219;175;199;166; 48; 86; 16;179;207;31;240; 32; 63; 15; 83; 93;161;116;147; 48;113;238; 55;204; 36;79;201;235; 79; 3; 81;156; 47;203; 26;244;243; 88;118;104; 54"))
               (salsa20-test--concat-byteseq "109; 42;178;168;156;240;248;238;168;196;190;203; 26;110;170;154;29; 29;150; 26;150; 30;235;249;190;163;251; 48; 69;144; 51; 57;118; 40;152;157;180; 57; 27; 94;107; 42;236; 35; 27;111;114;114;219;236;232;135;111;155;110; 18; 24;232; 95;158;179; 19; 48;202")))

(should (equal (salsa20-hash
                (salsa20-test--concat-byteseq "88;118;104; 54; 79;201;235; 79; 3; 81;156; 47;203; 26;244;243;191;187;234;136;211;159; 13;115; 76; 55; 82;183; 3;117;222; 37;86; 16;179;207; 49;237;179; 48; 1;106;178;219;175;199;166; 48;238; 55;204; 36; 31;240; 32; 63; 15; 83; 93;161;116;147; 48;113"))
               (salsa20-test--concat-byteseq "179; 19; 48;202;219;236;232;135;111;155;110; 18; 24;232; 95;158;26;110;170;154;109; 42;178;168;156;240;248;238;168;196;190;203;69;144; 51; 57; 29; 29;150; 26;150; 30;235;249;190;163;251; 48;27;111;114;114;118; 40;152;157;180; 57; 27; 94;107; 42;236; 35")))

;; Salsa20^1000000( 6;124; 83;146; 38;191; 9; 50; 4;161; 47;222;122;182;223;185;
;; 75; 27; 0;216; 16;122; 7; 89;162;104;101;147;213; 21; 54; 95;
;; 225;253;139;176;105;132; 23;116; 76; 41;176;207;221; 34;157;108;
;; 94; 94; 99; 52; 90;117; 91;220;146;190;239;143;196;176;130;186)
;; = ( 8; 18; 38;199;119; 76;215; 67;173;127;144;162;103;212;176;217;
;; 192; 19;233; 33;159;197;154;160;128;243;219; 65;171;136;135;225;
;; 123; 11; 68; 86;237; 82; 20;155;133;189; 9; 83;167;116;194; 78;
;; 122;127;195;185;185;204;188; 90;245; 9;183;248;226; 85;245;104):

(should
 (equal
  (salsa20-expansion
   ;; Define k0 = (1; 2; 3; 4; 5; : : : ; 16), k1 = (201; 202; 203; 204; 205; : : : ; 216), and n =
   ;; (101; 102; 103; 104; 105; : : : ; 116). Then
   (vconcat
    (loop for i from 1 to 16 collect i)
    (loop for i from 201 to 216 collect i))
   (vconcat (loop for i from 101 to 116 collect i)))
  (salsa20-test--concat-byteseq "69; 37; 68; 39; 41; 15;107;193;255;139;122; 6;170;233;217; 98;89;144;182;106; 21; 51;200; 65;239; 49;222; 34;215;114; 40;126;104;197; 7;225;197;153; 31; 2;102; 78; 76;176; 84;245;246;184;177;160;133;130; 6; 72;149;119;192;195;132;236;234;103;246; 74")))

(should
 (equal
  (salsa20-expansion
   ;; Define k0 = (1; 2; 3; 4; 5; : : : ; 16), k1 = (201; 202; 203; 204; 205; : : : ; 216), and n =
   ;; (101; 102; 103; 104; 105; : : : ; 116). Then
   (vconcat (loop for i from 1 to 16 collect i))
   (vconcat (loop for i from 101 to 116 collect i)))
  (salsa20-test--concat-byteseq "39;173; 46;248; 30;200; 82; 17; 48; 67;254;239; 37; 18; 13;247;241;200; 61;144; 10; 55; 50;185; 6; 47;246;253;143; 86;187;225;134; 85;110;246;161;163; 43;235;231; 94;171; 51;145;214;112; 29;14;232; 5; 16;151;140;183;141;171; 9;122;181;104;182;177;193")))

(should (equal (salsa20-increment-8byte! [0 0 0 0 0 0 0 0]) [1 0 0 0 0 0 0 0]))
(should (equal (salsa20-increment-8byte! [1 0 0 0 0 0 0 0]) [2 0 0 0 0 0 0 0]))
(should (equal (salsa20-increment-8byte! [255 0 0 0 0 0 0 0]) [0 1 0 0 0 0 0 0]))
(should (equal (salsa20-increment-8byte! [255 255 255 255 255 255 255 255]) [0 0 0 0 0 0 0 0]))

(provide 'salsa20)

;;; salsa20.el ends here
