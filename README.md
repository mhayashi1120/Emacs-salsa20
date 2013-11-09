Summary
========

Salsa20 Encrypt/Hash algorithm for Emacs

Functions
=========

salsa20-encrypt:

    Encrypt/Decrypt BYTES (string) with IV by KEY.
    BYTES: Encrypt/Decrypt target and is destructively changed.
    KEY: 16 or 32 byte vector.
    IV: 8 byte vector.
    ROUNDS: Optional integer to reduce the number of rounds.
      See http://cr.yp.to/snuffle/812.pdf refering about Salsa20/8 Salsa20/12
      as a secure option.

salsa20-generator:

    Return a function which generate random sequence as byte list.
    This function accept following one of arg indicate the command of this function.
    
    * length of the byte list.
    * `t' means destruct this function.
    
    Optional ROUNDS arg see `salsa20-encrypt' description.
    
    Sample:
    (let ((generator (salsa20-generator (make-vector 16 0) (salsa20-generate-random-iv))))
      (unwind-protect
          (loop repeat 5
                collect (funcall generator 50))
        ;; Should not forget destruct
        (funcall generator t)))


Test
====

    M-x ert (tag salsa20)

Or

    make check


