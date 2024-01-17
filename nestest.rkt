#lang typed/racket

(provide create-nestest-bytes
         parse-reference-log)

(: create-nestest-bytes (-> Bytes))
(define (create-nestest-bytes)
  (let* ([port (open-input-file "nestest.nes")]
         [bytes (read-bytes 16 port)] ; discard first 16 bytes (the header), I'll deal with this later
         [bytes (read-bytes #x5000 port)])
    (close-input-port port)
    (if (eof-object? bytes)
        (error "nestest.nes did not contain enough bytes")
        bytes)))

(define-type LogItem (List String String String String String String String String String String)) 

; The nestest.log file comes from https://www.qmtpro.com/~nes/misc/nestest.log
; This is a known good log that we can use to test our CPU.
(: parse-reference-log (-> (Listof LogItem)))
(define (parse-reference-log)
  (: parse-one (-> String LogItem))
  (define (parse-one str)
    (let* ([pc (substring str 0 4)]
           [bytes (substring str 6 15)]
           [bytes (string-trim bytes)]
           ; WARNING - the log prepends the disassembly of illegal instructions
           ; with an `*` at index 15, but I don't care for now
           [disasm (substring str 16 48)]
           [disasm (string-trim disasm)]
           [a (substring str 48 52)]
           [x (substring str 53 57)]
           [y (substring str 58 62)]
           [p (substring str 63 67)]
           [sp (substring str 68 73)]
           [ppu (substring str 74 85)]
           [cyc (string-trim (substring str 86))])
      (list pc bytes disasm a x y p sp ppu cyc)))
  (let* ([port (open-input-file "nestest.log")]
         [strings (port->list read-line port)])
    (close-input-port port)
    (map parse-one (filter string? strings))))

(module+ main
  (define reflog (parse-reference-log))
  (define (find-opcode [opcode : String])
    (filter (lambda ([item : (Listof String)])
              (equal? opcode (substring (second item) 0 2)))
            reflog))
  ; Looking for an example
  #;(find-opcode "DD")
  (find-opcode "7D")
  )
