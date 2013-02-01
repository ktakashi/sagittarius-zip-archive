;;; -*- mode:scheme; coding:utf-8;  -*-
;;;
;;; zip-archive.scm - ZIP archive library.
;;;  
;;;   Copyright (c) 2010-2012  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

(library (zip-archive)
    (export open-output-zip-archive
	    zip-add-entry
	    zip-close
	    call-with-output-zip-archive)
    (import (rnrs) (rnrs r5rs)
	    (clos user)
	    (sagittarius)
	    (sagittarius control)
	    (sagittarius object)
	    (util file)
	    (binary pack) (rfc zlib)
	    (srfi :19) (srfi :26))

  (define-syntax ash (identifier-syntax bitwise-arithmetic-shift))
  (define (date->dos-format date)
    (+ (ash (- (date-year date) 1980) 25)
       (ash (date-month date)         21)
       (ash (date-day date)           16)
       (ash (date-hour date)          11)
       (ash (date-minute date)        5)
       (quotient (date-second date) 2)))

  (define-class <local-file-header> ()
    ((compress-method :init-keyword :compress-method)
     (timestamp :init-keyword :timestamp)
     (checksum :init-keyword :checksum)
     (compressed-size :init-keyword :compressed-size)
     (uncompressed-size :init-keyword :uncompressed-size)
     (filename-size :init-keyword :filename-size)
     (offset :init-keyword :offset)
     (filename :init-keyword :filename)))

  (define-class <zip-archive> ()
    ((port :init-keyword :port)
     (name :init-keyword :name)
     (tempname :init-keyword :tempname)
     (timestamp :init-form (current-date))
     (local-file-headers :init-form '()
			 :accessor %zip-archive-headers)))

  #|
  Local file header (little endian)

  offset - byte
  0 - 4 signature = #x04034b50
  4 - 2 version   = #x20
  6 - 2 general purpos = #x00
  8 - 2 compression method
  10 - 2 file last modification time
  12 - 2 file last modification date
  14 - 4 CRC-32
  18 - 4 compressed size
  22 - 4 uncompressed size
  26 - 2 file name length (n)
  28 - 2 extra field length (m)
  30 - n file name
  30+n - m extra field

  example:

  00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F
  -----------------------------------------------
  50 4b 03 04 14 00 bb bb mm 00 tt tt dd dd CC CC
  CC CC ss ss ss ss SS SS SS SS nn nn mm mm FF...
  ...........

  |#
  (define-method write-pk0304 ((za <zip-archive>) (lfh <local-file-header>))
    (put-bytevector (~ za 'port)
		    ;; no alignment, little endian
		    (apply pack "u<L3S4L2S*C"
			   #x04034b50
			   20
			   0
			   (~ lfh 'compress-method)
			   (date->dos-format (~ lfh 'timestamp))
			   (~ lfh 'checksum)
			   (~ lfh 'compressed-size)
			   (~ lfh 'uncompressed-size)
			   (~ lfh 'filename-size)
			   0
			   (bytevector->u8-list (~ lfh 'filename)))))

  (define (open-output-zip-archive filename)
    (let*-values (((dir base ext) (decompose-path filename))
		  ((port tempname) (make-temporary-file 
				    (if dir
					(build-path dir "ziptmp")
					"ziptmp"))))
      (make <zip-archive> :port port :name filename :tempname tempname)))

  (define-method zip-add-entry
    ((za <zip-archive>) (name <string>) (content <string>)
     :key (timestamp (~ za 'timestamp))
     (compression-level Z_DEFAULT_COMPRESSION))
    (zip-add-entry za name (string->utf8 content)
		   :timestamp timestamp :compression-level compression-level))

  (define-method zip-add-entry
    ((za <zip-archive>) (name <string>) (content <bytevector>)
     :key (timestamp (~ za 'timestamp))
     (compression-level Z_DEFAULT_COMPRESSION))
    (zip-add-entry za (string->utf8 name) content
		   :timestamp timestamp :compression-level compression-level))

  (define-method zip-add-entry
    ((za <zip-archive>) (name <bytevector>) (content <bytevector>)
     :key (timestamp (~ za 'timestamp))
     (compression-level Z_DEFAULT_COMPRESSION))
    (let* ((position (port-position (~ za 'port)))
	   (compress-method (if (= compression-level Z_NO_COMPRESSION) 0 8))
	   (compressed
	    (if (= compress-method 0)
		content
		(deflate-bytevector content
		  :window-bits -15
		  :compression-level compression-level)))
	   (local-file-header
	    (make <local-file-header>
	      :compress-method compress-method
	      :timestamp timestamp
	      :checksum (crc32 content)
	      :compressed-size (bytevector-length compressed)
	      :uncompressed-size (bytevector-length content)
	      :filename-size (bytevector-length name)
	      :offset position
	      :filename name)))
      (write-pk0304 za local-file-header)
      (put-bytevector (~ za 'port) compressed)
      (%zip-archive-headers za (cons local-file-header 
				     (%zip-archive-headers za)))))

  (define-method write-pk0102 ((za <zip-archive>) (lfh <local-file-header>))
    (put-bytevector (~ za 'port)
		    (apply pack "u<L4S4L5S2L*C"
			   #x02014b50
			   20 20 0
			   (~ lfh 'compress-method)
			   (date->dos-format (~ lfh 'timestamp))
			   (~ lfh 'checksum)
			   (~ lfh 'compressed-size)
			   (~ lfh 'uncompressed-size)
			   (~ lfh 'filename-size)
			   0 0 0 0 0
			   (~ lfh 'offset)
			   (bytevector->u8-list (~ lfh 'filename)))))

  (define-method zip-close ((za <zip-archive>))
    (let ((cd (port-position (~ za 'port)))
	  (num (length (~ za 'local-file-headers))))
      (for-each (cut write-pk0102 za <>) (reverse (~ za 'local-file-headers)))
      (let1 eoc (port-position (~ za 'port))
	(put-bytevector (~ za 'port)
			(pack "u<L4S2LS"
			      #x06054b50 0 0 num num (- eoc cd) cd 0)))
      (close-output-port (~ za 'port)))
    (rename-file (~ za 'tempname) (~ za 'name)))

  (define-syntax call-with-output-zip-archive
    (syntax-rules ()
      ((_ filename proc)
       (let1 za (open-output-zip-archive filename)
	 (guard (e (else (close-output-port (~ za 'port))
			 (delete-file (~ za 'tempname))
			 (raise e)))
	   (proc za)
	   (zip-close za))))))
)