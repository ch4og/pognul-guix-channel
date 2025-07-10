;;; Copyright © 2025 Nikita Mitasov <mitanick@ya.ru>
;;;
;;; This file is NOT part of GNU Guix.
(define-module (pognul packages ksh93u+m)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages pkg-config))

(define-public ksh93u+m
  (let ((commit "11981f5fd6d6d6cb758ddf176b47126162daa618")
        (revision "0"))
    (package
      (name "ksh93u+m")
      (version (git-version "1.0.10" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ksh93/ksh")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "10r0h2i1n6zh6s0n4pgkv6z44ld77im2ma7p9i6272r0v6jr8d9y"))))
      (outputs '("out" "fun" "include"))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'build
              (lambda* (#:key parallel-build? #:allow-other-keys)
                (setenv "CC"
                        #$(file-append gcc "/bin/gcc"))
                (apply invoke "sh" "bin/package" "make"
                       (if parallel-build?
                           (list (string-append "-j"
                                                (number->string (parallel-job-count))))
                           '()))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "sh" "bin/package" "test"))))
            (replace 'install
              (lambda* (#:key outputs inputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (fun (assoc-ref outputs "fun"))
                       (include (assoc-ref outputs "include"))
                       (lib (string-append out "/lib"))
                       (bin (string-append out "/bin"))
                       (gcc (assoc-ref inputs "gcc-toolchain"))
                       (rpath (string-append lib ":" gcc "/lib")))
                  
                  (invoke "sh" "bin/package" "install" out)

                  (copy-recursively (string-append out "/include") include)
                  (copy-recursively (string-append out "/share/fun") fun)
                  (delete-file-recursively (string-append out "/include"))
                  (delete-file-recursively (string-append out "/share/fun"))

                  (delete-file-recursively (string-append out
                                                          "/share/man/man3"))

                  (for-each (lambda (file)
                              (invoke "patchelf" "--set-rpath" rpath file))
                            (append (find-files lib "\\.so")
                                    (find-files bin ".*")))))))))
      (inputs (list bash-minimal ncurses))
      (native-inputs (list pkg-config patchelf gcc-toolchain))
      (home-page "https://github.com/ksh93/ksh")
      (synopsis "Modern maintained version of KornShell 93")
      (description
       "KornShell is an interactive UNIX command interpreter as well as a
POSIX compliant scripting language which is a superset of sh.
This is a maintained modern fork of the original AT&T KornShell (ksh93).")
      (license epl2.0))))
ksh93u+m
